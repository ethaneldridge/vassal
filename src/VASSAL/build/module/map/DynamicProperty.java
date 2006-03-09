/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.build.module.map;

import java.awt.Component;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Iterator;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.PropertyProducer;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyCommand;
import VASSAL.tools.FormattedString;
import VASSAL.tools.PlayerIdFormattedString;
import VASSAL.tools.SequenceEncoder;

/**
 * DynamicProperty exposes the value a single value via its getProperty(name)
 * call, and supports an arbitrary list of commands to manipulate the value.
 * 
 * Current Key Command types supported are:
 * 
 *  Set - Set to a specified value. 
 *  Increase - Increment by the specified value. 
 *  Decrease - Decrement by the specified value.
 * 
 * The value to Set/Increment/Decrement can be one of: 
 *  - Fixed Value. 
 *  - Value of another Marker, System Property or Dynamic Property. 
 *  - Requested from the player.
 * 
 * Dynamic property can be used as either a counter trait and as a launch button
 * component of Maps and the GameModule, under the control of intermediate
 * adapter classes:
 * 
 *   CounterDynamicProperty - Counter trait version MapProperties - Map version
 *   ModuleProperties - Module version.
 * 
 * Key Commands are exposed as right-click menu options for counter trait's, or
 * as sub-menus on a pop-up menu for launch button components.
 * 
 * ******* NOTE ********
 * If you add or remove an AbstractConfigurable attribute, you MUST also modify 
 * getType() and setType()
 * *********************
 *  
 */
public class DynamicProperty extends AbstractConfigurable implements GameComponent, CommandEncoder, PropertyProducer,
    ActionListener {

  public static final String COMMAND_PREFIX = "PROP";

  public static final String OLD_VALUE = "oldValue";
  public static final String NEW_VALUE = "newValue";
  public static final String PROPERTY_NAME = "propertyName";
  public static final String DESCRIPTION = "description";
  public static final String COMMAND = "command";

  public static final String NO_RESTRICTION = "No Restrictions";
  public static final String NUMBER_RESTRICTION = "Whole Numbers Only";
  public static final String LIST_RESTRICTION = "List of Valid Values";

  public static final String NAME = "name";
  public static final String VALUE = "value";
  public static final String DESC = "desc";
  public static final String REPORT_FORMAT = "reportFormat";
  public static final String RESTRICT = "restrict";
  public static final String VALID_VALUES = "validValues";
  public static final String WRAP = "wrap";
  public static final String HAS_MINIMUM = "hasMin";
  public static final String MINIMUM = "min";
  public static final String HAS_MAXIMUM = "hasMax";
  public static final String MAXIMUM = "max";
  public static final String KEY_COMMANDS = "keyCommands";

  protected static final String DEFAULT_REPORT_FORMAT = "$" + DESCRIPTION + "$ changed from $" + OLD_VALUE + "$ to $"
      + NEW_VALUE + "$";
  protected String myValue = "";
  protected String previousValue = "";
  protected String description = "Description";
  protected String reportFormat = "";

  protected String restriction = NO_RESTRICTION;
  protected ArrayList dynamicKeyCommands = new ArrayList();
  protected static DynamicProperty instance;
  protected MapProperties globalParent = null;
  protected GamePiece counterParent = null;
  protected boolean inMiniConfig = false;

  // List Type
  protected String[] validValues = new String[0];
  protected boolean allowWrapAround = false;

  // Counter Type
  protected boolean hasMinimum = false;
  protected int minimum = 0;
  protected boolean hasMaximum = false;
  protected int maximum = 9;

  public DynamicProperty() {
    super();
    instance = this;
  }

  public DynamicProperty(String type) {
    this();
    setType(type);
  }

  public DynamicProperty(GamePiece piece) {
    this();
    counterParent = piece;
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Property Name:  ", "Property Value:  ", "Description:  ", "Report Format:  ",
        "Restrict Values to:  ", "Valid Values:  ", "Allow List to Wrap Around?", "Enforce a Minimum Value?",
        "Minimum Value:  ", "Enforce a Maximum Value?", "Maximum Value:  ", "Key Commands" };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, String.class, String.class, ReportFormatConfig.class, TypeConfig.class,
        String[].class, Boolean.class, Boolean.class, Integer.class, Boolean.class, Integer.class,
        DynamicKeyCommandConfig.class };
  }

  public static class ReportFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[] { OLD_VALUE, NEW_VALUE, PROPERTY_NAME,
          DESCRIPTION, COMMAND });
    }
  }

  public static class DynamicKeyCommandConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      DynamicProperty p = (DynamicProperty) c;
      return p.new DynamicKeyCommandArrayConfigurer(key, name, p.getDynamicKeyCommands());
    }
  }

  public static class TypeConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { NO_RESTRICTION, NUMBER_RESTRICTION, LIST_RESTRICTION };
    }
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, VALUE, DESC, REPORT_FORMAT, RESTRICT, VALID_VALUES, WRAP, HAS_MINIMUM, MINIMUM,
        HAS_MAXIMUM, MAXIMUM, KEY_COMMANDS };
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (VALUE.equals(key)) {
      setValue((String) value);
    }
    else if (DESC.equals(key)) {
      description = (String) value;
    }
    else if (RESTRICT.equals(key)) {
      restriction = (String) value;
    }
    else if (VALID_VALUES.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      validValues = (String[]) value;
    }
    else if (WRAP.equals(key)) {
      if (value instanceof String) {
        value = new Boolean((String) value);
      }
      allowWrapAround = ((Boolean) value).booleanValue();
    }
    else if (HAS_MINIMUM.equals(key)) {
      if (value instanceof String) {
        value = new Boolean((String) value);
      }
      hasMinimum = ((Boolean) value).booleanValue();
    }
    else if (MINIMUM.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      minimum = ((Integer) value).intValue();
    }
    else if (HAS_MAXIMUM.equals(key)) {
      if (value instanceof String) {
        value = new Boolean((String) value);
      }
      hasMaximum = ((Boolean) value).booleanValue();
    }
    else if (MAXIMUM.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      maximum = ((Integer) value).intValue();
    }
    else if (KEY_COMMANDS.equals(key)) {
      if (value instanceof String) {
        value = StringToDynamicKeyArray((String) value);
      }
      dynamicKeyCommands = (ArrayList) value;
    }
    else if (REPORT_FORMAT.equals(key)) {
      reportFormat = (String) value;
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (DESC.equals(key)) {
      return description;
    }
    else if (VALUE.equals(key)) {
      return getValue();
    }
    else if (RESTRICT.equals(key)) {
      return restriction;
    }
    else if (VALID_VALUES.equals(key)) {
      return StringArrayConfigurer.arrayToString(validValues);
    }
    else if (WRAP.equals(key)) {
      return String.valueOf(allowWrapAround);
    }
    else if (HAS_MINIMUM.equals(key)) {
      return String.valueOf(hasMinimum);
    }
    else if (MINIMUM.equals(key)) {
      return String.valueOf(minimum);
    }
    else if (HAS_MAXIMUM.equals(key)) {
      return String.valueOf(hasMaximum);
    }
    else if (MAXIMUM.equals(key)) {
      return String.valueOf(maximum);
    }
    else if (KEY_COMMANDS.equals(key)) {
      return DynamicKeyArrayToString(dynamicKeyCommands);
    }
    else if (REPORT_FORMAT.equals(key)) {
      return reportFormat;
    }
    return null;
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    VisibilityCondition cond = null;
    if (inMiniConfig) {
      if (VALUE.equals(name)) {
        cond = new VisibilityCondition() {
          public boolean shouldBeVisible() {
            return true;
          }
        };
      }
      else {
        cond = new VisibilityCondition() {
          public boolean shouldBeVisible() {
            return false;
          }
        };
      }
    }
    else if (VALID_VALUES.equals(name) || WRAP.equals(name)) {
      cond = new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return restriction.equals(LIST_RESTRICTION);
        }
      };
    }
    else if (HAS_MINIMUM.equals(name) || HAS_MAXIMUM.equals(name)) {
      cond = new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return restriction.equals(NUMBER_RESTRICTION);
        }
      };
    }
    else if (MINIMUM.equals(name)) {
      cond = new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return restriction.equals(NUMBER_RESTRICTION) && hasMinimum;
        }
      };
    }
    else if (MAXIMUM.equals(name)) {
      cond = new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return restriction.equals(NUMBER_RESTRICTION) && hasMaximum;
        }
      };
    }
    else if (REPORT_FORMAT.equals(name)) {
      cond = new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return isGlobal();
        }
      };
    }
    return cond;
  }

  public void removeFrom(Buildable parent) {
    GameModule.getGameModule().removeCommandEncoder(this);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void addTo(Buildable parent) {
    GameModule.getGameModule().addCommandEncoder(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    this.globalParent = (MapProperties) parent;
    reportFormat = isGlobal() ? DEFAULT_REPORT_FORMAT : "";
  }

  protected boolean isGlobal() {
    return (globalParent != null);
  }

  protected boolean isList() {
    return restriction.equals(LIST_RESTRICTION);
  }

  protected boolean isInteger() {
    return restriction.equals(NUMBER_RESTRICTION);
  }

  public static String getConfigureTypeName() {
    return "Dynamic Property";
  }

  public Object getProperty(Object key) {
    if (key.equals(getConfigureName())) {
      return getValue();
    }
    else if (key.equals("old" + getConfigureName())) {
      return getPreviousValue();
    }
    return null;
  }

  public void setProperty(Object key, Object value) {
    if (key.equals(getConfigureName())) {
      setValue(value.toString());
    }
  }

  public String getName() {
    return getConfigureName();
  }

  public String getDescription() {
    return description;
  }

  public ArrayList getDynamicKeyCommands() {
    return dynamicKeyCommands;
  }

  public void setup(boolean gameStarting) {
    return;
  }

  public Command getRestoreCommand() {
    return new SetGlobalProperty(this, "", getValue());
  }

  public Command decode(String command) {
    Command comm = null;

    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(command, ';');
    String prefix = sd.nextToken("");
    if (prefix.equals(COMMAND_PREFIX)) {
      String marker = sd.nextToken("");
      if (marker.equals(getConfigureName())) {
        comm = new SetGlobalProperty(this, getValue(), sd.nextToken(""));
      }
    }
    return comm;
  }

  public String encode(Command c) {
    String s = null;
    if (c instanceof SetGlobalProperty ) {
      if (((SetGlobalProperty) c).getTargetName().equals(getConfigureName())) {
        SequenceEncoder se = new SequenceEncoder(COMMAND_PREFIX, ';');
        se.append(getConfigureName());
        se.append(((SetGlobalProperty) c).newValue);
        s = se.getValue();
      }
    }
    return s;
  }

  /**
   * DynamicProperty is also used to provide editing for the Counter trait
   * version (CounterDynamicProperty).
   */
  public void setType(String type) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(type, ';');
    sd.nextToken(); //Skip over command prefix
    setConfigureName(sd.nextToken(""));
    restriction = sd.nextToken(NO_RESTRICTION);
    validValues = sd.nextStringArray(0);
    allowWrapAround = sd.nextBoolean(false);
    hasMinimum = sd.nextBoolean(false);
    minimum = sd.nextInt(0);
    hasMaximum = sd.nextBoolean(false);
    maximum = sd.nextInt(9);
    dynamicKeyCommands = StringToDynamicKeyArray(sd.nextToken(""));
    description = sd.nextToken("Menu Command");
    reportFormat = sd.nextToken(isGlobal() ? DEFAULT_REPORT_FORMAT : "");
  }

  public String getType() {
    SequenceEncoder se = new SequenceEncoder(COMMAND_PREFIX, ';');
    se.append(getConfigureName());
    se.append(restriction);
    se.append(validValues);
    se.append(allowWrapAround);
    se.append(hasMinimum);
    se.append(minimum);
    se.append(hasMaximum);
    se.append(maximum);
    se.append(DynamicKeyArrayToString(dynamicKeyCommands));
    se.append(description);
    se.append(reportFormat);
    return se.getValue();
  }

  /**
   * Give the property a new value. For a counter trait, redisplay of the
   * counter is handled automatically by the Decorator KeyEvent handler. For
   * Global Properties, tell the parent to redisplay the appropriate maps.
   */
  public void setValue(String s) {
    previousValue = myValue;
    myValue = s;
    if (globalParent != null) {
      globalParent.childWasUpdated();
    }
  }

  /**
   * Setting a new value. If restricted to integers, ignore non-integers, check
   * for min/max. If restricted to List of value, ignore values not in list
   */
  public void setNewValue(String valueString) {

    String newValue = valueString;

    if (restriction.equals(NUMBER_RESTRICTION)) {
      try {
        int i = Integer.parseInt(valueString);
        if (hasMinimum && i < minimum) {
          i = minimum;
        }
        if (hasMaximum && i > maximum) {
          i = maximum;
        }
        newValue = Integer.toString(i);
      }
      catch (Exception e) {
        return;
      }
    }
    else if (restriction.equals(LIST_RESTRICTION)) {
      boolean found = false;
      for (int i = 0; i < validValues.length && !found; i++) {
        found = validValues[i].equals(valueString);
      }
      if (!found) {
        try {
          int idx = Integer.parseInt(valueString);
          if (idx >= 0 && idx < getValidValueCount()) {
            newValue = validValues[idx];
            found = true;
          }
        }
        catch (Exception e) {

        }
        if (!found) {
          return;
        }
      }
    }
    setValue(newValue);
  }

  /**
   * Incrementing/Decrementing
   */
  public void addToValue(String valueString, int parity) {
    if (restriction.equals(NUMBER_RESTRICTION) || restriction.equals(NO_RESTRICTION)) {
      try {
        int newValue = Integer.parseInt(getValue()) + parity * Integer.parseInt(valueString);
        setNewValue(Integer.toString(newValue));
      }
      catch (Exception e) {
        // Do nothing on error
      }
    }
    else if (restriction.equals(LIST_RESTRICTION)) {
      int currentIndex = getListIndex();
      try {
        int newIndex = currentIndex + parity * Integer.parseInt(valueString);
        if (newIndex < 0) {
          if (allowWrapAround) {
            newIndex = newIndex % getValidValueCount();
            if (newIndex < 0) {
              newIndex = getValidValueCount() + newIndex;
            }
          }
          else {
            newIndex = 0;
          }
        }
        else if (newIndex > getValidValueCount() - 1) {
          if (allowWrapAround) {
            newIndex = newIndex % getValidValueCount();
          }
          else {
            newIndex = getValidValueCount() - 1;
          }
        }
        setNewValue(validValues[newIndex]);
      }
      catch (Exception e) {
        //      Do nothing on error
      }
    }

  }

  protected int getListIndex() {
    int index = -1;
    for (int i = 0; i < validValues.length && index < 0; i++) {
      if (validValues[i].equals(getValue())) {
        index = i;
      }
    }
    return index;
  }

  protected int getValidValueCount() {
    return validValues.length;
  }

  public String getValue() {
    return myValue;
  }

  public String getPreviousValue() {
    return previousValue;
  }

  public KeyCommand[] getKeyCommands(GamePiece target) {
    KeyCommand[] keys = new KeyCommand[dynamicKeyCommands.size()];
    for (int i = 0; i < dynamicKeyCommands.size(); i++) {
      DynamicKeyCommand command = (DynamicKeyCommand) dynamicKeyCommands.get(i);
      keys[i] = new KeyCommand(command.getCommand(), command.getKey(), target);
    }
    return keys;
  }

  /**
   * DynamicProperty is being used as a property trait and user has selected
   * from the right click menu on the counter.
   */
  public Command keyEvent(KeyStroke stroke) {
    for (int i = 0; i < dynamicKeyCommands.size(); i++) {
      DynamicKeyCommand keyCommand = (DynamicKeyCommand) dynamicKeyCommands.get(i);
      if (keyCommand.getKey().equals(stroke)) {
        apply(keyCommand);
      }
    }
    return null;
  }

  /**
   * DynamicProperty is being used as a Global Property and user has selected a
   * command from the launch bar button. If the value has changed, report and
   * sendAndLog a change command,
   */
  public void actionPerformed(ActionEvent e) {
    String command = e.getActionCommand();
    for (int i = 0; i < dynamicKeyCommands.size(); i++) {
      DynamicKeyCommand dynamicKeyCommand = (DynamicKeyCommand) dynamicKeyCommands.get(i);
      if (dynamicKeyCommand.getCommand().equals(command)) {
        String oldValue = getValue();
        apply(dynamicKeyCommand);
        if (!oldValue.equals(getValue())) {
          FormattedString format = new PlayerIdFormattedString(reportFormat);
          format.setProperty(PROPERTY_NAME, getName());
          format.setProperty(DESCRIPTION, getDescription());
          format.setProperty(OLD_VALUE, oldValue);
          format.setProperty(NEW_VALUE, getValue());
          format.setProperty(COMMAND, command);
          Command c = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), format.getText());
          c.execute();
          c.append(new SetGlobalProperty(this, oldValue, getValue()));
          GameModule.getGameModule().sendAndLog(c);
        }
      }
    }
  }

  /**
   * Apply a Dynamic Key Command to this Property
   * 
   * @param command
   */
  protected void apply(DynamicKeyCommand command) {

    String commandType = command.getCommandType();
    String valueType = command.getValueType();
    String newValue = command.getAdjustValue();

    /*
     * Ask player for a new value?
     */
    if (valueType.equals(ASK_TYPE)) {
      newValue = askValue();

    }
    /*
     * Expand any property strings in new value
     */
    newValue = expandMarkers(newValue);

    if (commandType.equals(SET_TYPE)) { // Set
      setNewValue(newValue);
    }
    else if (commandType.equals(INC_TYPE)) { // Increase
      addToValue(newValue, 1);
    }
    else if (commandType.equals(DEC_TYPE)) { // Decrease
      addToValue(newValue, -1);
    }

    return;
  }

  protected String askValue() {

    String s = (String) JOptionPane.showInputDialog(null, getDescription(), null, JOptionPane.QUESTION_MESSAGE, null,
        restriction.equals(LIST_RESTRICTION) ? validValues : null, getValue());
    return s;
  }

  /**
   * Expand any $property_name$ property markers. If this DynamicProperty
   * belongs to a Map, or a module, then create an intermediate dummy GamePiece
   * to pass to FormattedString.getText() to expand any Map or Module level
   * property names.
   */
  public String expandMarkers(String s) {
    FormattedString f = new FormattedString(s);
    if (counterParent != null) {
      return f.getText(counterParent);
    }
    else if (globalParent != null) {
      GamePiece dummy = new DummyGamePiece(globalParent);
      return f.getText(dummy);
    }
    return s;
  }

  protected class DummyGamePiece extends BasicPiece {

    MapProperties parent;

    public DummyGamePiece(MapProperties parent) {
      this.parent = parent;
    }

    public Object getProperty(Object key) {
      Map map = parent.getMap();
      Object value = map != null ? map.getProperty(key) : null;
      if (value== null) {
        value = GameModule.getGameModule().getProperty(key);
      }
      return value;
    }

  }

  /**
   * Command to pass a new Global property value to other players or into the
   * logfile.
   */
  protected static class SetGlobalProperty extends Command {

    protected String newValue;
    protected String oldValue;
    protected DynamicProperty target;
    protected String targetName;

    protected SetGlobalProperty(DynamicProperty target, String oldV, String newV) {

      oldValue = oldV;
      newValue = newV;
      this.target = target;
    }

    public String getTargetName() {
      return target == null ? "" : target.getConfigureName();
    }
    
    protected void executeCommand() {
      target.setValue(newValue);
    }

    protected Command myUndoCommand() {
      return new SetGlobalProperty(target, newValue, oldValue);
    }
  }

  /**
   * DynamicKeyCommand A class that represents an action to be performed on a
   * Dynamic property
   */
  public static final String SET_TYPE = "Set";
  public static final String INC_TYPE = "Increase";
  public static final String DEC_TYPE = "Decrease";

  public static final String VALUE_TYPE = "Value";
  public static final String ASK_TYPE = "Ask";

  protected class DynamicKeyCommand {

    protected final char DELIM = '|';

    protected String command = "";
    protected KeyStroke key;
    protected String type = SET_TYPE;
    protected String valueType = VALUE_TYPE;
    protected String value = "";
    protected Box controls = null;

    public DynamicKeyCommand() {
      this("Command", null, SET_TYPE, VALUE_TYPE, "");
    }

    public DynamicKeyCommand(String command, KeyStroke key, String type, String valueType, String value) {
      this.command = command;
      this.key = key;
      this.type = type;
      this.valueType = valueType;
      this.value = value;
    }

    public DynamicKeyCommand(String line) {
      SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(line, DELIM);
      command = sd.nextToken("");
      key = sd.nextKeyStroke('S');
      type = sd.nextToken(SET_TYPE);
      valueType = sd.nextToken(VALUE_TYPE);
      value = sd.nextToken("");
    }

    public String getValueString() {
      SequenceEncoder se = new SequenceEncoder(DELIM);
      se.append(command);
      se.append(key);
      se.append(type);
      se.append(valueType);
      se.append(value);
      return se.getValue();
    }

    public String getCommand() {
      return command;
    }

    public KeyStroke getKey() {
      return key;
    }

    public String getCommandType() {
      return type;
    }

    public String getValueType() {
      return valueType;
    }

    public String getAdjustValue() {
      return value;
    }

  }

  /**
   * Utility routines converting between String and DynamicKeyCommand arrays.
   */
  public static String DynamicKeyArrayToString(ArrayList keys) {
    SequenceEncoder se = new SequenceEncoder(',');
    Iterator i = keys.iterator();
    while (i.hasNext()) {
      se.append(((DynamicKeyCommand) i.next()).getValueString());
    }
    return se.getValue();
  }

  public static ArrayList StringToDynamicKeyArray(String s) {
    ArrayList a = new ArrayList();
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
    while (sd.hasMoreTokens()) {
      a.add(instance.new DynamicKeyCommand(sd.nextToken("")));
    }
    return a;
  }

  /**
   * Configure a varying length list of Dynamic Key Commands lines.
   */
  public static final String ADD_ACTION = "Add";
  public static final String DEL_ACTION = "Remove";

  protected class DynamicKeyCommandArrayConfigurer extends Configurer implements ActionListener {

    protected Box controls;
    protected Box arrayBox;
    protected Box keyBox = null;
    protected Configurer[] configurers;

    public DynamicKeyCommandArrayConfigurer(String key, String name, Object val) {
      super(key, name, val);
    }

    public ArrayList getValueArrayList() {
      return (ArrayList) getValue();
    }

    public String getValueString() {
      return DynamicKeyArrayToString(getValueArrayList());
    }

    public void setValue(String s) {
      setValue(StringToDynamicKeyArray(s));
    }

    protected void updateValue() {
      ArrayList newArray = new ArrayList();
      for (int i = 0; i < configurers.length; i++) {
        newArray.add(new DynamicKeyCommand(configurers[i].getValueString()));
      }
      setValue(newArray);
    }

    public Component getControls() {
      if (controls == null) {
        buildControls();
      }
      return controls;
    }

    protected void buildControls() {
      controls = Box.createVerticalBox();
      controls.setBorder(BorderFactory.createEtchedBorder());
      controls.add(new JLabel(getName()));

      arrayBox = Box.createVerticalBox();
      arrayBox.setBorder(BorderFactory.createEtchedBorder());
      updateControls();
      controls.add(arrayBox);

      Box buttonBox = Box.createHorizontalBox();
      JButton addButton = new JButton(ADD_ACTION);
      addButton.addActionListener(this);
      buttonBox.add(addButton);
      JButton delButton = new JButton(DEL_ACTION);
      delButton.addActionListener(this);
      buttonBox.add(delButton);

      controls.add(buttonBox);
    }

    public void actionPerformed(ActionEvent e) {
      String action = e.getActionCommand();

      if (action.equals(ADD_ACTION)) {
        getValueArrayList().add(new DynamicKeyCommand());
        updateControls();
      }
      else if (action.equals(DEL_ACTION)) {
        ArrayList a = getValueArrayList();
        if (a.size() > 0) {
          a.remove(a.size() - 1);
          updateControls();
        }
      }

    }

    protected void updateControls() {
      if (keyBox != null) {
        arrayBox.remove(keyBox);
      }
      keyBox = Box.createVerticalBox();
      ArrayList keys = getValueArrayList();
      configurers = new Configurer[keys.size()];
      int idx = 0;
      Iterator i = keys.iterator();
      while (i.hasNext()) {
        DynamicKeyCommand d = (DynamicKeyCommand) i.next();
        Configurer c = new DynamicKeyCommandConfigurer(null, "", d);
        c.addPropertyChangeListener(new PropertyChangeListener() {
          public void propertyChange(PropertyChangeEvent arg0) {
            updateValue();
          }
        });
        keyBox.add(c.getControls());
        configurers[idx++] = c;
      }
      arrayBox.add(keyBox);
      repack();
    }

    protected void repack() {
      Window w = SwingUtilities.getWindowAncestor(controls);
      if (w != null) {
        w.pack();
      }
    }
  }

  /**
   * 
   * Configure a single Dynamic Key Command line
   */
  protected class DynamicKeyCommandConfigurer extends Configurer {

    protected StringConfigurer commandConfig;
    protected HotKeyConfigurer keyConfig;
    protected StringEnumConfigurer typeConfig;
    protected StringEnumConfigurer valueTypeConfig;
    protected StringConfigurer valueConfig;

    protected Box controls = null;

    public DynamicKeyCommandConfigurer(String key, String name, Object val) {
      super(key, name, val);
    }

    public String getValueString() {
      return ((DynamicKeyCommand) getValue()).getValueString();
    }

    public DynamicKeyCommand getValueDynamicKeyCommand() {
      return (DynamicKeyCommand) getValue();
    }

    public void setValue(String s) {
      super.setValue(new DynamicKeyCommand(s));
    }

    public Component getControls() {
      if (controls == null) {
        buildControls();
      }
      return controls;
    }

    protected void updateValue() {
      setValue(new DynamicKeyCommand(commandConfig.getValueString(), (KeyStroke) keyConfig.getValue(), typeConfig
          .getValueString(), valueTypeConfig.getValueString(), valueConfig.getValueString()));
    }

    protected void buildControls() {
      DynamicKeyCommand dkc = getValueDynamicKeyCommand();
      PropertyChangeListener pl = new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent e) {
          updateValue();
        }
      };

      controls = Box.createHorizontalBox();
      commandConfig = new StringConfigurer(null, "Command: ", dkc.command);
      commandConfig.addPropertyChangeListener(pl);
      controls.add(commandConfig.getControls());
      keyConfig = new HotKeyConfigurer(null, " Key: ", dkc.key);
      keyConfig.addPropertyChangeListener(pl);
      controls.add(keyConfig.getControls());
      typeConfig = new StringEnumConfigurer(null, " Action:  ", new String[] { SET_TYPE, INC_TYPE, DEC_TYPE });
      typeConfig.setValue(dkc.type);
      typeConfig.addPropertyChangeListener(pl);
      controls.add(typeConfig.getControls());
      valueTypeConfig = new StringEnumConfigurer(null, " Type:  ", new String[] { VALUE_TYPE, ASK_TYPE });
      valueTypeConfig.setValue(dkc.valueType);
      valueTypeConfig.addPropertyChangeListener(pl);
      controls.add(valueTypeConfig.getControls());
      valueConfig = new StringConfigurer(null, " Value: ", dkc.value);
      valueConfig.addPropertyChangeListener(pl);
      controls.add(valueConfig.getControls());
    }

  }

 

}
