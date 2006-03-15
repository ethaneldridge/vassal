package VASSAL.counters;

import java.awt.Component;
import java.awt.Shape;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.net.MalformedURLException;
import java.util.ArrayList;

import javax.swing.Box;
import javax.swing.KeyStroke;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.properties.EnumeratedPropertyPrompt;
import VASSAL.build.module.properties.IncrementProperty;
import VASSAL.build.module.properties.NumericPropertyPrompt;
import VASSAL.build.module.properties.PropertyChanger;
import VASSAL.build.module.properties.PropertyPrompt;
import VASSAL.build.module.properties.PropertySetter;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.Configurer;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.tools.SequenceEncoder;

public class DynamicProperty extends Decorator implements PropertyPrompt.DialogParent {

  public static final String ID = "PROP;";

  protected String value;

  protected String key;
  protected boolean numeric;
  protected int minValue;
  protected int maxValue;
  protected boolean wrap;

  private DynamicKeyCommand[] keyCommands;

  public DynamicProperty() {
    this(ID, null);
  }

  public DynamicProperty(String type, GamePiece p) {
    mySetType(type);
    setInner(p);
  }

  public void mySetType(String s) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ';');
    sd.nextToken(); // Skip over command prefix
    decodeConstraints(sd.nextToken(""));
    keyCommands = decodeKeyCommands(sd.nextToken(""));
  }

  protected void decodeConstraints(String s) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
    numeric = sd.nextBoolean(false);
    minValue = sd.nextInt(0);
    maxValue = sd.nextInt(100);
    wrap = sd.nextBoolean(false);
  }

  protected String encodeConstraints() {
    return new SequenceEncoder(',').append(numeric).append(minValue).append(maxValue).append(wrap).getValue();
  }

  protected DynamicKeyCommand[] decodeKeyCommands(String s) {
    ArrayList a = new ArrayList();
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
    while (sd.hasMoreTokens()) {
      a.add(DynamicKeyCommand.decode(sd.nextToken(""), this));
    }
    return (DynamicKeyCommand[]) a.toArray(new DynamicKeyCommand[a.size()]);
  }

  protected String encodeKeyCommands(DynamicKeyCommand[] commands) {
    SequenceEncoder se = new SequenceEncoder(',');
    for (int i = 0; i < commands.length; i++) {
      se.append(commands[i].encode());
    }
    return se.getValue();
  }

  protected PropertyChanger decodeProperty(String string) {
    return null;
  }

  protected String encodeProperty(PropertyChanger p) {
    return null;
  }

  public void draw(java.awt.Graphics g, int x, int y, java.awt.Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  public String getName() {
    return piece.getName();
  }

  public java.awt.Rectangle boundingBox() {
    return piece.boundingBox();
  }

  public Shape getShape() {
    return piece.getShape();
  }

  public Object getProperty(Object key) {
    if (key.equals(getKey())) {
      return getValue();
    }
    return super.getProperty(key);
  }

  public void setProperty(Object key, Object value) {
    if (key.equals(getKey())) {
      setValue((String) value);
    }
    super.setProperty(key, value);
  }

  public String myGetState() {
    return getValue();
  }

  public Component getComponent() {
    return getMap() != null ? getMap().getView().getTopLevelAncestor() : GameModule.getGameModule().getFrame();
  }

  public void mySetState(String state) {
    setValue(state);
  }

  public String getKey() {
    return key;
  }

  public String getValue() {
    return value;
  }

  public void setValue(String value) {
    Stack parent = getParent();
    Map map = getMap();
    this.value = value;
    // If the property has changed the layer to which this piece belongs,
    // re-insert it into the map
    if (map != null) {
      GamePiece outer = Decorator.getOutermost(this);
      if (parent == null || !map.getPieceCollection().canMerge(parent, outer)) {
        map.placeOrMerge(outer, getPosition());
      }
    }
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(key);
    se.append(encodeConstraints());
    se.append(encodeKeyCommands(keyCommands));
    return ID + se.getValue();
  }

  protected KeyCommand[] myGetKeyCommands() {
    return keyCommands;
  }

  public Command myKeyEvent(KeyStroke stroke) {
    ChangeTracker tracker = new ChangeTracker(this);
    for (int i = 0; i < keyCommands.length; i++) {
      if (keyCommands[i].matches(stroke)) {
        setValue(keyCommands[i].propChanger.getNewValue(value));
      }
    }
    Command comm = tracker.getChangeCommand();
    return comm;
  }

  public String getDescription() {
    String s = "Dynamic Property";
    if (getKey() != null && getKey().length() > 0) {
      s += " - " + getKey();
    }
    return s;
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "DynamicProperty.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public int getMaxValue() {
    return maxValue;
  }

  public int getMinValue() {
    return minValue;
  }

  public boolean isNumeric() {
    return numeric;
  }

  public boolean isWrap() {
    return wrap;
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  protected static class Ed implements PieceEditor {

    public Ed(DynamicProperty m) {
    }

    public Component getControls() {
      throw new RuntimeException("not implemented");
    }

    public String getType() {
      throw new RuntimeException("not implemented");
    }

    public String getState() {
      throw new RuntimeException("not implemented");
    }

  }

  /**
   * DynamicKeyCommand A class that represents an action to be performed on a
   * Dynamic property
   */
  protected static class DynamicKeyCommand extends KeyCommand {
    protected PropertyChanger propChanger = null;

    public DynamicKeyCommand(String name, KeyStroke key, GamePiece target, PropertyChanger propChanger) {
      super(name, key, target);
      this.propChanger = propChanger;
    }

    protected static DynamicKeyCommand decode(String s, DynamicProperty target) {
      SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
      String name = sd.nextToken("Command");
      KeyStroke stroke = sd.nextKeyStroke('C');
      PropertyChanger p = null;
      switch (sd.nextChar('P')) {
      case 'R':
        p = target.isNumeric() ? new NumericPropertyPrompt(target, sd.nextToken("Enter new value"), target.getMinValue(), target.getMaxValue())
            : new PropertyPrompt(target, sd.nextToken("Enter new value"));
        break;
      case 'I':
        p = new IncrementProperty(sd.nextInt(1), target.getMinValue(), target.getMaxValue(), target.isWrap());
        break;
      case 'E':
        p = new EnumeratedPropertyPrompt(target,sd.nextToken("Select new value"),sd.nextStringArray(0));
        break;
      case 'P':
      default:
        p = new PropertySetter(sd.nextToken("new value"));
      }
      return new DynamicKeyCommand(name, stroke, Decorator.getOutermost(target), p);
    }

    protected String encode() {
      SequenceEncoder se = new SequenceEncoder(',');
      se.append(getName()).append(getKeyStroke());
      if (propChanger instanceof EnumeratedPropertyPrompt) {
        se.append('E').append(((PropertyPrompt) propChanger).getPrompt()).append(((EnumeratedPropertyPrompt)propChanger).getValidValues());
      }
      else if (propChanger instanceof PropertyPrompt) {
        se.append('R').append(((PropertyPrompt) propChanger).getPrompt());
      }
      else if (propChanger instanceof IncrementProperty) {
        se.append('I').append(((IncrementProperty) propChanger).getIncrement());
      }
      else {
        se.append('P').append(propChanger.getNewValue(null));
      }
      return se.getValue();
    }

  }
  
  /**
   * 
   * Configure a single Dynamic Key Command line
   */
  protected static class DynamicKeyCommandConfigurer extends Configurer {
    protected static final String PLAIN_TYPE = "Set value directly";
    protected static final String INCREMENT_TYPE = "Increment numeric value";
    protected static final String PROMPT_TYPE = "Prompt user";
    protected static final String SELECT_TYPE = "Prompert user to select from list";

    protected StringConfigurer commandConfig;
    protected HotKeyConfigurer keyConfig;
    protected StringEnumConfigurer typeConfig;
    protected StringConfigurer valueConfig;

    protected Box controls = null;
    
    protected DynamicProperty target;
    private static final String[] ALL_TYPES = new String[] { PLAIN_TYPE, INCREMENT_TYPE, PROMPT_TYPE, SELECT_TYPE};
    private static final Class[] ALL_TYPE_CLASSES = new Class[]{PropertyChanger.class, IncrementProperty.class, PropertyPrompt.class, EnumeratedPropertyPrompt.class};
    

    public DynamicKeyCommandConfigurer(DynamicProperty target) {
      super(target.getKey(), target.getName());
      this.target = target;
    }

    public String getValueString() {
      return getKeyCommand().encode();
    }

    public DynamicKeyCommand getKeyCommand() {
      return (DynamicKeyCommand) getValue();
    }

    public void setValue(String s) {
      super.setValue(DynamicKeyCommand.decode(s,target));
    }

    public Component getControls() {
      if (controls == null) {
        buildControls();
      }
      return controls;
    }

    protected void updateValue() {
      setValue(new DynamicKeyCommand(commandConfig.getValueString(), (KeyStroke) keyConfig.getValue(), target, getPropertyChanger()));
    }
    
    protected PropertyChanger getPropertyChanger() {
      throw new RuntimeException("not implemented");
    }

    protected void buildControls() {
      DynamicKeyCommand dkc = getKeyCommand();
      PropertyChangeListener pl = new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent e) {
          updateValue();
        }
      };

      controls = Box.createHorizontalBox();
      commandConfig = new StringConfigurer(null, "Command: ", dkc.getName());
      commandConfig.addPropertyChangeListener(pl);
      controls.add(commandConfig.getControls());
      keyConfig = new HotKeyConfigurer(null, " Key: ", dkc.getKeyStroke());
      keyConfig.addPropertyChangeListener(pl);
      controls.add(keyConfig.getControls());
      typeConfig = new StringEnumConfigurer(null, " Type:  ", ALL_TYPES);
      typeConfig.setValue(getType(dkc));
      typeConfig.addPropertyChangeListener(pl);
      controls.add(typeConfig.getControls());
      valueConfig = new StringConfigurer(null, " Value: ", dkc.propChanger.getNewValue(null));
      valueConfig.addPropertyChangeListener(pl);
      controls.add(valueConfig.getControls());
    }
    
    protected String getType(DynamicKeyCommand c) {
      for (int i = 0; i < ALL_TYPE_CLASSES.length; i++) {
        if (c.getClass().equals(ALL_TYPE_CLASSES[i])) {
          return ALL_TYPES[i];
        }
      }
      return ALL_TYPES[0];
    }
    
    

  }



}
