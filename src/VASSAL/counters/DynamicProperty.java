package VASSAL.counters;

import java.awt.Component;
import java.awt.Shape;
import java.io.File;
import java.net.MalformedURLException;
import java.util.ArrayList;

import javax.swing.Box;
import javax.swing.KeyStroke;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.CompoundPieceCollection;
import VASSAL.build.module.map.PieceCollection;
import VASSAL.build.module.properties.PropertyChanger;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.AutoConfigurer;
import VASSAL.tools.SequenceEncoder;

public class DynamicProperty extends Decorator {

  public static final String ID = "PROP;";

  protected String reportFormat;

  protected String value;

  private String key;
  
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
    sd.nextToken(); //Skip over command prefix
    keyCommands = decodeKeyCommands(sd.nextToken(""));
    reportFormat = sd.nextToken("");
  }

  protected DynamicKeyCommand[] decodeKeyCommands(String s) {
  ArrayList a = new ArrayList();
  SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
  while (sd.hasMoreTokens()) {
    a.add(DynamicKeyCommand.decode(sd.nextToken("")));
  }
  return (DynamicKeyCommand[]) a.toArray(new DynamicKeyCommand[a.size()]);
  }
  
  protected String encodeKeyCommands(DynamicKeyCommand[] commands) {
    SequenceEncoder se = new SequenceEncoder(',');
    for (int i = 0; i < commands.length; i++) {
      se.append(commands[i].getValueString());
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
    saveCurrentLayer();
    this.value = value;
    checkForLayerChange();
  }
  
  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(key);
    se.append(encodeKeyCommands(keyCommands));
    return ID+se.getValue();
  }

  protected KeyCommand[] myGetKeyCommands() {
    KeyCommand[] keys = new KeyCommand[keyCommands.length];
    for (int i = 0; i < keyCommands.length; i++) {
      DynamicKeyCommand command = keyCommands[i];
      keys[i] = new KeyCommand(command.getCommand(), command.getKey(), Decorator.getOutermost(this));
    }
    return keys;
  }

  public Command myKeyEvent(KeyStroke stroke) {
    saveCurrentLayer();
    ChangeTracker tracker = new ChangeTracker(this);
    for (int i = 0; i < keyCommands.length; i++) {
      DynamicKeyCommand keyCommand = keyCommands[i];
      if (keyCommand.getKey().equals(stroke)) {
        apply(keyCommand);
      }
    }
    Command comm = tracker.getChangeCommand();
    checkForLayerChange();
    return comm;
  }
  
  /**
   * Apply a Dynamic Key Command to this Property
   * 
   * @param command
   */
  protected void apply(DynamicKeyCommand command) {
    return;
  }


  public String getDescription() {
    String s = "Dynamic Property";
    if (getKey() != null && getKey().length() > 0) {
      s += " - " + getKey();
    }
    return s;
  }

  /*
   * Changing the value of a DynamicProperty attached to a counter
   * may cause the piece to change layers.
   */
  protected int initialLayer;
  
  protected void checkForLayerChange() {
    if (getLayer() != initialLayer) {
      if (getMap() != null) {
        /*
         * Re-stack piece if necessary!!!
         */
      }
    }
  }

  protected void saveCurrentLayer() {
    initialLayer = getLayer(); 
  }
  
  protected int getLayer() {
    int layer = 0;
    if (getMap() != null) {
      PieceCollection pc = getMap().getPieceCollection();
      if (pc instanceof CompoundPieceCollection) {
        layer = ((CompoundPieceCollection) pc).getLayerForPiece(Decorator.getOutermost(this));
      }
    }
    return layer;
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

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  protected static class Ed implements PieceEditor {

    VASSAL.build.module.map.DynamicProperty edit;
    
    public Ed(DynamicProperty m) {
      edit = new VASSAL.build.module.map.DynamicProperty(m.myGetType());
      edit.setValue(m.getValue());
    }
    
    public Component getControls() {
      return new AutoConfigurer(edit).getControls();
    }

    public String getType() {
      return edit.getType();
    }

    public String getState() {
      return edit.getValue();
    }

  }
  
  /**
   * DynamicKeyCommand A class that represents an action to be performed on a
   * Dynamic property
   */
  protected static class DynamicKeyCommand {
    public static final String SET_TYPE = "Set";
    public static final String INC_TYPE = "Increase";
    public static final String DEC_TYPE = "Decrease";

    public static final String VALUE_TYPE = "Value";
    public static final String ASK_TYPE = "Ask";


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

    public static DynamicKeyCommand decode(String string) {
      // TODO Auto-generated method stub
      return null;
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
}
