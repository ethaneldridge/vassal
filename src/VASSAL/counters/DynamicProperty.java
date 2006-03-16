package VASSAL.counters;

import java.awt.Component;
import java.awt.Shape;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.net.MalformedURLException;

import javax.swing.Box;
import javax.swing.KeyStroke;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.properties.PropertyChanger;
import VASSAL.build.module.properties.PropertyChangerConfigurer;
import VASSAL.build.module.properties.PropertyPrompt;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.ListConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.SequenceEncoder;

public class DynamicProperty extends Decorator implements PropertyPrompt.DialogParent, PropertyChangerConfigurer.Constraints {

  public static final String ID = "PROP;";

  protected String value;

  protected String key;
  protected boolean numeric;
  protected int minValue;
  protected int maxValue;
  protected boolean wrap;

  private DynamicKeyCommand[] keyCommands;

  private ListConfigurer keyCommandListConfig;

  public DynamicProperty() {
    this(ID, null);
  }

  public DynamicProperty(String type, GamePiece p) {
    mySetType(type);
    setInner(p);
    keyCommandListConfig = new ListConfigurer(null, "Commands") {
      protected Configurer buildChildConfigurer() {
        return new DynamicKeyCommandConfigurer(DynamicProperty.this);
      }
    };
  }

  public void mySetType(String s) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ';');
    sd.nextToken(); // Skip over command prefix
    decodeConstraints(sd.nextToken(""));
    keyCommandListConfig.setValue(sd.nextToken(""));
    keyCommands = (DynamicKeyCommand[]) keyCommandListConfig.getListValue().toArray(new DynamicKeyCommand[keyCommandListConfig.getListValue().size()]);
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
    se.append(encodeConstraints());
    se.append(keyCommandListConfig.getValueString());
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
    protected StringConfigurer initialValueConfig;
    protected BooleanConfigurer numericConfig;
    protected IntConfigurer minConfig;
    protected IntConfigurer maxConfig;
    protected BooleanConfigurer wrapConfig;
    protected ListConfigurer keyCommandListConfig;
    protected Box controls;

    public Ed(final DynamicProperty m) {
      keyCommandListConfig = new ListConfigurer(null,"Commands") {
        protected Configurer buildChildConfigurer() {
          return new DynamicKeyCommandConfigurer(m);
        }
      };
      PropertyChangeListener l = new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          boolean isNumeric = numericConfig.booleanValue().booleanValue();
          minConfig.getControls().setVisible(isNumeric);
          maxConfig.getControls().setVisible(isNumeric);
          wrapConfig.getControls().setVisible(isNumeric);
        }
      };
      controls = Box.createVerticalBox();
      initialValueConfig = new StringConfigurer(null,"Value: ",m.getValue());
      controls.add(initialValueConfig.getControls());
      numericConfig = new BooleanConfigurer(null,"Is numeric: ",m.isNumeric());
      controls.add(numericConfig.getControls());
      minConfig = new IntConfigurer(null,"Minimum value: ",new Integer(m.getMinValue()));
      controls.add(initialValueConfig.getControls());
      maxConfig = new IntConfigurer(null,"Maximum value: ",new Integer(m.getMaxValue()));
      controls.add(initialValueConfig.getControls());
      wrapConfig = new BooleanConfigurer(null,"Wrap ",m.isWrap());
      controls.add(wrapConfig.getControls());
      controls.add(keyCommandListConfig.getControls());
      
      numericConfig.addPropertyChangeListener(l);
      numericConfig.fireUpdate();
    }

    public Component getControls() {
      return controls;
    }

    protected String encodeConstraints() {
      return new SequenceEncoder(',').append(numericConfig.getValueString()).append(minConfig.getValueString()).append(maxConfig.getValueString()).append(
          wrapConfig.getValueString()).getValue();
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(encodeConstraints());
      se.append(keyCommandListConfig.getValueString());
      return ID + se.getValue();
    }

    public String getState() {
      return initialValueConfig.getValueString();
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

  }

  /**
   * 
   * Configure a single Dynamic Key Command line
   */
  protected static class DynamicKeyCommandConfigurer extends Configurer {
    protected HotKeyConfigurer keyConfig;
    protected PropertyChangerConfigurer propChangeConfig;
    protected StringConfigurer commandConfig;

    protected Box controls = null;
    protected DynamicProperty target;

    public DynamicKeyCommandConfigurer(DynamicProperty target) {
      super(target.getKey(), target.getName());
      commandConfig = new StringConfigurer(null, "Menu Command:  ");
      keyConfig = new HotKeyConfigurer(null, "Key Command:  ");
      propChangeConfig = new PropertyChangerConfigurer(null, null, target);
      this.target = target;
      propChangeConfig = new PropertyChangerConfigurer(null, "Action:  ", target);
    }

    public String getValueString() {
      SequenceEncoder se = new SequenceEncoder(':');
      se.append(commandConfig.getValueString()).append(keyConfig.getValueString()).append(propChangeConfig.getValueString());
      return se.getValue();
    }

    public DynamicKeyCommand getKeyCommand() {
      return (DynamicKeyCommand) getValue();
    }

    public void setValue(String s) {
      SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s == null ? "" : s, ':');
      commandConfig.setValue(sd.nextToken(""));
      keyConfig.setValue(sd.nextToken(""));
      propChangeConfig.setValue(sd.nextToken(""));
      updateValue();
    }

    public Component getControls() {
      if (controls == null) {
        buildControls();
      }
      return controls;
    }

    protected void updateValue() {
      setValue(new DynamicKeyCommand(commandConfig.getValueString(), (KeyStroke) keyConfig.getValue(), target, propChangeConfig.getPropertyChanger()));
    }

    protected void buildControls() {
      controls = Box.createHorizontalBox();
      controls.add(keyConfig.getControls());
      controls.add(propChangeConfig.getControls());
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
      controls.add(propChangeConfig.getControls());
    }

  }

}
