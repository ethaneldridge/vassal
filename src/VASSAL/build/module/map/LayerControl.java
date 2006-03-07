package VASSAL.build.module.map;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Icon;
import javax.swing.KeyStroke;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.LaunchButton;

/**
 * Adds a button to the toolbar to manipulate Game Piece Layers
 * 
 * @author Brent Easton
 *  
 */
public class LayerControl extends AbstractConfigurable {

  public static final String BUTTON_TEXT = "text";
  public static final String BUTTON_ICON = "icon";
  public static final String BUTTON_HOTKEY = "hotkey";
  public static final String COMMAND = "command";
  public static final String SKIP = "skip";
  public static final String LAYERS = "layers";

  public static final String CMD_ROTATE_UP = "Rotate Layer Order Up";
  public static final String CMD_ROTATE_DN = "Rotate Layer Order Down";
  public static final String CMD_ENABLE = "Make Layer Active";
  public static final String CMD_DISABLE = "Make Layer InActive";
  public static final String CMD_TOGGLE = "Switch Layer between Active and Inactive";
  public static final String CMD_RESET = "Reset All Layers";

  protected LaunchButton launch;
  protected static final String[] COMMANDS = new String[] {CMD_ROTATE_UP, CMD_ROTATE_DN, CMD_RESET, CMD_ENABLE, CMD_DISABLE, CMD_TOGGLE};
  protected String command = CMD_RESET;
  protected boolean skip = true;
  protected Map map;
  protected String[] layers = new String[0];

  public LayerControl() {
    launch = new LaunchButton("Reset Layers", BUTTON_TEXT, BUTTON_HOTKEY, BUTTON_ICON, new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        launch();
      }
    });
  }

  public void launch() {
    if (map != null && ! (map.getPieceCollection() instanceof CompoundPieceCollection)) {
      return;
    }
    CompoundPieceCollection collection = (CompoundPieceCollection) map.getPieceCollection();
    if (command.equals(CMD_RESET)) {
      collection.reset();
    }
    else if (command.equals(CMD_ROTATE_UP)) {
      collection.rotate(true, skip);
    }
    else if (command.equals(CMD_ROTATE_DN)) {
      collection.rotate(false, skip); 
    }
    else if (command.equals(CMD_ENABLE)) {
      for (int i = 0; i < layers.length; i++) {
        try {
          int l = Integer.parseInt(layers[i]);
          collection.setLayerEnabled(i, true);
        }
        catch (Exception e) {
          collection.setLayerEnabled(layers[i], true);
        }
      }
    }
    else if (command.equals(CMD_DISABLE)) {
      for (int i = 0; i < layers.length; i++) {
        try {
          int l = Integer.parseInt(layers[i]);
          collection.setLayerEnabled(i, false);
        }
        catch (Exception e) {
          collection.setLayerEnabled(layers[i], false);
        }
      }
    }
    else if (command.equals(CMD_TOGGLE)) {
      for (int i = 0; i < layers.length; i++) {
        try {
          int l = Integer.parseInt(layers[i]);
          collection.toggleLayerEnabled(i);
        }
        catch (Exception e) {
          collection.toggleLayerEnabled(layers[i]);
        }
      }
    }
    else {
      return;
    }
    map.repaint();
  }

  public LaunchButton getLaunchButton() {
    return launch;
  }

  public void setMap (Map m) {
    map = m;
  }
  
  public String[] getAttributeDescriptions() {
    return new String[] {"Button text", "Button Icon", "Hotkey", "Action", "Skip layers with no counters?", "Affect which layers? (Use layer names or numbers)"};
  }

  public Class[] getAttributeTypes() {
    return new Class[] {String.class, Icon.class, KeyStroke.class, CommandConfig.class, Boolean.class, String[].class};
  }

  public static class CommandConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return COMMANDS;
    }
  }
  
  public String[] getAttributeNames() {
    return new String[] {BUTTON_TEXT, BUTTON_ICON, BUTTON_HOTKEY, COMMAND, SKIP, LAYERS};
  }

  public String getAttributeValueString(String key) {
    if (COMMAND.equals(key)) {
      return command;
    }
    else if (SKIP.equals(key)) {
      return String.valueOf(skip);
    }
    else if (LAYERS.equals(key)) {
      return StringArrayConfigurer.arrayToString(layers);
    }
    else  {
      return launch.getAttributeValueString(key);
    }
  }

  public void setAttribute(String key, Object value) {
    if (COMMAND.equals(key)) {
      command = (String) value;
    }
    else if (SKIP.equals(key)) {
      if (value instanceof String) {
        value = new Boolean((String) value);
      }
      skip = ((Boolean) value).booleanValue();
    }
    else if (LAYERS.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      layers = (String[]) value;
    }
    else {
      launch.setAttribute(key, value);
    }
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (SKIP.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return command.equals(CMD_ROTATE_UP) || command.equals(CMD_ROTATE_DN);
        }
      };
    }
    else if (LAYERS.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return command.equals(CMD_ENABLE) || command.equals(CMD_DISABLE) || command.equals(CMD_TOGGLE);
        }
      };
    }
    else {
      return null;
    }
  }
  
  public void addTo(Buildable parent) {

  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public static String getConfigureTypeName() {
    return "Game Piece Layer Control";
  }

  public void removeFrom(Buildable parent) {
    if (map != null) {
      map.getToolBar().remove(launch);
    }
  }

}
