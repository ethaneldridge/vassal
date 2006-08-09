package GameTimer;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.KeyStroke;

import Inventory.Inventory.Dest;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.DiceButton.IconConfig;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.tools.LaunchButton;

public class GameTimer extends AbstractConfigurable implements GameComponent {

  protected LaunchButton launch;

  public static final String VERSION = "1.0";
  public static final String HOTKEY = "hotkey";
  public static final String BUTTON_TEXT = "text";
  public static final String NAME = "name";
  public static final String ICON = "icon";
  
  public GameTimer() {
    super();
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
      }
    };
    launch = new LaunchButton(null, BUTTON_TEXT, HOTKEY, ICON, al);
    setAttribute(NAME, "Inventory");
    setAttribute(BUTTON_TEXT, "Inventory");
    launch.setToolTipText(getConfigureName());
    launch.setEnabled(false);
  }
  
  public static String getConfigureTypeName() {
    return "Game Timer v"+VERSION;
  }
  
  public String[] getAttributeDescriptions() {
    return new String[] { "Name", "Button text", "Button icon", "Hotkey", "Report Destination",
        "Include Counters from All Maps?", "Include Counters from these Maps only",
        "Only include counters with Marker", "Sort and Group By Marker", "Total and Report Integer values in Marker" };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, String.class, IconConfig.class, KeyStroke.class, Dest.class, Boolean.class,
        String[].class, String.class, String.class, String.class };
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, BUTTON_TEXT, ICON, HOTKEY};
  }

  public void setAttribute(String key, Object o) {

    if (NAME.equals(key)) {
      setConfigureName((String) o);
      launch.setToolTipText((String) o);
    }
    else {
      launch.setAttribute(key, o);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }

  public void removeFrom(Buildable parent) {
    GameModule.getGameModule().getToolBar().remove(getComponent());
    GameModule.getGameModule().getGameState().removeGameComponent(this);
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void addTo(Buildable parent) {
    launch.setAlignmentY(0.0F);
    GameModule.getGameModule().getToolBar().add(getComponent());
    GameModule.getGameModule().getGameState().addGameComponent(this);
  }

  protected Component getComponent() {
    return launch;
  }
  
  public void setup(boolean gameStarting) {
    launch.setEnabled(gameStarting);   
  }

  public Command getRestoreCommand() {
    return null;
  }
}
