package VASSAL.build.module.properties;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Icon;
import javax.swing.KeyStroke;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.FormattedString;
import VASSAL.tools.LaunchButton;

public class ChangePropertyButton extends AbstractConfigurable {
  public static final String BUTTON_TEXT = "text";
  public static final String BUTTON_ICON = "icon";
  public static final String HOTKEY = "hotkey";
  public static final String PROMPT = "prompt";
  public static final String PROMPT_TEXT = "promptText";
  public static final String NEW_VALUE = "value";

  public static final String REPORT_FORMAT = "reportFormat";
  public static final String OLD_VALUE_FORMAT = "oldValue";
  public static final String NEW_VALUE_FORMAT = "newValue";

  private LaunchButton launch;
  private boolean prompt;
  private String promptText;
  private FormattedString report = new FormattedString();
  private String value;
  private GlobalProperty property;

  public ChangePropertyButton() {
    launch = new LaunchButton("Change", BUTTON_TEXT, HOTKEY, BUTTON_ICON, new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        launch();
      }
    });
  }

  public void launch() {
    String oldValue = (String) property.getPropertyValue();
    String newValue = value;
    if (prompt) {
      newValue = property.prompt(promptText);
    }
    property.setPropertyValue(newValue);
    if (report.getFormat().length() > 0) {
      report.setProperty(OLD_VALUE_FORMAT,oldValue);
      report.setProperty(NEW_VALUE_FORMAT,property.getPropertyValue());
      GameModule.getGameModule().sendAndLog(new Chatter.DisplayText(GameModule.getGameModule().getChatter(),report.getText()));
    }
  }

  public String[] getAttributeDescriptions() {
    return new String[] {"Button text", "Button icon", "Hotkey", "Prompt user for new value", "Prompt text", "Value", "Report format"};
  }

  public Class[] getAttributeTypes() {
    return new Class[] {String.class, Icon.class, KeyStroke.class, Boolean.class, String.class, String.class, ReportFormatConfig.class};
  }

  public static class ReportFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[]{OLD_VALUE_FORMAT,NEW_VALUE_FORMAT});
    }
  }

  public String[] getAttributeNames() {
    return new String[] {BUTTON_TEXT, BUTTON_ICON, HOTKEY, PROMPT, PROMPT_TEXT, NEW_VALUE, REPORT_FORMAT};
  }

  public void setAttribute(String key, Object value) {
    if (PROMPT.equals(key)) {
      prompt = Boolean.TRUE.equals(value) || "true".equals(value);
    }
    else if (PROMPT_TEXT.equals(key)) {
      promptText = (String) value;
    }
    else if (NEW_VALUE.equals(key)) {
      this.value = (String) value;
    }
    else if (REPORT_FORMAT.equals(key)) {
      report.setFormat((String) value);
    }
    else {
      launch.setAttribute(key, value);
    }
  }

  public String getAttributeValueString(String key) {
    if (PROMPT.equals(key)) {
      return String.valueOf(prompt);
    }
    else if (PROMPT_TEXT.equals(key)) {
      return promptText;
    }
    else if (NEW_VALUE.equals(key)) {
      return this.value;
    }
    else if (REPORT_FORMAT.equals(key)) {
      return report.getFormat();
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (NEW_VALUE.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return !prompt;
        }
      };
    }
    if (PROMPT_TEXT.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return prompt;
        }
      };
    }
    return null;
  }

  public void removeFrom(Buildable parent) {
    property.getToolBar().remove(launch);
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void addTo(Buildable parent) {
    property = (GlobalProperty) parent;
    property.getToolBar().add(launch);
  }

}
