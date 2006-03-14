package VASSAL.build.module.properties;

import java.awt.Component;
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
import VASSAL.command.Command;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.FormattedString;
import VASSAL.tools.LaunchButton;

/**
 * Adds a toolbar button that changes the value of a global property
 * 
 * @author rkinney
 * 
 */
public class ChangePropertyButton extends AbstractConfigurable implements PropertyPrompt.DialogParent {
  public static final String BUTTON_TEXT = "text";
  public static final String BUTTON_ICON = "icon";
  public static final String HOTKEY = "hotkey";
  public static final String PROMPT = "prompt";
  public static final String PROMPT_TEXT = "promptText";
  public static final String NEW_VALUE = "value";

  public static final String REPORT_FORMAT = "reportFormat";
  public static final String OLD_VALUE_FORMAT = "oldValue";
  public static final String NEW_VALUE_FORMAT = "newValue";
  public static final String DESCRIPTION_FORMAT = "description";
  public static final String TYPE = "restriction";
  public static final String VALID_VALUES = "validValues";

  protected static final String PLAIN_TYPE = "Set value directly";
  protected static final String INCREMENT_TYPE = "Increment numeric value";
  protected static final String PROMPT_TYPE = "Prompt user";
  protected static final String SELECT_TYPE = "Prompert user to select from list";

  protected LaunchButton launch;
  protected PropertyChanger propChanger;
  protected String promptText;
  protected String type;
  protected String[] validValues;
  protected FormattedString report = new FormattedString();
  protected String value;
  protected GlobalProperty property;

  public ChangePropertyButton() {
    launch = new LaunchButton("Change", BUTTON_TEXT, HOTKEY, BUTTON_ICON, new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        launch();
      }
    });
  }

  public void launch() {
    String oldValue = (String) property.getPropertyValue();
    String newValue = getNewValue();
    if (newValue != null && !newValue.equals(oldValue)) {
      Command c = new GlobalProperty.SetGlobalProperty(property, oldValue, newValue);
      property.setPropertyValue(newValue);
      if (report.getFormat().length() > 0) {
        report.setProperty(OLD_VALUE_FORMAT, oldValue);
        report.setProperty(NEW_VALUE_FORMAT, property.getPropertyValue());
        report.setProperty(DESCRIPTION_FORMAT, property.getDescription());
        c.append(new Chatter.DisplayText(GameModule.getGameModule().getChatter(), report.getText()));
      }
      GameModule.getGameModule().sendAndLog(c);
    }
  }

  protected String getNewValue() {
    return getPropertyChanger().getNewValue(property.getPropertyValue());
  }

  public PropertyChanger getPropertyChanger() {
    if (propChanger == null) {
      PropertyChanger pc = null;
      if (PLAIN_TYPE.equals(type)) {
        pc = new PropertyChanger(value);
      }
      else if (PROMPT_TYPE.equals(type)) {
        if (property.isNumeric()) {
          pc = new NumericPropertyPrompt(this, promptText, property.getMinValue(), property.getMaxValue());
        }
        else {
          pc = new PropertyPrompt(this, promptText);
        }
      }
      else if (INCREMENT_TYPE.equals(type)) {
        try {
          int value = Integer.parseInt(this.value);
          pc = new IncrementProperty(value, property.getMinValue(), property.getMaxValue(), property.isWrap());
        }
        catch (NumberFormatException e) {
          pc = new PropertyChanger(value);
        }
      }
      propChanger = pc;
    }
    return propChanger;
  }

  public String[] getAttributeDescriptions() {
    return new String[] {"Button text", "Button icon", "Hotkey", "Report format", "Prompt text", "Restrictions", "Value", "Valid values"};
  }

  public Class[] getAttributeTypes() {
    return new Class[] {String.class, Icon.class, KeyStroke.class, ReportFormatConfig.class, String.class, Restrictions.class, String.class, String[].class};
  }

  public String[] getAttributeNames() {
    return new String[] {BUTTON_TEXT, BUTTON_ICON, HOTKEY, REPORT_FORMAT, PROMPT_TEXT, TYPE, NEW_VALUE, VALID_VALUES};
  }

  public static class ReportFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[] {OLD_VALUE_FORMAT, NEW_VALUE_FORMAT});
    }
  }

  public static class Restrictions extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      if (((ChangePropertyButton) target).property.isNumeric()) {
        return new String[] {PLAIN_TYPE, INCREMENT_TYPE, PROMPT_TYPE, SELECT_TYPE};
      }
      else {
        return new String[] {PLAIN_TYPE, PROMPT_TYPE, SELECT_TYPE};
      }
    }
  }

  public void setAttribute(String key, Object value) {
    if (PROMPT_TEXT.equals(key)) {
      promptText = (String) value;
    }
    else if (NEW_VALUE.equals(key)) {
      this.value = (String) value;
      propChanger = null;
    }
    else if (REPORT_FORMAT.equals(key)) {
      report.setFormat((String) value);
    }
    else if (TYPE.equals(key)) {
      type = (String) value;
      propChanger = null;
    }
    else if (VALID_VALUES.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      validValues = (String[]) value;
      propChanger = null;
    }
    else {
      launch.setAttribute(key, value);
    }
  }

  public String getAttributeValueString(String key) {
    if (PROMPT_TEXT.equals(key)) {
      return promptText;
    }
    else if (NEW_VALUE.equals(key)) {
      return this.value;
    }
    else if (REPORT_FORMAT.equals(key)) {
      return report.getFormat();
    }
    else if (TYPE.equals(key)) {
      return type;
    }
    else if (VALID_VALUES.equals(key)) {
      return StringArrayConfigurer.arrayToString(validValues);
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (NEW_VALUE.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return PLAIN_TYPE.equals(type) || INCREMENT_TYPE.equals(type);
        }
      };
    }
    if (PROMPT_TEXT.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return PROMPT_TYPE.equals(type);
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

  public static String getConfigureTypeName() {
    return "Change-property Toolbar Button";
  }

  public Component getComponent() {
    return launch;
  }

}
