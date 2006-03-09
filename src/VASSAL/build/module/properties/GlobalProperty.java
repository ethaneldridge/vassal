package VASSAL.build.module.properties;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.tools.ToolBarComponent;

public class GlobalProperty extends AbstractConfigurable {
  public static final String NAME = "name";
  public static final String INITIAL_VALUE = "initialValue";
  public static final String DESCRIPTION = "description";
  private Property property = new Property(null, null, null);
  private String initialValue;
  private ToolBarComponent toolbarComponent;

  public String[] getAttributeDescriptions() {
    return new String[] {"Name", "Initial value", "Description"};
  }

  public Class[] getAttributeTypes() {
    return new Class[] {String.class, String.class, String.class};
  }

  public String[] getAttributeNames() {
    return new String[] {NAME, INITIAL_VALUE, DESCRIPTION};
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
      property = new Property((String) value, property.getValue(), property.getDescription());
    }
    else if (INITIAL_VALUE.equals(key)) {
      initialValue = (String) value;
    }
    else if (DESCRIPTION.equals(key)) {
      property.setDescription((String) value);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (INITIAL_VALUE.equals(key)) {
      return initialValue;
    }
    else if (DESCRIPTION.equals(key)) {
      return property.getDescription();
    }
    return null;
  }

  public void removeFrom(Buildable parent) {
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[]{ChangePropertyButton.class};
  }

  public void addTo(Buildable parent) {
    toolbarComponent = (ToolBarComponent) parent;
  }

  public ToolBarComponent getToolbarComponent() {
    return toolbarComponent;
  }

  public Property getProperty() {
    return property;
  }

}
