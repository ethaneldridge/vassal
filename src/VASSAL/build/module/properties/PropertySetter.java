package VASSAL.build.module.properties;

/**
 * Provides a fixed value
 * @author rkinney
 *
 */
public class PropertySetter implements PropertyChanger {
  private String newValue;

  public PropertySetter(String newValue) {
    this.newValue = newValue;
  }

  public String getNewValue(String oldValue) {
    return newValue;
  }

  public void setNewValue(String newValue) {
    this.newValue = newValue;
  }


}
