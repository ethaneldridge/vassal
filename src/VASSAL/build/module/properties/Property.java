package VASSAL.build.module.properties;

import java.awt.Container;

import javax.swing.JOptionPane;

/**
 * A key-value pair
 * 
 * @author rkinney
 * 
 */
public class Property {
  private String key;
  private String value;
  private String description;

  public Property(String key, String value, String description) {
    this.key = key;
    this.value = value;
    this.description = description;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public String getValue() {
    return value;
  }

  public void setValue(String value) {
    if (!isValidValue(value)) {
      throw new IllegalArgumentException();
    }
    this.value = value;
  }

  public String getKey() {
    return key;
  }

  public boolean isValidValue(String value) {
    return true;
  }

  public String prompt(Container dialogParent, String promptText) {
    String s = (String) JOptionPane.showInputDialog(dialogParent, promptText, null, JOptionPane.QUESTION_MESSAGE, null, null, getValue());
    return s;
  }

}
