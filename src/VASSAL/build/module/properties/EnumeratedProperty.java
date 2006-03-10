package VASSAL.build.module.properties;

import java.awt.Container;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JOptionPane;

/**
 * A Property with values restricted to a given set of Strings
 * @author rkinney
 *
 */
public class EnumeratedProperty extends Property {
  private List validValues = new ArrayList();

  public EnumeratedProperty(String key, String value) {
    super(key, value);
  }

  public List getValidValues() {
    return validValues;
  }

  public void setValidValues(List validValues) {
    this.validValues = validValues;
  }

  public boolean isValidValue(String value) {
    return validValues.contains(value);
  }

  public String prompt(Container dialogParent, String promptText) {
    Object[] values = validValues.toArray();
    String s = (String) JOptionPane.showInputDialog(dialogParent, promptText, null, JOptionPane.QUESTION_MESSAGE, null,values,getValue());
    return s;
  }
  
  
}
