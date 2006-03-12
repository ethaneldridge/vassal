package VASSAL.build.module.properties;

import javax.swing.JOptionPane;

/**
 * Prompts user to select from a list
 * @author rkinney
 *
 */
public class EnumeratedPropertyPrompt extends PropertyPrompt {
  private String[] validValues;

  public EnumeratedPropertyPrompt(DialogParent dialogParent, String prompt, String[] validValues) {
    super(dialogParent, prompt);
    this.validValues = validValues;
  }

  public String getNewValue(String oldValue) {
    return (String) JOptionPane.showInputDialog(dialogParent.getComponent(), promptText, null, JOptionPane.QUESTION_MESSAGE, null,validValues,oldValue);
  }
  
  
  

}
