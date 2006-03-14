package VASSAL.build.module.properties;

import java.awt.Component;

import javax.swing.JOptionPane;

/**
 * Prompts user for a new value
 * @author rkinney
 *
 */
public class PropertyPrompt extends PropertyChanger {
  protected DialogParent dialogParent;
  protected String promptText;
  
  public String getNewValue(String oldValue) {
    return (String) JOptionPane.showInputDialog(dialogParent.getComponent(), promptText, null, JOptionPane.QUESTION_MESSAGE, null, null, oldValue);
  }


  public PropertyPrompt(DialogParent dialogParent, String prompt) {
    this.dialogParent = dialogParent;
    this.promptText = prompt;
  }

  public static interface DialogParent {
    Component getComponent();
  }

  public String getPrompt() {
    return promptText;
  }

}
