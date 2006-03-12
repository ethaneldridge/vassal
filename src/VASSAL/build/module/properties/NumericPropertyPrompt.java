package VASSAL.build.module.properties;


/**
 * Prompts for an integer value
 * @author rkinney
 *
 */
public class NumericPropertyPrompt extends PropertyPrompt {
  private int min;
  private int max;

  public NumericPropertyPrompt(DialogParent dialogParent, String prompt, int minValue, int maxValue) {
    super(dialogParent, prompt);
    min = minValue;
    max = maxValue;
  }

  public String getNewValue(String oldValue) {
    String s = null;
    do {
      s = super.getNewValue(oldValue);
    } while (s != null && !isValidValue(s));
    return s;
  }

  private boolean isValidValue(String s) {
    try {
      int value = Integer.parseInt(s.toString());
      return value <= max && value >= min;
    }
    catch (NumberFormatException e) {
      return false;
    }
  }
  

}
