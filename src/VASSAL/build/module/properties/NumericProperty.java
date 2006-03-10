package VASSAL.build.module.properties;

import java.awt.Container;

import javax.swing.JOptionPane;

/**
 * A Property restricted to numeric values. The values are String instances, but
 * must parse to an integer
 * 
 * @author rkinney
 * 
 */
public class NumericProperty extends Property {
  private int minimum = Integer.MIN_VALUE;
  private int maximum = Integer.MAX_VALUE;
  private boolean wrap;
  private int value;

  public NumericProperty(String key, String value) {
    super(key, value);
  }

  /**
   * Maximum value.
   * 
   * @return
   */
  public int getMaximum() {
    return maximum;
  }

  public void setMaximum(int maximum) {
    this.maximum = maximum;
  }

  /**
   * Minimum value.
   * 
   * @return
   */
  public int getMinimum() {
    return minimum;
  }

  public void setMinimum(int minimum) {
    this.minimum = minimum;
  }

  public String getValue() {
    return String.valueOf(value);
  }

  public void setValue(String value) {
    if (value != null) {
      try {
        this.value = Integer.parseInt(value.toString());
      }
      catch (NumberFormatException e) {
        throw new IllegalArgumentException();
      }
    }
  }

  public boolean isValidValue(String value) {
    try {
      Integer.parseInt(value.toString());
      return true;
    }
    catch (NumberFormatException e) {
      return false;
    }
  }

  public boolean isWrap() {
    return wrap;
  }

  public void setWrap(boolean wrap) {
    this.wrap = wrap;
  }

  public void increment(int incr) {
    if (value + incr > maximum && wrap) {
      value = minimum + (value + incr - maximum - 1);
    }
    else if (value + incr < minimum && wrap) {
      value = maximum + (value + incr - minimum + 1);
    }
    else {
      value = Math.min(maximum, value + incr);
    }
  }

  public String prompt(Container dialogParent, String promptText) {
    String s = null;
    do {
      s = (String) JOptionPane.showInputDialog(dialogParent, promptText, null, JOptionPane.QUESTION_MESSAGE, null, null, getValue());
    } while (s != null && !isValidValue(s));
    return s;
  }

}
