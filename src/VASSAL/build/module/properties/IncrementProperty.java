package VASSAL.build.module.properties;

/**
 * Increments a property by a given value
 * 
 * @author rkinney
 * 
 */
public class IncrementProperty extends PropertyChanger {
  private int incr;
  private int min;
  private int max;
  private boolean wrap;

  public IncrementProperty(int incr, int min, int max, boolean wrap) {
    super();
    this.incr = incr;
    this.min = min;
    this.max = max;
    this.wrap = wrap;
  }

  public String getNewValue(String oldValue) {
    try {
      int value = Integer.parseInt(oldValue);
      if (wrap) {
        if (value + incr > max) {
          value = min + (value + incr - max - 1);
        }
        else if (value + incr < min) {
          value = max + (value + incr - min + 1);
        }
      }
      else {
        value += incr;
        value = Math.min(max, value);
        value = Math.max(min, value);
      }
      return String.valueOf(value);
    }
    catch (NumberFormatException e) {
      return oldValue;
    }
  }

  public int getIncrement() {
    return incr;
  }

}
