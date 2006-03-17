package VASSAL.build.module.properties;

/**
 * Increments a property by a given value
 * 
 * @author rkinney
 * 
 */
public class IncrementProperty implements PropertyChanger {
  protected Constraints constraints;
  protected int incr;

  public IncrementProperty(int incr, Constraints constraints) {
    super();
    this.constraints = constraints;
    this.incr = incr;
  }

  public String getNewValue(String oldValue) {
    try {
      int value = Integer.parseInt(oldValue);
      if (constraints.isWrap()) {
        if (value + incr > constraints.getMaximumValue()) {
          value = constraints.getMinimumValue() + (value + incr - constraints.getMaximumValue() - 1);
        }
        else if (value + incr < constraints.getMinimumValue()) {
          value = constraints.getMaximumValue() + (value + incr - constraints.getMinimumValue() + 1);
        }
      }
      else {
        value += incr;
        value = Math.min(constraints.getMaximumValue(), value);
        value = Math.max(constraints.getMinimumValue(), value);
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
  
  public static interface Constraints {
    int getMinimumValue();
    int getMaximumValue();
    boolean isWrap();
  }

}
