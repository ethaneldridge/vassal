package VASSAL.build.module.properties;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Adds a toolbar button that increments a global numeric property
 * @author rkinney
 *
 */
public class IncrementPropertyButton extends ChangePropertyButton {
  public static final String INCREMENT = "increment";
  protected int incr=0;

  public String[] getAttributeDescriptions() {
    List l = new ArrayList(Arrays.asList(super.getAttributeDescriptions()).subList(0,3));
    l.add("Increment by value");
    return (String[]) l.toArray(new String[l.size()]);
  }

  public String[] getAttributeNames() {
    List l = new ArrayList(Arrays.asList(super.getAttributeDescriptions()).subList(0,3));
    l.add(INCREMENT);
    return (String[]) l.toArray(new String[l.size()]);
  }

  public Class[] getAttributeTypes() {
    List l = new ArrayList(Arrays.asList(super.getAttributeDescriptions()).subList(0,3));
    l.add(Integer.class);
    return (Class[]) l.toArray(new Class[l.size()]);
  }

  public String getAttributeValueString(String key) {
    if (INCREMENT.equals(key)) {
      return String.valueOf(incr);
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  public void launch() {
    ((GlobalNumericProperty)property).increment(incr);
  }

  public void setAttribute(String key, Object value) {
    if (INCREMENT.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String)value);
      }
      incr = ((Integer)value).intValue();
    }
    else {
      super.setAttribute(key,value);
    }
  }
  
  public static String getConfigureTypeName() {
    return "Increment-property Toolbar Button";
  }

}
