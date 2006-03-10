package VASSAL.build.module.properties;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Adds a NumericProperty to a Map or Module
 * @author rkinney
 *
 */
public class GlobalNumericProperty extends GlobalProperty {

  protected Property createProperty(String key) {
    return new NumericProperty(key,property.getValue());
  }

  public Class[] getAllowableConfigureComponents() {
    List l = new ArrayList(Arrays.asList(super.getAllowableConfigureComponents()));
    l.add(IncrementPropertyButton.class);
    return (Class[]) l.toArray(new Class[l.size()]);
  }

  public void increment(int incr) {
    String oldValue = getPropertyValue();
    ((NumericProperty)property).increment(incr);
    propertyChangeSupport.firePropertyChange(property.getKey(),oldValue,property.getValue());
  }

}
