package VASSAL.build.module.properties;

/**
 * Add an Enumerated property to a Map or Module
 * @author rkinney
 *
 */
public class GlobalEnumeratedProperty extends GlobalProperty {

  protected Property createProperty(String key) {
    EnumeratedProperty p = new EnumeratedProperty(key,property.getValue());
    if (property instanceof EnumeratedProperty) {
      p.setValidValues(((EnumeratedProperty)property).getValidValues());
    }
    return p;
  }

}
