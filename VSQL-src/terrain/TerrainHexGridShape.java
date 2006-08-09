/*
 * 
 */
package terrain;

import java.awt.Color;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;

public class TerrainHexGridShape extends AbstractConfigurable {
  
  protected static final String NAME = "name";
  

  public String[] getAttributeNames() {
    return new String[] {NAME};
  }

  public String[] getAttributeDescriptions() {
    return new String[] {"Name:  "};
  }

  public Class[] getAttributeTypes() {
    return new Class[] {String.class, Color.class};
  }
  
  public void setAttribute(String key, Object value) {
    if(NAME.equals(key)) {
      setConfigureName((String) value);
    }
  }

  public String getAttributeValueString(String key) {
    if(NAME.equals(key)) {
      return getConfigureName();
    }
    return null;
  }
  
  public void addTo(Buildable b) {

  }

  public void removeFrom(Buildable b) {

  }

  public HelpFile getHelpFile()  {
      return null;
  }

  public static String getConfigureTypeName() {
    return "Terrain Range";
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

}