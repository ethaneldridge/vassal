package VASSAL.build.module;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;

public class SpecialDieFace extends AbstractConfigurable {

  public static final String NAME = "name";
  public static final String VALUE = "value";
  public static final String ICON = "icon";
  public static final String IMAGE = "image";

  protected SpecialDie myDie;
  protected String strValue;
  protected static IconConfigurer ic;
  protected static String imageName;
  protected String iconName;

  public static String getConfigureTypeName() {
    return "Symbolic Die Face";
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Name", "Value", "Icon"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{
      String.class,
      String.class,
      IconConfig.class};
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(
        AutoConfigurable c,
        String key,
        String name) {
      ic = new IconConfigurer(key, name, imageName);
      return ic;
    }
  }


  public String[] getAttributeNames() {
    String s[] = {NAME, VALUE, ICON};
    return s;
  }

  public void setAttribute(String key, Object o) {
    if (NAME.equals(key)) {
      setConfigureName((String) o);
    }
    else if (VALUE.equals(key)) {
      strValue = (String) o;
    }
    else if (ICON.equals(key)) {
      iconName = (String) o;
      imageName = "/images/" + iconName;
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (VALUE.equals(key)) {
      return strValue;
    }
    else if (ICON.equals(key)) {
      return iconName;
    }
    else if (IMAGE.equals(key)) {
      return imageName;
    }
    else
      return null;
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void addTo(Buildable parent) {
    myDie = (SpecialDie) parent;
    myDie.addFace(this);
  }

  public void removeFrom(Buildable parent) {
    ((SpecialDie) parent).removeFace(this);
  }
}