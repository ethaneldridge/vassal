package VASSAL.build.module;

import java.util.Vector;
import java.util.Enumeration;
import java.io.File;
import java.net.MalformedURLException;

import VASSAL.build.*;
import VASSAL.build.module.documentation.HelpFile;

public class SpecialDie extends AbstractConfigurable {

  private Vector dieFaceList = new Vector();
  protected boolean bNumeric = false;

  public static final String NAME = "name";
  public static final String NUMERIC = "numeric";


  /**
   * Find a SpecialDie below any SpecialDiceButton
   * @param name
   * @return the {@link SpecialDie} with the given name
   */
  public static SpecialDie findSpecialDie(String name) {
    for (Enumeration e = GameModule.getGameModule().getComponents(SpecialDie.class); e.hasMoreElements();) {
      SpecialDie s = (SpecialDie) e.nextElement();
      if (s.getConfigureName().equals(name)) {
        return s;
      }
    }
    for (Enumeration e = GameModule.getGameModule().getComponents(SpecialDiceButton.class); e.hasMoreElements();) {
      SpecialDiceButton b = (SpecialDiceButton) e.nextElement();
      for (Enumeration e2 = b.getComponents(SpecialDie.class); e2.hasMoreElements();) {
        SpecialDie s = (SpecialDie) e2.nextElement();
        if (s.getConfigureName().equals(name)) {
          return s;
        }
      }
    }
    return null;
  }

  public void addFace(SpecialDieFace f) {
    dieFaceList.addElement(f);
  }

  public void removeFace(SpecialDieFace f) {
    dieFaceList.removeElement(f);
  }

  public static String getConfigureTypeName() {
    return "Symbolic Die";
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Name", "is numeric?"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class, Boolean.class};
  }

  public String[] getAttributeNames() {
    String s[] = {NAME, NUMERIC};
    return s;
  }

  public void setAttribute(String key, Object o) {
    if (NAME.equals(key)) {
      setConfigureName((String) o);
    }
    else if (NUMERIC.equals(key)) {
      if (o instanceof Boolean) {
        bNumeric = ((Boolean) o).booleanValue();
      }
      else if (o instanceof String) {
        bNumeric = "true".equals(o);
      }
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (NUMERIC.equals(key)) {
      return "" + bNumeric;
    }
    else {
      return null;
    }
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "GameModule.htm"), "#SpecialDiceButton");
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[]{SpecialDieFace.class};
  }

  public void addTo(Buildable parent) {
  }

  public void removeFrom(Buildable parent) {
  }

  public boolean isNumeric() {
    return bNumeric;
  }

  // look if all faces has an image stored
  public boolean hasImages() {
    for (int ii = 0; ii < dieFaceList.size(); ii++) {
      if (null == ((SpecialDieFace) dieFaceList.get(ii)).getAttributeValueString(SpecialDieFace.ICON))
        return false;
    }
    return true;
  }

  public int getSides() {
    return dieFaceList.size();
  }

  public String getStrVal(int pRoll) {
    if (pRoll > 0 && pRoll <= dieFaceList.size()) {
      SpecialDieFace aFace = (SpecialDieFace) dieFaceList.get(pRoll - 1);
      return aFace.strValue;
    }
    else
      return null;
  }

  public int getIntVal(int pRoll) {
    if (bNumeric && pRoll > 0 && pRoll <= dieFaceList.size()) {
      SpecialDieFace aFace = (SpecialDieFace) dieFaceList.get(pRoll - 1);
      return Integer.parseInt(aFace.strValue);
    }
    else
      return pRoll;
  }

  public String getImageName(int pRoll) {
    if (pRoll > 0 && pRoll <= dieFaceList.size()) {
      SpecialDieFace aFace = (SpecialDieFace) dieFaceList.get(pRoll - 1);
      return aFace.getAttributeValueString(SpecialDieFace.ICON);
    }
    else
      return null;
  }

}