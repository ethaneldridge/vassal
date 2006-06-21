package vp;

import javax.swing.JTable;
import javax.swing.table.AbstractTableModel;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;

public class VPCounter extends AbstractConfigurable{
  
  protected static final String NAME = "name";
  protected static final String DESC = "desc";
  
  protected String description = "";
  
  public VPCounter() {
    super();
  }

  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  public Class[] getAttributeTypes() {
    return new Class[] {String.class, String.class};
  }

  public String[] getAttributeNames() {
    return new String[] {NAME, DESC};
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (DESC.equals(key)) {
      description = (String) key;
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (DESC.equals(key)) {
      return description;
    }
    else {
      return null;
    }
  }

  public void removeFrom(Buildable parent) {
    
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void addTo(Buildable parent) {
    
  }
  


}
