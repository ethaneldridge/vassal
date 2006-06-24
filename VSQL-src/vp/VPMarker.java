package vp;

import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import vp.VPTracker.VPTable;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;

public class VPMarker extends AbstractConfigurable implements VPTracker.VPItem {

  public VPMarker() {
    super();
  }

  public String[] getAttributeDescriptions() {
    // TODO Auto-generated method stub
    return null;
  }

  public Class[] getAttributeTypes() {
    // TODO Auto-generated method stub
    return null;
  }

  public String[] getAttributeNames() {
    // TODO Auto-generated method stub
    return null;
  }

  public void setAttribute(String key, Object value) {
    // TODO Auto-generated method stub
    
  }

  public String getAttributeValueString(String key) {
    // TODO Auto-generated method stub
    return null;
  }

  public String getDescription() {
    // TODO Auto-generated method stub
    return null;
  }

  public int[] getValues() {
    // TODO Auto-generated method stub
    return null;
  }

  public void setValue(int col, Object value) {
    // TODO Auto-generated method stub
    
  }

  public boolean isEditable() {
    // TODO Auto-generated method stub
    return false;
  }

  public TableCellRenderer getCellRenderer(int column, TableCellRenderer dflt) {
    // TODO Auto-generated method stub
    return null;
  }

  public TableCellEditor getCellEditor(int column, TableCellEditor dflt) {
    // TODO Auto-generated method stub
    return null;
  }

  public void setColumnCount(int columns) {
    // TODO Auto-generated method stub
    
  }

  public void setTable(VPTable t) {
    // TODO Auto-generated method stub
    
  }

  public void removeFrom(Buildable parent) {
    // TODO Auto-generated method stub
    
  }

  public HelpFile getHelpFile() {
    // TODO Auto-generated method stub
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    // TODO Auto-generated method stub
    return null;
  }

  public void addTo(Buildable parent) {
    // TODO Auto-generated method stub
    
  }
}