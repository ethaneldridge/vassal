package vp;

import java.awt.Component;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.AbstractCellEditor;
import javax.swing.JFormattedTextField;
import javax.swing.JSpinner;
import javax.swing.JTable;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;
import javax.swing.JSpinner.DefaultEditor;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;

public class VPCounter extends AbstractConfigurable implements VPTracker.VPItem {

  protected static final String NAME = "name";
  protected static final String MIN = "min";
  protected static final String MAX = "max";
  protected static final String START = "start";
  protected static final String STEP = "step";

  protected SpinnerEditor[] spinners = new SpinnerEditor[0];
  protected int[] values = new int[0];
  protected VPTracker.VPTable table;
  protected int min = 0;
  protected int max = 99;
  protected int start = 0;
  protected int step = 1;

  public VPCounter() {
    super();
  }

  public void setTable(VPTracker.VPTable t) {
    table = t;
  }

  public String[] getAttributeDescriptions() {
    return new String[] {"Name:  ", "Minimum Value:  ", "Maximum Value:  ", "Starting Value:  ", "Step:  "};
  }

  public Class[] getAttributeTypes() {
    return new Class[] {String.class, Integer.class, Integer.class, Integer.class, Integer.class};
  }

  public String[] getAttributeNames() {
    return new String[] {NAME, MIN, MAX, START, STEP};
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (MIN.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      min = ((Integer) value).intValue();
      rebuild();
    }
    else if (MAX.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      max = ((Integer) value).intValue();
      rebuild();
    }
    else if (START.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      start = ((Integer) value).intValue();
      rebuild();
    }
    else if (STEP.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      step = ((Integer) value).intValue();
      rebuild();
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (MIN.equals(key)) {
      return String.valueOf(min);
    }
    else if (MAX.equals(key)) {
      return String.valueOf(max);
    }
    else if (START.equals(key)) {
      return String.valueOf(start);
    }
    else if (STEP.equals(key)) {
      return String.valueOf(step);
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

  public String getDescription() {
    return getConfigureName();
  }

  public void rebuild() {
    setColumnCount(getColumnCount());
  }
  
  public int getColumnCount() {
    return spinners.length;
  }
  
  public void setColumnCount(int columns) {
    spinners = new SpinnerEditor[columns];
    for (int i = 0; i < columns; i++) {
      spinners[i] = new SpinnerEditor();
    }
    values = new int[columns];
  }

  public int[] getValues() {
    for (int i = 0; i < getColumnCount(); i++) {
      values[i] = ((Integer) spinners[i].getCellEditorValue()).intValue();
    }
    return values;
  }

  public void setValue(int col, Object value) {
    if (col >= 0 && col < getColumnCount() && value instanceof Integer) {
      spinners[col].setValue((Integer) value);
    }
  }

  public boolean isEditable() {
    return true;
  }

  public TableCellRenderer getCellRenderer(int col, TableCellRenderer dflt) {
    return dflt;
  }

  public TableCellEditor getCellEditor(int col, TableCellEditor dflt) {
    if (col >= 0 && col < getColumnCount()) {
      return spinners[col];
    }
    return dflt;
  }

  public class SpinnerEditor extends AbstractCellEditor implements TableCellEditor, KeyListener {

    private static final long serialVersionUID = 1L;
    protected SpinnerModel spinnerModel;
    protected JSpinner spinner;
    protected JFormattedTextField spinnerText;

    public SpinnerEditor() {
      spinner = new JSpinner(new SpinnerNumberModel(start, min, max, step));
      spinnerText = ((DefaultEditor) spinner.getEditor()).getTextField();
      spinnerText.addKeyListener(this);
    }

    public void setValue(Integer value) {
      spinner.setValue(value);
    }

    public Object getCellEditorValue() {
      return spinner.getValue();
    }

    public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
      spinner.setValue((Integer) value);
      return spinner;
    }

    /*
     * Workaround for JRE Bug - Spinners in JTables not updating correctly when values typed in.
     */
    public void keyReleased(KeyEvent e) {

        try {
          spinnerText.commitEdit();
        }
        catch (Exception ex) {
          spinnerText.setValue(spinner.getValue());
        }

    }

    public void keyPressed(KeyEvent e) {
    }
    
    public void keyTyped(KeyEvent e) {
    }

  }

}
