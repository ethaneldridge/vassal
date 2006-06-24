/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package vp;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Iterator;

import javax.swing.BoxLayout;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;
import javax.swing.border.LineBorder;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.UniqueIdManager;

/**
 * Generic VP Counter
 */
public class VPTracker extends AbstractConfigurable implements GameComponent, UniqueIdManager.Identifyable {

  protected static UniqueIdManager idMgr = new UniqueIdManager("VPTracker");
  
  protected static final String COMMAND_PREFIX = "VP";
  public static final String VERSION = "1.1";

  public static final String NAME = "name";
  public static final String HOT_KEY = "hotkey";
  public static final String ICON = "icon";
  public static final String BUTTON_TEXT = "buttonText";
  public static final String SIDES = "sides";

  protected VPWindow vpWindow;
  protected LaunchButton launch;
  protected JTextArea VPLabel = new JTextArea();
  protected String id;
  protected String[] sides = new String[0];
  protected VPTable table;
  protected ArrayList items = new ArrayList();

  public VPTracker() {
    
    ActionListener al = new ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent e) {
        vpWindow.setVisible(!vpWindow.isShowing());
      }
    };
    launch = new LaunchButton("VP", BUTTON_TEXT, HOT_KEY, ICON, al);
    launch.setToolTipText("VP");
    launch.setEnabled(false);
    
    table = new VPTable();
  }
  
  public void add(Buildable b) {
    super.add(b);
    if (b instanceof VPItem) {
      VPItem item = (VPItem) b;
      item.setColumnCount(sides.length);
      item.setTable(table);
      items.add(item);
      table.restructure();
    }
  }
  
  public void remove(Buildable b) {
    super.remove(b);
    if (b instanceof VPItem) {
      items.remove(b);
      table.restructure();
    }
  }
  
  /*
   * Module level Configuration stuff
   */
  public String[] getAttributeNames() {
    return new String[] { NAME, BUTTON_TEXT, ICON, HOT_KEY, SIDES };
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
      launch.setToolTipText((String) value);
    }
    else if (SIDES.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      sides = ((String[]) value);
      for (int i=0; i < items.size(); i++) {
        getItemAt(i).setColumnCount(sides.length);
      }
      table.restructure();
    }
    else {
      launch.setAttribute(key, value);
    }
  }
  
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName() + "";
    }
    else if (SIDES.equals(key)) {
      return StringArrayConfigurer.arrayToString(sides);
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }
  
  public String[] getAttributeDescriptions() {
    return new String[] { "Name:  ", "Button text:  ", "Button Icon:  ", "Hotkey:  ", "Sides:  "};
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, String.class, IconConfig.class, KeyStroke.class, String[].class};
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, ((VPTracker) c).launch.getAttributeValueString(ICON));
    }
  }

  
  public Class[] getAllowableConfigureComponents() {
    return new Class[] { VPCounter.class };
  }

  public static String getConfigureTypeName() {
    return "VP Tracker v"+VERSION;
  }

  public void addTo(Buildable b) {
    GameModule.getGameModule().getGameState().addGameComponent(this);
    GameModule.getGameModule().getToolBar().add(launch);
    launch.setAlignmentY(0.0F);
    idMgr.add(this);
 
    vpWindow = new VPWindow();    
    vpWindow.pack(); 

  }

  public void removeFrom(Buildable b) {
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    GameModule.getGameModule().getToolBar().remove(launch);
  }

  public HelpFile getHelpFile() {
    return null;
  }
  
  public void setup(boolean gameStarting) {
    launch.setEnabled(gameStarting);
  }
  
  public Command getRestoreCommand() {
    return null;
  }

  public void setId(String id) {
    this.id = id;
  }

  public String getId() {
    return id;
  }

  protected void updateVPDisplay() {
    vpWindow.repaint();
  }

  protected int totalColumn(int col) {
    int total = 0;
    Iterator i = items.iterator();
    while (i.hasNext()) {
      total += ((VPItem) i.next()).getValues()[col];
    }
    return total;
  }
  
  protected VPItem getItemAt(int i) {
    if (i >=0 && i < items.size()) {
      return (VPItem) items.get(i); 
    }
    else {
      return null;
    }
  }
  
  protected class VPWindow extends JDialog  {

    protected static final long serialVersionUID = 1L;
    protected JPanel mainPanel;
    protected JPanel controlPanel;
    protected JPanel VPPanel;


    protected VPWindow() {
      super(GameModule.getGameModule().getFrame());
      initComponents();
    }


    public void setColor(Color color) {
      mainPanel.setBackground(color);
   
    }

    protected void initComponents() {

      setTitle(getConfigureName());

      mainPanel = new JPanel();
      mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
      
      table = new VPTable();
      JScrollPane scroll = new JScrollPane(table, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
      scroll.setBorder(LineBorder.createGrayLineBorder());
      table.setPreferredScrollableViewportSize(new Dimension(400, 70));
      mainPanel.add(scroll);
      
      getContentPane().add(mainPanel);

      pack();
      setLocation(100, 100);
    }    
 
  }

  public class VPTable extends JTable {

    private static final long serialVersionUID = 1L;

    AbstractTableModel model;
    
    public VPTable() {
      model = new MyTableModel(); 
      setModel(model);
      setColumnWidths();
    }
    
    public void restructure() {
      model.fireTableStructureChanged();
      setColumnWidths();
    }

    protected void setColumnWidths() {
      getColumnModel().getColumn(0).setPreferredWidth(160);
      for (int i=1;i < model.getColumnCount(); i++) {
        getColumnModel().getColumn(i).setPreferredWidth(80);  
      }

    }

    public void updateTotals() {
      model.fireTableRowsUpdated(items.size(), items.size());
    }
    
    public void redisplay() {
      repaint();
    }
    
    public TableCellRenderer getCellRenderer(int row, int col) {
      TableCellRenderer defaultRenderer = super.getCellRenderer(row, col);
      if (col > 0 && row < items.size()) {
          return getItemAt(row).getCellRenderer(col-1, defaultRenderer);
      }
      return defaultRenderer;
    }
    
    public TableCellEditor getCellEditor(int row, int col) {
      TableCellEditor defaultEditor = super.getCellEditor(row, col);
      if (col > 0 && row < items.size()) {
          return getItemAt(row).getCellEditor(col-1, defaultEditor);
      }
      return defaultEditor;
    }
    
    public class MyTableModel extends AbstractTableModel {
      private static final long serialVersionUID = 1L;

      public int getColumnCount() {
        return sides.length+1;
      }
      
      public String getColumnName(int col) {
        if (col == 0) {
          return "";
        }
        else {
          return sides[col-1];
        }
      }
      
      /*
       * Only display Totals column if more than one row
       */
      public int getRowCount() {
        return items.size()+ (items.size() > 1 ? 1 : 0);
      }
      
      public Object getValueAt(int row, int col) {
        if (row == items.size()) {
          if (col == 0) {
            return "Total";
          }
          else {
            return new Integer(totalColumn(col-1));
          }
        }
        else {
          if (col == 0) {
            return getItemAt(row).getDescription();
          }
          else {
            return new Integer(getItemAt(row).getValues()[col-1]);
          }
        }
      }

      public void setValueAt(Object value, int row, int col) {      
        if (row > 0 && row < items.size()) {
          getItemAt(row).setValue(col-1, value);
        }
        table.updateTotals();
      }
      
      public Class getColumnClass(int c) {
        if (c == 0) {
          return String.class;
        }
        return Integer.class;
      }
      
      public boolean isCellEditable(int row, int col) {
        if (col == 0 || row == items.size()) {
          return false;
        }
        else {
         return getItemAt(row).isEditable();
        }
      }
      
    }
  }

  public interface VPItem {
    
    public String getDescription();
    public int[] getValues();
    public void setValue(int col, Object value);
    public boolean isEditable();
    public TableCellRenderer getCellRenderer(int column, TableCellRenderer dflt);
    public TableCellEditor getCellEditor(int column, TableCellEditor dflt);
    public void setColumnCount(int columns);
    public void setTable(VPTable t);
    
  }
}
