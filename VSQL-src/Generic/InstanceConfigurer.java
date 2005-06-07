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

package Generic;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Window;
import java.util.Iterator;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableColumn;

import VASSAL.configure.Configurer;
import VASSAL.configure.StringArrayConfigurer;

/**
 *  
 */
public class InstanceConfigurer extends Configurer {

  protected ImageDefn defn;
  protected Box visBox;
  protected Visualizer visualizer = new Visualizer();
  protected JPanel panel;
  protected TextPanel itemPanel;
  protected SymbolPanel symbolPanel;
  protected InstanceConfigurer me;

  protected InstanceConfigurer() {
    super(null, null);
    me = this;
  }

  protected InstanceConfigurer(String key, String name, ImageDefn defn) {
    super(key, name);
    this.defn = defn;
    setValue(defn.getInstances());
    me = this;
  }

  public String getValueString() {
    return PropertiesToString((InstanceList) value);
  }
  
  public InstanceList getValueInstanceList() {
    return (InstanceList) getValue();
  }

  public void setValue(String s) {
    setValue(StringToProperties(s, defn));
    if (symbolPanel != null) {
      symbolPanel.reset();
    }
  }
  
  public Component getControls() {
    if (panel == null) {

      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

      Box filler = Box.createHorizontalBox();
      filler.setPreferredSize(new Dimension(50, 10));
      panel.add(filler);

      visBox = Box.createHorizontalBox();
      visBox.setAlignmentX(Box.CENTER_ALIGNMENT);
      visualizer = new Visualizer(defn);
      visBox.add(visualizer);
      panel.add(visBox);

      filler = Box.createHorizontalBox();
      filler.setPreferredSize(new Dimension(50, 10));
      panel.add(filler);

      symbolPanel = new SymbolPanel();
      panel.add(symbolPanel);

      filler = Box.createHorizontalBox();
      filler.setPreferredSize(new Dimension(50, 10));
      panel.add(filler);

      itemPanel = new TextPanel();
      panel.add(itemPanel);
    }
    
    return panel;
  }

  public static String PropertiesToString(InstanceList props) {
    String[] p = new String[props.size()];
    Iterator e = props.iterator();
    int i = 0;
    while (e.hasNext()) {
      Instance prop = (Instance) e.next();
      p[i++] = prop.encode();
    }
    return StringArrayConfigurer.arrayToString(p);
  }
  
  public static InstanceList StringToProperties(String s, ImageDefn defn) {
    InstanceList props = new InstanceList();
    String[] p = StringArrayConfigurer.stringToArray(s);
    for (int i = 0; i < p.length; i++) {
      if (p[i].startsWith(SymbolItem.TYPE)) {
        props.add(new SymbolInstance(p[i], defn));
      }
      else if (p[i].startsWith(TextItem.TYPE)) {
        props.add(new TextInstance(p[i], defn));
      }
    }
    return props;
  }
  
  public void refresh() {
    if (symbolPanel != null) {
      symbolPanel.refresh();
    }
    visualizer.rebuild();
  }
  
  protected class SymbolPanel extends JPanel {

    protected JTable table;
    protected AbstractTableModel model;
    protected JScrollPane scrollPane;
    protected JButton addSymbolBtn, addTextBtn, remBtn;
    protected JPanel mainPanel;
    protected JPanel detailPanel;
    protected Component detailControls;
    protected int currentDetail;
    protected static final int NO_CURRENT_ITEM = -1;

    final int NAME_COL = 0;
    final int TYPE_COL = 1;
    final int LOC_COL = 2;
    final int MAX_COL = 2;
    
    public SymbolPanel() {
      setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

      mainPanel = new JPanel();
      mainPanel.setBorder(BorderFactory.createLineBorder(Color.black));
      mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

      Box box = Box.createHorizontalBox();
      box.add(new JLabel("Symbols"));
      mainPanel.add(box);

      model = new SymbolTableModel();
      table = new JTable(model);
      table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
      if (getValueInstanceList() != null && getValueInstanceList().size() > 0) {
        table.getSelectionModel().setSelectionInterval(0, 0);
      }
      ListSelectionModel rowSM = table.getSelectionModel();
      rowSM.addListSelectionListener(new ListSelectionListener() {
        public void valueChanged(ListSelectionEvent e) {
          if (e.getValueIsAdjusting()) return;

          ListSelectionModel lsm = (ListSelectionModel) e.getSource();
          if (lsm.isSelectionEmpty()) {
            showItem(NO_CURRENT_ITEM);
          }
          else {
            int selectedRow = lsm.getMinSelectionIndex();
            showItem(selectedRow);
          }
        }
      });
//      
//      for (int col = 0; col < MAX_COL; col++) {
//        TableColumn column = table.getColumnModel().getColumn(col);
//        if ((col == SIZE_COL || col == SYMBOL1_COL || col == SYMBOL2_COL)) {
//          column.setPreferredWidth(100);
//        }
//        else {
//          column.setPreferredWidth(50);
//        }
//      }

      scrollPane = new JScrollPane(table);
      table.setPreferredScrollableViewportSize(new Dimension(500, 100));
      mainPanel.add(scrollPane);
      
      detailPanel = new JPanel();
      mainPanel.add(detailPanel);

      add(mainPanel);
      
      showItem(0);

    }
    
    protected void showItem(int itemNo) {

      if (detailControls != null) {
        detailPanel.remove(detailControls);
        detailControls = null;
        currentDetail = NO_CURRENT_ITEM;
      }

      int symbolCount = getValueInstanceList().getSymbolCount();
      
      if (itemNo != NO_CURRENT_ITEM && symbolCount > 0 && itemNo < symbolCount) {
        SymbolInstance instance = getValueInstanceList().getSymbol(itemNo);
        instance.setConfig(me);
        Configurer c = instance.getConfigurer();
        detailControls = c.getControls();
        detailPanel.add(detailControls);
        currentDetail = itemNo;
      }

      reshow();
    }

    public void reset() {
      showItem(currentDetail);
    }
    
    public void reshow() {

      repack();
      detailPanel.repaint();

    }

    public void refresh() {
      showItem(currentDetail);
      reshow();
    }
    
    protected void repack() {

      Window w = SwingUtilities.getWindowAncestor(panel);
      if (w != null) {
        w.pack();
      }
    }
    
    class SymbolTableModel extends AbstractTableModel {

      private String[] columnNames = new String[] { "Name", "Type", "Position" };

      public int getColumnCount() {
        return columnNames.length;
      }

      public int getRowCount() {
        return getValueInstanceList() == null ? 0 : getValueInstanceList().getSymbolCount();
      }

      public String getColumnName(int col) {
        return columnNames[col];
      }

      public Object getValueAt(int row, int col) {
        if (col == NAME_COL) {
          return ((SymbolInstance) getValueInstanceList().getSymbol(row)).getName();
        }
        else if (col == TYPE_COL) {
          return ((SymbolInstance) getValueInstanceList().getSymbol(row)).getType();
        }
        else if (col == LOC_COL) {
          return ((SymbolInstance) getValueInstanceList().getSymbol(row)).getLocation();
        }
//        else if (col == SIZE_COL) {
//          return ((SymbolInstance) getValueInstanceList().getSymbol(row)).getSize();
//        }
//        else if (col == SYMBOL1_COL) {
//          return ((SymbolInstance) getValueInstanceList().getSymbol(row)).getSymbol1();
//        }
//        else if (col == SYMBOL2_COL) {
//          return ((SymbolInstance) getValueInstanceList().getSymbol(row)).getSymbol2();
//        }
        else
          return null;
      }

      public Class getColumnClass(int col) {
        return String.class;
      }

      public boolean isCellEditable(int row, int col) {
        //return (col == SIZE_COL || col == SYMBOL1_COL || col == SYMBOL2_COL);
        return false;
      }
//
//      public void setValueAt(Object value, int row, int col) {
//
//        if (col == SIZE_COL) {
//          ((SymbolInstance) getValueInstanceList().getSymbol(row)).setSize((String) value);
//        }
//        else if (col == SYMBOL1_COL) {
//          ((SymbolInstance) getValueInstanceList().getSymbol(row)).setSymbol1((String) value);
//        }
//        else if (col == SYMBOL2_COL) {
//          ((SymbolInstance) getValueInstanceList().getSymbol(row)).setSymbol2((String) value);
//        }
//        fireTableCellUpdated(row, col);
//        visualizer.rebuild();
//      }

    }
  }


  protected class TextPanel extends JPanel {

    protected JTable table;
    protected AbstractTableModel model;
    protected JScrollPane scrollPane;
    protected JButton addSymbolBtn, addTextBtn, remBtn;
    protected JPanel mainPanel;

    final int NAME_COL = 0;
    final int TYPE_COL = 1;
    final int LOC_COL = 2;
    final int VALUE_COL = 3;
    final int MAX_COL = 3;
    
    public TextPanel() {
      setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

      mainPanel = new JPanel();
      mainPanel.setBorder(BorderFactory.createLineBorder(Color.black));
      mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

      Box box = Box.createHorizontalBox();
      box.add(new JLabel("Text Items"));
      mainPanel.add(box);

      model = new TextTableModel();
      table = new JTable(model);
      table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
      if (getValueInstanceList() != null && getValueInstanceList().size() > 0) {
        table.getSelectionModel().setSelectionInterval(0, 0);
      }

      for (int i = 0; i < MAX_COL; i++) {
        TableColumn column = table.getColumnModel().getColumn(i);
        if (i == VALUE_COL) {
          column.setPreferredWidth(100);
        }
        else {
          column.setPreferredWidth(50);
        }
      }

      scrollPane = new JScrollPane(table);
      table.setPreferredScrollableViewportSize(new Dimension(500, 100));
      mainPanel.add(scrollPane);

      add(mainPanel);

    }

    class TextTableModel extends AbstractTableModel {

      private String[] columnNames = new String[] { "Name", "Type", "Position", "Value" };

      public int getColumnCount() {
        return columnNames.length;
      }

      public int getRowCount() {
        return getValueInstanceList() == null ? 0 : getValueInstanceList().getTextCount();
      }

      public String getColumnName(int col) {
        return columnNames[col];
      }

      public Object getValueAt(int row, int col) {
        if (col == NAME_COL) {
          return ((TextInstance) getValueInstanceList().getText(row)).getName();
        }
        else if (col == TYPE_COL) {
          return ((TextInstance) getValueInstanceList().getText(row)).getType();
        }
        else if (col == LOC_COL) {
          return ((TextInstance) getValueInstanceList().getText(row)).getLocation();
        }
        else if (col == VALUE_COL) {
          return ((TextInstance) getValueInstanceList().getText(row)).getValue();
        }
        else
          return null;
      }

      public Class getColumnClass(int col) {
        return String.class;
      }

      public boolean isCellEditable(int row, int col) {
        return (col == VALUE_COL);
      }

      public void setValueAt(Object value, int row, int col) {

        if (col == VALUE_COL) {
          ((TextInstance) getValueInstanceList().getText(row)).setValue((String) value);
        }
        fireTableCellUpdated(row, col);
        rebuildViz();
      }

    }
  }


  public void rebuildViz() {
    if (visualizer != null) {
      visualizer.rebuild();
    }
  }
  
  /**
   * 
   */
  public void repack() {
    if (panel != null) { 
      Window w = SwingUtilities.getWindowAncestor(panel);
      if (w != null) {
        w.pack();
      }
    }
    rebuildViz();    
  }

}
