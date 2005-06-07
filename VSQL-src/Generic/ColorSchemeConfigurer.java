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

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultCellEditor;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableColumn;

import VASSAL.configure.Configurer;

/**
 *  
 */
public class ColorSchemeConfigurer extends Configurer {

  protected ColorScheme scheme;
  protected Box visBox;
  protected Visualizer visualizer = new Visualizer();
  protected JPanel panel;
  protected ItemPanel itemPanel;

  protected static final int NAME_COL = 0;
  protected static final int TYPE_COL = 1;
  protected static final int LOC_COL = 2;
  protected static final int FG_COL = 3;
  protected static final int BG_COL = 4;
  protected static final int MAX_COL = 4;

  protected ColorSchemeConfigurer() {
    super(null, null);
  }

  protected ColorSchemeConfigurer(String key, String name, ColorScheme def) {
    super(key, name);
    scheme = def;
  }

  public String getValueString() {
    return null;
  }

  public void setValue(String s) {

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
      visualizer = new Visualizer(scheme);
      visBox.add(visualizer);
      panel.add(visBox);

      filler = Box.createHorizontalBox();
      filler.setPreferredSize(new Dimension(50, 10));
      panel.add(filler);

      itemPanel = new ItemPanel();
      panel.add(itemPanel);

    }
    return panel;
  }

  public void refresh() {

  }

  protected class ItemPanel extends JPanel {

    protected JTable table;
    protected AbstractTableModel model;
    protected JScrollPane scrollPane;
    protected JButton addSymbolBtn, addTextBtn, remBtn;
    protected JPanel mainPanel;

    public ItemPanel() {
      setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

      mainPanel = new JPanel();
      mainPanel.setBorder(BorderFactory.createLineBorder(Color.black));
      mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

      Box box = Box.createHorizontalBox();
      box.add(new JLabel("Items"));
      mainPanel.add(box);

      model = new MyTableModel();
      table = new JTable(model);
      table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
      if (scheme.getElementCount() > 0) {
        table.getSelectionModel().setSelectionInterval(0, 0);
      }

      for (int i = 0; i < MAX_COL; i++) {
        TableColumn column = table.getColumnModel().getColumn(i);
        if (i == LOC_COL || i == FG_COL || i == BG_COL) {
          column.setPreferredWidth(100);
        }
        else {
          column.setPreferredWidth(50);
        }
      }

      TableColumn tc = table.getColumnModel().getColumn(FG_COL);
      SwatchComboBox comboBox = new SwatchComboBox();
      tc.setCellEditor(new DefaultCellEditor(comboBox));
      tc.setCellRenderer(comboBox.new SwatchTableRenderer());

      TableColumn tc2 = table.getColumnModel().getColumn(BG_COL);
      SwatchComboBox comboBox2 = new SwatchComboBox();
      tc2.setCellEditor(new DefaultCellEditor(comboBox2));
      tc2.setCellRenderer(comboBox2.new SwatchTableRenderer());

      scrollPane = new JScrollPane(table);
      table.setPreferredScrollableViewportSize(new Dimension(500, 100));
      mainPanel.add(scrollPane);

      add(mainPanel);

    }

    class MyTableModel extends AbstractTableModel {

      private String[] columnNames = new String[] { "Name", "Type", "Position", "Fg Color", "Bg Color" };

      public int getColumnCount() {
        return columnNames.length;
      }

      public int getRowCount() {
        return scheme.getElementCount();
      }

      public String getColumnName(int col) {
        return columnNames[col];
      }

      public Object getValueAt(int row, int col) {
        if (col == NAME_COL) {
          return (scheme.getElement(row)).getName();
        }
        else if (col == TYPE_COL) {
          return (scheme.getLayout().getItem(row)).getType();
        }
        else if (col == LOC_COL) {
          return (scheme.getLayout().getItem(row)).getLocation();
        }
        else if (col == FG_COL) {
          return (scheme.getElement(row)).getFgColor();
        }
        else if (col == BG_COL) {
          return (scheme.getElement(row)).getBgColor();
        }
        else
          return null;
      }

      public Class getColumnClass(int col) {
        if (col < FG_COL) {
          return String.class;
        }
        else {
          return ColorSwatch.class;
        }
      }

      public boolean isCellEditable(int row, int col) {
        if (col <= LOC_COL) {
          return false;
        }
        else {
          return true;
        }
      }

      public void setValueAt(Object value, int row, int col) {
        if (col == FG_COL) {
          scheme.setElementFg(row, ColorManager.getColorManager().getColorSwatch((String) value));
        }
        else if (col == BG_COL) {
          scheme.setElementBg(row, ColorManager.getColorManager().getColorSwatch((String) value));
        }
        fireTableCellUpdated(row, col);
        visualizer.rebuild();
      }

    }
  }

}
