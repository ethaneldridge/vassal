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
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.table.AbstractTableModel;

import VASSAL.configure.Configurer;

/**

 */
public class SchemeConfigurer extends Configurer {

  protected ColorScheme scheme;
  protected Visualizer visualizer;
  protected JPanel panel;
  protected ItemPanel itemPanel;
  
  protected SchemeConfigurer() {
    super(null, null);
  }

  protected SchemeConfigurer(String key, String name, ColorScheme def) {
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
      
      Box box = Box.createHorizontalBox();
      box.setAlignmentX(Box.CENTER_ALIGNMENT);
      visualizer = new Visualizer(scheme);
      box.add(visualizer);
      panel.add(box);
      
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

  protected class ItemPanel extends JPanel  {

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
      if (scheme.getItemCount() > 0) {
        table.getSelectionModel().setSelectionInterval(0, 0);
      }

      scrollPane = new JScrollPane(table);
      table.setPreferredScrollableViewportSize(new Dimension(400, 100));
      mainPanel.add(scrollPane);

      add(mainPanel);

    }


    class MyTableModel extends AbstractTableModel {
      private String[] columnNames = new String[] { "Name", "Type", "Position", "Fg Color", "Bg Color" };

      public int getColumnCount() {
        return columnNames.length;
      }

      public int getRowCount() {
        return scheme.getItemCount();
      }

      public String getColumnName(int col) {
        return columnNames[col];
      }

      public Object getValueAt(int row, int col) {
//        if (col == 0) {
//          return (scheme.getItem(row)).getConfigureName();
//        }
//        else if (col == 1) {
//          return (scheme.getItem(row)).getType();
//        }
//        else if (col == 2) {
//          return (scheme.getItem(row)).getLocation();
//        }
//        else
          return null;
      }

      public Class getColumnClass(int c) {
        return String.class;
      }
    }
  }
  
}
