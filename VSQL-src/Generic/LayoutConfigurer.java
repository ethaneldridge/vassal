/*
 * $Id$
 * 
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License (LGPL) as published by
 * the Free Software Foundation.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, copies are available at
 * http://www.opensource.org.
 */

package Generic;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Window;
import java.awt.event.ActionEvent;
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
import javax.swing.SwingUtilities;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.AbstractTableModel;

import VASSAL.configure.Configurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.StringConfigurer;

public class LayoutConfigurer extends Configurer {

  protected static final String ADD_SYMBOL = "Add Symbol";
  protected static final String ADD_TEXT = "Add Text";
  protected static final String REMOVE = "Remove";
  protected static final int NO_CURRENT_ITEM = -1;
  
  protected JPanel panel;
  protected JPanel itemPanel;
  protected JPanel itemConfigPanel;
  protected Component currentItemControls;
  protected int currentItem = NO_CURRENT_ITEM;
  protected Visualizer visualizer;
  protected JLabel visLabel;
  protected Box filler;
  protected CounterLayout layout;

  protected StringConfigurer defName;
  protected NewIntConfigurer height, width;

  protected LayoutConfigurer() {
    super(null, null);
  }

  protected LayoutConfigurer(String key, String name, CounterLayout def) {
    super(key, name);
    layout = def;
  }
  
//  protected LayoutConfigurer(CounterLayout def) {
//    this();
//    defn = def;
//  }

  public Object getValue() {
    if (layout != null) {

      layout.setConfigureName(defName.getValueString());
      layout.setHeight(((Integer) height.getValue()).intValue());
      layout.setWidth(((Integer) width.getValue()).intValue());
    }
    return layout;
  }

  public void setValue(String s) {
  }

  public Component getControls() {
    if (panel == null) {

      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

//      defName = new StringConfigurer(null, "Layout Name:  ", layout.getConfigureName());
//      panel.add(defName.getControls());
//
//      
//      Box box = Box.createHorizontalBox();
//      width = new NewIntConfigurer(null, "Width:  ", new Integer(layout.width));
//      width.addPropertyChangeListener(new PropertyChangeListener() {
//        public void propertyChange(PropertyChangeEvent evt) {
//          setAttribute(WIDTH, width.getValue());
//          refresh();
//          visualizer.setSize(new Dimension(width.getIntValue(), height.getIntValue()));
//          visualizer.setPreferredSize(new Dimension(width.getIntValue(), height.getIntValue()));
//          SwingUtilities.getWindowAncestor(panel).pack();
//          visualizer.repaint();
//        }
//      });
//
//      height = new NewIntConfigurer(null, "  Height:  ", new Integer(layout.height));
//      height.addPropertyChangeListener(new PropertyChangeListener() {
//        public void propertyChange(PropertyChangeEvent evt) {
//          setAttribute(HEIGHT, height.getValue());
//          refresh();
//          visualizer.setSize(new Dimension(width.getIntValue(), height.getIntValue()));
//          visualizer.setPreferredSize(new Dimension(width.getIntValue(), height.getIntValue()));
//          SwingUtilities.getWindowAncestor(panel).pack();
//          visualizer.repaint();
//        }
//      });
//
//      box.add(width.getControls());
//      box.add(height.getControls());
//      width.setColumns(3);
//      height.setColumns(3);
//      panel.add(box);
//
//      panel.add(filler);

//      visPanel = new JPanel();
//      visPanel.setSize(new Dimension(layout.getLayoutWidth(), layout.getLayoutHeight()));
//      visPanel.setPreferredSize(new Dimension(layout.getLayoutWidth() + 20, layout.getLayoutHeight() + 20));
//      visPanel.add(getVisualizer());
//      panel.add(visPanel);

      filler = Box.createHorizontalBox();
      filler.setPreferredSize(new Dimension(50, 10));
      panel.add(filler);
      
      Box box = Box.createHorizontalBox();
      box.setAlignmentX(Box.CENTER_ALIGNMENT);
      visualizer = new Visualizer(layout);
      box.add(visualizer);
      panel.add(box);
      
      filler = Box.createHorizontalBox();
      filler.setPreferredSize(new Dimension(50, 10));
      panel.add(filler);

      itemPanel = new ItemPanel();      
      panel.add(itemPanel);
      
      filler = Box.createHorizontalBox();
      filler.setPreferredSize(new Dimension(50, 10));
      panel.add(filler);

      Window w = SwingUtilities.getWindowAncestor(itemPanel);
      if (w != null) {
        w.pack();
      }
    }

    return panel;
  }

  public String getValueString() {

    return null;
  }


//  protected JComponent getVisualizer() {
//    if (visualizer == null) {
//      visualizer = new JPanel() {
//        public void paint(Graphics g) {
//          visualizer.setSize(new Dimension(layout.getLayoutWidth(), layout.getLayoutHeight()));
//          visualizer.setPreferredSize(new Dimension(layout.getLayoutWidth(), layout.getLayoutHeight()));
//          g.clearRect(0, 0, layout.getLayoutWidth(), layout.getLayoutHeight());
//          BufferedImage bi = (BufferedImage) layout.getImage(layout.getTestProps());
//          g.drawImage(layout.getImage(null), 0, 0, this);
//        }
//      };
//    }
//    return visualizer;
//  }
//  
//  
  
  protected void repack() {
    
    Window w = SwingUtilities.getWindowAncestor(panel);
    if (w != null) {
      w.pack();
    }
    visualizer.refresh();
  }
  
  protected class ItemPanel extends JPanel implements ActionListener {

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
      if (layout.getItemCount() > 0) {
        table.getSelectionModel().setSelectionInterval(0, 0);
      }
      ListSelectionModel rowSM = table.getSelectionModel();
      rowSM.addListSelectionListener(new ListSelectionListener() {
        public void valueChanged(ListSelectionEvent e) {
          if (e.getValueIsAdjusting()) return;

          ListSelectionModel lsm = (ListSelectionModel) e.getSource();
          if (lsm.isSelectionEmpty()) {
            showItem(-1);
          }
          else {
            int selectedRow = lsm.getMinSelectionIndex();
            showItem(selectedRow);
          }
        }
      });

      scrollPane = new JScrollPane(table);
      table.setPreferredScrollableViewportSize(new Dimension(300, 100));
      mainPanel.add(scrollPane);

      box = Box.createHorizontalBox();
      addSymbolBtn = new JButton(ADD_SYMBOL);
      addSymbolBtn.addActionListener(this);
      box.add(addSymbolBtn);
      addTextBtn = new JButton(ADD_TEXT);
      addTextBtn.addActionListener(this);
      box.add(addTextBtn);
      remBtn = new JButton(REMOVE);
      remBtn.addActionListener(this);
      box.add(remBtn);
      mainPanel.add(box);
      add(mainPanel);
      
      box = Box.createHorizontalBox();
      box.setPreferredSize(new Dimension(50, 10));
      add(box);
   
      itemConfigPanel = new JPanel();
      itemConfigPanel.setBorder(BorderFactory.createLineBorder(Color.black));
      add(itemConfigPanel);
      
      showItem(0);

    }

    public void actionPerformed(ActionEvent e) {
      String action = e.getActionCommand();

      if (action.equals(ADD_SYMBOL)) {
        SymbolItem item = new SymbolItem();
        layout.addItem(item);
        int pos = layout.getItemCount()-1;
        model.fireTableRowsInserted(pos, pos);
        table.getSelectionModel().setSelectionInterval(pos, pos);
      }
      else if (action.equals(ADD_TEXT)) {
        TextItem item = new TextItem();
        layout.addItem(item);
        int pos = layout.getItemCount()-1;
        model.fireTableRowsInserted(pos, pos);
        table.getSelectionModel().setSelectionInterval(pos, pos);
      }
      else if (action.equals(REMOVE)) {
        int i = table.getSelectedRow();
        if (i >= 0) {
          layout.removeItem(i);
          model.fireTableRowsDeleted(i, i);
        }
        if (layout.getItemCount() > 1) {
          if (i >= layout.getItemCount()) {
            table.getSelectionModel().setSelectionInterval(layout.getItemCount()-1, layout.getItemCount()-1);
          } 
          else {
            table.getSelectionModel().setSelectionInterval(i, i);
          }
        }
      }

    }

    protected void showItem(int itemNo) {
      
      if (currentItemControls != null) {
        itemConfigPanel.remove(currentItemControls);
        currentItemControls = null;
        currentItem = NO_CURRENT_ITEM;
      }
      
      if (itemNo != NO_CURRENT_ITEM && layout.getItemCount() > 0 && itemNo < layout.getItemCount()) {
        Item item = layout.getItem(itemNo);
        Configurer c = item.getConfigurer();
        currentItemControls = c.getControls();
        itemConfigPanel.add(currentItemControls);
        currentItem = itemNo;
      }
      
      repack();
      visualizer.refresh();
      itemConfigPanel.repaint();
      
    }

    class MyTableModel extends AbstractTableModel {
      private String[] columnNames = new String[] { "Name", "Type", "Position" };

      public int getColumnCount() {
        return columnNames.length;
      }

      public int getRowCount() {
        return layout.getItemCount();
      }

      public String getColumnName(int col) {
        return columnNames[col];
      }

      public Object getValueAt(int row, int col) {
        if (col == 0) {
          return (layout.getItem(row)).getConfigureName();
        }
        else if (col == 1) {
          return (layout.getItem(row)).getType();
        }
        else if (col == 2) {
          return (layout.getItem(row)).getLocation();
        }
        else
          return null;
      }

      public Class getColumnClass(int c) {
        return String.class;
      }
    }
  }
  
  protected class NewIntConfigurer extends IntConfigurer {

    NewIntConfigurer(String name, String key, Integer i) {
      super(name, key, i);
    }

    public void setColumns(int cols) {
      nameField.setColumns(cols);
    }

    public int getIntValue() {
      return ((Integer) getValue()).intValue();
    }

  }
}
