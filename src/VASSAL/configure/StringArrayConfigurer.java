/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
/*
 * Created by IntelliJ IDEA.
 * User: rkinney
 * Date: Jul 20, 2002
 * Time: 4:10:29 AM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.configure;

import VASSAL.tools.SequenceEncoder;
import VASSAL.counters.PieceEditor;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.reflect.Array;
import java.util.Vector;
import java.util.ArrayList;

/**
 * A Configurer that returns an array of Strings
 */
public class StringArrayConfigurer extends Configurer {
  private JPanel panel;
  private JList list;
  private DefaultListModel model;
  private static final String[] EMPTY = new String[0];

  public StringArrayConfigurer(String key, String name, Object val) {
    super(key, name, val);
  }

  public StringArrayConfigurer(String key, String name) {
    super(key, name);
  }

  public DefaultListModel getModel() {
    return model;
  }

  public void addValue(String s) {
    if (value == null) {
      setValue(new String[]{s});
    }
    else {
      String[] newValue = new String[Array.getLength(value) + 1];
      System.arraycopy(value, 0, newValue, 0, newValue.length - 1);
      newValue[newValue.length - 1] = s;
      setValue(newValue);
    }
  }

  public void removeValue(String s) {
    String[] oldValue = getStringArray();
    for (int i = 0; i < oldValue.length; ++i) {
      if (oldValue[i].equals(s)) {
        String[] newValue = new String[Array.getLength(value) - 1];
        if (i > 0) {
          System.arraycopy(oldValue, 0, newValue, 0, i);
        }
        if (i < oldValue.length - 1) {
          System.arraycopy(oldValue, i + 1, newValue, i, oldValue.length - 1 - i);
        }
        setValue(newValue);
        return;
      }
    }
  }


  public Component getControls() {
    if (panel == null) {
      panel = new JPanel();
      Box buttonBox = Box.createHorizontalBox();
      Box leftBox = Box.createVerticalBox();
      panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
      model = new DefaultListModel();
      updateModel();
      list = new JList(model);
      list.setPrototypeCellValue("MMMMMMMM");
      list.setVisibleRowCount(2);
      final JTextField tf = new JTextField(8);
      ActionListener al = new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          String s = tf.getText();
          addValue(s);
          tf.setText("");
        }
      };
      JButton addButton = new JButton("Add");
      addButton.addActionListener(al);
      tf.addActionListener(al);
      buttonBox.add(tf);
      buttonBox.add(addButton);
      JButton removeButton = new JButton("Remove");
      removeButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          Object[] o = list.getSelectedValues();
          for (int i = 0; i < o.length; ++i) {
            removeValue((String) o[i]);
          }
        }
      });
      buttonBox.add(removeButton);
      leftBox.add(buttonBox);
      panel.add(leftBox);
      panel.add(new JScrollPane(list));
      panel.setBorder(new TitledBorder(name));
    }
    return panel;
  }

  public String[] getStringArray() {
    if (value instanceof String[]) {
      return (String[]) value;
    }
    else {
      return EMPTY;
    }
  }

  public String getValueString() {
    return arrayToString(getStringArray());
  }

  public static String arrayToString(String[] s) {
    if (s == null || s.length == 0) {
      return "";
    }
    SequenceEncoder se = new SequenceEncoder(',');
    for (int i = 0; i < s.length; ++i) {
      se.append(s[i]);
    }
    return se.getValue();
  }

  public void setValue(Object o) {
    if (o == null) {
      o = EMPTY;
    }
    super.setValue(o);
    updateModel();
  }

  public void setValue(String s) {
    String[] val = stringToArray(s);
    setValue(val);
  }

  public static String[] stringToArray(String s) {
    if (s == null
      || s.length() == 0) {
      return EMPTY;
    }
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ',');
    java.util.List l = new ArrayList();
    while (st.hasMoreTokens()) {
      l.add(st.nextToken());
    }
    return (String[]) l.toArray(new String[l.size()]);
  }

  protected void updateModel() {
    if (model != null) {
      model.removeAllElements();
      String[] s = getStringArray();
      for (int i = 0; i < s.length; ++i) {
        model.addElement(s[i]);
      }
    }
  }

  public static void main(String[] args) {
    JFrame f = new JFrame();
    final StringArrayConfigurer c = new StringArrayConfigurer(null, "Visible to these players:  ");
    c.addPropertyChangeListener(new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        System.err.println(c.getName() + " = " + c.getValueString());
      }
    });
    c.setValue("Rack,Shack,Benny");
    f.getContentPane().add(c.getControls());
    f.pack();
    f.setVisible(true);
  }
}
