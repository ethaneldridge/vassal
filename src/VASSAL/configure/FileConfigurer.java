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
package VASSAL.configure;

import VASSAL.tools.ArchiveWriter;
import VASSAL.build.module.Documentation;

import java.io.File;
import javax.swing.*;

/**
 * A Configurer for java.io.File values
 */
public class FileConfigurer extends Configurer {
  protected VASSAL.tools.ArchiveWriter archive;

  protected JPanel p;
  protected JTextField tf;
  private static JFileChooser fc;

  public FileConfigurer(String key, String name) {
    super(key, name);
    setValue(null);
    initFileChooser();
  }

  private static void initFileChooser() {
    if (fc == null) {
      fc = new JFileChooser(Documentation.getDocumentationBaseDir());
    }
  }

  /**
   * If a non-null {@link ArchiveWriter} is used in the constructor, then
   * invoking {@link #setValue} on this FileConfigurer will
   * automatically add the file to the archive */
  public FileConfigurer(String key, String name, ArchiveWriter archive) {
    this(key, name);
    this.archive = archive;
  }

  public String getValueString() {
    if (archive == null) {
      return fileValue() == null ? "null" : fileValue().getPath();
    }
    else {
      return fileValue() == null ? "null" : fileValue().getName();
    }
  }

  public void setValue(Object o) {
    File f = (File) o;
    if (f != null) {
      if (!f.exists()) {
        f = null;
      }
      else if (archive != null) {
        archive.addFile(f.getPath(), f.getName());
      }
    }
    super.setValue(f);
    if (tf != null && !noUpdate) {
      tf.setText(getValueString());
      //            tf.revalidate();
    }
  }

  public void setValue(String s) {
    if (s == null)
      setValue((Object) null);
    else {
      setValue(new File(s));
    }
  }

  public java.awt.Component getControls() {
    if (p == null) {
      p = new JPanel();
      p.setLayout(new BoxLayout(p, BoxLayout.X_AXIS));
      p.add(new JLabel(getName()));
      JButton b = new JButton("Select");
      p.add(b);
      tf = new JTextField(getValueString());
      tf.setMaximumSize
        (new java.awt.Dimension(tf.getMaximumSize().width,
                                tf.getPreferredSize().height));
      tf.getDocument().addDocumentListener(new javax.swing.event.DocumentListener() {
        public void changedUpdate(javax.swing.event.DocumentEvent evt) {
          update();
        }

        public void insertUpdate(javax.swing.event.DocumentEvent evt) {
          update();
        }

        public void removeUpdate(javax.swing.event.DocumentEvent evt) {
          update();
        }

        public void update() {
          File f = new File(tf.getText());
          if (f.exists()) {
            noUpdate = true;
            setValue(f);
            noUpdate = false;
          }
        }
      });
      p.add(tf);
      b.addActionListener
        (new java.awt.event.ActionListener() {
          public void actionPerformed
            (java.awt.event.ActionEvent e) {
            chooseNewValue();
          }
        });
    }
    return p;
  }

  public void chooseNewValue() {
    if (fc.showOpenDialog(null) == JFileChooser.CANCEL_OPTION) {
      setValue((Object) null);
    }
    else {
      setValue(fc.getSelectedFile());
    }
  }

  protected File fileValue() {
    return (File) value;
  }

  public static void main(String args[]) {
    JFrame f = new JFrame();
    FileConfigurer c = new ImageConfigurer(null, "Test file", new VASSAL.tools.ArchiveWriter("testArchive"));
    c.addPropertyChangeListener(new java.beans.PropertyChangeListener() {
      public void propertyChange(java.beans.PropertyChangeEvent evt) {
        System.err.println("" + evt.getNewValue());
      }
    });
    f.getContentPane().add(c.getControls());
    f.pack();
    f.setVisible(true);
  }
}


