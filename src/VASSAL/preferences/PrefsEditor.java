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
package VASSAL.preferences;

import VASSAL.build.GameModule;
import VASSAL.configure.Configurer;
import VASSAL.tools.ArchiveWriter;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;
import java.io.IOException;

public class PrefsEditor extends JDialog {
  private Vector options = new Vector();
  private Hashtable savedValues;
  private Vector prefs;
  private JButton save, cancel;
  private JTabbedPane tab;
  private JMenuItem launch;
  private JDialog setupDialog;
  private ArchiveWriter archive;
  private Action editAction;

  public PrefsEditor(ArchiveWriter archive) {
    super(GameModule.getGameModule() == null ? (Frame)null
	: GameModule.getGameModule().getFrame(), true);
    setTitle("Preferences");

    savedValues = new Hashtable();
    this.archive = archive;

    editAction = new AbstractAction("Edit Preferences") {
      public void actionPerformed(ActionEvent e) {
        storeValues();
        pack();
        Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
        setLocation(d.width/2-getWidth()/2,0);
        setVisible(true);
      }
    };
    editAction.putValue(Action.MNEMONIC_KEY,new Integer((int)'P'));

    setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);

    prefs = new Vector();

    save = new JButton("Save");
    save.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        save();
      }
    });
    cancel = new JButton("Cancel");
    cancel.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        cancel();
      }
    });

    JPanel pan = new JPanel();
    pan.add(save);
    pan.add(cancel);
    getContentPane().setLayout
      (new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
    tab = new JTabbedPane();
    getContentPane().add(tab);
    getContentPane().add(pan);
  }

  public void addPrefs(Prefs p) {
    prefs.addElement(p);
  }

  public void addOption(String category, Configurer c, String prompt) {
    if (prompt != null) {
      if (setupDialog == null) {
        setupDialog = new JDialog((JFrame) null, true);
        setupDialog.setTitle("Initial Setup");
        setupDialog.getContentPane().setLayout
          (new BoxLayout(setupDialog.getContentPane(), BoxLayout.Y_AXIS));
        setupDialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
      }
      setupDialog.getContentPane().add(new JLabel(prompt));
      setupDialog.getContentPane().add(c.getControls());
      JButton b = new JButton("Ok");
      b.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          setupDialog.setVisible(false);
        }
      });
      JPanel p = new JPanel();
      p.add(b);
      setupDialog.getContentPane().add(p);
      setupDialog.pack();
      Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
      setupDialog.setLocation(d.width / 2 - setupDialog.getSize().width / 2,
                              d.height / 2 - setupDialog.getSize().height / 2);
      setupDialog.setVisible(true);
      setupDialog.getContentPane().removeAll();
    }
    addOption(category, c);
  }

  public void addOption(String category, Configurer c) {
    if (category == null) {
      category = "General";
    }
    JPanel pan = null;
    int i = 0;
    for (i = 0; i < tab.getTabCount(); ++i) {
      if (category.equals(tab.getTitleAt(i))) {
        pan = (JPanel) tab.getComponentAt(i);
        break;
      }
    }
    if (i >= tab.getTabCount()) { // No match
      pan = new JPanel();
      pan.setLayout(new BoxLayout(pan, BoxLayout.Y_AXIS));
      tab.addTab(category, pan);
    }
    options.addElement(c);
    Box b = Box.createHorizontalBox();
    b.add(c.getControls());
    b.add(Box.createHorizontalGlue());
    pan.add(b);
  }

  private void storeValues() {
    savedValues.clear();
    for (Enumeration e = options.elements();
         e.hasMoreElements();) {
      Configurer c = (Configurer) e.nextElement();
      c.setFrozen(true);
      if (c.getValue() != null) {
        savedValues.put(c, c.getValue());
      }
    }
  }

  public ArchiveWriter getArchive() {
    return archive;
  }

  protected void cancel() {
    for (Enumeration e = options.elements();
         e.hasMoreElements();) {
      Configurer c = (Configurer) e.nextElement();
      c.setValue(savedValues.get(c));
      c.setFrozen(false);
    }
    setVisible(false);
  }

  protected void save() {
    for (Enumeration e = options.elements();
         e.hasMoreElements();) {
      Configurer c = (Configurer) e.nextElement();
      c.fireUpdate();
      c.setFrozen(false);
    }
    try {
      for (Enumeration e = prefs.elements(); e.hasMoreElements();) {
        ((Prefs)e.nextElement()).save();
      }
      write();
    }
    catch (IOException e) {
      JOptionPane.showMessageDialog(getOwner(),"Unable to save preferences.\n","Save error",JOptionPane.ERROR_MESSAGE);
    }
    setVisible(false);
  }

  public Action getEditAction() {
    return editAction;
  }

  public void write() throws IOException {
    archive.write();
  }
}
