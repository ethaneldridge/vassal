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

import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpWindow;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.ConfigureTree;

import javax.swing.*;
import java.awt.*;
import java.awt.event.WindowListener;
import java.awt.event.WindowEvent;
import java.awt.event.ActionEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.net.MalformedURLException;

/**
 * The editing window for a module
 */
public class ModuleEditWindow extends JFrame implements WindowListener {
  protected HelpWindow helpWindow;
  protected JToolBar toolbar;

  public ModuleEditWindow() {
    helpWindow = new HelpWindow("Reference Manual", null);
    setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
    addWindowListener(this);
    refreshTitle();
    GameModule.getGameModule().addPropertyChangeListener(new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        if (Configurable.NAME_PROPERTY.equals(evt.getPropertyName())) {
          refreshTitle();
        }
      }
    });
    initComponents(new JScrollPane(new ConfigureTree(GameModule.getGameModule(), helpWindow)));
  }

  protected void initComponents(Component view) {
    getContentPane().add(view);
    toolbar = new JToolBar();
    toolbar.setFloatable(false);
    toolbar.add(new SaveAction() {
      public void actionPerformed(ActionEvent e) {
        ModuleEditWindow.this.save();
      }
    });
    toolbar.add(new SaveAsAction() {
      public void actionPerformed(ActionEvent e) {
        saveAs();
      }
    });
    try {
      File dir = new File("docs");
      dir = new File(dir, "ReferenceManual");
      Action a = new ShowHelpAction(helpWindow, HelpFile.toURL(new File(dir, "index.htm")), helpWindow.getClass().getResource("/images/Help16.gif"));
      a.putValue(Action.SHORT_DESCRIPTION, "Reference Manual");
      toolbar.add(a);
    }
    catch (MalformedURLException e) {
      e.printStackTrace();
    }
    getContentPane().add(toolbar, BorderLayout.NORTH);
    pack();
  }

  protected void saveAs() {
    GameModule.getGameModule().saveAs();
  }

  protected void save() {
    GameModule.getGameModule().save();
  }

  protected void refreshTitle() {
    String configureName = GameModule.getGameModule().getConfigureName();
    if (configureName == null) {
      configureName = "Module";
    }
    setTitle("Edit "+configureName);
  }

  public void windowActivated(WindowEvent e) {
  }

  public void windowClosed(WindowEvent e) {
  }

  public void windowClosing(WindowEvent e) {
    GameModule.getGameModule().quit();
  }

  public void windowDeactivated(WindowEvent e) {
  }

  public void windowDeiconified(WindowEvent e) {
  }

  public void windowIconified(WindowEvent e) {
  }

  public void windowOpened(WindowEvent e) {
  }
}
