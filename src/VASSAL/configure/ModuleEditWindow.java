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
import java.awt.event.ActionListener;
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
    SaveAction saveAction = new SaveAction() {
      public void actionPerformed(ActionEvent e) {
        ModuleEditWindow.this.save();
      }
    };
    toolbar.add(saveAction);
    SaveAsAction saveAsAction = new SaveAsAction() {
      public void actionPerformed(ActionEvent e) {
        ModuleEditWindow.this.saveAs();
      }
    };
    toolbar.add(saveAsAction);
    Action helpAction = null;
    ;
    try {
      File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
      dir = new File(dir, "ReferenceManual");
      helpAction = new ShowHelpAction(helpWindow, HelpFile.toURL(new File(dir, "index.htm")), helpWindow.getClass().getResource("/images/Help16.gif"));
      helpAction.putValue(Action.SHORT_DESCRIPTION, "Reference Manual");
      toolbar.add(helpAction);
    }
    catch (MalformedURLException e) {
      e.printStackTrace();
    }
    getContentPane().add(toolbar, BorderLayout.NORTH);
    JMenuBar mb = new JMenuBar();
    JMenuItem mi;
    JMenu fileMenu = new JMenu("File");
    mi = new JMenuItem("Save");
    mi.addActionListener(saveAction);
    fileMenu.add(mi);
    mi = new JMenuItem("Save as ...");
    mi.addActionListener(saveAsAction);
    fileMenu.add(mi);
    mb.add(fileMenu);
    if (helpAction != null) {
      JMenu helpMenu = new JMenu("Help");
      mi = new JMenuItem("Reference Manual");
      mi.addActionListener(helpAction);
      helpMenu.add(mi);
      mb.add(helpMenu);
    }
    mb.add(createUpdateMenu());
    setJMenuBar(mb);
    pack();
  }

  protected JMenu createUpdateMenu() {
    JMenu updaterMenu = new JMenu("Updates");
    JMenuItem mi = new JMenuItem("Create module updater");
    mi.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        new ModuleUpdaterDialog(ModuleEditWindow.this, helpWindow).setVisible(true);
      }
    });
    updaterMenu.add(mi);
    return updaterMenu;
  }

  protected void saveAs() {
    ValidationReport report = new ValidationReport();
    GameModule.getGameModule().validate(GameModule.getGameModule(), report);
    if (report.getWarnings().size() == 0) {
      GameModule.getGameModule().saveAs();
    }
    else {
      new ValidationReportDialog(report, new ValidationReportDialog.CallBack() {
        public void ok() {
          GameModule.getGameModule().saveAs();
        }

        public void cancel() {
        }
      }).setVisible(true);
    }
  }

  protected void save() {
    ValidationReport report = new ValidationReport();
    GameModule.getGameModule().validate(GameModule.getGameModule(), report);
    if (report.getWarnings().size() == 0) {
      GameModule.getGameModule().save();
    }
    else {
      new ValidationReportDialog(report, new ValidationReportDialog.CallBack() {
        public void ok() {
          GameModule.getGameModule().save();
        }

        public void cancel() {
        }
      }).setVisible(true);
    }
  }

  protected void refreshTitle() {
    String configureName = GameModule.getGameModule().getConfigureName();
    if (configureName == null) {
      configureName = "Module";
    }
    setTitle("Edit " + configureName);
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
