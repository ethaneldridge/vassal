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
package VASSAL.build.module;

import VASSAL.build.GameModule;
import VASSAL.build.IllegalBuildException;
import VASSAL.build.module.ModuleExtension;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.tools.DataArchive;
import VASSAL.tools.SequenceEncoder;
import VASSAL.command.CommandEncoder;
import VASSAL.command.Command;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.Enumeration;

public class ExtensionsLoader implements CommandEncoder {
  // Preferences key for the list of extensions to load
  private static final String EXTENSION_LIST = "extensions";
  public static final String COMMAND_PREFIX = "EXT\t";

  public void addTo(GameModule mod) {
    mod.addCommandEncoder(this);
    String[] extensions = getExtensionNames();
    if (extensions != null) {
      for (int i = 0; i < extensions.length; ++i) {
        try {
          new ModuleExtension(new DataArchive(extensions[i])).build();
        }
        catch (IOException e) {
          reportBuildError(e,extensions[i]);
        }
        catch (IllegalBuildException e) {
          reportBuildError(e,extensions[i]);
        }
      }
    }
  }

  private void reportBuildError(Exception e, String name) {
    String msg = e.getMessage();
    if (msg == null || msg.length() == 0) {
      msg = e.getClass().getName();
      msg = msg.substring(msg.lastIndexOf('.'));
    }
    System.err.println("Unable to load extension " + name + ":  " + msg);
  }

  public Command decode(String command) {
    Command c = null;
    if (command.startsWith(COMMAND_PREFIX)) {
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(command.substring(COMMAND_PREFIX.length()), '\t');
      c = new ModuleExtension.RegCmd(st.nextToken(), st.nextToken());
    }
    return c;
  }

  public String encode(Command c) {
    String s = null;
    if (c instanceof ModuleExtension.RegCmd) {
      ModuleExtension.RegCmd cmd = (ModuleExtension.RegCmd) c;
      SequenceEncoder se = new SequenceEncoder('\t');
      se.append(cmd.getName()).append(cmd.getVersion());
      s = COMMAND_PREFIX + se.getValue();
    }
    return s;
  }

  private String[] getExtensionNames() {
    String dirName = getExtensionDirectory();
    File dir = new File(new File(GameModule.getGameModule().getDataArchive().getName()).getParent(), dirName);
    String[] s = dir.list();
    if (s == null) {
       s = new String[0];
    }
    for (int i = 0; i < s.length; ++i) {
      s[i] = new File(dir, s[i]).getPath();
    }
    return s;
  }

  public static String getExtensionDirectory() {
    String dirName = new File(GameModule.getGameModule().getDataArchive().getName()).getName();
    int index = dirName.lastIndexOf('.');
    if (index > 0) {
      dirName = dirName.substring(0,index);
    }
    dirName = dirName+"_ext";
    return dirName;
  }

  private String[] getExtensionNamesFromPreferences() {
    StringArrayConfigurer config = new StringArrayConfigurer(EXTENSION_LIST, null);
    GameModule.getGameModule().getPrefs().addOption(null, config);
    String[] extensions = (String[]) config.getValue();
    return extensions;
  }

  private void addLoadMenu(GameModule mod) {
    JMenu menu = mod.getFileMenu();
    JMenuItem item = new JMenuItem("Load Extensions");
    item.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        chooseExtensions();
      }
    });
    item.setMnemonic('X');
    menu.insert(item, menu.getItemCount() - 1);
  }

  public void chooseExtensions() {
    final DefaultListModel model = new DefaultListModel();
    Enumeration e = GameModule.getGameModule().getComponents(ModuleExtension.class);
    while (e.hasMoreElements()) {
      ModuleExtension ext = (ModuleExtension) e.nextElement();
      model.addElement(ext.getDataArchive().getName());
    }
    final JList list = new JList(model);
    final JDialog d = new JDialog(GameModule.getGameModule().getFrame(), "Extensions", true);
    d.getContentPane().setLayout(new BoxLayout(d.getContentPane(), BoxLayout.Y_AXIS));
    d.getContentPane().add(new JScrollPane(list));
    Box buttonBox = Box.createHorizontalBox();
    JButton addButton = new JButton("Add");
    addButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        FileDialog dialog = GameModule.getGameModule().getFileDialog();
        dialog.setMode(FileDialog.LOAD);
        dialog.setVisible(true);
        if (dialog.getFile() != null) {
          File f = new File(dialog.getDirectory());
          f = new File(f, dialog.getFile());
          if (f.exists()) {
            try {
              String name = f.getPath();
              if (name.startsWith(System.getProperty("user.dir"))
                && name.length() > System.getProperty("user.dir").length()) {
                name = name.substring(System.getProperty("user.dir").length() + 1);
              }
              DataArchive archive = new DataArchive(name);
              new ModuleExtension(archive).build();
              model.addElement(archive.getName());
            }
            catch (IOException e1) {
              String msg = "Cannot load extension";
              if (e1.getMessage() != null) {
                msg += ".\n" + e1.getMessage();
              }
              JOptionPane.showMessageDialog(GameModule.getGameModule().getFrame(), msg, "Extension loader", JOptionPane.ERROR_MESSAGE);
            }
            catch (IllegalBuildException e1) {
              String msg = "Cannot load extension";
              if (e1.getMessage() != null) {
                msg += ".\n" + e1.getMessage();
              }
              JOptionPane.showMessageDialog(GameModule.getGameModule().getFrame(), msg, "Extension loader", JOptionPane.ERROR_MESSAGE);
            }
          }
        }
      }
    });
    final JButton removeButton = new JButton("Remove");
    removeButton.addActionListener(new ActionListener() {
      private boolean warned = false;

      public void actionPerformed(ActionEvent e) {
        model.removeElementAt(list.getSelectedIndex());
        if (!warned) {
          JOptionPane.showMessageDialog(GameModule.getGameModule().getFrame(), "Extension will not be loaded next time you start the program.");
          warned = true;
        }
      }
    });
    removeButton.setEnabled(false);
    list.addListSelectionListener(new ListSelectionListener() {
      public void valueChanged(ListSelectionEvent e) {
        removeButton.setEnabled(list.getSelectedIndex() >= 0);
      }
    });
    JButton okButton = new JButton("Done");
    okButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        String[] ext = new String[model.size()];
        for (int i = 0; i < ext.length; ++i) {
          ext[i] = (String) model.elementAt(i);
        }
        GameModule.getGameModule().getPrefs().getOption(EXTENSION_LIST).setValue(ext);
        d.dispose();
      }
    });
    buttonBox.add(addButton);
    buttonBox.add(removeButton);
    buttonBox.add(okButton);
    d.getContentPane().add(buttonBox);
    d.pack();
    d.setLocationRelativeTo(GameModule.getGameModule().getFrame());
    d.setVisible(true);
  }
}
