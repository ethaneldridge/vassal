/*
 * $Id$
 *
 * Copyright (c) 2004 by Rodney Kinney
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
package VASSAL.build.module.documentation;

import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Documentation;
import org.w3c.dom.Element;
import org.w3c.dom.Document;

import javax.swing.*;
import java.io.*;
import java.awt.event.ActionEvent;
import java.awt.*;

/**
 * Provides tutorial functionality by reading in a logfile
 */
public class Tutorial extends AbstractConfigurable {
  public static final String FILE_NAME = "logfile";
  public static final String NAME = "name";
  public static final String LAUNCH_ON_STARTUP = "launchOnStartup";
  private String fileName;
  private Action launch;
  private JMenuItem item;
  private boolean launchOnFirstStartup;

  public Tutorial() {
    launch = new AbstractAction("Tutorial") {
      public void actionPerformed(ActionEvent e) {
        launch();
      };
    };
  }

  private void launch() {
    try {
      int index = fileName.indexOf(".");
      String prefix = index > 3 ? fileName.substring(0, index) : "VSL";
      String suffix = index >= 0 ? fileName.substring(index) : ".log";
      File tmp = File.createTempFile(prefix, suffix);
      BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(tmp));
      BufferedInputStream in = new BufferedInputStream(GameModule.getGameModule().getDataArchive().getFileStream(fileName));
      int len = 0;
      byte[] b = new byte[in.available()];
      while ((len = in.read(b)) > 0) {
        out.write(b, 0, len);
      }
      in.close();
      out.close();
      File renamed = new File(tmp.getParent(), (String) launch.getValue(Action.NAME));
      if (tmp.renameTo(renamed)) {
        tmp = renamed;
      }
      GameModule.getGameModule().getGameState().loadGame(tmp);
      GameModule.getGameModule().warn("Hit the \"Step forward\" button in the toolbar to step through the tutorial");
    }
    catch (IOException e1) {
      String msg = "Unable to launch tutorial " + name;
      if (e1.getMessage() != null) {
        msg += ":  " + e1.getMessage();
      }
      GameModule.getGameModule().warn(msg);
    }
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Name", "Logfile", "Launch automatically on first startup"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class, File.class, Boolean.class};
  }

  public String[] getAttributeNames() {
    return new String[]{NAME, FILE_NAME, LAUNCH_ON_STARTUP};
  }

  public String getAttributeValueString(String key) {
    if (FILE_NAME.equals(key)) {
      return fileName;
    }
    else if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (LAUNCH_ON_STARTUP.equals(key)) {
      return "" + launchOnFirstStartup;
    }
    else {
      return null;
    }
  }

  public void setAttribute(String key, Object value) {
    if (FILE_NAME.equals(key)) {
      if (value instanceof File) {
        value = ((File) value).getName();
      }
      fileName = (String) value;
    }
    else if (NAME.equals(key)) {
      launch.putValue(Action.NAME, value);
      setConfigureName((String) value);
    }
    else if (LAUNCH_ON_STARTUP.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      launchOnFirstStartup = ((Boolean) value).booleanValue();
    }
  }

  public void addTo(Buildable parent) {
    item = ((Documentation) parent).getHelpMenu().add(launch);
    if (launchOnFirstStartup) {
      if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(GameModule.getGameModule().getFrame(),
                                                                  "Load the tutorial?",
                                                                  "First startup",
                                                                  JOptionPane.YES_NO_OPTION)) {
        launch();
      }
    }
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public void removeFrom(Buildable parent) {
    if (item != null) {
      ((Documentation) parent).getHelpMenu().remove(item);
    }
  }
}
