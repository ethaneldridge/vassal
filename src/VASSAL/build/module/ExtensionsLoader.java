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
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.tools.DataArchive;
import VASSAL.tools.SequenceEncoder;

import java.io.File;
import java.io.IOException;

public class ExtensionsLoader implements CommandEncoder {
  // Preferences key for the list of extensions to load
  private static final String SPECIFY_DIR_IN_PREFS = "specifyExtensionDirInPrefs";
  private static final String EXTENSION_DIR = "extensionDIR";
  public static final String COMMAND_PREFIX = "EXT\t";

  public void addTo(GameModule mod) {
    if ("true".equals(GlobalOptions.getInstance().getAttributeValueString(SPECIFY_DIR_IN_PREFS))) {
      DirectoryConfigurer config = new DirectoryConfigurer(EXTENSION_DIR, "Extensions Directory");
      config.setValue((Object)null);
      GameModule.getGameModule().getPrefs().addOption("Extensions", config);
    }
    mod.addCommandEncoder(this);
    String[] extensions = getExtensionNames();
    if (extensions != null) {
      for (int i = 0; i < extensions.length; ++i) {
        try {
          new ModuleExtension(new DataArchive(extensions[i])).build();
        }
        catch (IOException e) {
          reportBuildError(e, extensions[i]);
        }
        catch (IllegalBuildException e) {
          reportBuildError(e, extensions[i]);
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
    String dirName;
    if ("true".equals(GlobalOptions.getInstance().getAttributeValueString(SPECIFY_DIR_IN_PREFS))) {
      dirName = GameModule.getGameModule().getPrefs().getOption(EXTENSION_DIR).getValueString();
    }
    else {
      dirName = new File(GameModule.getGameModule().getDataArchive().getName()).getName();
      int index = dirName.lastIndexOf('.');
      if (index > 0) {
        dirName = dirName.substring(0, index);
      }
      dirName = dirName + "_ext";
    }
    return dirName;
  }
}
