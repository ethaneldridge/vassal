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

import java.io.ByteArrayOutputStream;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Properties;

public class Prefs {
  private ArchiveWriter archive;
  private Hashtable options = new Hashtable();
  private Properties storedValues = new Properties();
  private PrefsEditor editor;
  private String filename;
  private Prefs child;

  public Prefs(String fileName) {
    editor = new PrefsEditor(this);
    archive = new ArchiveWriter(fileName);
  }

  public Prefs(Prefs parent, String fileName) {
    parent.child = this;
    archive = parent.archive;
    editor = parent.editor;
    init(fileName);
  }

  public void addTo(GameModule theModule) {
    theModule.setPrefs(this);
    init(theModule.getGameName());
  }

  public PrefsEditor getEditor() {
    return editor;
  }

  public void addOption(Configurer o) {
    addOption("General", o);
  }

  public void addOption(String category, Configurer o) {
    addOption(category, o, null);
  }

  /**
   * Add a configurable property to the preferences in the given category
   * @param category the tab under which to add the Configurer's controls
   * in the editor window.  If null, do not add controls
   * @param prompt If non-null and the value was not read from the
   * preferences file on initialization (i.e. first-time setup),
   * prompt the user for an initial value
   *
   */
  public void addOption(String category, Configurer o, String prompt) {
    if (o != null
        && options.get(o.getKey()) == null) {
      options.put(o.getKey(), o);
      String val = storedValues.getProperty(o.getKey());
      if (val != null) {
        o.setValue(val);
        prompt = null;
      }
      if (category != null && o.getControls() != null) {
        editor.addOption(category, o, prompt);
      }
    }
  }

  public Configurer getOption(String s) {
    return (Configurer) options.get(s);
  }

  public Object getValue(String s) {
    Configurer c = (Configurer) options.get(s);
    return c == null ? null : c.getValue();
  }

  /**
   *
   * @param key the name of a Preferences option
   * @return the value for the option that was read from the Preferences file at startup
   */
  public String getStoredValue(String key) {
    return storedValues.getProperty(key);
  }

  public void init(String moduleName) {
    filename = moduleName;
    try {
      archive.getFileStream(filename);
      storedValues.clear();
      storedValues.load(archive.getFileStream(filename));
      for (Enumeration e = storedValues.keys(); e.hasMoreElements();) {
        String key = (String) e.nextElement();
        String value = storedValues.getProperty(key);
        Configurer c = (Configurer) options.get(key);
        if (c != null) {
          c.setValue(value);
        }
      }
    }
    catch (java.io.IOException e) {
    }
  }

  public void write() {
    if (child != null) {
      child.write();
    }
    for (Enumeration e = options.elements(); e.hasMoreElements();) {
      Configurer c = (Configurer) e.nextElement();
      storedValues.put(c.getKey(), c.getValueString());
    }
    try {
      ByteArrayOutputStream out = new ByteArrayOutputStream();
      storedValues.store(out, null);
      archive.addFile(filename,
                      new java.io.ByteArrayInputStream(out.toByteArray()));
      archive.write();
    }
    catch (Exception e) {
      e.printStackTrace();
      javax.swing.JOptionPane.showMessageDialog
          (null,
           e.getMessage(),
           "Error writing preferences",
           javax.swing.JOptionPane.ERROR_MESSAGE);
    }
  }

}
