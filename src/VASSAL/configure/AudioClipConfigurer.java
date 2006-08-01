/*
 * $Id$
 *
 * Copyright (c) 2000-2006 by Rodney Kinney
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

import javax.swing.JFileChooser;

import VASSAL.build.module.Documentation;
import VASSAL.tools.ArchiveWriter;

/**
 * Class for selecting an AudioClip while editing a module and adding it to
 * module
 * 
 * @author rkinney
 * 
 */
public class AudioClipConfigurer extends FileConfigurer {
  private static final JFileChooser fc = new Chooser();

  public AudioClipConfigurer(String key, String name, ArchiveWriter archive) {
    super(key, name);
    this.archive = archive;
  }

  public void chooseNewValue() {
    if (fc.showOpenDialog(null) == JFileChooser.CANCEL_OPTION) {
      setValue((Object) null);
    }
    else {
      setValue(fc.getSelectedFile().exists() ? fc.getSelectedFile() : (Object) null);
    }
  }

  public void setValue(Object o) {
    java.io.File f = (java.io.File) o;
    Object oldValue = value;
    if (f != null && f.exists()) {
      archive.addSound(f.getPath(), f.getName());
    }
    value = f;
    changeSupport.firePropertyChange(key, oldValue, value);
    if (tf != null && !noUpdate) {
      tf.setText(getValueString());
    }
  }

  public java.awt.Component getControls() {
    java.awt.Component c = super.getControls();
    tf.setEditable(false);
    tf.setColumns(20);
    return c;
  }

  private static class Chooser extends JFileChooser {
    private Chooser() {
      super(Documentation.getDocumentationBaseDir());
      setFileFilter(new javax.swing.filechooser.FileFilter() {
        public boolean accept(java.io.File f) {
          String name = f.getName();
          return name.endsWith(".au") || name.endsWith(".AU") || name.endsWith(".wav") || name.endsWith(".WAV") || name.endsWith(".aiff")
              || name.endsWith(".AIFF") || f.isDirectory();
        }

        public String getDescription() {
          return "Audio files";
        }
      });
    }
  }
}
