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
package VASSAL.build.module.map;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.counters.*;
import VASSAL.build.*;
import VASSAL.build.module.Map;
import VASSAL.configure.*;
import VASSAL.build.module.documentation.HelpFile;

import org.w3c.dom.*;

import java.io.*;
import java.awt.*;
import java.awt.event.*;
import java.net.URL;
import java.net.MalformedURLException;
import javax.swing.JOptionPane;

public class TextSaver implements ActionListener, Configurable {
  private javax.swing.JButton launch = new javax.swing.JButton("Save text");
  private Map map;

  public void addTo(Buildable b) {
    map = (Map) b;
    launch.setAlignmentY(0.0F);
    map.getToolBar().add(launch);
  }

  public void removeFrom(Buildable b) {
    map = (Map) b;
  }

  public void add(Buildable b) {
    throw new IllegalBuildException("Cannot contain children");
  }

  public void remove(Buildable b) {
    throw new IllegalBuildException("Cannot contain children");
  }

  public void build(Element e) {
    launch.addActionListener(this);
    launch.setToolTipText("Save map contents as plain text file");
  }

  public void actionPerformed(ActionEvent e) {

    switch (JOptionPane.showConfirmDialog
      (null, "Write contents as seen by opponents?", "", JOptionPane.YES_NO_OPTION)) {
      case JOptionPane.NO_OPTION:
        writeMapAsText();
        break;
      case JOptionPane.YES_OPTION:
        String myId = GameModule.getUserId();
        GameModule.setUserId("yendoR117");
        writeMapAsText();
        GameModule.setUserId(myId);
        break;
    }
  }

  protected void writeMapAsText() {
    javax.swing.JFileChooser fc
      = GameModule.getGameModule().getFileChooser();
    if (fc.showSaveDialog(null) == javax.swing.JFileChooser.CANCEL_OPTION) {
      return;
    }
    try {
      PrintWriter p = new PrintWriter
        (new FileOutputStream(fc.getSelectedFile().getPath()));
      GamePiece stack[] = map.getPieces();
      for (int i = 0; i < stack.length; ++i) {
        String s = stack[i].getName();
        if (s.length() > 0) {
          p.println(map.locationName(stack[i].getPosition()) + ": " + s);
        }
      }
      p.close();
    }
    catch (IOException e) {
      JOptionPane.showMessageDialog(null, e.getMessage());
    }
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "Map.htm"), "#TextCapture");
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public org.w3c.dom.Element getBuildElement(org.w3c.dom.Document doc) {
    return doc.createElement(getClass().getName());
  }

  public Configurer getConfigurer() {
    return null;
  }

  /** The name of this Configurable Object
   */
  public String getConfigureName() {
    return null;
  }

  public static String getConfigureTypeName() {
    return "Text Capture Tool";
  }

  /** @return an array of Configurer objects representing
   * the attributes of this Configurable object
   */
  public Configurer[] getAttributeConfigurers() {
    return new Configurer[0];
  }

  /** @return an array of Configurer objects representing
   * the Buildable children of this Configurable object
   */
  public Configurable[] getConfigureComponents() {
    return new Configurable[0];
  }

  /** @return an array of Configurer objects representing
   * all possible classes of Buildable children of this Configurable object
   */
  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void addPropertyChangeListener(java.beans.PropertyChangeListener l) {
  }
}
