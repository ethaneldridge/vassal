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
package VASSAL.counters;

import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;

import javax.swing.*;
import java.awt.*;
import java.io.File;
import java.net.MalformedURLException;

public class Immobilized extends Decorator implements EditablePiece {

  public static final String ID = "immob;";
  private boolean immobile = false;
  private boolean ignoreGrid = false;

  public Immobilized() {
    this(null, Immobilized.ID);
  }

  public Immobilized(GamePiece p, String type) {
    setInner(p);
    mySetType(type);
  }

  public void mySetType(String type) {
    immobile = false;
    if (type.length() > ID.length()) {
      type = type.substring(ID.length());
      immobile = type.indexOf('i') >= 0;
      ignoreGrid = type.indexOf('g') >= 0;
    }
  }

  public void setMap(Map m) {
    super.setMap(m);
    if (m != null) {
      m.reposition(Decorator.getOutermost(this), 0);
    }
  }

  public String getName() {
    return piece.getName();
  }

  public KeyCommand[] myGetKeyCommands() {
    return new KeyCommand[0];
  }

  public Command myKeyEvent(KeyStroke e) {
    return null;
  }

  public Object getProperty(Object key) {
    if (Properties.NO_STACK.equals(key)) {
      return Boolean.TRUE;
    }
    else if (Properties.IMMOBILE.equals(key)) {
      return new Boolean(immobile);
    }
    else if (Properties.IGNORE_GRID.equals(key)) {
      return new Boolean(ignoreGrid);
    }
    else {
      return super.getProperty(key);
    }
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  public Shape getShape() {
    return piece.getShape();
  }

  public String myGetType() {
    String s = ID;
    if (immobile) {
      s += 'i';
    }
    if (ignoreGrid) {
      s += 'g';
    }
    return s;
  }

  public String myGetState() {
    return "";
  }

  public void mySetState(String s) {
  }

  public String getDescription() {
    return "Does not stack";
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir,"ReferenceManual");
    try {
      return new HelpFile(null,new File(dir,"NonStacking.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  private static class Ed implements PieceEditor {
    private JCheckBox immobileBox;
    private JCheckBox ignoreGridBox;
    private Box controls;

    public Ed(Immobilized p) {
      immobileBox = new JCheckBox("Use shift-click to move/select");
      immobileBox.setSelected(p.immobile);
      ignoreGridBox = new JCheckBox("Ignore map grid when moving");
      ignoreGridBox.setSelected(p.ignoreGrid);
      controls = Box.createVerticalBox();
      controls.add(immobileBox);
      controls.add(ignoreGridBox);
    }

    public String getState() {
      return "";
    }

    public String getType() {
      String s = ID;
      if (immobileBox.isSelected()) {
        s += 'i';
      }
      if (ignoreGridBox.isSelected()) {
        s += 'g';
      }
      return s;
    }

    public Component getControls() {
      return controls;
    }
  }
}

