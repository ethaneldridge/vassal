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
package wga;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.InputEvent;
import java.io.File;
import java.net.MalformedURLException;

import javax.swing.Box;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.KeyStroke;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.counters.Decorator;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.EventFilter;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Immobilized;
import VASSAL.counters.KeyCommand;
import VASSAL.counters.PieceEditor;
import VASSAL.counters.Properties;

public class WgaImmobilized extends Decorator implements EditablePiece {

  public static final String ID = "immob;";
  protected boolean useShift = false;
  protected boolean useCtlShift = false;
  protected boolean ignoreGrid = false;
  protected boolean neverSelect = false;
  protected EventFilter filter;

  protected class UseShiftFilter implements EventFilter {
    public boolean rejectEvent(InputEvent evt) {
      return !evt.isShiftDown() && !Boolean.TRUE.equals(getProperty(Properties.SELECTED));
    }
  };

  protected class UseCtlShiftFilter implements EventFilter {
    public boolean rejectEvent(InputEvent evt) {
      return !(evt.isShiftDown() && evt.isControlDown()) &&!Boolean.TRUE.equals(getProperty(Properties.SELECTED));
    }
  };
  
  protected static EventFilter NEVER_SELECT = new EventFilter() {
    public boolean rejectEvent(InputEvent evt) {
      return true;
    }
  };

  public WgaImmobilized() {
    this(Immobilized.ID, null);
  }

  public WgaImmobilized(String type, GamePiece p) {
    setInner(p);
    mySetType(type);
  }

  public void mySetType(String type) {
    useShift = false;
    if (type.length() > ID.length()) {
      type = type.substring(ID.length());
      useShift = type.indexOf('i') >= 0;
      useCtlShift = type.indexOf('c') >= 0;
      ignoreGrid = type.indexOf('g') >= 0;
      neverSelect = type.indexOf('n') >= 0;
    }
    if (neverSelect) {
      filter = NEVER_SELECT;
    }
    else if (useShift) {
      filter = new UseShiftFilter();
    }
    else if (useCtlShift) {
      filter = new UseCtlShiftFilter();
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
    else if (Properties.TERRAIN.equals(key)) {
      return new Boolean(useShift || useCtlShift || neverSelect);
    }
    else if (Properties.IGNORE_GRID.equals(key)) {
      return new Boolean(ignoreGrid);
    }
    else if (Properties.EVENT_FILTER.equals(key)) {
      return filter;
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
    if (neverSelect) {
      s += 'n';
    }
    else if (useShift) {
      s += 'i';
    }
    else if (useCtlShift) {
      s += 'c';
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
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "NonStacking.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  private static class Ed implements PieceEditor {
    private JComboBox selectionOption;
    private JCheckBox ignoreGridBox;
    private Box controls;

    public Ed(WgaImmobilized p) {
      selectionOption = new JComboBox();
      selectionOption.addItem("normally");
      selectionOption.addItem("when shift-key down");
      selectionOption.addItem("never");
      selectionOption.addItem("when ctl-shift-key down");
      if (p.neverSelect) {
        selectionOption.setSelectedIndex(2);
      }
      else if (p.useShift) {
        selectionOption.setSelectedIndex(1);
      }
      else if (p.useCtlShift) {
        selectionOption.setSelectedIndex(3);
      }
      else {
        selectionOption.setSelectedIndex(0);
      }
      ignoreGridBox = new JCheckBox("Ignore map grid when moving");
      ignoreGridBox.setSelected(p.ignoreGrid);
      controls = Box.createVerticalBox();
      Box b = Box.createHorizontalBox();
      b.add(new JLabel("Select piece"));
      b.add(selectionOption);
      controls.add(b);
      controls.add(ignoreGridBox);
    }

    public String getState() {
      return "";
    }

    public String getType() {
      String s = ID;
      switch (selectionOption.getSelectedIndex()) {
        case 1:
          s += 'i';
          break;
        case 3:
          s += 'c';
          break;
        case 2:
          s += 'n';
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

