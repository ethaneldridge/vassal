/*
 * $Id$
 *
 * Copyright (c) 2000-2005 by Rodney Kinney, Brent Easton
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
package VSQL;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.StringConfigurer;
import VASSAL.counters.Decorator;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyCommand;
import VASSAL.counters.Marker;
import VASSAL.counters.PieceEditor;

/**
 * 
 * */
public class VSQLLevel extends Decorator implements EditablePiece {

  public static final String ID = "level;";
  
  public static final int LEVEL_IN = 0;
  public static final int LEVEL_GROUND = 1;
  public static final int LEVEL_UP = 2;
  
  public VSQLLevel() {
    this(ID, null);
  }

  public VSQLLevel(String type, GamePiece p) {
    mySetType(type);
    setInner(p);
  }
  
  
  /* (non-Javadoc)
   * @see VASSAL.counters.Decorator#mySetState(java.lang.String)
   */
  public void mySetState(String newState) {
    // TODO Auto-generated method stub
    
  }

  /* (non-Javadoc)
   * @see VASSAL.counters.Decorator#myGetState()
   */
  public String myGetState() {
    // TODO Auto-generated method stub
    return null;
  }

  /* (non-Javadoc)
   * @see VASSAL.counters.Decorator#myGetType()
   */
  public String myGetType() {
    // TODO Auto-generated method stub
    return null;
  }

  /* (non-Javadoc)
   * @see VASSAL.counters.Decorator#myGetKeyCommands()
   */
  protected KeyCommand[] myGetKeyCommands() {
    // TODO Auto-generated method stub
    return null;
  }

  /* (non-Javadoc)
   * @see VASSAL.counters.Decorator#myKeyEvent(javax.swing.KeyStroke)
   */
  public Command myKeyEvent(KeyStroke stroke) {
    // TODO Auto-generated method stub
    return null;
  }

  public String getDescription() {
    return "VSQL Level Control";
  }

  /* (non-Javadoc)
   * @see VASSAL.counters.EditablePiece#mySetType(java.lang.String)
   */
  public void mySetType(String type) {
    // TODO Auto-generated method stub
    
  }

  public HelpFile getHelpFile() {
    return null;
  }

  /* (non-Javadoc)
   * @see VASSAL.counters.GamePiece#draw(java.awt.Graphics, int, int, java.awt.Component, double)
   */
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    // TODO Auto-generated method stub
    
  }

  public String getName() {
    return piece.getName();
  }

  public java.awt.Rectangle boundingBox() {
    return piece.boundingBox();
  }

  public Shape getShape() {
    return piece.getShape();
  }
  
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  private static class Ed implements PieceEditor {
    private StringConfigurer propName;
    private StringConfigurer propValue;
    private JPanel panel;
    private Ed(VSQLLevel m) {
      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel,BoxLayout.Y_AXIS));
      //propName = new StringConfigurer(null,"Property name:  ",m.keys.length == 0 ? "" : m.keys[0]);
      //propValue = new StringConfigurer(null,"Property value:  ",m.values.length == 0 ? "" : m.values[0]);
      panel.add(propName.getControls());
      panel.add(propValue.getControls());
    }

    public Component getControls() {
      return panel;
    }

    public String getState() {
      return propValue.getValueString();
    }

    public String getType() {
      return VSQLLevel.ID+propName.getValueString();
    }
  }
}