package VASSAL.counters;

import VASSAL.command.Command;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.PrototypeDefinition;
import VASSAL.build.GameModule;
import VASSAL.build.widget.PieceSlot;

import javax.swing.*;
import java.awt.*;
import java.util.Enumeration;

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

/**
 * This trait is a placeholder for a pre-defined series of traits specified
 * in a {@link VASSAL.build.module.PrototypeDefinition} object.  When a piece
 * that uses a prototype is defined in a module, it is simply assigned the name
 * of a particular prototype definition.  When that piece is during a game,
 * the UsePrototype trait is substituted for the list of traits in the prototype
 * definition.  From that point on, the piece has no record that those traits
 * were defined in a prototype instead of assigned to piece directly.  This is
 * necessary so that subsequent changes to a prototype definition don't invalidate
 * games that were saved using previous versions of the module.
 *
 */
public class UsePrototype extends Decorator implements EditablePiece {
  public static final String ID = "prototype;";
  private String prototypeName;

  public UsePrototype() {
    this(ID, null);
  }

  public UsePrototype(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  public String getDescription() {
    return prototypeName != null && prototypeName.length() > 0 ? "Prototype - "+prototypeName : "Prototype";
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public void mySetType(String type) {
    prototypeName = type.substring(ID.length());
  }

  protected KeyCommand[] myGetKeyCommands() {
    return new KeyCommand[0];
  }

  public String myGetState() {
    return "";
  }

  public String myGetType() {
    return ID+prototypeName;
  }

  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  public void mySetState(String newState) {
  }

  public Rectangle boundingBox() {
    return getInner().boundingBox();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    getInner().draw(g,x,y,obs,zoom);
  }

  public String getName() {
    return getInner().getName();
  }

  public Shape getShape() {
    return getInner().getShape();
  }

  public String getPrototypeName() {
    return prototypeName;
  }

}
