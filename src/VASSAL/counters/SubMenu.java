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
package VASSAL.counters;

import VASSAL.command.Command;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.tools.SequenceEncoder;
import VASSAL.configure.StringArrayConfigurer;

import javax.swing.*;
import java.awt.*;

/** A trait that groups menu items of other traits into a sub-menu */
public class SubMenu extends Decorator implements EditablePiece {
  public static final String ID = "submenu;";
  private KeyCommandSubMenu subMenu = new KeyCommandSubMenu("Sub Menu",this);
  private String menuCommand;

  public SubMenu() {
  }

  public SubMenu(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  public String getDescription() {
    return null;
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type,';');
    st.nextToken();
    menuCommand = st.nextToken();
    String[] subCommands = StringArrayConfigurer.stringToArray(st.nextToken());
  }

  protected KeyCommand[] myGetKeyCommands() {
    return new KeyCommand[]{
      new KeyCommandSubMenu(menuCommand,this)
    };
  }

  public String myGetState() {
    return null;
  }

  public String myGetType() {
    return null;
  }

  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  public void mySetState(String newState) {
  }

  public Rectangle boundingBox() {
    return null;
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
  }

  public String getName() {
    return null;
  }

  public Shape getShape() {
    return null;
  }

}
