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

import VASSAL.build.GameModule;
import VASSAL.build.module.GlobalOptions;
import VASSAL.command.Command;
import VASSAL.configure.HotKeyConfigurer;

import javax.swing.*;

public class KeyCommand extends AbstractAction {
  private String name;
  private KeyStroke stroke;
  private GamePiece target;

  public KeyCommand(String name, KeyStroke key, GamePiece target) {
    super(key == null ? name : name + "  " + HotKeyConfigurer.getString(key));
    this.target = target;
    this.name = name;
    this.stroke = key;
  }

  public String getName() {
    return name;
  }

  public boolean matches(KeyStroke key) {
    return isEnabled() && key == stroke;
  }

  public KeyStroke getKeyStroke() {
    return stroke;
  }

  public GamePiece getTarget() {
    return target;
  }

  public void actionPerformed(java.awt.event.ActionEvent evt) {
    BoundsTracker t = new BoundsTracker();
    GamePiece outer = Decorator.getOutermost(target);
    t.addPiece(outer);
    GlobalOptions.setInitialState(outer);
    Command c = target.keyEvent(stroke);
    if (target.getId() != null) {
      GameModule.getGameModule().sendAndLog(c);
    }
    t.addPiece(outer);
    t.repaint();
  }
}


