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
package VASL.counters;

import VASSAL.build.GameModule;
import VASSAL.build.module.ObscurableOptions;
import VASSAL.command.ChangePiece;
import VASSAL.command.Command;
import VASSAL.counters.*;

import javax.swing.*;
import java.awt.*;

/**
 * A Concealment counter
 */
public class Concealment extends Decorator implements EditablePiece {
  public static final String ID = "concealment;";

  private KeyCommand[] commands;
  private String owner;

  public Concealment() {
    this(ID, null);
  }

  public Concealment(String type, GamePiece p) {
    setInner(p);
    mySetType(type);
  }

  public void mySetType(String type) {
    if (type.length() > ID.length()) {
      owner = type.substring(ID.length());
    }
    else {
      owner = null;
    }
  }

  public void setId(String id) {
    super.setId(id);
    if (id == null) {
      owner = GameModule.getUserId();
    }
  }

  public void mySetState(String newState) {
  }

  public String myGetState() {
    return "";
  }

  public String myGetType() {
    return ID + owner;
  }

  protected KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      commands = new KeyCommand[1];
      commands[0] = new KeyCommand("Conceal",
                                   KeyStroke.getKeyStroke('C', java.awt.event.InputEvent.CTRL_MASK),
                                   Decorator.getOutermost(this));
    }
    commands[0].setEnabled(owner == null
                           || owner.equals(GameModule.getUserId())
                           || ObscurableOptions.getInstance().isUnmaskable(owner));
    return commands;
  }

  public Command myKeyEvent(javax.swing.KeyStroke stroke) {
    return null;
  }

  public Command keyEvent(javax.swing.KeyStroke stroke) {
    Stack parent = getParent();
    if (parent != null) {
      BoundsTracker tracker = new BoundsTracker();
      tracker.addPiece(parent);
      int lastIndex = getParent().indexOf(Decorator.getOutermost(this));
      Command c = super.keyEvent(stroke);
      if (c == null || c.isNull()) {
        return c;
      }
      // Concealment counter was deleted or moved in the stack
      int newIndex = getParent() == null ? -1 : getParent().indexOf(Decorator.getOutermost(this));
      if (newIndex > lastIndex) {
        for (int i = lastIndex; i < newIndex; ++i) {
          c.append(setConcealed(parent.getPieceAt(i), true));
        }
      }
      else if (newIndex < lastIndex) {
        if (getParent() == null) {
          lastIndex--;
        }
        for (int i = lastIndex; i > newIndex; --i) {
          GamePiece child = parent.getPieceAt(i);
          if (Decorator.getDecorator(child,Concealment.class) != null) {
            break;
          }
          c.append(setConcealed(child, false));
        }
      }
      tracker.repaint();
      return c;
    }
    else {
      return super.keyEvent(stroke);
    }
  }

  /**
   * Conceal or unconceal the given unit.  Do nothing if the
   * the unit is not concealable by this concealment counter
   */
  public Command setConcealed(GamePiece p, boolean concealed) {
    if (canConceal(p)) {
      String state = p.getState();
      p.setProperty(Obscurable.ID,
                    concealed ? GameModule.getGameModule().getUserId()
                    : null);
      return new ChangePiece(p.getId(), state, p.getState());
    }
    else {
      return null;
    }
  }

  /** @return true if this concealment counter is
   * applicable to the given piece (i.e. if the piece
   * is a concealable counter of the same nationality)
   */
  public boolean canConceal(GamePiece p) {
    Concealable c = (Concealable) Decorator.getDecorator(p, Concealable.class);
    if (c == null
      || !c.isMaskableBy(GameModule.getUserId())) {
      return false;
    }
    else {
      ColoredBox myNation = (ColoredBox) Decorator.getDecorator(this, ColoredBox.class);
      ColoredBox pNation = (ColoredBox) Decorator.getDecorator(p, ColoredBox.class);
      if (!compareColors(myNation, pNation)) {
        return false;
      }
      else {
        myNation = (ColoredBox) Decorator.getDecorator(myNation.getInner(), ColoredBox.class);
        if (myNation == null) {
          return true;
        }
        else {
          return compareColors(myNation, (ColoredBox) Decorator.getDecorator(pNation.getInner(), ColoredBox.class));
        }
      }
    }
  }

  protected static boolean compareColors(ColoredBox c1, ColoredBox c2) {
    if (c1 == null || c2 == null) {
      return false;
    }
    return c1.getColor().equals(c2.getColor());
  }

  public Rectangle selectionBounds() {
    return getInner().selectionBounds();
  }

  public Rectangle boundingBox() {
    return getInner().boundingBox();
  }

  public String getName() {
    return getInner().getName();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    getInner().draw(g, x, y, obs, zoom);
  }

  public String getDescription() {
    return "Is Concealment counter";
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return null;
  }

  public PieceEditor getEditor() {
    return new SimplePieceEditor(this);
  }
}
