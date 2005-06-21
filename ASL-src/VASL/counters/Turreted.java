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
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.counters.*;
import VASSAL.tools.SequenceEncoder;

import javax.swing.*;
import java.awt.*;

/**
 * This class draws a turret counter at different offsets depending on the CA
 * and doesn't draw the counter at all if the TCA=CA
 * (unless CE status differs from the default)
 */
public class Turreted extends Embellishment implements EditablePiece {
  public static final String ID = "turr;";

  protected String front,back;
  protected boolean flipped;
  protected boolean rotateWithCounter; // As of VASL 4.2+, vehicles counters rotate in their entirety,
  // rather than just the silhouette

  public Turreted() {
//    this(ID + "tcaCE;tca;S;A;r", null);   // Enable this in VASL 5.0
    this(ID + "tcaCE;tca;SX;AZ", null);
  }

  public Turreted(String type, GamePiece p) {
    setInner(p);
    mySetType(type);
  }

  public void mySetType(String type) {
    String embType;
    if (type.startsWith(Embellishment.ID)
      || type.startsWith(Embellishment.OLD_ID)) {
      embType = type;
    }
    else {
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
      st.nextToken();
      front = st.nextToken();
      back = st.nextToken();
      embType = Embellishment.OLD_ID + ";_;" + st.nextToken() + ";TCA cw;"
          + st.nextToken() + ";TCA ccw;0;0";
      if (st.hasMoreTokens()) {
        rotateWithCounter = true;
        embType += ";;;;;;";
      }
      else {
        embType += ";,+ TCA = 1;,+ TCA = 2;,+ TCA = 3;,+ TCA = 4;,+ TCA = 5;,+ TCA = 6";
      }
    }
    super.mySetType(embType);
  }

  public Shape getShape() {
    return piece.getShape();
  }


  protected Image getCurrentImage() throws java.io.IOException {
    if (flipped || value != getVehicleCA()) {
      return GameModule.getGameModule().getDataArchive()
          .getCachedImage((flipped ? back : front) + value + ".gif");
    }
    else {
      return null;
    }
  }

  public Rectangle getCurrentImageBounds() {
    Rectangle r;
    if (flipped || value != getVehicleCA()) {
      r = new Rectangle(super.getCurrentImageBounds());
      switch (value) {
        case 1:
          r.translate(20, -10);
          ;
          break;
        case 2:
          r.translate(20, 5);
          break;
        case 3:
          r.translate(20, 20);
          break;
        case 4:
          r.translate(-10, 20);
          break;
        case 5:
          r.translate(-10, 5);
          break;
        case 6:
          r.translate(-10, -10);
          break;
      }
    }
    else {
      r = new Rectangle();
    }
    return r;
  }

  protected int getVehicleCA() {
    if (rotateWithCounter) {
      return 3;
    }
    else {
      for (GamePiece p = piece; p instanceof Decorator; p = ((Decorator) p).getInner()) {
        if (p instanceof Embellishment
            && p.getType().indexOf("Rotate") >= 0) {
          return ((Embellishment) p).getValue() + 1;
        }
      }
    }
    return -1;
  }

  public boolean isFlipped() {
    return flipped;
  }

  public void setFlipped(boolean val) {
    flipped = val;
  }

  public String myGetType() {
    String s = ID + front + ';' + back + ';' + upKey + ';' + downKey;
    if (rotateWithCounter) {
      s += ";r";
    }
    return s;
  }

  public String myGetState() {
    return new SequenceEncoder(super.myGetState(),';').append(String.valueOf(flipped)).getValue();
  }

  public void mySetState(String s) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ';');
    super.mySetState(st.nextToken());
    flipped = st.nextBoolean(false);
  }

  public KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      KeyCommand[] c = super.myGetKeyCommands();
      commands = new KeyCommand[c.length + 1];
      System.arraycopy(c, 0, commands, 0, c.length);
      commands[c.length]
          = new KeyCommand
              ("BU",
               KeyStroke.getKeyStroke('B', java.awt.event.InputEvent.CTRL_MASK),
               Decorator.getOutermost(this));
    }
    return commands;
  }

  public String getName() {
    if (value != getVehicleCA()) {
      return super.getName();
    }
    else {
      return piece.getName();
    }
  }

  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    if (commands[commands.length - 1].matches(stroke)) {
      ChangeTracker c = new ChangeTracker(this);
      flipped = !flipped;
      return c.getChangeCommand();
    }
    else {
      return super.myKeyEvent(stroke);
    }
  }

  public String getDescription() {
    return "Turreted";
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return null;
  }

  public PieceEditor getEditor() {
    return new SimplePieceEditor(this);
  }
}
