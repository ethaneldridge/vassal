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

  private String front,back;
  private boolean flipped = false;

  public Turreted() {
    this(ID + "tcaCE;tca;SX;AZ", null);
  }

  public Turreted(String type, GamePiece p) {
    setInner(p);
    mySetType(type);
  }

  public void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    front = st.nextToken();
    back = st.nextToken();
    String embType = ID + ";;" + st.nextToken() + ";Rotate TCA Right;"
        + st.nextToken() + ";Rotate TCA Left;0;0;,+ TCA = 1;,+ TCA = 2;,+ TCA = 3;,+ TCA = 4;,+ TCA = 5;,+ TCA = 6";
    super.mySetType(embType);
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    offsets();
    super.draw(g, x, y, obs, zoom);
    if (Boolean.TRUE.equals(getProperty(Properties.SELECTED))) {
      getInner().draw(g, x, y, obs, zoom);
    }
  }

  public Rectangle boundingBox() {
    offsets();
    return super.boundingBox();
  }

  public Rectangle selectionBounds() {
    return getInner().selectionBounds();
  }

  private void offsets() {
    switch (value) {
      case 1:
        xOff = 20;
        yOff = -10;
        break;
      case 2:
        xOff = 20;
        yOff = 5;
        break;
      case 3:
        xOff = 20;
        yOff = 20;
        break;
      case 4:
        xOff = -10;
        yOff = 20;
        break;
      case 5:
        xOff = -10;
        yOff = 5;
        break;
      case 6:
        xOff = -10;
        yOff = -10;
        break;
    }
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

  public Dimension getCurrentImageSize() {
    if (flipped || value != getVehicleCA()) {
      return super.getCurrentImageSize();
    }
    else {
      return new Dimension(0, 0);
    }
  }

  protected int getVehicleCA() {
    for (GamePiece p = getInner(); p instanceof Decorator;
         p = ((Decorator) p).getInner()) {
      if (p instanceof Embellishment
          && p.getType().indexOf("Rotate") >= 0) {
        return ((Embellishment) p).getValue() + 1;
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
    return ID + front + ';' + back + ';' + upKey + ';' + downKey;
  }

  public String myGetState() {
    return super.myGetState() + ";" + flipped;
  }

  public void mySetState(String s) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ';');
    super.mySetState(st.nextToken());
    flipped = new Boolean(st.nextToken()).booleanValue();
  }

  public KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      KeyCommand[] c = super.myGetKeyCommands();
      commands = new KeyCommand[c.length + 1];
      System.arraycopy(c, 0, commands, 0, c.length);
      commands[c.length]
          = new KeyCommand
              ("Button up",
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
      return getInner().getName();
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
