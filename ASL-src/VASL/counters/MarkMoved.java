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
import VASSAL.command.Command;
import VASSAL.command.TrackPiece;
import VASSAL.counters.*;
import VASSAL.tools.SequenceEncoder;

import javax.swing.*;
import java.awt.*;

/**
 * Allows a piece to be marked as having moved
 */
public class MarkMoved extends Decorator implements EditablePiece {
  public static final String ID = "moved;";

  private static final KeyStroke markStroke = KeyStroke.getKeyStroke('M', java.awt.event.InputEvent.CTRL_MASK);
  private String markImage;
  private boolean hasMoved = false;

  public MarkMoved() {
    this(ID + "moved", null);
  }

  public MarkMoved(String type, GamePiece p) {
    mySetType(type);
    setInner(p);
  }

  public boolean isMoved() {
    return hasMoved;
  }

  public void setMoved(boolean b) {
    hasMoved = b;
  }

  public Object getProperty(Object key) {
    if (Properties.MOVED.equals(key)) {
      return new Boolean(isMoved());
    }
    else {
      return super.getProperty(key);
    }
  }

  public void setProperty(Object key, Object val) {
    if (Properties.MOVED.equals(key)) {
      setMoved(Boolean.TRUE.equals(val));
    }
    else {
      super.setProperty(key, val);
    }
  }

  public void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    markImage = st.nextToken();
  }

  public void mySetState(String newState) {
    hasMoved = "true".equals(newState);
  }

  public String myGetState() {
    return "" + hasMoved;
  }

  public String myGetType() {
    return ID + markImage;
  }

  protected KeyCommand[] myGetKeyCommands() {
    return new KeyCommand[]{new KeyCommand("Mark Moved", markStroke, Decorator.getOutermost(this))};
  }

  public Command myKeyEvent(javax.swing.KeyStroke stroke) {
    if (stroke.equals(markStroke)) {
      TrackPiece c = new TrackPiece(this);
      hasMoved = !hasMoved;
      c.finalize();
      return c;
    }
    else {
      return null;
    }
  }

  public Rectangle selectionBounds() {
    return getInner().selectionBounds();
  }

  public Rectangle boundingBox() {
    Rectangle r = getInner().boundingBox();
    Rectangle r2 = getInner().selectionBounds();
    r2.width += 20;
    return r.union(r2);
  }

  public String getName() {
    return getInner().getName();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    getInner().draw(g, x, y, obs, zoom);
    if (hasMoved) {
      Rectangle r = getInner().selectionBounds();
      Point p = getInner().getPosition();
      try {
        Image im =
          GameModule.getGameModule().getDataArchive().getCachedImage(markImage + ".gif");
        g.drawImage(im,
                    x + (int) (zoom * (r.x - p.x + r.width)),
                    y + (int) (zoom * (r.y - p.y)),
                    (int) (zoom * im.getWidth(obs)),
                    (int) (zoom * im.getHeight(obs)),
                    obs);
      }
      catch (java.io.IOException ex) {
        ex.printStackTrace();
      }
    }
  }

  public String getDescription() {
    return "Can be marked moved";
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return null;
  }

  public PieceEditor getEditor() {
    return new SimplePieceEditor(this);
  }
}
