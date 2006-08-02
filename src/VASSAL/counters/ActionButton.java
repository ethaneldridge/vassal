/*
 * $Id$
 *
 * Copyright (c) 2000-2006 by Rodney Kinney
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

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.net.MalformedURLException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import javax.swing.Box;
import javax.swing.KeyStroke;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.counters.ActionButton.ButtonPusher.ComponentMouseListener;
import VASSAL.tools.SequenceEncoder;

/**
 * A trait that acts like a button on a GamePiece, such that clicking on a
 * particular area of the piece invokes a keyboard command
 * 
 * @author rkinney
 * 
 */
public class ActionButton extends Decorator implements EditablePiece {
  public static final String ID = "button;";
  protected KeyStroke stroke;
  protected Rectangle bounds = new Rectangle();
  protected ButtonPusher pusher;
  protected static ButtonPusher globalPusher = new ButtonPusher();

  public ActionButton() {
    this(ID, null);
  }

  public ActionButton(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
    pusher = globalPusher;
  }

  public void mySetState(String newState) {
  }

  public String myGetState() {
    return "";
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(stroke).append(bounds.x).append(bounds.y).append(bounds.width).append(bounds.height);
    return ID + se.getValue();
  }

  protected KeyCommand[] myGetKeyCommands() {
    return null;
  }

  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
    if (getMap() != null) {
      pusher.register(getMap());
    }
    else {
      pusher.register(obs, Decorator.getOutermost(this), x, y);
    }
  }

  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  public Shape getShape() {
    return piece.getShape();
  }

  public String getName() {
    return piece.getName();
  }

  public String getDescription() {
    return "Action Button - " + HotKeyConfigurer.getString(stroke);
  }

  public void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    stroke = st.nextKeyStroke('A');
    bounds.x = st.nextInt(-20);
    bounds.y = st.nextInt(-20);
    bounds.width = st.nextInt(40);
    bounds.height = st.nextInt(40);
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "ActionButton.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public static class Ed implements PieceEditor {
    private Box box;
    private IntConfigurer xConfig;
    private IntConfigurer yConfig;
    private IntConfigurer widthConfig;
    private IntConfigurer heightConfig;
    private HotKeyConfigurer strokeConfig;

    public Ed(ActionButton p) {
      box = Box.createVerticalBox();
      strokeConfig = new HotKeyConfigurer(null, "Invoke Key Command:  ", p.stroke);
      box.add(strokeConfig.getControls());
      xConfig = new IntConfigurer(null, "Button X-offset ", new Integer(p.bounds.x));
      box.add(xConfig.getControls());
      yConfig = new IntConfigurer(null, "Button Y-offset", new Integer(p.bounds.y));
      box.add(yConfig.getControls());
      widthConfig = new IntConfigurer(null, "Button Width", new Integer(p.bounds.width));
      box.add(widthConfig.getControls());
      heightConfig = new IntConfigurer(null, "Button Height", new Integer(p.bounds.height));
      box.add(heightConfig.getControls());
    }

    public Component getControls() {
      return box;
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(strokeConfig.getValueString()).append(xConfig.getValueString()).append(yConfig.getValueString()).append(widthConfig.getValueString()).append(
          heightConfig.getValueString());
      return ID + se.getValue();
    }

    public String getState() {
      return "";
    }
  }

  /**
   * Registers mouse listeners with Maps and other components. Clicking the
   * mouse checks for pieces with an ActionButton trait and invokes them if the
   * click falls within the button's boundaries
   */
  protected static class ButtonPusher {
    private Set maps = new HashSet();
    private java.util.Map components = new HashMap();

    public void register(Map map) {
      if (map != null) {
        if (!maps.contains(map)) {
          map.addLocalMouseListener(new MapMouseListener(map));
          maps.add(map);
        }
      }
    }

    public void register(Component obs, GamePiece piece, int x, int y) {
      if (obs != null) {
        ComponentMouseListener l = (ComponentMouseListener) components.get(obs);
        if (l == null) {
          l = new ComponentMouseListener(piece, x, y);
          obs.addMouseListener(l);
          components.put(obs,l);
        }
        else {
          l.xOffset = x;
          l.yOffset = y;
          l.target = piece;
        }
      }
    }

    /**
     * Handle a mouse click on the given GamePiece at the given location (where
     * 0,0 is the center of the piece)
     * 
     * @param p
     * @param x
     * @param y
     * @param Offset
     *          A function to determine the offset of the target piece. This is
     *          done for efficiency reasons, since computing the offset may be
     *          expensive (as in the case of a piece in an expanded stack on a
     *          map) and is only needed if the piece has the ActionButton trait
     */
    public void doClick(GamePiece p, Point point, Offset offset) {
      for (GamePiece piece = p; piece instanceof Decorator; piece = ((Decorator) piece).getInner()) {
        if (piece instanceof ActionButton) {
          ActionButton action = (ActionButton) piece;
          Point relative = offset.getRelativeOffset(point);
          System.err.println(point+"->"+relative);
          if (action.bounds.contains(relative)) {
            System.err.println(action.bounds+" contains "+relative);
            p.setProperty(Properties.SNAPSHOT, PieceCloner.getInstance().clonePiece(p)); // save state prior to command
            Command command = p.keyEvent(action.stroke);
            GameModule.getGameModule().sendAndLog(command);
          }
          else {
            System.err.println("MISS:  "+action.bounds+" does not contain "+relative);
          }
        }
      }
    }

    protected static interface Offset {
      /**
       * Given a click at the original location, find the offset relative to the
       * position of the target GamePiece
       */
      Point getRelativeOffset(Point originalLocation);
    }

    protected class MapMouseListener extends MouseAdapter {
      private Map map;

      public MapMouseListener(Map map) {
        this.map = map;
      }

      public void mouseReleased(MouseEvent e) {
        final GamePiece p = map.findPiece(e.getPoint(), PieceFinder.PIECE_IN_STACK);
        if (p != null) {
          doClick(p, e.getPoint(), new Offset() {
            public Point getRelativeOffset(Point point) {
              Point offset = new Point(point);
              Point rel = map.positionOf(p);
              offset.translate(rel.x, rel.y);
              return offset;
            }
          });
        }
      }
    }

    protected class ComponentMouseListener extends MouseAdapter implements Offset {
      private GamePiece target;
      private int xOffset;
      private int yOffset;
      private MouseEvent mouseDown;

      public ComponentMouseListener(GamePiece piece, int x, int y) {
        target = piece;
        xOffset = x;
        yOffset = y;
      }

      public void mouseReleased(MouseEvent e) {
        System.err.println(e);
        System.err.println(e.getSource());
        if (mouseDown != null && mouseDown.getPoint().distance(e.getPoint()) < 5) {
          doClick(target, e.getPoint(), this);
        }
        mouseDown = null;
        e.getComponent().repaint();
      }
      
      public void mousePressed(MouseEvent e) {
        System.err.println(e);
        System.err.println(e.getSource());
        mouseDown = e;
      }

      public Point getRelativeOffset(Point originalLocation) {
        return new Point(originalLocation.x - xOffset, originalLocation.y - yOffset);
      }
    }

  }

}
