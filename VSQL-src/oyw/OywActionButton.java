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
package oyw;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
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
import VASSAL.counters.Decorator;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyCommand;
import VASSAL.counters.PieceCloner;
import VASSAL.counters.PieceEditor;
import VASSAL.counters.PieceFinder;
import VASSAL.counters.Properties;
import VASSAL.tools.SequenceEncoder;

/**
 * A trait that acts like a button on a GamePiece, such that clicking on a
 * particular area of the piece invokes a keyboard command
 * 
 * @author rkinney
 * 
 */
public class OywActionButton extends Decorator implements EditablePiece {
  public static final String ID = "button;";
  protected KeyStroke stroke;
  protected Rectangle bounds = new Rectangle();
  protected ButtonPusher pusher;
  protected static ButtonPusher globalPusher = new ButtonPusher();

  public OywActionButton() {
    this(ID, null);
  }

  public OywActionButton(String type, GamePiece inner) {
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
    return stroke == null ? "Action Button" : "Action Button - " + HotKeyConfigurer.getString(stroke);
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

    public Ed(OywActionButton p) {
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
          components.put(obs, l);
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
     *          A function to determine the offset of the target piece. This
     *          callback is done for efficiency reasons, since computing the
     *          offset may be expensive (as in the case of a piece in an
     *          expanded stack on a map) and is only needed if the piece has the
     *          ActionButton trait
     */
    public void doClick(GamePiece p, Point point) {
      for (GamePiece piece = p; piece instanceof Decorator; piece = ((Decorator) piece).getInner()) {
        if (piece instanceof OywActionButton) {
          OywActionButton action = (OywActionButton) piece;
          if (action.bounds.contains(point)) {
            // Save state prior to command
            p.setProperty(Properties.SNAPSHOT, PieceCloner.getInstance().clonePiece(p));
            Command command = p.keyEvent(action.stroke);
            GameModule.getGameModule().sendAndLog(command);
          }
        }
      }
    }

    protected class MapMouseListener extends MouseAdapter {
      private Map map;

      public MapMouseListener(Map map) {
        this.map = map;
      }

      public void mouseClicked(MouseEvent e) {
        Point point = e.getPoint();
        final GamePiece p = map.findPiece(point, PieceFinder.PIECE_IN_STACK);
        if (p != null) {
          //Point rel = map.positionOf(p);
          Point rel = p.getPosition();
          point.translate(-rel.x, -rel.y);
          doClick(p, point);
        }
      }
    }

    protected class ComponentMouseListener extends MouseAdapter {
      private GamePiece target;
      private int xOffset;
      private int yOffset;

      public ComponentMouseListener(GamePiece piece, int x, int y) {
        target = piece;
        xOffset = x;
        yOffset = y;
      }

      public void mouseClicked(MouseEvent e) {
        Point point = e.getPoint();
        point.translate(-xOffset,-yOffset);
        doClick(target, point);
        e.getComponent().repaint();
      }
    }

  }

}
