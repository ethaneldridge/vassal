/*
 * $Id$
 *
 * Copyright (c) 2003 by David Sullivan and Rodney Kinney
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
package VASSAL.build.module.map;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.IllegalBuildException;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.counters.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.io.File;
import java.net.MalformedURLException;
import java.util.Enumeration;
import java.util.Vector;

/**
 * This is a {@link Drawable} class that draws the counters horizontally
 * when the mouse is held over a stack with the control key down.
 *
 * @author       David Sullivan
 * @version      1.0
 */
public class CounterDetailViewer
  extends AbstractConfigurable
  implements Drawable,
  MouseMotionListener, Runnable {

  public static final String SHOW_DETAILS = "ShowCounterDetails";

  protected Map map;
  protected Thread delayThread;
  protected int delay = 700;
  protected long expirationTime;
  protected boolean visible = false;
  protected MouseEvent currentMousePosition;
  protected GamePiece currentPiece;

  public void addTo(Buildable b) {
    map = (Map) b;
    Enumeration e = map.getComponents(getClass());
    if (e.hasMoreElements()) {
      throw new IllegalBuildException("Mouse-over Stack Viewer already enabled");
    }
    map.addDrawComponent(this);
    GameModule.getGameModule().getPrefs().addOption
      ("General", new BooleanConfigurer(SHOW_DETAILS, "Enable mouse-over stack details", Boolean.TRUE));

    map.getView().addMouseMotionListener(this);
  }

  public void draw(Graphics g, Map map) {
    if (currentMousePosition != null) {
      draw(g,currentMousePosition.getPoint(),map.getView());
    }
  }

  public void draw(Graphics g, Point pt, JComponent comp) {
    if (visible && currentPiece != null) {
      Enumeration pieces;
      if (currentPiece instanceof Stack) {
        pieces = ((Stack) currentPiece).getPieces();
      }
      else {
        pieces = new Enumeration() {
          boolean finished = false;

          public boolean hasMoreElements() {
            return !finished;
          }

          public Object nextElement() {
            finished = true;
            return currentPiece;
          }
        };
      }
      PieceIterator pi = PieceIterator.visible(pieces);
      Rectangle bounds = new Rectangle(pt.x,pt.y,0,0);
      Vector v = new Vector();
      while (pi.hasMoreElements()) {
        GamePiece piece = pi.nextPiece();
        v.addElement(piece);
        bounds.width += piece.selectionBounds().width;
        bounds.height = Math.max(bounds.height,piece.selectionBounds().height);
      }
      if (bounds.width > 0) {
        Rectangle r = comp.getVisibleRect();
        bounds.x = Math.min(bounds.x,r.x+r.width-bounds.width);
        bounds.y = Math.min(bounds.y,r.y+r.height-bounds.height);
        Color outline = map.getHighlighter() instanceof ColoredBorder ? ((ColoredBorder)map.getHighlighter()).getColor() : Color.black;;
        Color background = new Color(255-outline.getRed(),255-outline.getGreen(),255-outline.getBlue());
        g.setColor(background);
        g.fillRect(bounds.x-1,bounds.y-1,bounds.width+2,bounds.height+2);
        g.setColor(outline);
        g.drawRect(bounds.x-2,bounds.y-2,bounds.width+3,bounds.height+3);
        g.drawRect(bounds.x-3,bounds.y-3,bounds.width+5,bounds.height+5);
        pi = new PieceIterator(v.elements());
        while (pi.hasMoreElements()) {
          // Draw the next piece
          // pt is the location of the left edge of the piece
          GamePiece piece = pi.nextPiece();
          piece.draw(g, bounds.x + piece.getPosition().x - piece.selectionBounds().x,
                     bounds.y + piece.getPosition().y - piece.selectionBounds().y, comp, 1.0);

          bounds.translate(piece.selectionBounds().width, 0);
        }
      }
    }
  }

  public void run() {
    while (System.currentTimeMillis() < expirationTime) {
      try {
        Thread.sleep(Math.max(0, expirationTime - System.currentTimeMillis()));
      }
      catch (InterruptedException e) {
      }
    }
    currentPiece = findPieceAtMousePosition();
    visible = shouldBeVisible();
    map.repaint();
  }

  protected boolean shouldBeVisible() {
    boolean val = false;
    if (currentPiece != null) {
      if (map.getZoom() < 0.5) {
        val = !Boolean.TRUE.equals(currentPiece.getProperty(Properties.IMMOBILE));
      }
      else if (currentPiece instanceof Stack) {
        Stack s = (Stack) currentPiece;
        val = !s.isExpanded()
          && s.topPiece() != s.bottomPiece();
      }
    }
    return val;
  }

  protected GamePiece findPieceAtMousePosition() {
    GamePiece p = map.findPiece(map.mapCoordinates(currentMousePosition.getPoint()), PieceFinder.MOVABLE);
    if (p != null && p.getParent() != null) {
      p = p.getParent();
    }
    return p;
  }

  public void mouseMoved(MouseEvent e) {
    // quit if not active
    if (Boolean.FALSE.equals(GameModule.getGameModule().getPrefs().getValue(SHOW_DETAILS))) {
      return;
    }

    // clear details when mouse moved
    if (visible) {
      visible = false;
      map.repaint();
    }
    else {
      // set the timer
      currentMousePosition = e;
      expirationTime = System.currentTimeMillis() + delay;
      if (delayThread == null || !delayThread.isAlive()) {
        delayThread = new Thread(this);
        delayThread.start();
      }
    }
  }

  public void mouseDragged(MouseEvent e) {
    mouseMoved(e);
  }

  public Configurer getConfigurer() {
    return null;
  }

  public String[] getAttributeNames() {
    return new String[0];
  }

  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  public Class[] getAttributeTypes() {
    return new Class[0];
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public HelpFile getHelpFile() {
    File dir = new File("docs");
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "Map.htm"), "#StackViewer");
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public void removeFrom(Buildable parent) {
    map.removeDrawComponent(this);
    map.getView().removeMouseMotionListener(this);
  }

  public void setAttribute(String name, Object value) {
  }

  public String getAttributeValueString(String name) {
    return null;
  }

  public static String getConfigureTypeName() {
    return "Mouse-over Stack Viewer";
  }

}

