package VASSAL.build.module.map;

import VASSAL.build.Buildable;
import VASSAL.build.module.Map;
import VASSAL.counters.*;

import java.awt.event.MouseMotionListener;
import java.awt.event.MouseEvent;
import java.awt.*;
import java.util.Enumeration;

/*
 * $Id$
 *
 * Copyright (c) 2003 by Rodney Kinney
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
 * A class for moving and rotating pieces, as for mineatures-based games
 */
public class FreePieceMover extends PieceMover implements MouseMotionListener, Drawable {
  protected GamePiece dragging;
  protected Transparent trans;
  protected FreeRotator rotator;
  protected Point anchor;
  protected Point arrow;

  public void addTo(Buildable b) {
    super.addTo(b);
    ((Map) b).addDrawComponent(this);
    ((Map) b).getView().addMouseMotionListener(this);
  }

  public void mousePressed(MouseEvent e) {
    super.mousePressed(e);
    PieceIterator pi = DragBuffer.getBuffer().getIterator();
    if (pi.hasMoreElements()) {
      dragging = pi.nextPiece();
      trans = new Transparent(dragging);
      anchor = map.componentCoordinates(dragging.getPosition());
      if (dragging instanceof Stack) {
        for (Enumeration enum = ((Stack)dragging).getPieces(); rotator == null && enum.hasMoreElements();) {
          GamePiece p = (GamePiece) enum.nextElement();
          rotator = (FreeRotator) Decorator.getDecorator(p, FreeRotator.class);
        }
      }
      else {
        rotator = (FreeRotator) Decorator.getDecorator(dragging, FreeRotator.class);
      }
      arrow = null;
    }
    else {
      clear();
    }
  }

  protected void clear() {
    dragging = null;
    anchor = null;
    arrow = null;
    rotator = null;
    trans = null;
  }

  public void draw(Graphics g, Map map) {
    if (anchor != null
        && arrow != null) {
      g.setColor(Color.black);
      Point p2 = getDragDestination(map.mapCoordinates(anchor), map.mapCoordinates(arrow));
      if (p2 != null) {
        p2 = map.componentCoordinates(p2);
        g.drawLine(anchor.x, anchor.y, p2.x, p2.y);
        trans.draw(g, p2.x, p2.y, map.getView(), map.getZoom());
      }
    }
  }

  /**
   * A Mouse release event will be translated to this location (in map coordinates).
   * This allows restrictions, such as distance limitations, to be put on where a piece may move.
   * @return null if the move is invalid
   */
  protected Point getDragDestination(Point src, Point dest) {
    Point p = dest;
    if (dest != null
      && src != null) {
      double dist = Math.sqrt(Math.pow(dest.x - src.x, 2.0) + Math.pow((dest.y - src.y), 2.0));
      int maxDrag = getMaxDragDistance(dragging);
      if (dist > maxDrag) {
        p = new Point(src.x + (int) Math.round(maxDrag * (dest.x - src.x) / dist),
                      src.y + (int) Math.round(maxDrag * (dest.y - src.y) / dist));
      }
    }
    return p;
  }

  public void mouseDragged(MouseEvent e) {
    if (e.isShiftDown()) {
      if (rotator != null) {
        Point p = map.mapCoordinates(e.getPoint());
        Point p2 = getDragDestination(map.mapCoordinates(anchor),p);
        double myAngle;
        if (p2.x == p2.x) {
          myAngle = 0.0;
        }
        else {
          myAngle = Math.atan((p.x-p2.x)/(p.y-p2.y));
        }
        rotator.setAngle(myAngle);
      }
    }
    else {
      arrow = e.getPoint();
    }
    map.repaint();
  }

  public void mouseReleased(MouseEvent e) {
    if (anchor != null) {
      Point p = getDragDestination(map.mapCoordinates(anchor),e.getPoint());
      e.translatePoint(p.x - e.getX(), p.y - e.getY());
    }
    super.mouseReleased(e);
    clear();
  }

  /**
   * The maximum distance that a piece may be dragged, in map-coordinate pixels
   * @param piece
   * @return
   */
  protected int getMaxDragDistance(GamePiece piece) {
    return 300;
  }

  public void mouseMoved(MouseEvent e) {
  }

}
