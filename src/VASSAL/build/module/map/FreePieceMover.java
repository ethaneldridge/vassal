package VASSAL.build.module.map;

import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.counters.*;
import VASSAL.command.Command;
import VASSAL.command.AddPiece;
import VASSAL.command.ChangeTracker;

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
  protected GamePiece ghost;
  protected GamePiece dragging;
  protected Transparent trans;
  protected FreeRotator ghostRotator;
  protected FreeRotator draggingRotator;
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
      anchor = map.componentCoordinates(dragging.getPosition());
      if (dragging instanceof Stack) {
        for (Enumeration enum = ((Stack) dragging).getPieces(); enum.hasMoreElements();) {
          dragging = (GamePiece) enum.nextElement();
          break;
        }
      }
      ghost = ((AddPiece) GameModule.getGameModule().decode(GameModule.getGameModule().encode(new AddPiece(dragging)))).getTarget();
      ghostRotator = (FreeRotator) Decorator.getDecorator(ghost, FreeRotator.class);
      draggingRotator = (FreeRotator) Decorator.getDecorator(dragging, FreeRotator.class);
      trans = new Transparent(ghost);
      trans.setAlpha(0.4);
      arrow = null;
    }
    else {
      clear();
    }
  }

  protected void clear() {
    ghost = null;
    anchor = null;
    arrow = null;
    ghostRotator = null;
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
      int maxDrag = getMaxDragDistance(ghost);
      if (dist > maxDrag) {
        p = new Point(src.x + (int) Math.round(maxDrag * (dest.x - src.x) / dist),
                      src.y + (int) Math.round(maxDrag * (dest.y - src.y) / dist));
      }
    }
    return p;
  }

  public void mouseDragged(MouseEvent e) {
    if (e.isShiftDown()) {
      if (ghostRotator != null) {
        Point p = map.mapCoordinates(e.getPoint());
        Point p2 = getDragDestination(map.mapCoordinates(anchor), map.mapCoordinates(arrow));
        double myAngle;
        if (p.y == p2.y) {
          myAngle = p.x < p2.x ? Math.PI / 2 : -Math.PI / 2;
        }
        else {
          myAngle = Math.atan((p.x - p2.x) / (p2.y - p.y));
          if (p2.y < p.y) {
            myAngle += Math.PI;
          }
        }
        System.err.println("Angle " + myAngle + ", mouse " + p + ", arrow " + p2);
        ghostRotator.setAngle(-180. * myAngle / Math.PI);
      }
    }
    else {
      arrow = e.getPoint();
    }
    map.repaint();
  }

  public void mouseReleased(MouseEvent e) {
    if (anchor != null) {
      Point p = getDragDestination(map.mapCoordinates(anchor), e.getPoint());
      e.translatePoint(p.x - e.getX(), p.y - e.getY());
    }
    super.mouseReleased(e);
    clear();
  }

  public Command movePieces(Map m, Point p) {
    Command c = super.movePieces(m,p);
    if (draggingRotator != null && ghostRotator != null) {
      ChangeTracker tracker = new ChangeTracker(dragging);
      draggingRotator.setAngle(ghostRotator.getAngle());
      c.append(tracker.getChangeCommand());
    }
    return c;
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
