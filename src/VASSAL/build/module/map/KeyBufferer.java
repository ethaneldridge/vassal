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
package VASSAL.build.module.map;

import VASSAL.build.Buildable;
import VASSAL.build.module.Map;
import VASSAL.counters.*;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;

/**
 * This component listens for mouse clicks on a map.
 * If the user clicks on a {@link GamePiece}, that piece is added to the {@link KeyBuffer}
 *
 * @see Map#addLocalMouseListener
 */
public class KeyBufferer extends MouseAdapter implements Buildable, MouseMotionListener, Drawable {
  protected Map map;
  protected Rectangle selection;
  protected Point anchor;
  protected Color color = Color.black;
  protected int thickness = 3;

  public void addTo(Buildable b) {
    map = (Map) b;
    map.addLocalMouseListener(this);
    map.getView().addMouseMotionListener(this);
    map.addDrawComponent(this);
  }

  public void add(Buildable b) {
  }

  public Element getBuildElement(Document doc) {
    return doc.createElement(getClass().getName());
  }

  public void build(Element e) {
  }

  public void mousePressed(MouseEvent e) {
    if (e.isConsumed()) {
      return;
    }
    GamePiece p = map.findPiece(e.getPoint(), PieceFinder.PIECE_IN_STACK);
    // Don't clear the buffer until we find the clicked-on piece
    // Because selecting a piece affects its visibility
    EventFilter filter = null;
    if (p != null) {
      filter = (EventFilter) p.getProperty(Properties.EVENT_FILTER);
    }
    boolean ignoreEvent = filter != null && filter.rejectEvent(e);
    if (!e.isShiftDown()
        || ignoreEvent) {
      KeyBuffer.getBuffer().clear();
    }
    if (p != null && !ignoreEvent) {
      KeyBuffer.getBuffer().add(p);
      if (p.getParent() != null) {
        map.getPieceCollection().moveToFront(p.getParent());
      }
      else {
        map.getPieceCollection().moveToFront(p);
      }
    }
    else {
      anchor = map.componentCoordinates(e.getPoint());
      selection = new Rectangle(anchor.x, anchor.y, 0, 0);
      if (map.getHighlighter() instanceof ColoredBorder) {
        ColoredBorder b = (ColoredBorder) map.getHighlighter();
        color = b.getColor();
        thickness = Math.max(1,(int)Math.round(map.getZoom()*b.getThickness()));
      }
    }
  }

  public void mouseReleased(MouseEvent evt) {
    if (selection != null) {
      selection.setLocation(map.mapCoordinates(selection.getLocation()));
      selection.width /= map.getZoom();
      selection.height /= map.getZoom();
      PieceVisitorDispatcher d = new PieceVisitorDispatcher(new DeckVisitor() {
        public Object visitDeck(Deck d) {
          return null;
        }

      public Object visitStack(Stack s) {
          if (s.isExpanded()) {
            Point[] pos = new Point[s.getPieceCount()];
            map.getStackMetrics().getContents(s, pos, null, null, s.getPosition().x, s.getPosition().y);
            for (int i=0;i<pos.length;++i) {
              if (selection.contains(pos[i])) {
                KeyBuffer.getBuffer().add(s.getPieceAt(i));
              }
            }
          }
          else if (selection.contains(s.getPosition())) {
            for (int i=0,n=s.getPieceCount();i<n;++i) {
              KeyBuffer.getBuffer().add(s.getPieceAt(i));
            }
          }
          return null;
        }

        public Object visitDefault(GamePiece p) {
          if (!Boolean.TRUE.equals(p.getProperty(Properties.TERRAIN))
              && selection.contains(p.getPosition())) {
            KeyBuffer.getBuffer().add(p);
          }
          return null;
        }
      });
      KeyBuffer.getBuffer().clear();
      map.apply(d);
    }
    selection = null;
  }

  public void mouseDragged(MouseEvent e) {
    if (selection != null) {
      selection.x = Math.min(e.getX(),anchor.x);
      selection.y = Math.min(e.getY(),anchor.y);
      selection.width = Math.abs(e.getX()-anchor.x);
      selection.height = Math.abs(e.getY()-anchor.y);
      map.repaint();
    }
  }

  public void mouseMoved(MouseEvent e) {
  }

  public void draw(Graphics g, Map map) {
    if (selection != null) {
      g.setColor(color);
      for (int i = 0; i < thickness; ++i) {
        g.drawRect(selection.x+i, selection.y+i, selection.width, selection.height);
      }
    }
  }
}
