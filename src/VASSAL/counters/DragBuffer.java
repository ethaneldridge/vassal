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
package VASSAL.counters;

import VASSAL.tools.Sort;
import VASSAL.tools.TransparentFilter;
import VASSAL.build.module.Map;

import javax.swing.*;
import java.awt.*;
import java.awt.image.FilteredImageSource;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Enumeration;
import java.util.Vector;

public class DragBuffer {
  private static DragBuffer theBuffer;
  private Vector pieces;
  private MouseEvent lastRelease;
  private Component dropTarget;
  private MouseListener dropHandler;

  private DragBuffer() {
    pieces = new Vector();
  }

  public static void init(DragBuffer db) {
    if (theBuffer == null)
      theBuffer = db;
  }

  public static DragBuffer getBuffer() {
    if (theBuffer == null) {
      theBuffer = new DragBuffer();
    }
    return theBuffer;
  }

  public void add(GamePiece p) {
    if (p != null
      && !pieces.contains(p)
      && !Boolean.TRUE.equals(p.getProperty(Properties.RESTRICTED))) {
      if (p instanceof Stack) {
        for (Enumeration e = ((Stack) p).getPieces(); e.hasMoreElements();) {
          if (Boolean.TRUE.equals(((GamePiece) e.nextElement()).getProperty(Properties.RESTRICTED))) {
            return;
          }
        }
        pieces.addElement(p);
      }
      else {
        pieces.addElement(p);
      }
    }
  }

  public void clear() {
    pieces.removeAllElements();
  }

  public void addDragSource(Component c) {
    c.addMouseListener(new MouseAdapter() {
      public void mousePressed(MouseEvent e) {
        lastRelease = null;
        dropTarget = null;
        dropHandler = null;
        e.getComponent().setCursor(createDragCursor(e.getComponent()));
      }

      public void mouseReleased(MouseEvent e) {
        e.getComponent().setCursor(null);
        Component source = (Component) e.getSource();
        if (dropTarget == null) {
          e.translatePoint(source.getLocationOnScreen().x,
                           source.getLocationOnScreen().y);
          lastRelease = e;
        }
        else {
          e.translatePoint(source.getLocationOnScreen().x,
                           source.getLocationOnScreen().y);
          e.translatePoint(-dropTarget.getLocationOnScreen().x,
                           -dropTarget.getLocationOnScreen().y);
          dropHandler.mouseReleased(e);
        }
      }
    });
  }

  public void addDropTarget(final Component c, final MouseListener l) {
    c.addMouseListener(new MouseAdapter() {
      public void mouseEntered(MouseEvent e) {
        Component source = (Component) e.getSource();
        if (source.isShowing()) {
          if (lastRelease != null) {
            e.translatePoint(source.getLocationOnScreen().x,
                             source.getLocationOnScreen().y);
            if (isCloseEnough(e.getPoint(), lastRelease.getPoint())) {
              e.translatePoint(-source.getLocationOnScreen().x,
                               -source.getLocationOnScreen().y);
              l.mouseReleased(e);
            }
          }
          else {
            dropTarget = source;
            dropHandler = l;
          }
        }
      }
    });
  }

  private boolean isCloseEnough(Point p1, Point p2) {
    return Math.abs(p1.x - p2.x) < 3
      && Math.abs(p1.y - p2.y) < 3;
  }

  /**
   * Forward a MouseReleased event to a GameComponent other than
   * the originating component.  This is a primitive Drag-and-Drag.
   public void forwardMouseReleased(MouseEvent e) {
   Component source = (Component)e.getSource();
   e.translatePoint(source.getLocationOnScreen().x,
   source.getLocationOnScreen().y);
   if (dropTarget != null && dropTargetHandler != null) {
   e.translatePoint(-dropTarget.getLocationOnScreen().x,
   -dropTarget.getLocationOnScreen().y);
   dropTargetHandler.mouseReleased(e);
   }
   }
   */

  public void remove(GamePiece p) {
    pieces.removeElement(p);
  }

  public boolean contains(GamePiece p) {
    return pieces.contains(p);
  }

  public PieceIterator getIterator() {
    return new PieceIterator(pieces.elements());
  }

  public String contents() {
    String s = "";
    for (Enumeration e = pieces.elements();
         e.hasMoreElements();) {
      GamePiece p = (GamePiece) e.nextElement();
      s = s.concat(p.getName());
      if (e.hasMoreElements())
        s = s.concat(",");
    }
    return s;
  }

  public static void main(String args[]) {
    JFrame f1 = new JFrame();
    f1.setSize(200, 200);
    f1.setVisible(true);
    JFrame f2 = new JFrame();
    f2.setSize(200, 200);
    f2.setLocation(200, 0);
    f2.setVisible(true);
    MouseListener l = new MouseAdapter() {
      public void mousePressed(MouseEvent evt) {
        evt.translatePoint(((JFrame) evt.getSource()).getLocationOnScreen().x, ((JFrame) evt.getSource()).getLocationOnScreen().y);
        System.err.println("Press at " + evt.getPoint());
      }

      public void mouseReleased(MouseEvent evt) {
        //		    evt.translatePoint(((JFrame)evt.getSource()).getLocationOnScreen().x,((JFrame)evt.getSource()).getLocationOnScreen().y);
        System.err.println("Release at " + evt.getPoint());
      }

      public void mouseEntered(MouseEvent evt) {
        evt.translatePoint(((JFrame) evt.getSource()).getLocationOnScreen().x, ((JFrame) evt.getSource()).getLocationOnScreen().y);
        System.err.println("Enter at " + evt.getPoint());
      }
    };
    //	f1.addMouseListener(l);
    //	f2.addMouseListener(l);
    DragBuffer.getBuffer().addDragSource(f1);
    DragBuffer.getBuffer().addDropTarget(f2, l);
  }

  public Cursor createDragCursor(Component comp) {
    Cursor c = null;
    if (pieces.size() > 0) {
      c = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);
/*
      GamePiece bottom = (GamePiece) pieces.firstElement();
      Rectangle outline = bottom.selectionBounds();
      Point offset;
      ColoredBorder border = new ColoredBorder();
      double zoom = 1.0;
      if (comp instanceof Map.View) {
        Map map = ((Map.View) comp).getMap();
        zoom = map.getZoom();
        if (map.getHighlighter() instanceof ColoredBorder) {
          border = (ColoredBorder) map.getHighlighter();
        }
      }
      outline.width *= zoom;
      outline.height *= zoom;
      Dimension d = Toolkit.getDefaultToolkit().getBestCursorSize(outline.width,outline.height);
      int thickness = (int) Math.round(zoom * border.getThickness());
      if (outline.width != d.width
        || outline.height != d.height) {
        outline.setSize(d);
        offset = new Point(outline.width/2,outline.height/2);
      }
      else {
        offset = new Point((int) Math.round(zoom * (bottom.getPosition().x - outline.x)),
                           (int) Math.round(zoom * (bottom.getPosition().y - outline.y)));
      }
      Image im = comp.createImage(outline.width, outline.height);
      im.getGraphics().setColor(border.getColor());
      for (int i = 0; i < thickness; ++i) {
        im.getGraphics().drawRect(i, i, outline.width - 2*i-1, outline.height - 2*i-1);
      }
      TransparentFilter f = new TransparentFilter();
      f.setAlpha(0.0, TransparentFilter.getOffscreenEquivalent(comp.getBackground().getRGB(), comp));
      im = comp.createImage(new FilteredImageSource(im.getSource(), f));
      try {
        c = Toolkit.getDefaultToolkit().createCustomCursor(im, offset, "DragBuffer");
      }
      catch (IndexOutOfBoundsException e) {
        c = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);
      }
*/
    }
    return c;
  }

  public void sort(Sort.Comparator comp) {
    Sort.quicksort(pieces, comp);
  }
}

