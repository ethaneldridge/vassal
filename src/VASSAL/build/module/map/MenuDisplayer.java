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

import VASSAL.counters.*;
import VASSAL.build.*;
import VASSAL.build.module.Map;

import java.awt.event.*;
import java.awt.*;
import java.util.Vector;
import java.util.Enumeration;
import javax.swing.*;

import org.w3c.dom.*;

public class MenuDisplayer extends MouseAdapter implements Buildable {
  public static Font POPUP_MENU_FONT = new Font("Dialog", 0, 10);

  protected Map map;

  public void addTo(Buildable b) {
    map = (Map) b;
    map.addLocalMouseListener(this);
  }

  public void add(Buildable b) {
    throw new IllegalBuildException("Cannot contain children");
  }

  public Element getBuildElement(Document doc) {
    return doc.createElement(getClass().getName());
  }

  public void build(Element e) {
  }

  public static JPopupMenu createPopup(GamePiece target) {
    JPopupMenu popup = new JPopupMenu();
    KeyCommand c[] = (KeyCommand[]) target.getProperty(Properties.KEY_COMMANDS);
    if (c != null) {
      Vector commands = new Vector();
      Vector strokes = new Vector();
      for (int i = 0; i < c.length; ++i) {
        KeyStroke stroke = c[i].getKeyStroke();
        if (strokes.contains(stroke)) {
          KeyCommand command = (KeyCommand) commands.elementAt(strokes.indexOf(stroke));
          if (command.getName().length() < c[i].getName().length()) {
            commands.setElementAt(c[i], strokes.indexOf(stroke));
          }
        }
        else {
          strokes.addElement(stroke);
          commands.addElement(c[i]);
        }
      }
      for (Enumeration enum = commands.elements();
           enum.hasMoreElements();) {
        popup.add((KeyCommand) enum.nextElement()).setFont(POPUP_MENU_FONT);
      }
    }
    return popup;
  }

  public void mouseReleased(MouseEvent e) {
    if (e.isMetaDown()) {
      final GamePiece p = map.findPiece(e.getPoint(), PieceFinder.PIECE_IN_STACK);
      if (p != null
        && (e.isShiftDown()
        || !Boolean.TRUE.equals(p.getProperty(Properties.IMMOBILE)))) {
        JPopupMenu popup = createPopup(p);
        Point pt = map.componentCoordinates(e.getPoint());
        popup.addPopupMenuListener(new javax.swing.event.PopupMenuListener() {
          public void popupMenuCanceled
            (javax.swing.event.PopupMenuEvent evt) {
            map.repaint();
          }

          public void popupMenuWillBecomeInvisible
            (javax.swing.event.PopupMenuEvent evt) {
            KeyBuffer.getBuffer().add(p);
            map.repaint();
          }

          public void popupMenuWillBecomeVisible
            (javax.swing.event.PopupMenuEvent evt) {
          }
        });
        KeyBuffer.getBuffer().clear();
        popup.show(map.getView(), pt.x, pt.y);
        e.consume();
      }
    }
  }
}
