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
import javax.swing.*;

import org.w3c.dom.*;

/**
 * This component listens for mouse clicks on a map.  If the user clicks on a {@link GamePiece}, that piece is added to the {@link KeyBuffer}
 *
 * @see Map#addLocalMouseListener
 */
public class KeyBufferer extends MouseAdapter implements Buildable {
  protected Map map;
  private boolean warnEnabled = true;
  private JOptionPane warnPane;
  private JDialog warnDialog;
  private GamePiece immobileTarget;

  public void addTo(Buildable b) {
    map = (Map) b;
    map.addLocalMouseListener(this);
  }

  public void add(Buildable b) {
  }

  public Element getBuildElement(Document doc) {
    return doc.createElement(getClass().getName());
  }

  public void build(Element e) {
  }

  public void mousePressed(MouseEvent e) {
//    lastTarget = null;
    if (e.isConsumed()) {
      return;
    }
    GamePiece p = map.findPiece(e.getPoint(), PieceFinder.PIECE_IN_STACK);
    // Don't clear the buffer until we find the clicked-on piece
    // Because selecting a piece affects its visibility
    boolean isImmobile = p != null && Boolean.TRUE.equals(p.getProperty(Properties.IMMOBILE));
    if (!e.isShiftDown()
      || isImmobile) {
      KeyBuffer.getBuffer().clear();
    }
    if (p != null) {
      if (e.isShiftDown()
        || !isImmobile) {
        KeyBuffer.getBuffer().add(p);
        if (!Boolean.TRUE.equals(p.getProperty(Properties.NO_STACK))) {
          if (p.getParent() != null) {
            map.getPieceCollection().moveToFront(p.getParent());
          }
          else {
            map.getPieceCollection().moveToFront(p);
          }
        }
      }
      else { // User has clicked on an immobile piece without the shift key
        if (!KeyBuffer.getBuffer().contains(p)) {
          immobileTarget = p;
        }
      }
    }
  }

  public void mouseReleased(MouseEvent evt) {
    if (warnEnabled
      && !evt.isConsumed()) {
/*
      GamePiece p = map.findPiece(evt.getPoint(), PieceFinder.PIECE_IN_STACK);
      if (p != null
        && p == lastTarget
        && !evt.isShiftDown()) {
*/
      if (immobileTarget != null) {
        showWarning(immobileTarget);
      }
    }
    immobileTarget = null;
  }

  private void showWarning(GamePiece p) {
    if (warnPane == null) {
      JButton okButton = new JButton("Ok");
      JButton disableButton = new JButton("Don't show this dialog again");
      warnPane = new JOptionPane
        ("To select this piece, click while holding down the shift key.\nTo move, shift-click to select it, then drag normally.",
         JOptionPane.DEFAULT_OPTION,
         JOptionPane.INFORMATION_MESSAGE,
         UIManager.getIcon("OptionPane.errorIcon"),
         new Object[]{okButton, disableButton},
         okButton);
      okButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          warnDialog.dispose();
        }
      });
      disableButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          warnEnabled = false;
          warnDialog.dispose();
        }
      });
    }
    java.awt.Component comp = null;
    if (p != null
      && p.getMap() != null) {
      comp = p.getMap().getView().getTopLevelAncestor();
    }
    else if (GameModule.getGameModule() != null) {
      comp = GameModule.getGameModule().getFrame();
    }
    warnDialog = warnPane.createDialog(comp, "Non-stacking piece");
    warnDialog.show();
  }
}
