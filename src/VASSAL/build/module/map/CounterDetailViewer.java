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

import VASSAL.build.*;
import VASSAL.build.module.map.*;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.counters.*;
import VASSAL.command.Command;
import VASSAL.configure.*;

import java.awt.event.*;

import javax.swing.Timer;


import java.awt.*;

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
  MouseMotionListener,
  GameComponent {

  public static final String SHOW_DETAILS = "ShowCounterDetails";

  private Map map;
  private Timer timer;
  private int delay = 700;
  private boolean visible = false;
  private Stack currentPiece;

  public void addTo(Buildable b) {
    map = (Map) b;
    map.addDrawComponent(this);
    GameModule.getGameModule().getPrefs().addOption
      ("General", new BooleanConfigurer(SHOW_DETAILS, "Enable mouse-over stack details", Boolean.TRUE));

    GameModule.getGameModule().getGameState().addGameComponent(this);
    map.getView().addMouseMotionListener(this);

  }

  public void draw(Graphics g, Map m) {
    if (visible && (currentPiece).getPieceCount() > 1) {

      int translateWidth = 0;

      Point pt = map.componentCoordinates(currentPiece.getPosition());
      PieceFilter visibleFilter = new PieceFilter() {
        public boolean accept(GamePiece piece) {
          return !Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME));
        }
      };
      for (PieceIterator pi = new PieceIterator(currentPiece.getPieces(),visibleFilter); pi.hasMoreElements();) {

        // Draw the next piece
        // pt is the location of the left edge of the piece
        GamePiece piece = pi.nextPiece();
        piece.draw(g, pt.x+piece.getPosition().x-piece.selectionBounds().x, pt.y, map.getView(), 1.0);

        pt.translate(piece.selectionBounds().width,0);
      }
    }
  }

  public void mouseMoved(MouseEvent e) {

    // quit if not active
    if (Boolean.FALSE.equals(GameModule.getGameModule().getPrefs().getValue(SHOW_DETAILS))) {
      return;
    }

    // clear details when mouse moved
    if (visible) {

      visible = false;
      timer.stop();
      map.repaint();
    }

    // set the timer
    else {
      // are we on a counter?
      GamePiece[] p = map.getPieces();

      for (int i = 0; i < p.length; ++i) {
        if (p[i] instanceof Stack) {
          if (p[i].selectionBounds().contains(map.mapCoordinates(e.getPoint()))) {
            currentPiece = (Stack) p[i];
            timer.start();
            return;
          }
        }
      }

      // not on a counter
      timer.stop();
    }
  }

  public void setup(boolean arg0) {

    // initiate the timer
    timer = new Timer(delay, new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        processTimer();
      }
    });
    timer.setCoalesce(true);
    timer.setDelay(delay);
    timer.setRepeats(false);
    timer.stop();
  }

  private void processTimer() {
    visible = true;
    map.repaint();
  }

  public void mouseDragged(MouseEvent e) {
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
    return null;
  }

  public void removeFrom(Buildable parent) {
    map.removeDrawComponent(this);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    map.getView().removeMouseMotionListener(this);
  }

  public void setAttribute(String name, Object value) {
  }

  public String getAttributeValueString(String name) {
    return null;
  }

  public Command getRestoreCommand() {
    return null;
  }

  public static String getConfigureTypeName() {
    return "Mouse-over Stack viewer";
  }

}

