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
package VASL.build.module.map;

import VASL.counters.ASLProperties;
import VASSAL.build.AbstractBuildable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.Drawable;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceIterator;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

/**
 * This is a {@link Drawable} class that draws only counters that have the
 * {@link ASLProperties#HINDRANCE} property set.  It is enables when
 * the LOS Thread is being drawn.
 */
public class HindranceKeeper extends AbstractBuildable implements Drawable, KeyListener {
  public static final String DRAW_HINDRANCES = "DrawHindrances";
  private Map map;

  public void addTo(Buildable b) {
    map = (Map) b;
    map.addDrawComponent(this);
    GameModule.getGameModule().getPrefs().addOption
      ("LOS", new BooleanConfigurer(DRAW_HINDRANCES, "Retain LOS-hindrance counters (toggle with shift-F10)"));
    map.getView().addKeyListener(this);
  }

  public String[] getAttributeNames() {
    return new String[0];
  }

  public void setAttribute(String name, Object value) {
  }

  public String getAttributeValueString(String name) {
    return null;
  }

  public void draw(Graphics g, Map m) {
    if (!m.isPiecesVisible()
      && Boolean.TRUE.equals(GameModule.getGameModule().getPrefs().getValue(DRAW_HINDRANCES))) {
      GamePiece[] p = m.getPieces();
      for (int i = 0; i < p.length; ++i) {
        if (p[i] instanceof Stack) {
          for (PieceIterator pi = new PieceIterator(((Stack) p[i]).getPieces());
               pi.hasMoreElements();) {
            draw(pi.nextPiece(), m, g);
          }
        }
        else {
          draw(p[i], m, g);
        }
      }
    }
  }

  private void draw(GamePiece p, Map map, Graphics g) {
    if (p.getProperty(ASLProperties.HINDRANCE) != null
      && !Boolean.TRUE.equals(p.getProperty(Properties.INVISIBLE_TO_ME))
      && !Boolean.TRUE.equals(p.getProperty(Properties.OBSCURED_TO_ME))) {
      java.awt.Point pt = map.componentCoordinates(p.getPosition());
      p.draw(g, pt.x, pt.y, map.getView(), map.getZoom());
    }
  }

  public void keyPressed(KeyEvent e) {
  }

  public void keyReleased(KeyEvent e) {
    if (!map.isPiecesVisible()
      && KeyStroke.getKeyStrokeForEvent(e).equals(KeyStroke.getKeyStroke(KeyEvent.VK_F10, KeyEvent.SHIFT_MASK, true))) {
      Configurer config = GameModule.getGameModule().getPrefs().getOption(DRAW_HINDRANCES);
      config.setValue(Boolean.TRUE.equals(config.getValue()) ? Boolean.FALSE : Boolean.TRUE);
      map.getView().repaint();
    }
  }

  public void keyTyped(KeyEvent e) {
  }
}
