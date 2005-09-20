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
package VASSAL.tools;

import VASSAL.build.GameModule;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/**
 * Displays an image centered on the screen
 */
public class SplashScreen extends JWindow {
  private static SplashScreen instance;

  public static SplashScreen getInstance() {
    return instance;
  }

  public SplashScreen(Image im) {
    instance = this;
    getContentPane().add(new JLabel(new ImageIcon(im)));
    pack();
    Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
    setLocation(d.width / 2 - getSize().width / 2,
                d.height / 2 - getSize().height / 2);
    addMouseListener(new MouseAdapter() {
      public void mouseReleased(MouseEvent evt) {
        setVisible(false);
      }
    });
  }

  public void setVisible(boolean vis) {
    super.setVisible(vis);
    if (vis) {
      toFront();
    }
  }
}
