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

import VASSAL.Info;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.AffineTransform;

public class ColoredBorder implements Highlighter {
  private Color c;
  private int thickness;

  public ColoredBorder() {
    this(Color.black, 3);
  }

  public ColoredBorder(Color c, int thickness) {
    this.c = c;
    this.thickness = thickness;
  }

  public void draw(GamePiece p, Graphics g, int x, int y, Component obs, double zoom) {
    if (Info.is2dEnabled() && g instanceof Graphics2D) {
      Graphics2D g2d = (Graphics2D) g;
      Shape s = (Shape) p.getProperty(Properties.SHAPE);
      if (s != null) {
        Rectangle r = ((JComponent)obs).getVisibleRect();
        x -= r.x*zoom;
        y -= r.y*zoom;
        Stroke str = g2d.getStroke();
        AffineTransform t = g2d.getTransform();
        g2d.setStroke(new BasicStroke(thickness));
        g2d.setColor(c);
        AffineTransform temp = AffineTransform.getTranslateInstance(x/zoom-p.getPosition().x,y/zoom-p.getPosition().y);
        temp.scale(zoom,zoom);
        g2d.setTransform(temp);
        g2d.draw(s);
        g2d.setStroke(str);
        g2d.setTransform(t);
      }
      else {
        highlightSelectionBounds(p,g,x,y,obs,zoom);
      }
    }
    else {
      highlightSelectionBounds(p, g, x, y, obs, zoom);
    }
  }

  private void highlightSelectionBounds(GamePiece p, Graphics g, int x, int y, Component obs, double zoom) {
    Rectangle r = p.selectionBounds();
    r.translate(-p.getPosition().x, -p.getPosition().y);
    g.setColor(c);
    for (int i = 1; i < thickness; ++i)
      g.drawRect(x + (int) (zoom * r.x) - i,
                 y + (int) (zoom * r.y) - i,
                 (int) (zoom * r.width) + 2 * i - 1,
                 (int) (zoom * r.height) + 2 * i - 1);
  }

  public java.awt.Rectangle boundingBox(GamePiece p) {
    Rectangle r = p.selectionBounds();
    r.translate(-thickness, -thickness);
    r.setSize(r.width + 2 * thickness, r.height + 2 * thickness);
    return r;
  }

  public void setColor(Color c) {
    this.c = c;
  }

  public Color getColor() {
    return c;
  }

  public int getThickness() {
    return thickness;
  }

  public void setThickness(int thickness) {
    this.thickness = thickness;
  }
}
