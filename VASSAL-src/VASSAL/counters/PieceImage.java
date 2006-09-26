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

import VASSAL.tools.TransparentFilter;

import java.awt.*;
import java.awt.image.FilteredImageSource;

/**
 * Maintains an {@link Image} built from the {@link GamePiece#draw}
 * method of a {@link GamePiece} */
public class PieceImage {
  private GamePiece piece;
  private String lastState = null;
  private Image im;

  public PieceImage(GamePiece piece) {
    this.piece = piece;
  }

  public Image getImage(Component obs) {
    if (isChanged()) {
      lastState = currentState();

      // First draw the inner piece so we can know the size
      piece.draw(obs.getGraphics(), 0, 0, obs, 0.0);

      // Now the boundingBox should be correct
      Rectangle bbox = piece.boundingBox();
      Point pos = piece.getPosition();
      im = obs.createImage(bbox.width, bbox.height);
      Graphics g = im.getGraphics();
      piece.draw(g, pos.x - bbox.x, pos.y - bbox.y, obs, 1.0);
      TransparentFilter f = new TransparentFilter();
      f.setAlpha(0.0, TransparentFilter.getOffscreenEquivalent(obs.getBackground().getRGB(), obs));
      im = obs.createImage(new FilteredImageSource(im.getSource(), f));
    }
    return im;
  }

  public boolean isChanged() {
    return !currentState().equals(lastState);
  }

  private String currentState() {
    StringBuffer buf = new StringBuffer();
    for (GamePiece p = piece;
         p instanceof Decorator;
         p = ((Decorator) p).getInner()) {
      buf.append(((Decorator) p).myGetState());
    }
    return buf.toString();
  }
}
