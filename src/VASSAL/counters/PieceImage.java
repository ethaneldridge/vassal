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

import java.awt.*;
import java.awt.image.BufferedImage;

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

      Rectangle bbox = piece.boundingBox();
      im = new BufferedImage(bbox.width, bbox.height, BufferedImage.TYPE_4BYTE_ABGR);
      ((BufferedImage)im).setRGB(0,0,bbox.width,bbox.height,new int[bbox.width*bbox.height],0,bbox.width);

      piece.draw(((BufferedImage)im).createGraphics(),-bbox.x, -bbox.y, obs, 1.0);
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
         p = ((Decorator) p).piece
        ) {
      buf.append(((Decorator) p).myGetState());
    }
    return buf.toString();
  }
}
