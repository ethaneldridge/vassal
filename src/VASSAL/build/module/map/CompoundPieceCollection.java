/*
 * $Id$
 *
 * Copyright (c) 2004 by Rodney Kinney
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

import VASSAL.counters.GamePiece;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Base class for PieceCollection implementation that organize
 * pieces into distinct layers.  The layers are drawn in order of their index, i.e.
 * layer 0 is on the bottom.
 */
public abstract class CompoundPieceCollection implements PieceCollection {
  protected SimplePieceCollection[] layers;

  protected CompoundPieceCollection(int layerCount) {
    initLayers(layerCount);
  }

  protected void initLayers(int layerCount) {
    layers = new SimplePieceCollection[layerCount];
    for (int i=0;i<layers.length;++i) {
      layers[i] = new SimplePieceCollection();
    }
  }

  protected abstract int getLayerForPiece(GamePiece p);

  protected PieceCollection getCollectionForPiece(GamePiece p) {
    return layers[getLayerForPiece(p)];
  }

  public void add(GamePiece p) {
    getCollectionForPiece(p).add(p);
  }

  public void clear() {
    for (int i=0;i<layers.length;++i) {
      layers[i].clear();
    }
  }

  public GamePiece[] getPieces() {
    List l = new ArrayList();
    for (int i=0;i<layers.length;++i) {
      l.addAll(Arrays.asList(layers[i].getPieces()));
    }
    return (GamePiece[]) l.toArray(new GamePiece[l.size()]);
  }

  public int indexOf(GamePiece p) {
    int layer = getLayerForPiece(p);
    int index = layers[layer].indexOf(p);
    if (index >= 0) {
      for (int i=0;i<layer-1;++i) {
        index += layers[i].getPieces().length;
      }
    }
    return index;
  }

  public void remove(GamePiece p) {
    getCollectionForPiece(p).remove(p);
  }

  public void moveToBack(GamePiece p) {
    getCollectionForPiece(p).moveToBack(p);
  }

  public void moveToFront(GamePiece p) {
    getCollectionForPiece(p).moveToFront(p);
  }

}
