package VASSAL.build.module.map.boardPicker.board.mapgrid;

import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.build.module.map.boardPicker.Board;

import java.awt.*;

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

/**
 * A Component that can contain a {@link VASSAL.build.module.map.boardPicker.board.MapGrid}
 */
public interface GridContainer {
  void setGrid(MapGrid grid);
  void removeGrid(MapGrid grid);
  Board getBoard();
  Dimension getSize();
}
