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
/*
 * Created by IntelliJ IDEA.
 * User: rkinney
 * Date: Jun 30, 2002
 * Time: 6:12:50 AM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.build.module;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.HandMetrics;
import VASSAL.build.module.map.StackExpander;
import VASSAL.build.module.map.StackMetrics;
import VASSAL.build.module.map.CounterDetailViewer;
import VASSAL.command.Command;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceFinder;
import VASSAL.counters.Stack;

import java.awt.*;
import java.io.File;
import java.net.MalformedURLException;
import java.util.Enumeration;

public class PlayerHand extends PrivateMap {
  public void build(org.w3c.dom.Element el) {
    super.build(el);
    if (el == null) {
      Enumeration e = getComponents(StackExpander.class);
      while (e.hasMoreElements()) {
        StackExpander se = (StackExpander) e.nextElement();
        remove(se);
        removeLocalMouseListener(se);
      }
      e = getComponents(CounterDetailViewer.class);
      while (e.hasMoreElements()) {
        CounterDetailViewer cdv = (CounterDetailViewer) e.nextElement();
        remove(cdv);
        cdv.removeFrom(this);
      }
    }
  }

  public static String getConfigureTypeName() {
    return "Player Hand";
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "PlayerHand.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public StackMetrics getStackMetrics() {
    if (metrics == null) {
      metrics = new HandMetrics();
      metrics.build(null);
      add(metrics);
      metrics.addTo(this);
    }
    return metrics;
  }

  public Dimension mapSize() {
    GamePiece[] stack = pieces.getPieces();
    if (stack.length > 0 && stack[0] != null) {
      return boundingBoxOf(stack[0]).getSize();
    }
    else {
      return super.mapSize();
    }
  }

  public Command placeAt(GamePiece piece, Point pt) {
    Command c = null;
    GamePiece[] stack = pieces.getPieces();
    if (stack.length == 0) {
      Rectangle r;
      if (piece instanceof Stack)  {
        r = ((Stack)piece).topPiece().getShape().getBounds();
      }
      else {
        r = piece.getShape().getBounds();
      }
      pt = new Point(-r.x,
                     -r.y);
      c = super.placeAt(piece, pt);
    }
    else if (stack[0] instanceof Stack) {
      GamePiece target = findPiece(pt, PieceFinder.PIECE_IN_STACK);
      if (target == null) {
        target = stack[0];
      }
      c = getStackMetrics().merge(target, piece);
    }
    else {
      c = getStackMetrics().merge(stack[0], piece);
    }
    getView().revalidate();
    return c;
  }
}
