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
 * Time: 9:16:23 PM
 * To change template for new class use 
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.build.module.map;

import VASSAL.counters.Stack;
import VASSAL.counters.GamePiece;

import java.awt.*;
import java.util.Enumeration;

public class HandMetrics extends StackMetrics {
    public HandMetrics() {
        super(false,12,0,12,0);
    }

    public Point relativePosition(Stack parent, GamePiece c) {
        Point pt = new Point(0,0);
        for (Enumeration e = parent.getPieces();e.hasMoreElements();) {
            GamePiece child = (GamePiece) e.nextElement();
/*
            if (pt == null) {
                pt = new Point(0,0);
                if (child == c) {
                    break; // Relative position is 0,0 for first piece in stack
                }
            }
            */
            Rectangle r = child.boundingBox();
//            pt.translate(child.getPosition().x-r.x,0);
            if (child == c) {
                break;
            }
//            pt.translate(r.x+r.width-child.getPosition().x+exSepX,0);
            pt.translate(r.width+exSepX,0);
        }
        return pt;
    }
}
