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
package VASL.counters;

import VASSAL.build.module.GlobalOptions;
import VASSAL.counters.ColoredBorder;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Labeler;

import java.awt.*;

/**
 * In VASL, we draw the hex location when a unit is selected
 */
public class ASLHighlighter extends ColoredBorder {
    Font f = new Font("Dialog", 0, 10);

    public void draw(GamePiece p, Graphics g, int x, int y, Component obs, double zoom) {
        super.draw(p, g, x, y, obs, zoom);
        if (p.getMap() != null
            && GlobalOptions.getInstance().autoReportEnabled()
            && p.getMap().locationName(p.getPosition()) != null) {
            Rectangle r = p.selectionBounds();
            Point pos = p.getPosition();
            if (p.getParent() != null) {
                Point rel = p.getMap().getStackMetrics().relativePosition(p.getParent(), p);
                x -= (int) (zoom * (rel.x));
                y -= (int) (zoom * (rel.y));
                r = p.getParent().bottomPiece().selectionBounds();
                pos = p.getParent().bottomPiece().getPosition();
            }
            y += (int) (zoom * (r.y + r.height - pos.y + 6));
            Labeler.drawLabel(g, p.getMap().locationName(p.getPosition()),
                              x, y, f, Labeler.CENTER, Labeler.TOP,
                              Color.black, Color.white, Color.black);
        }
    }

    public Rectangle boundingBox(GamePiece p) {
        Rectangle r = p.getParent() == null ? super.boundingBox(p)
            : p.getParent().boundingBox();
        r.height += 20;
        return r;
    }
}
