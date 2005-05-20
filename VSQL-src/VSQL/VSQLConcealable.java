/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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

package VSQL;

import java.awt.Component;
import java.awt.Graphics;

import VASL.counters.Concealable;
import VASL.counters.Concealment;
import VASL.counters.MarkMoved;
import VASSAL.build.GameModule;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.Clone;
import VASSAL.counters.Decorator;
import VASSAL.counters.Delete;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Hideable;
import VASSAL.counters.KeyCommand;
import VASSAL.counters.Labeler;
import VASSAL.counters.Properties;

/**
 *  
 */
public class VSQLConcealable extends Concealable {

  public VSQLConcealable() {
    super();
  }

  public VSQLConcealable(String type, GamePiece inner) {
    super(type, inner);
  }

  /*
   * @return a new GamePiece that is a concealment counter appropriate for this
   *         unit that is indistinguishable from a standard ? counter.
   */
  public GamePiece createConcealment() {

    boolean large = imageName.indexOf("58") > 0;
    if (!imageName.startsWith(nation)) {
      large = imageName.substring(0, 1).toUpperCase().equals(imageName.substring(0, 1));
    }

    GamePiece p = new BasicPiece(BasicPiece.ID + ";;" + imageName + ";?");
    p = new Delete(Delete.ID + "Delete;D", p);
    p = new Clone(Clone.ID + "Clone;K", p);
    p = new Concealment(Concealment.ID + GameModule.getUserId() + ";" + nation, p);
    p = new Labeler(Labeler.ID + "L;Change Label;10;0,0,0;255,255,255;t;0;c;0;b;c;$pieceName$ ($label$)", p);
    p = new Hideable("hide;H;HIP;255,255,255", p);
    p = new VSQLConcealable(Concealable.ID + "C;" + imageName + ";" + nation, p);
    p = new MarkMoved(MarkMoved.ID + (large ? "moved58" : "moved"), p);
    p.setProperty(Properties.OBSCURED_TO_OTHERS, new Boolean(true));
    p.setProperty(Properties.OBSCURED_BY, GameModule.getGameModule().getUserId());

    return p;
  }
  
  
  /*
   * If this unit is obscured to Me,then make sure I can't see any Control commands from
   * inner pieces
   * For compatibility vsql 2.5.8 to  3.0
   */
  
  public KeyCommand[] getKeyCommands() {
    KeyCommand[] commands = super.getKeyCommands();
    if (obscuredToMe()) {
      commands = new KeyCommand[] { commands[0] };
    }
    return commands;
  }

  /*
   * Don't draw the little question mark on concealment counters.
   * From vsql 3.0. Concealment counters are now concealable also
   */
  protected void drawObscuredToOthers(Graphics g, int x, int y, Component obs, double zoom) {
    
    boolean isConcealment = (Decorator.getDecorator(Decorator.getOutermost(this), VSQLConcealment.class) != null
        || Decorator.getDecorator(Decorator.getOutermost(this), Concealment.class) != null);
    
    if (isConcealment) {
      loadImages(obs);
      piece.draw(g, x, y, obs, zoom);      
    }
    else {
      super.drawObscuredToOthers(g, x, y, obs, zoom);
    }
    
  }

}
