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

import javax.swing.KeyStroke;

import VASL.counters.ColoredBox;
import VASL.counters.Concealable;
import VASL.counters.Concealment;
import VASL.counters.MarkMoved;
import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.Clone;
import VASSAL.counters.Decorator;
import VASSAL.counters.Delete;
import VASSAL.counters.Embellishment;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Hideable;
import VASSAL.counters.Labeler;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;

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

  /**
   * @return a new GamePiece that is a concealment counter appropriate for this
   *         unit
   */
  public GamePiece createConcealment() {
    //    GamePiece p = new BasicPiece(BasicPiece.ID + "K;D;" + imageName + ";?");
    //    boolean large = imageName.indexOf("58") > 0;
    //    if (!imageName.startsWith(nation)) { // Backward compatibility with
    // generic
    //                                         // concealment markers
    //      large = imageName.substring(0,
    // 1).toUpperCase().equals(imageName.substring(0, 1));
    //      String size = large ? "60;60" : "48;48";
    //      if (nation2 != null) {
    //        p = new ColoredBox(ColoredBox.ID + "ru" + ";" + size, p);
    //        p = new ColoredBox(ColoredBox.ID + "ge" + ";" + (large ? "48;48" :
    // "36;36"), p);
    //      }
    //      else {
    //        p = new ColoredBox(ColoredBox.ID + nation + ";" + size, p);
    //      }
    //      p = new Embellishment(Embellishment.ID + ";;;;;;0;0;" + imageName + ",?",
    // p);
    //    }
    //    p = new Concealment(Concealment.ID + GameModule.getUserId() + ";" +
    // nation, p);
    //    p = new MarkMoved(MarkMoved.ID + (large ? "moved58" : "moved"), p);
    //    p = new Hideable("hide;H;HIP;255,255,255", p);
    //    return p;

    boolean large = imageName.indexOf("58") > 0;
    if (!imageName.startsWith(nation)) {
      large = imageName.substring(0, 1).toUpperCase().equals(imageName.substring(0, 1));
    }

    GamePiece p = new BasicPiece(BasicPiece.ID + ";;" + imageName + ";?");
    p = new Delete(Delete.ID + "Delete;D", p);
    p = new Clone(Clone.ID + "Clone;K", p);
//    String size = large ? "60;60" : "48;48";
//    p = new Embellishment(Embellishment.ID + ";;;;;;0;0;" + imageName + ",?", p);
//    p = new ColoredBox(ColoredBox.ID + nation + ";" + size, p);
    p = new Concealment(Concealment.ID + GameModule.getUserId() + ";" + nation, p);
    p = new Labeler(Labeler.ID + "L;Change Label;10;0,0,0;255,255,255;t;0;c;0;b;c;$pieceName$ ($label$)", p);
    p = new Hideable("hide;H;HIP;255,255,255", p);
    p = new VSQLConcealable(Concealable.ID + "C;" + imageName + ";" + nation, p);
    p = new MarkMoved(MarkMoved.ID + (large ? "moved58" : "moved"), p);
    p.setProperty(Properties.OBSCURED_TO_OTHERS, new Boolean(true));
    p.setProperty(Properties.OBSCURED_BY, GameModule.getGameModule().getUserId());

    return p;
  }

  public Command keyEvent(javax.swing.KeyStroke stroke) {
    Stack parent = getParent();
    if (parent != null) {
      int lastIndex = getParent().indexOf(Decorator.getOutermost(this));
      Command c = super.keyEvent(stroke);
      if (getParent() != null) {
        int newIndex = getParent().indexOf(Decorator.getOutermost(this));
        if (newIndex != lastIndex) {
          c.append(adjustConcealment());
        }
      }
      return c;
    }
    else {
      if (!obscuredToMe()) {
        return super.keyEvent(stroke);
      }
      else
        return null;
    }
  }

  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    Command c = null;
    if (commands[0].matches(stroke) && getMap() != null && !obscuredToOthers() && !obscuredToMe()) {
      c = super.myKeyEvent(stroke);
      boolean concealmentExists = false;
      GamePiece outer = Decorator.getOutermost(this);
      if (getParent() != null) {
        for (int i = getParent().indexOf(outer), j = getParent().getPieceCount(); i < j; ++i) {
          Concealment conceal = (Concealment) Decorator.getDecorator(getParent().getPieceAt(i), Concealment.class);
          if (conceal != null && conceal.canConceal(outer)) {
            concealmentExists = true;
            break;
          }
        }
      }
      if (!concealmentExists) {
        GamePiece concealOuter = createConcealment();
        Concealment conceal = (Concealment) Decorator.getDecorator(concealOuter, Concealment.class);
        c.append(getMap().getStackMetrics().merge(outer, concealOuter));
        for (int i = 0, j = getParent().indexOf(outer); i < j; ++i) {
          c.append(conceal.setConcealed(getParent().getPieceAt(i), true));
        }
      }
    }
    else {
      c = super.myKeyEvent(stroke);
    }
    return c;
  }
}
