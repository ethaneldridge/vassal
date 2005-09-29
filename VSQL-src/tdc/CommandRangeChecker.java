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

package tdc;

import java.util.ArrayList;
import java.util.Enumeration;
import java.awt.Point;

import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceFilter;
import VASSAL.counters.PieceVisitor;
import VASSAL.counters.PieceVisitorDispatcher;
import VASSAL.counters.PropertiesPieceFilter;
import VASSAL.counters.Stack;

/**
 * Checks command ranges and finds leaders.
 * 
 * @author Brent Easton
 *  
 */
public class CommandRangeChecker {

  GamePiece piece;
  boolean isUnitIndependent = false;	// Unit is part of an Independent Formation
  boolean canBeActivated;				// Game Piece can be activated (i.e. is a combat unit)
  String leaderFormationType;
  String leaderFormationValue;

  public CommandRangeChecker() {
    this(null);
  }

  public CommandRangeChecker(GamePiece p) {
    piece = p;
    setLeaderAttributes();
  }

  /**
   * Determine the Markers to be matched to find the leader for this unit. Units
   * of Independent formations need any leader of their Division. Other units
   * need a leader of their particular Formation.
   *  
   */
  protected void setLeaderAttributes() {
    canBeActivated = Boolean.valueOf((String) piece.getProperty(TdcProperties.ACTIVE)).booleanValue();
    if (canBeActivated) {
      String unitClass = (String) piece.getProperty(TdcProperties.CLASS);
      if (unitClass != null) {
        if (unitClass.equals(TdcProperties.INFANTRY) || unitClass.equals(TdcProperties.VEHICLE)
            || unitClass.equals(TdcProperties.GUN)) {
          leaderFormationType = TdcProperties.FORMATION;
          leaderFormationValue = (String) piece.getProperty(leaderFormationType) + "";
          isUnitIndependent = Boolean.valueOf((String) piece.getProperty(TdcProperties.IS_INDEPENDENT)).booleanValue();
          if (leaderFormationValue != null) {
            if (isUnitIndependent) {
              leaderFormationType = TdcProperties.DIVISION;
              leaderFormationValue = (String) piece.getProperty(leaderFormationType) + "";
            }
          }
        }
      }
    }
  }

  public boolean inCommand() {

    /*
     * Units that cannot be Activated are always displayed as In Command
     */
    if (!canBeActivated) {
      return true;
    }
    
    if (leaderFormationValue != null && leaderFormationValue.length() > 0 && piece.getMap() != null) {
      PieceFilter filter = PropertiesPieceFilter.parse(TdcProperties.CLASS + "=" + TdcProperties.LEADER + " && "
          + leaderFormationType + " = " + leaderFormationValue);
      LeaderVisitor visitor = new LeaderVisitor(filter, piece);
      PieceVisitorDispatcher dispatcher = new PieceVisitorDispatcher(visitor);
      GamePiece[] p = piece.getMap().getPieces();
      for (int i = 0; i < p.length; ++i) {
        dispatcher.accept(p[i]);
      }
      return visitor.isInCommand();
    }

    return false;
  }

  protected class LeaderVisitor implements PieceVisitor {
    protected GamePiece sourcePiece;
    protected PieceFilter filter;
    protected boolean inCommand = false;
    protected ArrayList matches = new ArrayList();

    public LeaderVisitor(PieceFilter filter, GamePiece source) {
      this.filter = filter;
      sourcePiece = source;
    }

    public boolean isInCommand() {
      return inCommand;
    }

    public ArrayList getMatches() {
      return matches;
    }

    public Object visitStack(Stack s) {
      for (Enumeration e = s.getPieces(); e.hasMoreElements();) {
        apply((GamePiece) e.nextElement());
      }
      return null;
    }

    public Object visitDefault(GamePiece p) {
      apply(p);
      return null;
    }

    protected void apply(GamePiece p) {
      if (p.getMap() != null && (filter == null || filter.accept(p))) {
        int commandRange = 0;
        try {
          commandRange = Integer.parseInt((String) p.getProperty(TdcProperties.RANGE));
        }
        catch (Exception e) {

        }

        if (commandRange > 0) {
          Point pos = p.getPosition();
          Board b = p.getMap().findBoard(pos);
          if (b != null) {
            MapGrid m = b.getGrid();
            if (m != null) {
              int range = m.range(pos, sourcePiece.getPosition());
              if (range <= commandRange) {
                inCommand = true;
                matches.add(p);
              }
            }
          }
        }
      }
    }
  }
}
