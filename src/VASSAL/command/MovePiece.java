package VASSAL.command;

import VASSAL.counters.GamePiece;
import VASSAL.counters.BoundsTracker;
import VASSAL.counters.Properties;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.GlobalOptions;

import java.awt.*;

/*
 * $Id$
 *
 * Copyright (c) 2003 by Rodney Kinney
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
 * Command that moves a piece to a new location and position within a stack.
 * While this can be accomplished with a {@link ChangePiece} command, this
 * command is safer in terms of recovering from changes to the game state that may have occurred
 * since the command was created.  For instance, A {@link ChangePiece} command that adds
 * a piece to a {@link VASSAL.counters.Stack} will cause the piece to disappear if the
 * stack has been deleted.  This Command will recover more gracefully.
 */
public class MovePiece extends Command {
  private String id;
  private String newMapId;
  private String oldMapId;
  private Point newPosition;
  private Point oldPosition;
  private String newUnderneathId;
  private String oldUnderneathId;

  /**
   *
   * @param id The id of the piece being moved
   * @param newMapId The id of the map being moved to
   * @param newPosition the new position
   * @param newUnderneathId The id of the piece which will be immediately beneath this piece in any containing Stack.  May be null
   * @param oldMapId The id of the map being moved from
   * @param oldPosition the old position
   * @param oldUnderneathId The id of the piece which was immediately beneath this piece in its original containing Stack.
   */
  public MovePiece(String id, String newMapId, Point newPosition, String newUnderneathId, String oldMapId, Point oldPosition, String oldUnderneathId) {
    this.id = id;
    this.newMapId = newMapId;
    this.oldMapId = oldMapId;
    this.newPosition = newPosition;
    this.oldPosition = oldPosition;
    this.newUnderneathId = newUnderneathId;
    this.oldUnderneathId = oldUnderneathId;
  }

  public String getId() {
    return id;
  }

  public String getNewMapId() {
    return newMapId;
  }

  public String getOldMapId() {
    return oldMapId;
  }

  public Point getNewPosition() {
    return newPosition;
  }

  public Point getOldPosition() {
    return oldPosition;
  }

  public String getNewUnderneathId() {
    return newUnderneathId;
  }

  public String getOldUnderneathId() {
    return oldUnderneathId;
  }

  protected void executeCommand() {
    GamePiece piece = GameModule.getGameModule().getGameState().getPieceForId(id);
    if (piece != null) {
      BoundsTracker bounds = new BoundsTracker();
      bounds.addPiece(piece);
      Map newMap = Map.getMapById(newMapId);
      if (newMap != null) {
        if (newUnderneathId != null) {
          GamePiece under = GameModule.getGameModule().getGameState().getPieceForId(newUnderneathId);
          if (under != null
            && under.getPosition().equals(newPosition)) {
            newMap.getStackMetrics().merge(under,piece);
          }
          else {
            newMap.placeOrMerge(piece,newPosition);
          }
        }
        else {
          newMap.placeOrMerge(piece,newPosition);
        }
      }
      else {
        Map oldMap = Map.getMapById(oldMapId);
        if (oldMap != null) {
          oldMap.removePiece(piece);
        }
      }
      bounds.addPiece(piece);
      bounds.repaint();
      if (piece.getMap() != null
        && GlobalOptions.getInstance().centerOnOpponentsMove()
        && !Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))) {
        piece.getMap().ensureVisible(piece.getMap().selectionBoundsOf(piece));
      }
    }
  }

  protected Command myUndoCommand() {
    return new MovePiece(id,oldMapId,oldPosition,oldUnderneathId,newMapId,newPosition,newUnderneathId);
  }

}
