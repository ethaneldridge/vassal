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

import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.Map;
import VASSAL.command.AddPiece;
import VASSAL.command.Command;
import VASSAL.command.MovePiece;
import VASSAL.command.NullCommand;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Obscurable;
import VASSAL.counters.Hideable;
import VASSAL.counters.Stack;
import VASSAL.tools.FormattedString;
import VASSAL.tools.PlayerIdFormattedString;

import java.awt.*;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * Builds an auto-report message for a collection of {@link MovementSummary}s
 */
public class MovementReporter {
  private FormattedString format = new PlayerIdFormattedString();

  private List moves = new ArrayList();

  public MovementReporter(Command moveCommand) {
    extractMoveCommands(moveCommand);
  }

  private void extractMoveCommands(Command c) {
    MoveSummary summary = null;
    if (c instanceof AddPiece
      && !(((AddPiece)c).getTarget() instanceof Stack)) {
      summary = new MoveSummary((AddPiece)c);
    }
    else if (c instanceof MovePiece) {
      summary = new MoveSummary((MovePiece)c);
    }
    if (summary != null) {
      int index = moves.indexOf(summary);
      if (index >= 0
          && c instanceof MovePiece) {
        MoveSummary existing = (MoveSummary) moves.get(index);
        existing.append((MovePiece)c);
      }
      else {
        moves.add(summary);
      }
    }
    Command[] sub = c.getSubCommands();
    for (int i = 0; i < sub.length; i++) {
      extractMoveCommands(sub[i]);
    }
  }

  public Command getReportCommand() {
    Hideable.setAllHidden(true);
    Obscurable.setAllHidden(true);

    Command c = new NullCommand();
    for (Iterator it = moves.iterator(); it.hasNext();) {
      MoveSummary ms = (MoveSummary) it.next();
      Map fromMap = Map.getMapById(ms.getOldMapId());
      Map toMap = Map.getMapById(ms.getNewMapId());
      format.clearProperties();
      if (fromMap == null) {
        format.setFormat(toMap.getCreateFormat());
      }
      else if (fromMap != toMap) {
        format.setFormat(toMap.getMoveToFormat());
      }
      else {
        format.setFormat(toMap.getMoveWithinFormat());
      }
      if (format.getFormat().length() == 0) {
        break;
      }
      format.setProperty(Map.PIECE_NAME, ms.getPieceName());
      format.setProperty(Map.LOCATION, toMap.locationName(ms.getNewPosition()));
      if (fromMap != null) {
        format.setProperty(Map.OLD_MAP, fromMap.getConfigureName());
        format.setProperty(Map.OLD_LOCATION, fromMap.locationName(ms.getOldPosition()));
      }
      format.setProperty(Map.MAP_NAME, toMap.getConfigureName());

      String moveText = format.getText();

      if (moveText.length() > 0) {
        c = c.append(new Chatter.DisplayText(GameModule.getGameModule().getChatter(), "* " + moveText));
      }
    }
    Hideable.setAllHidden(false);
    Obscurable.setAllHidden(false);
    return c;
  }

  public static class MoveSummary {
    private String oldMapId, newMapId;
    private Point oldPosition, newPosition;
    private StringBuffer names = new StringBuffer();

    public MoveSummary(AddPiece c) {
      newMapId = c.getTarget().getMap().getConfigureName();
      newPosition = c.getTarget().getPosition();
      names.append(c.getTarget().getName());
    }

    public MoveSummary(MovePiece c) {
      GamePiece target = GameModule.getGameModule().getGameState().getPieceForId(c.getId());
      oldMapId = c.getOldMapId();
      newMapId = c.getNewMapId();
      oldPosition = c.getOldPosition();
      newPosition = c.getNewPosition();
      if (target != null) {
        names.append(target.getName());
      }
      else {
        names.append("???");
      }
    }

    public String getNewMapId() {
      return newMapId;
    }

    public Point getNewPosition() {
      return newPosition;
    }

    public String getOldMapId() {
      return oldMapId;
    }

    public Point getOldPosition() {
      return oldPosition;
    }

    public boolean equals(Object o) {
      if (this == o) return true;
      if (!(o instanceof MoveSummary)) return false;

      final MoveSummary moveSummary = (MoveSummary) o;

      if (!newPosition.equals(moveSummary.newPosition)) return false;
      if (!newMapId.equals(moveSummary.newMapId)) return false;
      if (oldPosition != null ? !oldPosition.equals(moveSummary.oldPosition) : moveSummary.oldPosition != null) return false;
      if (oldMapId != null ? !oldMapId.equals(moveSummary.oldMapId) : moveSummary.oldMapId != null) return false;

      return true;
    }

    public int hashCode() {
      int result;
      result = (oldMapId != null ? oldMapId.hashCode() : 0);
      result = 29 * result + newMapId.hashCode();
      result = 29 * result + (oldPosition != null ? oldPosition.hashCode() : 0);
      result = 29 * result + newPosition.hashCode();
      return result;
    }

    public void append(MovePiece movePiece) {
      GamePiece target = GameModule.getGameModule().getGameState().getPieceForId(movePiece.getId());
      if (target != null) {
        names.append(", ").append(target.getName());
      }
      else {
        names.append(", ").append("???");
      }
    }

    public String getPieceName() {
      return names.toString();
    }
  }
}
