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
package VASSAL.tools;

import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.PlayerRoster;
import VASSAL.build.GameModule;
import VASSAL.counters.GamePiece;

/** Utility subclass of {@link FormattedString} which automatically includes
 * variables for Player name, side, and id
 */
public class PlayerIdFormattedString extends FormattedString {
  public PlayerIdFormattedString() {
  }

  public PlayerIdFormattedString(String s) {
    super(s);
  }

  public String getText(GamePiece piece) {
    setProperty(GlobalOptions.PLAYER_NAME, (String) GameModule.getGameModule().getPrefs().getValue(GameModule.REAL_NAME));
    setProperty(GlobalOptions.PLAYER_SIDE,PlayerRoster.getMySide());
    setProperty(GlobalOptions.PLAYER_ID,GlobalOptions.getInstance().getPlayerId());
    return super.getText(piece);
  }
}
