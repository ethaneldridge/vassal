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
 
package TRC;

import java.awt.event.InputEvent;

import javax.swing.KeyStroke;

import wga.BasicMassKeyCommand;
import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.Map;
import VASSAL.command.NullCommand;
import VASSAL.counters.BoundsTracker;
import VASSAL.counters.GamePiece;

/**
 * @author Brent
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class RemoveAllCommand extends BasicMassKeyCommand {
  
  public RemoveAllCommand() {
    super();
  }
  
  public void apply(Map m) {
    String mapFormat = m.getChangeFormat();
    if (reportSingle) {
      m.setAttribute(Map.CHANGE_FORMAT, "");
    }
    String reportText = reportFormat.getText();
    if (reportText.length() > 0) {
      keyCommand = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), "*" + reportText);
      keyCommand.execute();
    }
    else {
      keyCommand = new NullCommand();
    }
    tracker = new BoundsTracker();
    
    stroke = KeyStroke.getKeyStroke('A', InputEvent.CTRL_MASK);
    doit(m);
    stroke = KeyStroke.getKeyStroke('S', InputEvent.CTRL_MASK);
    doit(m);
    stroke = KeyStroke.getKeyStroke('F', InputEvent.CTRL_MASK);
    doit(m);
    stroke = KeyStroke.getKeyStroke('E', InputEvent.CTRL_MASK);
    doit(m);

    tracker.repaint();
    GameModule.getGameModule().sendAndLog(keyCommand);
    if (reportSingle) {
      m.setAttribute(Map.CHANGE_FORMAT, mapFormat);
    }
  }
  
  public void doit(Map m) {
    GamePiece[] p = m.getPieces();
    for (int i = 0; i < p.length; ++i) {
      dispatcher.accept(p[i]);
    }
    
  }
}
