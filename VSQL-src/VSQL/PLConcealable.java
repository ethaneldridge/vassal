/*
 * $Id$
 * 
 * Copyright (c) 2000-2005 by Brent Easton, Rodney Kinney
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License (LGPL) as published by
 * the Free Software Foundation.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, copies are available at
 * http://www.opensource.org.
 */
package VSQL;

import java.awt.event.InputEvent;

import javax.swing.KeyStroke;

import VASL.counters.Concealable;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyCommand;

public class PLConcealable extends Concealable {

	public static final String ID = "plconceal;";

	public static final String NATIONALITY_COMMAND = "Nationality";

	public static final KeyStroke NATIONALITY_KEY = KeyStroke.getKeyStroke('N',InputEvent.CTRL_MASK);

	public PLConcealable() {
		super();
	}

	public PLConcealable(String type, GamePiece inner) {
		super(type, inner);
	}

	/**
	 * Add the Nationality command if counter is not already concealed
	 *  
	 */
	public KeyCommand[] myGetKeyCommands() {

		KeyCommand[] oldCommands = super.myGetKeyCommands();
		KeyCommand[] newCommands;

		if (obscuredToMe() || obscuredToOthers()) {
			newCommands = oldCommands;
		} else {
			newCommands = new KeyCommand[oldCommands.length + 1];
			System
					.arraycopy(oldCommands, 0, newCommands, 0,
							oldCommands.length);
			newCommands[oldCommands.length] = new KeyCommand(
					NATIONALITY_COMMAND, NATIONALITY_KEY, Decorator.getOutermost(this));
		}

		return newCommands;
	}

	public Command myKeyEvent(KeyStroke stroke) {

		if (stroke.equals(NATIONALITY_KEY)) {
			return doPopup();
		} else {
			return super.myKeyEvent(stroke);
		}

	}

	public Command doPopup() {

		Command c = new NullCommand();

		return c;

	}
}