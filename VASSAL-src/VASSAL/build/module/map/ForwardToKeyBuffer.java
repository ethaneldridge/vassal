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
package VASSAL.build.module.map;

import VASSAL.build.module.Map;
import VASSAL.build.*;
import VASSAL.counters.KeyBuffer;
import VASSAL.command.Command;

import java.awt.event.*;
import javax.swing.KeyStroke;

/**
 * This KeyListener forwards key event from a {@link Map} to the
 * {@link KeyBuffer}, where it is given to selected GamePieces to
 * interpret.  The event is forwarded only if not consumed
 *
 * @see KeyBuffer
 * @see VASSAL.counters.GamePiece#keyEvent
 * @see InputEvent#isConsumed */
public class ForwardToKeyBuffer implements Buildable, KeyListener {
    public void build(org.w3c.dom.Element e) {
    }

    public void addTo(Buildable parent) {
	Map map = (Map)parent;
	map.getView().addKeyListener(this);
    }
    public void add(Buildable b) {
	throw new IllegalBuildException("Cannot contain children");
    }

    public org.w3c.dom.Element getBuildElement(org.w3c.dom.Document doc) {
	return doc.createElement(getClass().getName());
    }
    public void keyPressed(KeyEvent e) {
	process(e);
    }
    public void keyReleased(KeyEvent e) {
	process(e);
    }
    public void keyTyped(KeyEvent e) {
	process(e);
    }
    private void process(KeyEvent e) {
	if (!e.isConsumed()) {
	    Command comm = KeyBuffer.getBuffer().keyCommand
		(KeyStroke.getKeyStrokeForEvent(e));
	    if (comm != null && !comm.isNull()) {
		GameModule.getGameModule().sendAndLog(comm);
		e.consume();
	    }
	}
    }
}
