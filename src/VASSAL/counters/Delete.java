package VASSAL.counters;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.InputEvent;
import java.io.File;
import java.net.MalformedURLException;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.RemovePiece;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.SequenceEncoder;

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
 * This trait adds a command that creates a duplicate of the selected Gamepiece
 */
public class Delete extends Decorator implements EditablePiece {
	public static final String ID = "delete;";
	private KeyCommand[] command;
	private String commandName;
	private char key;

	public Delete() {
		this(ID + "Delete;D", null);
	}

	public Delete(String type, GamePiece inner) {
		mySetType(type);
		setInner(inner);
	}

	public void mySetType(String type) {
		type = type.substring(ID.length());
		SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
		commandName = st.nextToken();
		String s = st.nextToken();
		key = s.length() > 0 ? s.charAt(0) : 0;
	}

	public String myGetType() {
		SequenceEncoder se = new SequenceEncoder(';');
		se.append(commandName).append(key == 0 ? "" : "" + key);
		return ID + se.getValue();
	}

	protected KeyCommand[] myGetKeyCommands() {
		if (command == null) {
			if (commandName.length() > 0 && key != 0) {
				command =
					new KeyCommand[] { new KeyCommand(commandName, KeyStroke.getKeyStroke(key, InputEvent.CTRL_MASK), Decorator.getOutermost(this))};
			}
			else {
				command = new KeyCommand[0];
			}
		}
		if (command.length > 0) {
			command[0].setEnabled(getMap() != null);
		}
		return command;
	}

	public String myGetState() {
		return "";
	}

	public Command myKeyEvent(KeyStroke stroke) {
		Command c = null;
		myGetKeyCommands();
		if (command[0].matches(stroke)) {
			GamePiece outer = Decorator.getOutermost(this);
			c = new RemovePiece(outer);
			c.execute();
		}
		return c;
	}

	public void mySetState(String newState) {
	}

	public Rectangle boundingBox() {
		return piece.boundingBox();
	}

	public void draw(Graphics g, int x, int y, Component obs, double zoom) {
		piece.draw(g, x, y, obs, zoom);
	}

	public String getName() {
		return piece.getName();
	}

	public Shape getShape() {
		return piece.getShape();
	}

	public PieceEditor getEditor() {
		return new Ed(this);
	}

	public String getDescription() {
		return "Delete";
	}

	public HelpFile getHelpFile() {
		File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
		dir = new File(dir, "ReferenceManual");
		try {
			return new HelpFile(null, new File(dir, "GamePiece.htm"),"#Delete");
		}
		catch (MalformedURLException ex) {
			return null;
		}
	}

	public static class Ed implements PieceEditor {
		private StringConfigurer nameInput;
		private KeySpecifier keyInput;
		private JPanel controls;

		public Ed(Delete p) {
			controls = new JPanel();
			controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

			nameInput = new StringConfigurer(null, "Command name:  ", p.commandName);
			controls.add(nameInput.getControls());

			keyInput = new KeySpecifier(p.key);
			controls.add(keyInput);

		}

		public Component getControls() {
			return controls;
		}

		public String getType() {
			SequenceEncoder se = new SequenceEncoder(';');
			se.append(nameInput.getValueString()).append(keyInput.getKey());
			return ID + se.getValue();
		}

		public String getState() {
			return "";
		}
	}
}
