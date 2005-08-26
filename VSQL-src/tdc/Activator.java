package tdc;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.InputEvent;
import java.util.Enumeration;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.counters.Decorator;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyCommand;
import VASSAL.counters.PieceEditor;
import VASSAL.counters.PieceVisitor;
import VASSAL.counters.PieceVisitorDispatcher;
import VASSAL.counters.Stack;
import VASSAL.tools.SequenceEncoder;

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

/**
 * This trait Sends an Activate command to related counters
 */
public class Activator extends Decorator implements EditablePiece, PieceVisitor {
  public static final String ID = "activate;";
  protected KeyCommand[] command;
  protected String commandName;
  protected KeyStroke key;
  protected String marker;
  protected String markerValue;
  protected PieceVisitorDispatcher dispatcher = new PieceVisitorDispatcher(this);

  public Activator() {
    this(ID + "Activate Formation;A", null);
  }

  public Activator(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  public void mySetType(String type) {
    type = type.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    commandName = st.nextToken("Activate Formation");
    key = st.nextKeyStroke('A');
    marker = st.nextToken("Formation");
    command = null;
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(commandName).append(key).append(marker);
    return ID + se.getValue();
  }

  protected KeyCommand[] myGetKeyCommands() {
    if (command == null) {
      if (commandName.length() > 0 && key != null) {
        command =
            new KeyCommand[]{new KeyCommand(commandName, key, Decorator.getOutermost(this))};
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
    Command com = null;
    myGetKeyCommands();
    if (command[0].matches(stroke)) {
      GamePiece outer = Decorator.getOutermost(this);
      markerValue = (String) outer.getProperty(marker);
      Enumeration e = GameModule.getGameModule().getGameState().getPieces();
      while (e.hasMoreElements()) {
        GamePiece p = (GamePiece) e.nextElement();
        Command c = (Command) dispatcher.accept(p);
        if (c != null) {
          if (com == null) {
            com = c;
          }
          else {
            com.append(c);
          }
        }
      }
    }
    return com;
  }


  public Object visitStack(Stack s) {
    Command com = null;
    for (Enumeration e = s.getPieces(); e.hasMoreElements();) {
      Command c = (Command) checkActivate((GamePiece) e.nextElement());
      if (c != null) {
        if (com == null) {
          com = c;
        }
        else {
          com.append(c);
        }
      }
    }
    return com;
  }

  public Object visitDefault(GamePiece p) {
    return checkActivate(p);
  }
  

  protected Object checkActivate(GamePiece p) {
    Command c = null;
    if (p.getProperty("Active") != null) {
      String active = (String) p.getProperty("Active_Active");
      if (active != null && active.equals("false")) {
        String formation = (String) p.getProperty(marker);
        if (formation != null && formation.equals(markerValue)) {
          ChangeTracker tracker = new ChangeTracker(p);
          p.keyEvent(KeyStroke.getKeyStroke('A', InputEvent.CTRL_MASK));
          c = tracker.getChangeCommand();
        }
      }
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
    return "Activator";
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public static class Ed implements PieceEditor {
    protected StringConfigurer nameInput;
    protected HotKeyConfigurer keyInput;
    protected StringConfigurer markerInput;
    protected JPanel controls;

    public Ed(Activator p) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      nameInput = new StringConfigurer(null, "Command name:  ", p.commandName);
      controls.add(nameInput.getControls());

      keyInput = new HotKeyConfigurer(null,"Keyboard Command:  ",p.key);
      controls.add(keyInput.getControls());
      
      markerInput = new StringConfigurer(null,"Marker Name:  ",p.marker);
      controls.add(markerInput.getControls());

    }

    public Component getControls() {
      return controls;
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(nameInput.getValueString())
      	.append((KeyStroke)keyInput.getValue())
      	.append(markerInput.getValueString());
      return ID + se.getValue();
    }

    public String getState() {
      return "";
    }
  }
}
