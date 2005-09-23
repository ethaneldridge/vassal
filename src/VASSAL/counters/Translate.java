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
package VASSAL.counters;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.MovementReporter;
import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.command.MoveTracker;
import VASSAL.command.ChangeTracker;
import VASSAL.command.NullCommand;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.SequenceEncoder;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.io.File;
import java.net.MalformedURLException;
import java.util.Enumeration;

/**
 * Give a piece a command that moves it a fixed amount in a particular direction,
 * optionally tracking the current rotation of the piece.
 */
public class Translate extends Decorator implements EditablePiece {
  public static final String ID = "translate;";
  private KeyCommand[] commands;
  private String commandName;
  private KeyStroke keyCommand;
  private int xDist;
  private int yDist;
  private boolean moveStack;
  private KeyCommand moveCommand;

  public Translate() {
    this(ID + "Move Forward", null);
  }

  public Translate(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  public String getDescription() {
    return "Move fixed distance";
  }

  public void mySetType(String type) {
    type = type.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    commandName = st.nextToken("Move Forward");
    keyCommand = st.nextKeyStroke('M');
    xDist = st.nextInt(0);
    yDist = st.nextInt(60);
    moveStack = st.nextBoolean(true);
    commands = null;
  }

  protected KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      moveCommand = new KeyCommand(commandName, keyCommand, Decorator.getOutermost(this));
      if (commandName.length() > 0) {
        commands = new KeyCommand[]{moveCommand};
      }
      else {
        commands = new KeyCommand[0];
      }
    }
    moveCommand.setEnabled(getMap() != null);
    return commands;
  }

  public String myGetState() {
    return "";
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(commandName).append(keyCommand).append(xDist).append(yDist).append(moveStack);
    return ID + se.getValue();
  }

  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    Command c = null;
    if (moveCommand.matches(stroke)) {
      GamePiece outer = Decorator.getOutermost(this);
      GamePiece target = outer;
      if (moveStack
          && outer.getParent() != null
          && !outer.getParent().isExpanded()) {
        // Only move entire stack if this is the top piece
        // Otherwise moves the stack too far if the whole stack is multi-selected
        if (outer != outer.getParent().topPiece(GameModule.getUserId())) {
          return null;
        }
        target = outer.getParent();
        c = new NullCommand();
        for (Enumeration e = outer.getParent().getPieces(); e.hasMoreElements();) {
          GamePiece p = (GamePiece) e.nextElement();
          ChangeTracker ct = new ChangeTracker(p);
          p.setProperty(Properties.MOVED, Boolean.TRUE);
          c = c.append(ct.getChangeCommand());
        }
      }
      else {
        ChangeTracker ct = new ChangeTracker(outer);
        outer.setProperty(Properties.MOVED, Boolean.TRUE);
        c = ct.getChangeCommand();
      }
      MoveTracker t = new MoveTracker(target);
      Point p = new Point(getPosition());
      p.translate(xDist, -yDist);
      FreeRotator myRotation = (FreeRotator) Decorator.getDecorator(this, FreeRotator.class);
      if (myRotation != null) {
        Point2D myPosition = getPosition().getLocation();
        Point2D p2d = p.getLocation();
        p2d = AffineTransform.getRotateInstance(myRotation.getAngleInRadians(), myPosition.getX(), myPosition.getY()).transform(p2d, null);
        p = new Point((int) p2d.getX(), (int) p2d.getY());
      }
      if (!Boolean.TRUE.equals(outer.getProperty(Properties.IGNORE_GRID))) {
        p = getMap().snapTo(p);
      }
      getMap().placeOrMerge(target, p);
      c = c.append(t.getMoveCommand());
      MovementReporter r = new MovementReporter(c);
      Command reportCommand = r.getReportCommand();
      if (reportCommand != null) {
        reportCommand.execute();
      }
      c.append(reportCommand);
      c.append(r.markMovedPieces());
      getMap().ensureVisible(getMap().selectionBoundsOf(outer));
    }
    return c;
  }

  public void mySetState(String newState) {
  }

  public Rectangle boundingBox() {
    return getInner().boundingBox();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    getInner().draw(g, x, y, obs, zoom);
  }

  public String getName() {
    return getInner().getName();
  }

  public Shape getShape() {
    return getInner().getShape();
  }

  public PieceEditor getEditor() {
    return new Editor(this);
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "Translate.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public static class Editor implements PieceEditor {
    private IntConfigurer xDist;
    private IntConfigurer yDist;
    private StringConfigurer name;
    private HotKeyConfigurer key;
    private JPanel controls;
    private BooleanConfigurer moveStack;

    public Editor(Translate t) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));
      name = new StringConfigurer(null, "Command Name:  ", t.commandName);
      controls.add(name.getControls());
      key = new HotKeyConfigurer(null, "Keyboard shortcut:  ", t.keyCommand);
      controls.add(key.getControls());
      xDist = new IntConfigurer(null, "Distance to the right:  ", new Integer(t.xDist));
      controls.add(xDist.getControls());
      yDist = new IntConfigurer(null, "Distance upwards:  ", new Integer(t.yDist));
      controls.add(yDist.getControls());
      moveStack = new BooleanConfigurer(null, "Move entire stack", new Boolean(t.moveStack));
      controls.add(moveStack.getControls());
    }

    public Component getControls() {
      return controls;
    }

    public String getState() {
      return "";
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(name.getValueString()).append((KeyStroke) key.getValue()).append(xDist.getValueString()).append(yDist.getValueString()).append(moveStack.getValueString());
      return ID + se.getValue();
    }
  }

}
