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
/*
 * Created by IntelliJ IDEA.
 * User: rkinney
 * Date: Oct 2, 2002
 * Time: 6:30:35 AM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.counters;

import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;

import javax.swing.*;
import java.awt.*;
import java.awt.event.InputEvent;
import java.io.File;
import java.net.MalformedURLException;

/**
 * A GamePiece with this trait will echo the piece's current name when any of a given key commands are pressed
 * (and after they take effect)
 */
public class ReportState extends Decorator implements EditablePiece {
  public static final String ID = "report;";
  private String keys = "";

  public ReportState() {
    this(ID, null);
  }

  public ReportState(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
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

  protected KeyCommand[] myGetKeyCommands() {
    return new KeyCommand[0];
  }

  public String myGetState() {
    return "";
  }

  public String myGetType() {
    return ID + keys;
  }

  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  public Command keyEvent(KeyStroke stroke) {
    Command c = super.keyEvent(stroke);
    if (getMap() != null) {
      GamePiece outer = getOutermost(this);
      if (!Boolean.TRUE.equals(outer.getProperty(Properties.OBSCURED_TO_OTHERS))
        && !Boolean.TRUE.equals(outer.getProperty(Properties.OBSCURED_TO_ME))
        && !Boolean.TRUE.equals(outer.getProperty(Properties.INVISIBLE_TO_OTHERS))) {
        String location = getMap().locationName(getPosition());
        if (location != null) {
          for (int i = 0; i < keys.length(); ++i) {
            if (stroke.equals(KeyStroke.getKeyStroke(keys.charAt(i), InputEvent.CTRL_MASK))) {
              Command display = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), " * " + location + ":  " + outer.getName() + " * ");
              display.execute();
              c = c == null ? display : c.append(display);
              break;
            }
          }
        }
      }
    }
    return c;
  }

  public void mySetState(String newState) {
  }

  public Shape getShape() {
    return piece.getShape();
  }

  public String getDescription() {
    return "Report Changes";
  }

  public HelpFile getHelpFile() {
    File dir = new File("docs");
    dir = new File(dir,"ReferenceManual");
    try {
      return new HelpFile(null,new File(dir,"Report Changes"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public void mySetType(String type) {
    keys = type.length() <= ID.length() ? "" : type.substring(ID.length());
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public static class Ed implements PieceEditor {
    private Container box = Box.createHorizontalBox();
    private JLabel label = new JLabel("Report when player presses CTRL-");
    private JTextField tf = new JTextField(8);

    public Ed(ReportState piece) {
      tf.setMaximumSize(tf.getPreferredSize());
      tf.setText(piece.keys);
      box.add(label);
      box.add(tf);
    }

    public Component getControls() {
      return box;
    }

    public String getState() {
      return "";
    }

    public String getType() {
      return ID + tf.getText();
    }
  }
}
