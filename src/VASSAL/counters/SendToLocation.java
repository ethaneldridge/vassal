package VASSAL.counters;

import VASSAL.build.module.Map;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.command.Command;
import VASSAL.tools.SequenceEncoder;

import javax.swing.*;
import java.awt.*;
import java.awt.event.InputEvent;

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
 * This traitadds a command that sends a piece to a particular location ona particular
 * board of a particular Map.
 */
public class SendToLocation extends Decorator {
  public static final String ID = "sendto;";
  private KeyCommand[] command;
  private String commandName;
  private char key;
  private String mapId;
  private String boardName;
  private int x;
  private int y;

  public SendToLocation() {
    this(ID+";;;;;",null);
  }

  public SendToLocation(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  private void mySetType(String type) {
    type = type.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type,';');
    commandName = st.nextToken();
    String s = st.nextToken();
    key = s.length() > 0 ? s.charAt(0) : 0;
    mapId = st.nextToken();
    boardName = st.nextToken();
    x = Integer.parseInt(st.nextToken());
    y = Integer.parseInt(st.nextToken());
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(commandName)
      .append(key == 0 ? "" : ""+key)
      .append(mapId)
      .append(boardName)
      .append(x+"")
      .append(y+"");
    return ID+se.getValue();
  }

  protected KeyCommand[] myGetKeyCommands() {
    if (command == null) {
      if (commandName.length() > 0
        && key != 0) {
        command = new KeyCommand[]{new KeyCommand(commandName,KeyStroke.getKeyStroke(key,InputEvent.CTRL_MASK),
                                                  Decorator.getOutermost(this))};
      }
      else {
        command = new KeyCommand[0];
      }
    }
    command[0].setEnabled(mapId.length() > 0);
    return command;
  }

  public String myGetState() {
    return "";
  }

  public Command myKeyEvent(KeyStroke stroke) {
    Command c = null;
    myGetKeyCommands();
    if (command[0].matches(stroke)) {
      Map m = Map.getMapById(mapId);
      if (m != null) {
        Point dest = new Point(x,y);
        Board b = m.getboardByName(boardName);
        if (b != null) {
          dest.translate(b.bounds().x,b.bounds().y);
        }
        c = m.placeOrMerge(this,dest);
      }
    }
    return c;
  }

  public void mySetState(String newState) {
  }

  public Rectangle boundingBox() {
    return getInner().boundingBox();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    getInner().draw(g,x,y,obs,zoom);
  }

  public String getName() {
    return getInner().getName();
  }

  public Rectangle selectionBounds() {
    return getInner().selectionBounds();
  }
}
