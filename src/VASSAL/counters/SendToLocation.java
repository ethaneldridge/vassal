package VASSAL.counters;

import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.command.Command;
import VASSAL.tools.SequenceEncoder;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.ChooseComponentDialog;
import VASSAL.configure.StringConfigurer;

import javax.swing.*;
import java.awt.*;
import java.awt.event.InputEvent;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.io.File;
import java.net.MalformedURLException;

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
public class SendToLocation extends Decorator implements EditablePiece {
  public static final String ID = "sendto;";
  private KeyCommand[] command;
  private String commandName;
  private char key;
  private String mapId;
  private String boardName;
  private int x;
  private int y;

  public SendToLocation() {
    this(ID+";;;;0;0",null);
  }

  public SendToLocation(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  public void mySetType(String type) {
    type = type.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type,';');
    commandName = st.nextToken();
    String s = st.nextToken();
    key = s.length() > 0 ? s.charAt(0) : 0;
    mapId = st.nextToken();
    boardName = st.nextToken();
    x = st.nextInt(0);
    y = st.nextInt(0);
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
    command[0].setEnabled(getMap() != null);
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
      if (m == null) {
        m = getMap();
      }
      if (m != null) {
        Point dest = new Point(x,y);
        Board b = m.getBoardByName(boardName);
        if (b != null) {
          dest.translate(b.bounds().x,b.bounds().y);
        }
        c = m.placeOrMerge(Decorator.getOutermost(this),dest);
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
    piece.draw(g,x,y,obs,zoom);
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
    return "Send to Location";
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "SendToLocation.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public static class Ed implements PieceEditor {
    private StringConfigurer nameInput;
    private KeySpecifier keyInput;
    private JTextField mapIdInput;
    private JTextField boardNameInput;
    private IntConfigurer xInput;
    private IntConfigurer yInput;
    private Map map;
    private JPanel controls;

    public Ed(SendToLocation p) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls,BoxLayout.Y_AXIS));

      nameInput = new StringConfigurer(null,"Command name:  ",p.commandName);
      controls.add(nameInput.getControls());

      keyInput = new KeySpecifier(p.key);
      controls.add(keyInput);

      Box b = Box.createHorizontalBox();
      mapIdInput = new JTextField(12);
      map = Map.getMapById(p.mapId);
      if (map != null) {
        mapIdInput.setText(map.getMapName());
      }
      mapIdInput.setEditable(false);
      b.add(new JLabel("Map:  "));
      b.add(mapIdInput);
      JButton select = new JButton("Select");
      select.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          selectMap();
        }
      });
      b.add(select);
      JButton clear = new JButton("Clear");
      clear.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          clearMap();
        }
      });
      b.add(clear);
      controls.add(b);

      b = Box.createHorizontalBox();
      boardNameInput = new JTextField(12);
      boardNameInput.setEditable(false);
      b.add(new JLabel("Board:  "));
      b.add(boardNameInput);
      select = new JButton("Select");
      select.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          selectBoard();
        }
      });
      clear = new JButton("Clear");
      clear.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          clearBoard();
        }
      });
      b.add(select);
      b.add(clear);
      controls.add(b);

      xInput = new IntConfigurer(null,"X Position:  ", new Integer(p.x));
      controls.add(xInput.getControls());

      yInput = new IntConfigurer(null,"Y Position:  ", new Integer(p.y));
      controls.add(yInput.getControls());
    }

    private void clearBoard() {
      boardNameInput.setText("");
    }

    private void clearMap() {
      map = null;
      mapIdInput.setText("");
    }

    private void selectBoard() {
      ChooseComponentDialog d = new ChooseComponentDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class,controls),Board.class);
      d.setVisible(true);
      if (d.getTarget() != null) {
        Board b = (Board) d.getTarget();
        boardNameInput.setText(b.getName());
      }
    }

    private void selectMap() {
      ChooseComponentDialog d = new ChooseComponentDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class,controls),Map.class);
      d.setVisible(true);
      if (d.getTarget() != null) {
        map = (Map) d.getTarget();
        mapIdInput.setText(map.getMapName());
      }
    }

    public Component getControls() {
      return controls;
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(nameInput.getValueString())
        .append(keyInput.getKey())
        .append(map == null ? "" : map.getId())
        .append(boardNameInput.getText())
        .append(xInput.getValueString())
        .append(yInput.getValueString());
      return ID+se.getValue();
    }

    public String getState() {
      return "";
    }
  }
}
