package rw;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.image.BufferedImage;

import javax.swing.BoxLayout;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.counters.Decorator;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyCommand;
import VASSAL.counters.PieceEditor;
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
 * This trait implements the Aircraft Status Pad
 */
public class StatusPad extends Decorator implements EditablePiece {
  
  public static final String ID = "statuspad;";
  protected static final String DEFAULT_COMMAND = "Status";
  protected static final char DEFAULT_KEY = 'S';
  
  public static final String BACKGROUND_IMAGE = "statuspad.png"; 
  protected static Image backgroundImage;
     
  protected KeyCommand[] command;
  protected String commandName;
  protected KeyStroke key;
  protected StatusPadDialog frame;

  public StatusPad() {
    this(ID + DEFAULT_COMMAND + ";" + DEFAULT_KEY, null);
  }

  public StatusPad(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  public void mySetType(String type) {
    type = type.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    commandName = st.nextToken(DEFAULT_COMMAND);
    key = st.nextKeyStroke(DEFAULT_KEY);
    command = null;
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(commandName).append(key);
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
    Command c = null;
    myGetKeyCommands();
    if (command[0].matches(stroke)) {
      if (frame == null) {
        frame = buildDialog();
        frame.pack();
      }
      frame.setVisible(true);
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
    return "Aircraft Status Pad";
  }

  public HelpFile getHelpFile() {
      return null;
  }

  /**
   * Share background image between all Status Pads
   * @return background Image
   */
  public static Image getBackgroundImage() {
    if (backgroundImage == null) {
      try {
        backgroundImage = GameModule.getGameModule().getDataArchive().getCachedImage(BACKGROUND_IMAGE);
      }
      catch (Exception e) {
        backgroundImage = new BufferedImage(100, 100, BufferedImage.TYPE_INT_ARGB);
      }
    }
    return backgroundImage;
  }
  
  /**
   * Build the Status Pad Dialog
   */
  public StatusPadDialog buildDialog() {
    Map map = piece.getMap();
    Frame parent = null;
    
    if (map != null && map.getView() != null) {
      Container topWin = map.getView().getTopLevelAncestor();
      if (topWin instanceof JFrame) {
        parent = (Frame) topWin;
      }
    }

    StatusPadDialog dialog = new StatusPadDialog(parent);
    return dialog;
  }
  
  /**
   * The Status Pad Dialog
   */
  public class StatusPadDialog extends JDialog {

    protected int width, height;

    protected Frame owner;
    protected JPanel mainPanel;
    
    public StatusPadDialog (Frame owner) {
      super(owner, false);
      this.owner = owner;
      mainPanel = new StatusPanel(this);
      getContentPane().add(mainPanel);
    }
  }
  
  /**
   * 
   * The Main Panel of the Status Pad, drawing the background Image.
   * Create all sub-components in here
   */
  protected class StatusPanel extends JPanel {
    
    protected Component parent;
    
    public StatusPanel(Component owner) {
      parent = owner;

      getBackgroundImage().getWidth(owner); // Forces the image to size itself
      Dimension size = new Dimension(getBackgroundImage().getWidth(owner), getBackgroundImage().getHeight(owner));

      setSize(size);
      setPreferredSize(size);
    }
    
    protected void paintComponent(Graphics g) {
      g.drawImage(getBackgroundImage(), 0, 0, parent);
    }
  }
  
  public static class Ed implements PieceEditor {
    private StringConfigurer nameInput;
    private HotKeyConfigurer keyInput;
    private JPanel controls;

    public Ed(StatusPad p) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      nameInput = new StringConfigurer(null, "Command name:  ", p.commandName);
      controls.add(nameInput.getControls());

      keyInput = new HotKeyConfigurer(null,"Keyboard Command:  ",p.key);
      controls.add(keyInput.getControls());

    }

    public Component getControls() {
      return controls;
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(nameInput.getValueString()).append((KeyStroke)keyInput.getValue());
      return ID + se.getValue();
    }

    public String getState() {
      return "";
    }
  }
}
