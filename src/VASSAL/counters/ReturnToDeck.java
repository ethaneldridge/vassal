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
package VASSAL.counters;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.DrawPile;
import VASSAL.command.Command;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.SequenceEncoder;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.io.File;
import java.net.MalformedURLException;

/**
 * GamePiece trait that returns a piece to a {@link DrawPile}
 */
public class ReturnToDeck extends Decorator implements EditablePiece {
  public static final String ID = "return;";
  private String deckId;
  private String returnCommand;
  private char returnKey;
  private DrawPile deck;

  private KeyCommand[] commands;

  public ReturnToDeck() {
    this(ID + "Return to Deck;R;null", null);
  }

  public ReturnToDeck(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  protected KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      commands = new KeyCommand[]{new KeyCommand(returnCommand,
                                                 KeyStroke.getKeyStroke(returnKey, InputEvent.CTRL_MASK),
                                                 Decorator.getOutermost(this))};
    }
    return commands;
  }

  public String myGetState() {
    return "";
  }

  public void mySetType(String s) {
    s = s.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ';');
    returnCommand = st.nextToken();
    returnKey = st.nextChar('\0');
    deckId = st.nextToken();
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    return ID + se.append(returnCommand).append("" + returnKey).append(deckId).getValue();
  }

  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    Command comm = null;
    if (commands[0].matches(stroke)) {
      if (deck == null) {
        findDeck();
      }
      comm = deck.addToContents(Decorator.getOutermost(this));
      deck.getMap().repaint();
    }
    return comm;
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

  private void findDeck() {
    DrawPile pile = DrawPile.findDrawPile(deckId);
    if (pile == null) {
      throw new IllegalArgumentException("Could not find deck "+deckId);
    }
    deck = pile;
  }

  public void mySetState(String newState) {
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public String getDescription() {
    return "Return to Deck";
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "ReturnToDeck.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  private static class Ed implements PieceEditor {
    private StringConfigurer menuName;
    private KeySpecifier menuKey;
    private JPanel controls;
    private String deckId;
    private final JTextField tf = new JTextField(12);

    public Ed(ReturnToDeck p) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));
      menuName = new StringConfigurer(null, "Menu Text", p.returnCommand);
      controls.add(menuName.getControls());
      menuKey = new KeySpecifier(p.returnKey);
      deckId = p.deckId;
      Box b = Box.createHorizontalBox();
      b.add(new JLabel("Key Command:  "));
      b.add(menuKey);
      controls.add(b);
      JButton select = new JButton("Select Deck");
      tf.setEditable(false);
      updateDeckName();
      select.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          VASSAL.configure.ChooseComponentDialog d = new VASSAL.configure.ChooseComponentDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class, controls), DrawPile.class);
          d.setTitle("Select Deck");
          d.setVisible(true);
          if (d.getTarget() != null) {
            deckId = d.getTarget().getConfigureName();
            updateDeckName();
          }
        }
      });
      Box box = Box.createHorizontalBox();
      box.add(select);
      box.add(tf);
      controls.add(box);
    }

    private void updateDeckName() {
      DrawPile p = DrawPile.findDrawPile(deckId);
      tf.setText(p != null ? p.getConfigureName() : "<none>");
    }

    public Component getControls() {
      return controls;
    }

    public String getState() {
      return "";
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      return ID + se.append(menuName.getValueString()).append(menuKey.getKey()).append(deckId).getValue();
    }
  }
}
