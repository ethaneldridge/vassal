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

import VASSAL.build.GameModule;
import VASSAL.build.module.map.MenuDisplayer;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.ColorConfigurer;
import VASSAL.tools.SequenceEncoder;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Vector;

/**
 * A collection of pieces that behaves like a deck, i.e.:
 * Doesn't move.
 * Can't be expanded.
 * Can be shuffled.
 * Can be turned face-up and face-down
 */
public class Deck extends Stack {
  public static final String ID = "deck;";
  public static final String ALWAYS = "Always";
  public static final String NEVER = "Never";
  public static final String USE_MENU = "Via right-click Menu";

  private boolean drawOutline = true;
  private Color outlineColor = Color.black;
  protected Dimension size = new Dimension(40, 40);
  protected boolean shuffle = true;
  private String faceDownOption = ALWAYS;
  private String shuffleOption = ALWAYS;
  private boolean allowMultipleDraw = false;
  private boolean allowSelectDraw = false;
  private boolean reversible = false;

  private boolean faceDown;
  protected int dragCount = 0;
  private ArrayList nextDraw;
  private KeyCommand[] commands;

  public Deck() {
    this(ID + "true;0,0,0;40;40;Always;Always;false;false;false");
  }

  public Deck(String type) {
    mySetType(type);
  }

  protected void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    drawOutline = "true".equals(st.nextToken());
    outlineColor = ColorConfigurer.stringToColor(st.nextToken());
    size = new Dimension(Integer.parseInt(st.nextToken()), Integer.parseInt(st.nextToken()));
    faceDownOption = st.nextToken();
    shuffleOption = st.nextToken();
    allowMultipleDraw = "true".equals(st.nextToken());
    allowSelectDraw = "true".equals(st.nextToken());
    reversible = "true".equals(st.nextToken());
  }

  public String getType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append("" + drawOutline)
        .append(ColorConfigurer.colorToString(outlineColor))
        .append(size.width + "").append(size.height + "")
        .append(faceDownOption)
        .append(shuffleOption)
        .append(allowMultipleDraw + "")
        .append(allowSelectDraw + "")
        .append(reversible+"");
    return ID + se.getValue();
  }

  /** Shuffle the contents of the Deck */
  public Command shuffle() {
    ArrayList indices = new ArrayList();
    for (int i = 0; i < getPieceCount(); ++i) {
      indices.add(new Integer(i));
    }
    ArrayList newContents = new ArrayList();
    DragBuffer.getBuffer().clear();
    for (int count = getPieceCount(); count > 0; --count) {
      int i = (int) (GameModule.getGameModule().getRNG().nextFloat()
          * indices.size());
      int index = ((Integer) indices.get(i)).intValue();
      indices.remove(i);
      newContents.add(getPieceAt(index));
    }
    return setContents(newContents.iterator());
  }

  /** Set the contents of this Deck to an Enumeration of GamePieces */
  protected Command setContents(Iterator it) {
    ChangeTracker track = new ChangeTracker(this);
    removeAll();
    while (it.hasNext()) {
      add((GamePiece) it.next());
    }
    return track.getChangeCommand();
  }

  public String getState() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append("" + faceDown);
    SequenceEncoder se2 = new SequenceEncoder(',');
    for (Enumeration e = getPieces(); e.hasMoreElements();) {
      GamePiece p = (GamePiece) e.nextElement();
      se2.append(p.getId());
    }
    return se.append(se2.getValue()).getValue();
  }

  public void setState(String state) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(state, ';');
    faceDown = "true".equals(st.nextToken());
    SequenceEncoder.Decoder st2 = new SequenceEncoder.Decoder(st.nextToken(), ',');
    ArrayList l = new ArrayList();
    while (st2.hasMoreTokens()) {
      GamePiece p = GameModule.getGameModule().getGameState().getPieceForId(st2.nextToken());
      if (p != null) {
        l.add(p);
      }
    }
    setContents(l.iterator());
  }

  public Command setContentsFaceDown(boolean value) {
    ChangeTracker t = new ChangeTracker(this);
    Command c = new NullCommand();
    faceDown = value;
    if (!faceDown) {
      for (Enumeration e = getPieces(); e.hasMoreElements();) {
        GamePiece p = (GamePiece) e.nextElement();
        if (p.getProperty(Obscurable.ID) != null) {
          ChangeTracker tracker = new ChangeTracker(p);
          p.setProperty(Obscurable.ID, null);
          c.append(tracker.getChangeCommand());
        }
      }
    }
    return t.getChangeCommand().append(c);
  }

  /** Reverse the order of the contents of the Deck */
  public Command reverse() {
    ArrayList list = new ArrayList();
    for (Enumeration e = getPiecesInReverseOrder();
         e.hasMoreElements();) {
      list.add(e.nextElement());
    }
    return setContents(list.iterator());
  }

  public boolean isDrawOutline() {
    return drawOutline;
  }

  public void setDrawOutline(boolean drawOutline) {
    this.drawOutline = drawOutline;
  }

  public Color getOutlineColor() {
    return outlineColor;
  }

  public void setOutlineColor(Color outlineColor) {
    this.outlineColor = outlineColor;
  }

  public void draw(java.awt.Graphics g, int x, int y, Component obs, double zoom) {
    int count = 0;
    GamePiece top = topPiece();
    if (top != null) {
      Rectangle r = top.getShape().getBounds();
      r.setLocation(x + (int) (zoom * (r.x)), y + (int) (zoom * (r.y)));
      r.setSize((int) (zoom * r.width), (int) (zoom * r.height));
      count = count > 10 ? 10 : count;
      for (int i = 0; i < count - 1; ++i) {
        g.setColor(Color.white);
        g.fillRect(r.x + (int) (zoom * 2 * i),
                   r.y - (int) (zoom * 2 * i), r.width, r.height);
        g.setColor(Color.black);
        g.drawRect(r.x + (int) (zoom * 2 * i),
                   r.y - (int) (zoom * 2 * i), r.width, r.height);
      }
      if (faceDown) {
        Obscurable.setAllHidden(true);
        top.draw(g, x + (int) (zoom * 2 * (count - 1)),
                 y - (int) (zoom * 2 * (count - 1)), obs, zoom);
        Obscurable.setAllHidden(false);
      }
      else {
        top.draw(g, x + (int) (zoom * 2 * (count - 1)),
                 y - (int) (zoom * 2 * (count - 1)), obs, zoom);
      }
    }
    else {
      if (drawOutline) {
        Rectangle r = boundingBox();
        r.setLocation(x + (int) (zoom * (r.x - getPosition().x)), y + (int) (zoom * (r.y - getPosition().y)));
        r.setSize((int) (zoom * r.width), (int) (zoom * r.height));
        g.setColor(outlineColor);
        g.drawRect(r.x, r.y, r.width, r.height);
      }
    }
  }

  public Object getProperty(Object key) {
    Object value = null;
    if (Properties.IMMOBILE.equals(key)) {
      value = Boolean.TRUE;
    }
    else if (Properties.KEY_COMMANDS.equals(key)) {
      value = getKeyCommands();
    }
    return value;
  }

  private KeyCommand[] getKeyCommands() {
    if (commands == null) {
      ArrayList l = new ArrayList();
      KeyCommand c = null;
      if (USE_MENU.equals(shuffleOption)) {
        c = new KeyCommand("Shuffle", null, this) {
                public void actionPerformed(ActionEvent e) {
                  GameModule.getGameModule().sendAndLog(shuffle());
                  map.repaint();
                }
              };
        l.add(c);
      }
      if (USE_MENU.equals(faceDownOption)) {
        KeyCommand faceDownAction = new KeyCommand(faceDown ? "Face up" : "Face down", null, this) {
          public void actionPerformed(ActionEvent e) {
            GameModule.getGameModule().sendAndLog(setContentsFaceDown(!faceDown));
            map.repaint();
          }
        };
        l.add(faceDownAction);
      }
      if (reversible) {
        c = new KeyCommand("Reverse order", null, this) {
          public void actionPerformed(ActionEvent e) {
            GameModule.getGameModule().sendAndLog(reverse());
            map.repaint();
          }
        };
        l.add(c);
      }
      if (allowMultipleDraw) {
        c = new KeyCommand("Draw multiple cards", null, this) {
          public void actionPerformed(ActionEvent e) {
            promptForDragCount();
          }
        };
        l.add(c);
      }
      if (allowSelectDraw) {
        c = new KeyCommand("Draw specific cards", null, this) {
          public void actionPerformed(ActionEvent e) {
            promptForNextDraw();
          }
        };
        l.add(c);
      }
      commands = (KeyCommand[]) l.toArray(new Object[l.size()]);
    }
    return commands;
  }

  public void promptForDragCount() {
    while (true) {
      String s = JOptionPane.showInputDialog("Enter number to grab.\nThen click and drag to draw that number.");
      if (s != null) {
        try {
          dragCount = Integer.parseInt(s);
          dragCount = Math.min(dragCount, getPieceCount());
          if (dragCount >= 0) {
            break;
          }
        }
        catch (NumberFormatException ex) {
        }
      }
    }
  }

  private void promptForNextDraw() {
    final JDialog d = new JDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class, map.getView()), true);
    d.setTitle("Draw");
    d.getContentPane().setLayout(new BoxLayout(d.getContentPane(), BoxLayout.Y_AXIS));
    final String[] pieces = new String[getPieceCount()];
    for (int i = 0; i < pieces.length; ++i) {
      pieces[pieces.length - i - 1] = Decorator.getInnermost(getPieceAt(i)).getName();
    }
    final JList list = new JList(pieces);
    list.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    d.getContentPane().add(new JScrollPane(list));
    d.getContentPane().add(new JLabel("Select cards to draw"));
    d.getContentPane().add(new JLabel("Then click and drag from the deck."));
    Box box = Box.createHorizontalBox();
    JButton b = new JButton("Ok");
    b.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        nextDraw = new ArrayList();
        int[] selection = list.getSelectedIndices();
        for (int i = 0; i < selection.length; ++i) {
          nextDraw.add(getPieceAt(pieces.length - selection[i] - 1));
        }
        d.dispose();
      }
    });
    box.add(b);
    b = new JButton("Cancel");
    b.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        d.dispose();
      }
    });
    box.add(b);
    d.getContentPane().add(box);
    d.pack();
    d.setLocationRelativeTo(d.getOwner());
    d.setVisible(true);
  }

  public boolean isExpanded() {
    return false;
  }
}
