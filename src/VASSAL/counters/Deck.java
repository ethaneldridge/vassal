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
import VASSAL.build.module.Chatter;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.DrawPile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.ColorConfigurer;
import VASSAL.tools.FormattedString;
import VASSAL.tools.PlayerIdFormattedString;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.UniqueIdManager;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;

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
  private static final String NO_USER = "null"; // Dummy user ID for turning cards face down

  private boolean drawOutline = true;
  private Color outlineColor = Color.black;
  protected Dimension size = new Dimension(40, 40);
  protected boolean shuffle = true;
  private String faceDownOption = ALWAYS;
  private String shuffleOption = ALWAYS;
  private boolean allowMultipleDraw = false;
  private boolean allowSelectDraw = false;
  private boolean reversible = false;
  private String reshuffleCommand = "";
  private String reshuffleTarget;
  private String reshuffleMsgFormat;
  private String reverseMsgFormat;
  private String shuffleMsgFormat;
  private String faceDownMsgFormat;

  private String deckName;

  private boolean faceDown;
  protected int dragCount = 0;
  private ArrayList nextDraw;
  private KeyCommand[] commands;

  public Deck() {
    this(ID + "true;0,0,0;40;40;Always;Always;false;false;false;;;");
  }

  public Deck(String type) {
    mySetType(type);
  }

  protected void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    drawOutline = "true".equals(st.nextToken());
    outlineColor = ColorConfigurer.stringToColor(st.nextToken());
    size.setSize(st.nextInt(40), st.nextInt(40));
    faceDownOption = st.nextToken();
    shuffleOption = st.nextToken();
    allowMultipleDraw = "true".equals(st.nextToken());
    allowSelectDraw = "true".equals(st.nextToken());
    reversible = "true".equals(st.nextToken());
    reshuffleCommand = st.nextToken();
    reshuffleTarget = st.nextToken();
    reshuffleMsgFormat = st.nextToken();
    deckName = st.nextToken("Deck");
    shuffleMsgFormat = st.nextToken("");
    reverseMsgFormat = st.nextToken("");
    faceDownMsgFormat = st.nextToken("");
  }

  public String getFaceDownOption() {
    return faceDownOption;
  }

  public void setFaceDownOption(String faceDownOption) {
    this.faceDownOption = faceDownOption;
    faceDown = !faceDownOption.equals(NEVER);
  }

  public Dimension getSize() {
    return size;
  }

  public void setSize(Dimension size) {
    this.size.setSize(size);
  }

  public String getShuffleOption() {
    return shuffleOption;
  }

  public void setShuffleOption(String shuffleOption) {
    this.shuffleOption = shuffleOption;
  }

  public boolean isShuffle() {
    return shuffle;
  }

  public String getFaceDownMsgFormat() {
    return faceDownMsgFormat;
  }

  public void setFaceDownMsgFormat(String faceDownMsgFormat) {
    this.faceDownMsgFormat = faceDownMsgFormat;
  }

  public String getReverseMsgFormat() {
    return reverseMsgFormat;
  }

  public void setReverseMsgFormat(String reverseMsgFormat) {
    this.reverseMsgFormat = reverseMsgFormat;
  }

  public String getShuffleMsgFormat() {
    return shuffleMsgFormat;
  }

  public void setShuffleMsgFormat(String shuffleMsgFormat) {
    this.shuffleMsgFormat = shuffleMsgFormat;
  }

  public void setShuffle(boolean shuffle) {
    this.shuffle = shuffle;
  }

  public boolean isAllowMultipleDraw() {
    return allowMultipleDraw;
  }

  public void setAllowMultipleDraw(boolean allowMultipleDraw) {
    this.allowMultipleDraw = allowMultipleDraw;
  }

  public boolean isAllowSelectDraw() {
    return allowSelectDraw;
  }

  public void setAllowSelectDraw(boolean allowSelectDraw) {
    this.allowSelectDraw = allowSelectDraw;
  }

  public boolean isReversible() {
    return reversible;
  }

  public void setReversible(boolean reversible) {
    this.reversible = reversible;
  }

  public void setDeckName(String n) {
    deckName = n;
  }

  public String getDeckName() {
    return deckName;
  }

  /**
   * The popup menu text for the command that sends the entire deck to another deck
   * @return
   */
  public String getReshuffleCommand() {
    return reshuffleCommand;
  }

  public void setReshuffleCommand(String reshuffleCommand) {
    this.reshuffleCommand = reshuffleCommand;
  }

  /**
   * The name of the {@link VASSAL.build.module.map.DrawPile} to which the contents of this deck
   * will be sent when the reshuffle command is selected
   */
  public String getReshuffleTarget() {
    return reshuffleTarget;
  }

  public void setReshuffleTarget(String reshuffleTarget) {
    this.reshuffleTarget = reshuffleTarget;
  }

  /**
   * The message to send to the chat window when the deck is reshuffled to another deck
   * @return
   */
  public String getReshuffleMsgFormat() {
    return reshuffleMsgFormat;
  }

  public void setReshuffleMsgFormat(String reshuffleMsgFormat) {
    this.reshuffleMsgFormat = reshuffleMsgFormat;
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
        .append(reversible + "")
        .append(reshuffleCommand)
        .append(reshuffleTarget)
        .append(reshuffleMsgFormat)
        .append(deckName)
          .append(shuffleMsgFormat)
        .append(reverseMsgFormat)
        .append(faceDownMsgFormat);
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
    return setContents(newContents.iterator()).append(reportCommand(shuffleMsgFormat, "Shuffle"));
  }

  /**
   * Return an iterator of pieces to be drawn from the Deck.
   * Normally, a random piece will be drawn, but if the Deck supports it,
   * the user may have specified a particular set of pieces or a
   * fixed number of pieces to select with the next draw.
   */
  public PieceIterator drawCards() {
    PieceIterator it;
    if (nextDraw != null) {
      it = new PieceIterator(nextDraw.iterator());
    }
    else {
      int count = Math.max(dragCount, Math.min(1, getPieceCount()));
      ArrayList pieces = new ArrayList();
      if (ALWAYS.equals(shuffleOption)) {
        ArrayList indices = new ArrayList();
        for (int i = 0; i < getPieceCount(); ++i) {
          indices.add(new Integer(i));
        }
        while (count-- > 0) {
          int i = GameModule.getGameModule().getRNG().nextInt(indices.size());
          int index = ((Integer) indices.get(i)).intValue();
          indices.remove(i);
          GamePiece p = getPieceAt(index);
          if (faceDown) {
            p.setProperty(Properties.OBSCURED_BY, NO_USER);
          }
          pieces.add(p);
        }
      }
      else {
        Enumeration e = getPiecesInReverseOrder();
        while (count-- > 0 && e.hasMoreElements()) {
          GamePiece p = (GamePiece) e.nextElement();
          if (faceDown) {
            p.setProperty(Properties.OBSCURED_BY, NO_USER);
          }
          pieces.add(p);
        }
      }
      it = new PieceIterator(pieces.iterator());
    }
    dragCount = 0;
    nextDraw = null;
    return it;
  }

  /** Set the contents of this Deck to an Enumeration of GamePieces */
  protected Command setContents(Iterator it) {
    ChangeTracker track = new ChangeTracker(this);
    removeAll();
    while (it.hasNext()) {
      GamePiece child = (GamePiece) it.next();
      insertChild(child,pieceCount);
    }
    return track.getChangeCommand();
  }

  public String getState() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(getMap() == null ? "null" : UniqueIdManager.getIdentifier(getMap()))
        .append("" + getPosition().x)
        .append("" + getPosition().y);
    se.append("" + faceDown);
    SequenceEncoder se2 = new SequenceEncoder(',');
    for (Enumeration e = getPieces(); e.hasMoreElements();) {
      GamePiece p = (GamePiece) e.nextElement();
      se2.append(p.getId());
    }
    if (se2.getValue() != null) {
      se.append(se2.getValue());
    }
    return se.getValue();
  }

  public void setState(String state) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(state, ';');
    String mapId = st.nextToken();
    setPosition(new Point(st.nextInt(0), st.nextInt(0)));
    Map m = null;
    if (!"null".equals(mapId)) {
      m = Map.getMapById(mapId);
      if (m == null) {
        throw new RuntimeException("Could not find map " + mapId);
      }
    }
    if (m != getMap()) {
      if (m != null) {
        m.addPiece(this);
      }
      else {
        setMap(null);
      }
    }
    faceDown = "true".equals(st.nextToken());
    ArrayList l = new ArrayList();
    if (st.hasMoreTokens()) {
      SequenceEncoder.Decoder st2 = new SequenceEncoder.Decoder(st.nextToken(), ',');
      while (st2.hasMoreTokens()) {
        GamePiece p = GameModule.getGameModule().getGameState().getPieceForId(st2.nextToken());
        if (p != null) {
          l.add(p);
        }
      }
    }
    setContents(l.iterator());
    commands = null; // Force rebuild of popup menu
  }

  public Command setContentsFaceDown(boolean value) {
    ChangeTracker t = new ChangeTracker(this);
    Command c = new NullCommand();
    faceDown = value;
    return t.getChangeCommand().append(c).append(reportCommand(faceDownMsgFormat, value ? "Face Down" : "Face Up"));
  }

  /** Reverse the order of the contents of the Deck */
  public Command reverse() {
    ArrayList list = new ArrayList();
    for (Enumeration e = getPiecesInReverseOrder();
         e.hasMoreElements();) {
      list.add(e.nextElement());
    }
    return setContents(list.iterator()).append(reportCommand(reverseMsgFormat, "Reverse"));
  }

  public boolean isDrawOutline() {
    return drawOutline;
  }

  public void setOutlineColor(Color outlineColor) {
    this.outlineColor = outlineColor;
  }

  public void setDrawOutline(boolean drawOutline) {
    this.drawOutline = drawOutline;
  }

  public Color getOutlineColor() {
    return outlineColor;
  }

  public boolean isFaceDown() {
    return faceDown;
  }

  public void setFaceDown(boolean faceDown) {
    this.faceDown = faceDown;
  }

  public void draw(java.awt.Graphics g, int x, int y, Component obs, double zoom) {
    int count = Math.min(getPieceCount(), 10);
    GamePiece top = topPiece();

    if (top != null) {
      top.setProperty(Properties.OBSCURED_BY, faceDown ? NO_USER : null);
      Color blankColor = getBlankColor();
      Rectangle r = top.getShape().getBounds();
      r.setLocation(x + (int) (zoom * (r.x)), y + (int) (zoom * (r.y)));
      r.setSize((int) (zoom * r.width), (int) (zoom * r.height));
      for (int i = 0; i < count - 1; ++i) {
        if (blankColor != null) {
          g.setColor(blankColor);
          g.fillRect(r.x + (int) (zoom * 2 * i),
                     r.y - (int) (zoom * 2 * i), r.width, r.height);
          g.setColor(Color.black);
          g.drawRect(r.x + (int) (zoom * 2 * i),
                     r.y - (int) (zoom * 2 * i), r.width, r.height);
        }
        else if (faceDown) {
          top.draw(g, x + (int) (zoom * 2 * i),
                   y - (int) (zoom * 2 * i), obs, zoom);
        }
        else {
          getPieceAt(count - i - 1).draw(g, x + (int) (zoom * 2 * i),
                                         y - (int) (zoom * 2 * i), obs, zoom);
        }
      }
      top.draw(g, x + (int) (zoom * 2 * (count - 1)),
               y - (int) (zoom * 2 * (count - 1)), obs, zoom);
    }
    else {
      if (drawOutline) {
        Rectangle r = boundingBox();
        r.setLocation(x + (int) (zoom * r.x), y + (int) (zoom * r.y));
        r.setSize((int) (zoom * r.width), (int) (zoom * r.height));
        g.setColor(outlineColor);
        g.drawRect(r.x, r.y, r.width, r.height);
      }
    }
  }

  /**
   * The color used to draw boxes representing cards underneath the top one.
   * If null, then draw each card normally for face-up decks,
   * and duplicate the top card for face-down decks
   * @return
   */
  protected Color getBlankColor() {
    Color c = Color.white;
    if (getMap() != null) {
      c = getMap().getStackMetrics().getBlankColor();
    }
    return c;
  }

  public Rectangle boundingBox() {
    GamePiece top = topPiece();
    Dimension d = top == null ? size : top.getShape().getBounds().getSize();
    Rectangle r = new Rectangle(new Point(), d);
    r.translate(-r.width / 2, -r.height / 2);
    return r;
  }

  public Shape getShape() {
    return boundingBox();
  }

  public Object getProperty(Object key) {
    Object value = null;
    if (Properties.NO_STACK.equals(key)) {
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
      if (reshuffleCommand.length() > 0) {
        c = new KeyCommand(reshuffleCommand, null, this) {
          public void actionPerformed(ActionEvent evt) {
            GameModule.getGameModule().sendAndLog(sendToDeck());
            map.repaint();
          }
        };
        l.add(c);
      }
      if (USE_MENU.equals(faceDownOption)) {
        KeyCommand faceDownAction = new KeyCommand(faceDown ? "Face up" : "Face down", null, this) {
          public void actionPerformed(ActionEvent e) {
            Command c = setContentsFaceDown(!faceDown);
            GameModule.getGameModule().sendAndLog(c);
            map.repaint();
          }
        };
        l.add(faceDownAction);
      }
      if (reversible) {
        c = new KeyCommand("Reverse order", null, this) {
          public void actionPerformed(ActionEvent e) {
            Command c = reverse();
            GameModule.getGameModule().sendAndLog(c);
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
      commands = (KeyCommand[]) l.toArray(new KeyCommand[l.size()]);
    }
    for (int i=0;i<commands.length;++i) {
      if ("Face up".equals(commands[i].getValue(Action.NAME)) && !faceDown) {
        commands[i].putValue(Action.NAME,"Face down");
      }
      else if ("Face down".equals(commands[i].getValue(Action.NAME)) && faceDown) {
        commands[i].putValue(Action.NAME,"Face up");
      }
    }
    return commands;
  }

  /*
   * Format command report as per module designers setup.
   */
  private Command reportCommand(String format, String commandName) {
    Command c = null;
    FormattedString reportFormat = new PlayerIdFormattedString(format);
    reportFormat.setProperty(DrawPile.DECK_NAME, getDeckName());
    reportFormat.setProperty(DrawPile.COMMAND_NAME, commandName);
    String rep = reportFormat.getText();
    if (rep.length() > 0) {
      c = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), "* " + rep);
      c.execute();
    }

    return c;
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

  /**
   * Combine the contents of this Deck with the contents of the
   * deck specified by {@link #reshuffleTarget}
   */
  public Command sendToDeck() {
    Command c = null;
    DrawPile target = DrawPile.findDrawPile(reshuffleTarget);
    if (target != null) {
      if (reshuffleMsgFormat.length() > 0) {
        c = reportCommand(reshuffleMsgFormat, reshuffleCommand);
        if (c == null) {
          c = new NullCommand();
        }
      }
      else {
        c = new NullCommand();
      }
      // move cards to deck
      int cnt = getPieceCount() - 1;
      for (int i = cnt; i >= 0; i--) {
        c.append(target.addToContents(getPieceAt(i)));
      }
    }
    return c;
  }

  public boolean isExpanded() {
    return false;
  }
}
