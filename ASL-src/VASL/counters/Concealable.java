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
package VASL.counters;

import VASSAL.build.GameModule;
import VASSAL.command.ChangePiece;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.counters.*;
import VASSAL.tools.SequenceEncoder;

import javax.swing.*;
import java.awt.*;

public class Concealable extends Obscurable implements EditablePiece {
  public static final String ID = "conceal;";

  private String nation;
  private String nation2;
  private Image concealedToMe;
  private Image concealedToOthers;
  private Dimension imageSize = new Dimension(-1, -1);

  public Concealable() {
    this(ID + "C;Qmark58;ge", null);
  }

  public Concealable(String type, GamePiece inner) {
    super(type, inner);
    mySetType(type);
    hideCommand = "Conceal";
  }

  public void mySetType(String in) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(in, ';');
    st.nextToken();
    obscureKey = st.nextToken().toUpperCase().charAt(0);
    imageName = st.nextToken();
    nation = st.nextToken();
    if (st.hasMoreTokens()) {
      nation2 = st.nextToken();
    }
  }

  public String myGetType() {
    String s = ID +
        obscureKey + ";" +
        imageName + ";" +
        nation;
    if (nation2 != null) {
      s += ";" + nation2;
    }
    return s;
  }

  protected void drawObscuredToMe(Graphics g, int x, int y, Component obs, double zoom) {
    loadImages(obs);
    int size = (int) (zoom * imageSize.width);
    g.setColor(getColor(nation));
    g.fillRect(x - size / 2, y - size / 2, size, size);
    if (nation2 != null) {
      g.setColor(getColor(nation2));
      g.fillRect(x - size / 2 + size / 8, y - size / 2 + size / 8, size - size / 4, size - size / 4);
    }
    g.drawImage(concealedToMe, x - size / 2, y - size / 2, size, size, obs);
  }

  protected void drawObscuredToOthers(Graphics g, int x, int y, Component obs, double zoom) {
    loadImages(obs);
    getInner().draw(g, x, y, obs, zoom);
    g.setColor(getColor(nation));
    int size = (int) (zoom * imageSize.width);
    g.fillRect(x - size / 2, y - size / 2, size / 2, size * 2 / 3);
    if (nation2 != null) {
      g.setColor(getColor(nation2));
      g.fillRect(x - size / 2 + size / 8, y - size / 2 + size / 8, size / 2 - size / 8, size * 2 / 3 - size / 8);
    }
    try {
      g.drawImage(concealedToOthers,
                  x - size / 2, y - size / 2, size, size, obs);
    }
    catch (Exception e) {
    }
  }

  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    Command c = null;
    if (commands[0].matches(stroke)
        && getMap() != null
        && !obscuredToOthers()
        && !obscuredToMe()) {
      c = super.myKeyEvent(stroke);
      boolean concealmentExists = false;
      GamePiece outer = Decorator.getOutermost(this);
      if (getParent() != null) {
        for (int i = getParent().indexOf(outer),j = getParent().getPieceCount(); i < j; ++i) {
          Concealment conceal = (Concealment) Decorator.getDecorator(getParent().getPieceAt(i), Concealment.class);
          if (conceal != null
              && conceal.canConceal(outer)) {
            concealmentExists = true;
            break;
          }
        }
      }
      if (!concealmentExists) {
        GamePiece concealOuter = createConcealment();
        Concealment conceal = (Concealment) Decorator.getDecorator(concealOuter, Concealment.class);
        c.append
            (getMap().getStackMetrics().merge
             (outer, concealOuter));
        for (int i = 0,j = getParent().indexOf(outer);
             i < j; ++i) {
          c.append(conceal.setConcealed(getParent().getPieceAt(i), true));
        }
      }
    }
    else {
      c = super.myKeyEvent(stroke);
    }
    return c;
  }

  public Command keyEvent(javax.swing.KeyStroke stroke) {
    Stack parent = getParent();
    if (parent != null) {
      int lastIndex = getParent().indexOf(Decorator.getOutermost(this));
      Command c = super.keyEvent(stroke);
      if (getParent() != null) {
        int newIndex = getParent().indexOf(Decorator.getOutermost(this));
        if (newIndex != lastIndex) {
          c.append(adjustConcealment());
        }
      }
      return c;
    }
    else {
      return super.keyEvent(stroke);
    }
  }

  /**
   * Conceal/unconceal this unit according to whether a concealment
   * counter is on top of it in a stack
   */
  public Command adjustConcealment() {
    if (isMaskableBy(GameModule.getUserId())) {
      GamePiece outer = Decorator.getOutermost(this);
      String state = outer.getState();
      setProperty(Obscurable.ID, null);
      if (getParent() != null) {
        for (int i = getParent().indexOf(outer),j = getParent().getPieceCount(); i < j; ++i) {
          Concealment p = (Concealment) Decorator.getDecorator(getParent().getPieceAt(i), Concealment.class);
          if (p != null && p.canConceal(this)) {
            setProperty(Obscurable.ID, GameModule.getUserId());
            break;
          }
        }
        getMap().repaint(getParent().boundingBox());
      }
      return outer.getState().equals(state) ? null
          : new ChangePiece(outer.getId(), state, outer.getState());
    }
    else {
      return null;
    }
  }

  /**
   * Conceal/unconceal units in this stack according to positions
   * of concealment counters in the stack
   */
  public static Command adjustConcealment(Stack s) {
    if (s == null || s.getMap() == null) {
      return null;
    }
    Command c = new NullCommand();
    for (int i = 0,j = s.getPieceCount(); i < j; ++i) {
      Concealable p = (Concealable) Decorator.getDecorator(s.getPieceAt(i), Concealable.class);
      if (p != null) {
        c = c.append(p.adjustConcealment());
      }
    }
    s.getMap().repaint(s.boundingBox());
    return c;
  }

  /**
   * @return a new GamePiece that is a concealment counter
   * appropriate for this unit
   */
  public GamePiece createConcealment() {
    GamePiece p = new BasicPiece(BasicPiece.ID + "K;D;" + imageName + ";?");
    boolean large = imageName.substring(0, 1).toUpperCase().
        equals(imageName.substring(0, 1));
    String size = large ? "60;60" : "48;48";
    if (nation2 != null) {
      p = new ColoredBox(ColoredBox.ID + "ru" + ";" + size, p);
      p = new ColoredBox(ColoredBox.ID + "ge" + ";"
                         + (large ? "48;48" : "36;36"), p);
    }
    else {
      p = new ColoredBox(ColoredBox.ID + nation + ";" + size, p);
    }
    p = new Embellishment(Embellishment.ID + ";;;;;;0;0;"
                          + imageName + ",?", p);
    p = new Concealment(Concealment.ID, p);
    p = new MarkMoved(MarkMoved.ID + (large ? "moved58" : "moved"), p);
    p = new Hideable("hide;H;HIP", p);
    return p;
  }

  public Rectangle boundingBox() {
    return getInner().boundingBox();
  }

  public Rectangle selectionBounds() {
    return getInner().selectionBounds();
  }

  private void loadImages(Component obs) {
    if (concealedToOthers == null) {
      try {
        concealedToOthers =
            GameModule.getGameModule().getDataArchive().getCachedImage("qmarkme.gif");
      }
      catch (java.io.IOException ex) {
        concealedToOthers = obs.createImage(20, 20);
        java.awt.Graphics g = concealedToOthers.getGraphics();
        g.drawString("?", 0, 0);
      }
    }
    if (concealedToMe == null) {
      try {
        concealedToMe = GameModule.getGameModule().getDataArchive()
            .getCachedImage(imageName + ".gif");
        if (concealedToMe != null) {
          JLabel l = new JLabel(new ImageIcon(concealedToMe));
          imageSize = l.getPreferredSize();
        }
        else {
          imageSize.setSize(0, 0);
        }
      }
      catch (java.io.IOException ex) {
        concealedToMe = obs.createImage(20, 20);
        java.awt.Graphics g = concealedToMe.getGraphics();
        g.drawString("?", 0, 0);
      }
    }
  }

  public String getName() {
    if (obscuredToMe()) {
      return "?";
    }
    else if (obscuredToOthers()) {
      return getInner().getName() + "(?)";
    }
    else {
      return getInner().getName();
    }
  }

  public Object getProperty(Object key) {
    if (ASLProperties.HINDRANCE.equals(key)
        && obscuredToMe()) {
      return null;
    }
    else {
      return super.getProperty(key);
    }
  }

  public Color getColor(String s) {
    return (Color) GameModule.getGameModule().getPrefs().getValue(s);
  }

  public String getDescription() {
    return "Can be concealed";
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return null;
  }

  public PieceEditor getEditor() {
    return new SimplePieceEditor(this);
  }
}
