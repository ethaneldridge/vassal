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

import VASSAL.command.Command;
import VASSAL.counters.*;

import javax.swing.*;
import java.awt.*;
import java.util.StringTokenizer;

/**
 * A GamePiece that draws itself as a box of a fixed size and color
 */
public class TextInfo extends Decorator implements EditablePiece {
  public static final String ID = "info;";

  private String info;
  private KeyCommand[] commands;
  private boolean showInfo = false;
  private Dimension infoSize;
  private Image infoImage;
  private static Font font = new Font("Dialog", 0, 11);

  public TextInfo() {
    this(ID, null);
  }

  public TextInfo(String type, GamePiece p) {
    setInner(p);
    info = type.substring(type.indexOf(';') + 1);
  }

  public void mySetType(String type) {
    info = type.substring(type.indexOf(';') + 1);
  }

  public void mySetState(String newState) {
  }

  public String myGetState() {
    return "";
  }

  public String myGetType() {
    return ID + info;
  }

  protected KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      commands = new KeyCommand[1];
      commands[0] = new KeyCommand("Show Info",
                                   KeyStroke.getKeyStroke('I', java.awt.event.InputEvent.CTRL_MASK),
                                   Decorator.getOutermost(this));
    }
    commands[0].setEnabled(getMap() != null);
    return commands;
  }

  public Command myKeyEvent(javax.swing.KeyStroke stroke) {
    myGetKeyCommands();
    if (commands[0].matches(stroke)) {
      showInfo = !showInfo;
    }
    return null;
  }

  public Rectangle selectionBounds() {
    return getInner().selectionBounds();
  }

  public Rectangle boundingBox() {
    Rectangle r = getInner().boundingBox();
    if (infoSize == null) {
      return r;
    }
    else {
      Rectangle infoRec = new Rectangle
        (getPosition().x + getInfoOffset().x,
         getPosition().y + getInfoOffset().y,
         infoSize.width, infoSize.height);
      return r.union(infoRec);
    }
  }

  private Point getInfoOffset() {
    return new Point(getInner().selectionBounds().width / 2 + 6,
                     -infoSize.height / 2);
  }

  public String getName() {
    return getInner().getName();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    getInner().draw(g, x, y, obs, zoom);
    if (showInfo) {
      if (infoSize == null) {
        g.setFont(font);
        infoSize = getInfoSize(info, g.getFontMetrics());
        infoImage = createInfoImage(obs);
      }
      g.drawImage(infoImage, x + (int) (zoom * getInfoOffset().x),
                  y + (int) (zoom * getInfoOffset().y),
                  (int) (zoom * infoSize.width),
                  (int) (zoom * infoSize.height), obs);
    }
  }

  /** Returns the size of the box into which the text info will be drawn
   */
  protected Dimension getInfoSize(String s, FontMetrics fm) {
    int wid = 0;
    StringTokenizer st = new StringTokenizer(s, "^,", true);
    while (st.hasMoreTokens()) {
      String token = st.nextToken();
      switch (token.charAt(0)) {
        case '^':
          break;
        case ',':
          wid += fm.stringWidth("  ");
          break;
        default:
          wid += fm.stringWidth(token);
      }
    }
    wid += 10;
    int hgt = fm.getAscent() * 2;
    return new Dimension(wid, hgt);
  }

  protected Image createInfoImage(Component obs) {
    Image im = obs.createImage(infoSize.width, infoSize.height);
    Graphics g = im.getGraphics();
    g.setFont(font);
    writeInfo(g, 0, infoSize.height / 2);
    return im;
  }

  /**
   * Write the special info.  The info string is broken into comma-separated
   * tokens, with each token separated by two spaces
   * Some characters are handled specially:
   * Tokens beginning with 'r' are written in red
   * 'R' is circled (radioless)
   * '^' indicates the beginning/end of a superscript
   */
  protected void writeInfo(Graphics g, int x, int y) {
    FontMetrics fm = g.getFontMetrics();

    g.setColor(Color.white);
    g.fillRect(x, y - infoSize.height / 2, infoSize.width, infoSize.height);
    g.setColor(Color.black);
    g.drawRect(x, y - infoSize.height / 2, infoSize.width - 1, infoSize.height - 1);

    StringTokenizer st = new StringTokenizer(info, "^,", true);

    x += 7;
    y += infoSize.height / 2 - 6;
    boolean superScript = false;
    while (st.hasMoreTokens()) {
      String s = st.nextToken();
      g.setColor(Color.black);
      switch (s.charAt(0)) {
        case 'r':
          s = s.substring(1);
          g.setColor(Color.red);
          break;
        case 'R':
          g.drawOval(x - 3, y - fm.getAscent(),
                     fm.getAscent() + 2,
                     fm.getAscent() + 2);
          break;
        case ',':
          s = "  ";
          break;
        case '^':
          s = "";
          superScript = !superScript;
          y += superScript ? -fm.getAscent() / 2 : fm.getAscent() / 2;
          break;
      }
      g.drawString(s, x, y);
      x += fm.stringWidth(s);
    }
  }

  public String getDescription() {
    return "Has info";
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return null;
  }

  public PieceEditor getEditor() {
    return new SimplePieceEditor(this);
  }
}
