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

import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.SequenceEncoder;

import javax.swing.*;
import java.awt.*;
import java.awt.event.InputEvent;
import java.io.File;
import java.net.MalformedURLException;

/** * Displays a text label, with content specified by the user at runtime */
public class Labeler extends Decorator implements EditablePiece {
  public static final String ID = "label;";
  protected Color textBg = Color.black;
  protected Color textFg = Color.white;

  public static final int CENTER = 0;
  public static final int RIGHT = 1;
  public static final int LEFT = 2;
  public static final int TOP = 3;
  public static final int BOTTOM = 4;

  public static int HORIZONTAL_ALIGNMENT = CENTER;
  public static int VERTICAL_ALIGNMENT = TOP;

  private String label = "";
  private char labelKey = 'L';
  private String menuCommand = "Change Label";
  private Font font = new Font("Dialog", 0, 10);
  private KeyCommand[] commands;

  private Image labelImage;
  private JLabel lbl;
  private char verticalJust = 'b';
  private char horizontalJust = 'c';
  private char verticalPos = 't';
  private char horizontalPos = 'c';
  private int verticalOffset = 0;
  private int horizontalOffset = 0;

  public Labeler() {
    this(ID, null);
  }

  public Labeler(String s, GamePiece d) {
    lbl = new JLabel();
    mySetType(s);
    setInner(d);
  }

  public void mySetType(String type) {
    commands = null;
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    labelKey = st.nextChar('\0');
    if (st.hasMoreTokens()) {
      menuCommand = st.nextToken();
      font = new Font("Dialog", Font.PLAIN, Integer.parseInt(st.nextToken()));
      Color c = ColorConfigurer.stringToColor(st.nextToken());
      if (c != null) {
        textBg = c;
      }
      c = ColorConfigurer.stringToColor(st.nextToken());
      if (c != null) {
        textFg = c;
      }
      verticalPos = st.nextChar('t');
      verticalOffset = st.nextInt(0);
      horizontalPos = st.nextChar('c');
      horizontalOffset = st.nextInt(0);
      verticalJust = st.nextChar('b');
      horizontalJust = st.nextChar('c');
    }
    lbl.setForeground(textFg);
    lbl.setFont(font);
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(labelKey == 0 ? "" : "" + labelKey);
    se.append(menuCommand);
    se.append("" + font.getSize());
    String s = ColorConfigurer.colorToString(textBg);
    se.append(s == null ? "" : s);
    s = ColorConfigurer.colorToString(textFg);
    se.append(s == null ? "" : s);
    se.append("" + verticalPos);
    se.append("" + verticalOffset);
    se.append("" + horizontalPos);
    se.append("" + horizontalOffset);
    se.append("" + verticalJust);
    se.append("" + horizontalJust);

    return ID + se.getValue();

  }

  public String myGetState() {
    return label;
  }

  public void mySetState(String s) {
    setLabel(s.trim());
  }

  public String getName() {
    if (label.length() == 0) {
      return piece.getName();
    }
    else {
      return piece.getName() + " (" + label + ")";
    }
  }

  public static void drawLabel(Graphics g, String text, int x, int y, int hAlign, int vAlign, Color fgColor, Color bgColor) {
    drawLabel(g, text, x, y, new Font("Dialog", Font.PLAIN, 10), hAlign, vAlign, fgColor, bgColor, null);
  }

  public static void drawLabel(Graphics g, String text, int x, int y, Font f, int hAlign, int vAlign, Color fgColor, Color bgColor, Color borderColor) {
    g.setFont(f);
    int width = g.getFontMetrics().stringWidth(text + "  ");
    int height = g.getFontMetrics().getHeight();
    int x0 = x;
    int y0 = y;
    switch (hAlign) {
      case CENTER:
        x0 = x - width / 2;
        break;
      case LEFT:
        x0 = x - width;
        break;
    }
    switch (vAlign) {
      case CENTER:
        y0 = y - height / 2;
        break;
      case BOTTOM:
        y0 = y - height;
        break;
    }
    if (bgColor != null) {
      g.setColor(bgColor);
      g.fillRect(x0, y0, width, height);
    }
    if (borderColor != null) {
      g.setColor(borderColor);
      g.drawRect(x0, y0, width, height);
    }
    g.setColor(fgColor);
    g.drawString(" " + text + " ", x0, y0 + g.getFontMetrics().getHeight() - g.getFontMetrics().getDescent());
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    if (labelImage == null && label != null && label.length() > 0) {
      labelImage = createImage(obs);
    }
    piece.draw(g, x, y, obs, zoom);

    if (labelImage != null) {
      Point p = getLabelPosition();
      x += (int) (zoom * p.x);
      y += (int) (zoom * p.y);
      if (zoom != 1.0) {
        Image scaled = labelImage;
        scaled = GameModule.getGameModule().getDataArchive().getScaledImage(labelImage, zoom);
        g.drawImage(scaled, x, y, obs);
      }
      else {
        g.drawImage(labelImage, x, y, obs);
      }
    }
  }

  /**
   * Return the relative position of the upper-left corner of the label, for a piece at position (0,0)
   */
  private Point getLabelPosition() {
    int x = horizontalOffset;
    int y = verticalOffset;

    Rectangle selBnds = piece.getShape().getBounds();
    switch (verticalPos) {
      case 't':
        y += selBnds.y;
        break;
      case 'b':
        y += selBnds.y + selBnds.height;
    }
    switch (horizontalPos) {
      case 'l':
        x += selBnds.x;
        break;
      case 'r':
        x += selBnds.x + selBnds.width;
    }
    switch (verticalJust) {
      case 'b':
        y -= lbl.getHeight();
        break;
      case 'c':
        y -= lbl.getHeight() / 2;
    }
    switch (horizontalJust) {
      case 'c':
        x -= lbl.getWidth() / 2;
        break;
      case 'r':
        x -= lbl.getWidth();
    }
    return new Point(x, y);
  }

  public void setLabel(String s) {
    if (s == null) {
      s = "";
    }
    label = s;
    if (getMap() != null && label != null && label.length() > 0) {
      labelImage = createImage(getMap().getView());
    }
    else {
      labelImage = null;
    }
  }

  public void setBackground(Color textBg) {
    this.textBg = textBg;
  }

  public void setForeground(Color textFg) {
    this.textFg = textFg;
  }

  protected Image createImage(Component obs) {
    lbl.setText(label);
    lbl.setSize(lbl.getPreferredSize());
    Image im = obs.createImage(lbl.getWidth(), lbl.getHeight());
    Graphics g = im.getGraphics();
    g.setColor(textBg);
    g.fillRect(0, 0, lbl.getWidth(), lbl.getHeight());
    lbl.paint(g);
    return im;
  }

  public String getLabel() {
    return label;
  }

  public Rectangle boundingBox() {
    Rectangle r = piece.boundingBox();
    Rectangle r2 = piece.getShape().getBounds();
    Point p2 = getLabelPosition();
    Rectangle r3 = new Rectangle(p2, lbl.getSize());
    return r.union(r2).union(r3);
  }

  public Shape getShape() {
    return piece.getShape();
  }

  public KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      if (labelKey == 0) {
        commands = new KeyCommand[0];
      }
      else {
        commands = new KeyCommand[1];
        commands[0] = new KeyCommand(menuCommand,
                                     KeyStroke.getKeyStroke(labelKey, InputEvent.CTRL_MASK),
                                     Decorator.getOutermost(this));
      }
    }
    return commands;
  }

  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    Command c = null;
    if (commands.length > 0
        && commands[0].matches(stroke)) {
      ChangeTracker tracker = new ChangeTracker(this);
      String s = (String) JOptionPane.showInputDialog
          (getMap() == null ? null : getMap().getView(),
           commands[0].getName(),
           null,
           JOptionPane.QUESTION_MESSAGE,
           null,
           null,
           label);
      if (s == null) {
        tracker = null;
      }
      else {
        setLabel(s);
        c = tracker.getChangeCommand();
      }
    }
    return c;
  }

  public String getDescription() {
    return "Text Label";
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "Label.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  private static class Ed implements PieceEditor {
    private KeySpecifier labelKeyInput;
    private JPanel controls = new JPanel();
    private StringConfigurer command;
    private StringConfigurer initialValue;
    private ColorConfigurer fg,bg;
    private JComboBox hPos,vPos,hJust,vJust;
    private IntConfigurer hOff,vOff,font;
    private ListCellRenderer renderer;

    public Ed(Labeler l) {
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      initialValue = new StringConfigurer(null, "Text", l.label);
      controls.add(initialValue.getControls());

      command = new StringConfigurer(null, "Menu Command", l.menuCommand);
      controls.add(command.getControls());

      Box b = Box.createHorizontalBox();
      labelKeyInput = new KeySpecifier(l.labelKey);
      b.add(new JLabel("Menu key command: "));
      b.add(labelKeyInput);
      controls.add(b);

      font = new IntConfigurer(null, "Font size", new Integer(l.font.getSize()));
      controls.add(font.getControls());

      fg = new ColorConfigurer(null, "Text Color", l.textFg);
      controls.add(fg.getControls());

      bg = new ColorConfigurer(null, "Background Color", l.textBg);
      controls.add(bg.getControls());

      renderer = new MyRenderer();

      Character[] rightLeft = new Character[]{new Character('c'),
                                              new Character('r'),
                                              new Character('l')};

      Character[] topBottom = new Character[]{new Character('c'),
                                              new Character('t'),
                                              new Character('b')};

      b = Box.createHorizontalBox();
      b.add(new JLabel("Vertical position:  "));
      vPos = new JComboBox(topBottom);
      vPos.setRenderer(renderer);
      vPos.setSelectedItem(new Character(l.verticalPos));
      b.add(vPos);
      vOff = new IntConfigurer(null, "Offset", new Integer(l.verticalOffset));
      b.add(vOff.getControls());
      controls.add(b);

      b = Box.createHorizontalBox();
      b.add(new JLabel("Horizontal position:  "));
      hPos = new JComboBox(rightLeft);
      hPos.setRenderer(renderer);
      hPos.setSelectedItem(new Character(l.horizontalPos));
      b.add(hPos);
      hOff = new IntConfigurer(null, "Offset", new Integer(l.horizontalOffset));
      b.add(hOff.getControls());
      controls.add(b);

      b = Box.createHorizontalBox();
      b.add(new JLabel("Vertical text justification:  "));
      vJust = new JComboBox(topBottom);
      vJust.setRenderer(renderer);
      vJust.setSelectedItem(new Character(l.verticalJust));
      b.add(vJust);
      controls.add(b);

      b = Box.createHorizontalBox();
      b.add(new JLabel("Horizontal text justification:  "));
      hJust = new JComboBox(rightLeft);
      hJust.setRenderer(renderer);
      hJust.setSelectedItem(new Character(l.horizontalJust));
      b.add(hJust);
      controls.add(b);
    }

    public String getState() {
      return initialValue.getValueString();
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(labelKeyInput.getKey());
      se.append(command.getValueString());

      Integer i = (Integer) font.getValue();
      if (i == null
          || i.intValue() < 0) {
        i = new Integer(10);
      }
      se.append(i.toString());
      se.append(bg.getValueString());
      se.append(fg.getValueString());
      se.append(vPos.getSelectedItem().toString());
      i = (Integer) vOff.getValue();
      if (i == null) {
        i = new Integer(0);
      }
      se.append(i.toString());
      se.append(hPos.getSelectedItem().toString());
      i = (Integer) hOff.getValue();
      if (i == null) {
        i = new Integer(0);
      }
      se.append(i.toString());
      se.append(vJust.getSelectedItem().toString());
      se.append(hJust.getSelectedItem().toString());
      return ID + se.getValue();
    }

    public Component getControls() {
      return controls;
    }

    private static class MyRenderer extends DefaultListCellRenderer {
      public Component getListCellRendererComponent(JList list,
                                                    Object value,
                                                    int index,
                                                    boolean sel,
                                                    boolean focus) {
        super.getListCellRendererComponent(list, value, index, sel, focus);
        switch (((Character) value).charValue()) {
          case 't':
            setText("Top");
            break;
          case 'b':
            setText("Bottom");
            break;
          case 'c':
            setText("Center");
            break;
          case 'l':
            setText("Left");
            break;
          case 'r':
            setText("Right");
        }
        return this;
      }
    }
  }
}
