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
import VASSAL.build.module.ObscurableOptions;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.ChangeTracker;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.tools.SequenceEncoder;

import javax.swing.*;
import java.awt.*;
import java.awt.event.InputEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.net.MalformedURLException;

public class Obscurable extends Decorator implements EditablePiece {
  public static final String ID = "obs;";
  private static boolean allHidden;
  protected static final char INSET = 'I';
  protected static final char BACKGROUND = 'B';
  protected static final char PEEK = 'P';
  protected static final char IMAGE = 'G';

  protected char obscureKey;
  protected char peekKey = 'P';
  protected String imageName;
  protected String obscuredToOthersImage;
  protected String obscuredBy;
  protected String hideCommand = "Mask";
  protected GamePiece obscuredToMeView;
  protected GamePiece obscuredToOthersView;
  protected boolean peeking;
  protected char displayStyle = INSET; // I = inset, B = background
  protected String maskName = "?";

  protected KeyCommand[] commands;

  public Obscurable() {
    this(ID + "M;", null);
  }

  public Obscurable(String type, GamePiece d) {
    mySetType(type);
    setInner(d);
  }

  public void mySetState(String in) {
    obscuredBy = "null".equals(in) ? null : in;
  }

  public void mySetType(String in) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(in, ';');
    st.nextToken();
    obscureKey = st.nextChar('\0');
    imageName = st.nextToken();
    obscuredToMeView = new BasicPiece(BasicPiece.ID + ";;" + imageName + ";;");
    if (st.hasMoreTokens()) {
      hideCommand = st.nextToken();
    }
    if (st.hasMoreTokens()) {
      String s = st.nextToken();
      displayStyle = s.charAt(0);
      switch (displayStyle) {
        case PEEK:
          if (s.length() > 1) {
            peekKey = s.charAt(1);
            peeking = false;
          }
          else {
            peekKey = 0;
            peeking = true;
          }
          break;
        case IMAGE:
          if (s.length() > 1) {
            obscuredToOthersImage = s.substring(1);
            obscuredToOthersView = new BasicPiece(BasicPiece.ID + ";;" + obscuredToOthersImage + ";;");
            obscuredToMeView.setPosition(new Point());
          }
      }
    }
    if (st.hasMoreTokens()) {
      maskName = st.nextToken();
    }
    commands = null;
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(obscureKey + "").append(imageName).append(hideCommand);
    switch (displayStyle) {
      case PEEK:
        se.append(peekKey == 0 ? displayStyle + "" : displayStyle + "" + peekKey);
        break;
      case IMAGE:
        se.append(displayStyle + obscuredToOthersImage);
        break;
      default:
        se.append("" + displayStyle);
    }
    se.append(maskName);
    return ID + se.getValue();
  }

  public String myGetState() {
    return obscuredBy == null ? "null" : obscuredBy;
  }

  public Rectangle boundingBox() {
    if (obscuredToMe()) {
      return bBoxObscuredToMe();
    }
    else if (obscuredToOthers()) {
      return bBoxObscuredToOthers();
    }
    else {
      return piece.boundingBox();
    }
  }

  public Shape getShape() {
    if (obscuredToMe()) {
      return bBoxObscuredToMe();
    }
    else if (obscuredToOthers()) {
      return bBoxObscuredToOthers();
    }
    else {
      return piece.getShape();
    }
  }

  public boolean obscuredToMe() {
    return obscuredBy != null
        && (allHidden || !obscuredBy.equals(GameModule.getUserId()));
  }

  public boolean obscuredToOthers() {
    return obscuredBy != null
        && (allHidden || obscuredBy.equals(GameModule.getUserId()));
  }

  public void setProperty(Object key, Object val) {
    if (ID.equals(key)) {
      if (val instanceof String
          || val == null) {
        obscuredBy = (String) val;
      }
    }
    else if (Properties.SELECTED.equals(key)) {
      if (!Boolean.TRUE.equals(val) && peekKey != 0) {
        peeking = false;
      }
      super.setProperty(key, val);
    }
    else {
      super.setProperty(key, val);
    }
  }

  public Object getProperty(Object key) {
    if (Properties.OBSCURED_TO_ME.equals(key)) {
      return new Boolean(obscuredToMe());
    }
    else if (Properties.OBSCURED_TO_OTHERS.equals(key)) {
      return new Boolean(obscuredToOthers());
    }
    else if (ID.equals(key)) {
      return obscuredBy;
    }
    else {
      return super.getProperty(key);
    }
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    if (obscuredToMe()) {
      drawObscuredToMe(g, x, y, obs, zoom);
    }
    else if (obscuredToOthers()) {
      drawObscuredToOthers(g, x, y, obs, zoom);
    }
    else {
      piece.draw(g, x, y, obs, zoom);
    }
  }

  protected void drawObscuredToMe(Graphics g, int x, int y, Component obs, double zoom) {
    obscuredToMeView.draw(g, x, y, obs, zoom);
  }

  protected void drawObscuredToOthers(Graphics g, int x, int y, Component obs, double zoom) {
    switch (displayStyle) {
      case BACKGROUND:
        obscuredToMeView.draw(g, x, y, obs, zoom);
        piece.draw(g, x, y, obs, zoom * .5);
        break;
      case INSET:
        piece.draw(g, x, y, obs, zoom);
        Rectangle bounds = piece.getShape().getBounds();
        Rectangle obsBounds = obscuredToMeView.getShape().getBounds();
        obscuredToMeView.draw(g, x - (int) (zoom * bounds.width / 2
                                            - .5 * zoom * obsBounds.width / 2),
                              y - (int) (zoom * bounds.height / 2
                                         - .5 * zoom * obsBounds.height / 2),
                              obs, zoom * 0.5);
        break;
      case PEEK:
        if (peeking && Boolean.TRUE.equals(getProperty(Properties.SELECTED))) {
          piece.draw(g, x, y, obs, zoom);
        }
        else {
          obscuredToMeView.draw(g, x, y, obs, zoom);
        }
        break;
      case IMAGE:
        piece.draw(g, x, y, obs, zoom);
        obscuredToOthersView.draw(g, x, y, obs, zoom);
    }
  }

  protected Rectangle bBoxObscuredToMe() {
    return obscuredToMeView.boundingBox();
  }

  protected Rectangle bBoxObscuredToOthers() {
    switch (displayStyle) {
      case BACKGROUND:
        return bBoxObscuredToMe();
      case IMAGE:
        return piece.boundingBox().union(obscuredToOthersView.boundingBox());
      default:
        return piece.boundingBox();
    }
  }

  public String getName() {
    String maskedName = maskName == null ? "?" : maskName;
    if (obscuredToMe()) {
      return maskedName;
    }
    else if (obscuredToOthers()) {
      return piece.getName() + "(" + maskedName + ")";
    }
    else {
      return piece.getName();
    }
  }

  public KeyCommand[] getKeyCommands() {
    if (obscuredToMe()) {
      KeyCommand myC[] = myGetKeyCommands();
      KeyCommand c[] = (KeyCommand[]) Decorator.getInnermost(this).getProperty(Properties.KEY_COMMANDS);
      if (c == null) {
        return myC;
      }
      else {
        KeyCommand all[] = new KeyCommand[c.length + myC.length];
        System.arraycopy(myC, 0, all, 0, myC.length);
        System.arraycopy(c, 0, all, myC.length, c.length);
        return all;
      }
    }
    else {
      return super.getKeyCommands();
    }
  }

  public KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      commands = new KeyCommand[2];
      commands[0] = new KeyCommand(hideCommand,
                                   KeyStroke.getKeyStroke(obscureKey, InputEvent.CTRL_MASK),
                                   Decorator.getOutermost(this));
      commands[1] = new KeyCommand("Peek",
                                   KeyStroke.getKeyStroke(peekKey, InputEvent.CTRL_MASK),
                                   Decorator.getOutermost(this));
    }
    commands[0].setEnabled(isMaskableBy(GameModule.getUserId()));
    if (obscuredToOthers()
        && displayStyle == PEEK
        && peekKey != 0) {
      return commands;
    }
    else {
      return new KeyCommand[]{commands[0]};
    }
  }

  /** Return true if this piece can be masked/un-masked by the player with the given id*/
  public boolean isMaskableBy(String id) {
    return obscuredBy == null
        || obscuredBy.equals(id)
        || ObscurableOptions.getInstance().isUnmaskable(obscuredBy);
  }

  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    if (commands[0].matches(stroke)) {
      ChangeTracker c = new ChangeTracker(this);
      if (obscuredToOthers()
          || obscuredToMe()) {
        obscuredBy = null;
      }
      else if (!obscuredToMe()) {
        obscuredBy = GameModule.getUserId();
      }
      return c.getChangeCommand();
    }
    else if (commands[1].matches(stroke)) {
      if (obscuredToOthers() && Boolean.TRUE.equals(getProperty(Properties.SELECTED))) {
        peeking = true;
      }
    }
    return null;
  }

  public String getDescription() {
    return "Mask";
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "Mask.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  /**
   * If true, then all masked pieces are considered masked to all players.
   * Used to temporarily draw pieces as they appear to other players
   * @param allHidden
   */
  public static void setAllHidden(boolean allHidden) {
    Obscurable.allHidden = allHidden;
  }

  private static class Ed implements PieceEditor {
    private ImagePicker picker;
    private KeySpecifier obscureKeyInput;
    private StringConfigurer obscureCommandInput, maskNameInput;
    private StringEnumConfigurer displayOption;
    private KeySpecifier peekKeyInput;
    private JPanel controls = new JPanel();
    private String[] optionNames = new String[]{"Background", "Plain", "Inset", "Use Image"};
    private char[] optionChars = new char[]{BACKGROUND, PEEK, INSET, IMAGE};
    private ImagePicker imagePicker;

    public Ed(Obscurable p) {
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      Box box = Box.createHorizontalBox();
      obscureCommandInput = new StringConfigurer(null, "Mask Command", p.hideCommand);
      box.add(obscureCommandInput.getControls());
      obscureKeyInput = new KeySpecifier(p.obscureKey);
      box.add(new JLabel("Key to hide: "));
      box.add(obscureKeyInput);
      controls.add(box);

      box = Box.createHorizontalBox();
      box.add(new JLabel("View when masked: "));
      picker = new ImagePicker();
      picker.setImageName(p.imageName);
      box.add(picker);
      controls.add(box);

      box = Box.createHorizontalBox();
      maskNameInput = new StringConfigurer(null, "Name when masked", p.maskName);
      box.add(maskNameInput.getControls());
      controls.add(box);

      box = Box.createHorizontalBox();
      displayOption = new StringEnumConfigurer(null, "Display style", optionNames);
      for (int i = 0; i < optionNames.length; ++i) {
        if (p.displayStyle == optionChars[i]) {
          displayOption.setValue(optionNames[i]);
          break;
        }
      }
      box.add(displayOption.getControls());

      final JPanel showDisplayOption = new JPanel() {
        public Dimension getMinimumSize() {
          return new Dimension(60, 60);
        }

        public Dimension getPreferredSize() {
          return new Dimension(60, 60);
        }

        public void paint(Graphics g) {
          g.clearRect(0, 0, getWidth(), getHeight());
          switch (displayOption.getValueString().charAt(0)) {
            case BACKGROUND:
              g.setColor(Color.black);
              g.fillRect(0, 0, 60, 60);
              g.setColor(Color.white);
              g.fillRect(15, 15, 30, 30);
              break;
            case INSET:
              g.setColor(Color.white);
              g.fillRect(0, 0, 60, 60);
              g.setColor(Color.black);
              g.fillRect(0, 0, 30, 30);
              break;
            case PEEK:
              g.setColor(Color.black);
              g.fillRect(0, 0, 60, 60);
              break;
          }
        }
      };

      box.add(showDisplayOption);
      controls.add(box);

      final Box peekBox = Box.createHorizontalBox();
      peekBox.add(new JLabel("Peek Command"));
      peekKeyInput = new KeySpecifier(p.peekKey);
      peekBox.setVisible(p.displayStyle == PEEK);
      peekBox.add(peekKeyInput);
      controls.add(peekBox);

      imagePicker = new ImagePicker();
      imagePicker.setImageName(p.obscuredToOthersImage);
      imagePicker.setVisible(p.displayStyle == IMAGE);
      controls.add(imagePicker);

      displayOption.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          showDisplayOption.repaint();
          peekBox.setVisible(optionNames[1].equals(evt.getNewValue()));
          imagePicker.setVisible(optionNames[3].equals(evt.getNewValue()));
          Window w = (Window) SwingUtilities.getAncestorOfClass(Window.class, controls);
          if (w != null) {
            w.pack();
          }
        }
      });
    }

    public String getState() {
      return "null";
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append("" + obscureKeyInput.getKey())
          .append(picker.getImageName())
          .append(obscureCommandInput.getValueString());
      char optionChar = INSET;
      for (int i = 0; i < optionNames.length; ++i) {
        if (optionNames[i].equals(displayOption.getValueString())) {
          optionChar = optionChars[i];
          break;
        }
      }
      switch (optionChar) {
        case PEEK:
          se.append(optionChar + peekKeyInput.getKey());
          break;
        case IMAGE:
          se.append(optionChar + imagePicker.getImageName());
          break;
        default:
          se.append("" + optionChar);
      }
      se.append(maskNameInput.getValueString());
      return ID + se.getValue();
    }

    public Component getControls() {
      return controls;
    }
  }
}
