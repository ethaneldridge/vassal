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
import VASSAL.command.Command;
import VASSAL.command.TrackPiece;
import VASSAL.configure.ColorConfigurer;
import VASSAL.tools.SequenceEncoder;

import javax.swing.*;
import java.awt.*;
import java.awt.event.InputEvent;
import java.io.File;
import java.net.MalformedURLException;

public class Hideable extends Decorator implements EditablePiece {

  public static final String ID = "hide;";
  public static final String HIDDEN_BY = "hiddenBy";

  private String hiddenBy;
  private char hideKey;
  private String command = "Invisible";

  private Transparent trans;
  private Color bgColor;

  private KeyCommand[] commands;

  public void setProperty(Object key, Object val) {
    if (HIDDEN_BY.equals(key)) {
      hiddenBy = (String) val;
    }
    else {
      super.setProperty(key, val);
    }
  }

  public Object getProperty(Object key) {
    if (HIDDEN_BY.equals(key)) {
      return hiddenBy;
    }
    else if (Properties.INVISIBLE_TO_ME.equals(key)) {
      return new Boolean(invisibleToMe());
    }
    else if (Properties.INVISIBLE_TO_OTHERS.equals(key)) {
      return new Boolean(invisibleToOthers());
    }
    else {
      return super.getProperty(key);
    }
  }


  public Hideable() {
    this(ID + "I", null);
  }

  public Hideable(String type, GamePiece p) {
    setInner(p);
    mySetType(type);
  }

  public void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    hideKey = st.nextToken().toUpperCase().charAt(0);
    if (st.hasMoreTokens()) {
      command = st.nextToken();
    }
    if (st.hasMoreTokens()) {
      bgColor = ColorConfigurer.stringToColor(st.nextToken());
    }
    commands = null;
  }

  public void setInner(GamePiece p) {
    super.setInner(p);
    if (trans == null) {
      trans = new Transparent(p);
      trans.setAlpha(0.3);
    }
    else {
      trans.setPiece(p);
    }
  }

  public void mySetState(String in) {
    hiddenBy = "null".equals(in) ? null : in;
  }

  public String myGetType() {
    return command == null ?
      ID + hideKey
      : ID + hideKey + ';' + command
      + (bgColor == null ? ";" : ';' + ColorConfigurer.colorToString(bgColor));
  }

  public String myGetState() {
    return hiddenBy == null ? "null" : hiddenBy;
  }

  private boolean invisibleToMe() {
    return hiddenBy != null
      && !hiddenBy.equals(GameModule.getUserId());
  }

  private boolean invisibleToOthers() {
    return hiddenBy != null
      && hiddenBy.equals(GameModule.getUserId());
  }

  public Rectangle selectionBounds() {
    if (invisibleToMe()) {
      return new Rectangle(getPosition().x, getPosition().y, 0, 0);
    }
    else {
      return getInner().selectionBounds();
    }
  }

  public Rectangle boundingBox() {
    if (invisibleToMe()) {
      return new Rectangle(getPosition().x, getPosition().y, 0, 0);
    }
    else {
      return getInner().boundingBox();
    }
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    if (invisibleToMe()) {
      return;
    }
    else if (invisibleToOthers()) {
      if (bgColor != null) {
        Rectangle r = getInner().selectionBounds();
        r.translate(-getInner().getPosition().x, -getInner().getPosition().y);
        g.setColor(bgColor);
        g.fillRect(x + (int) (zoom * r.x),
                   y + (int) (zoom * r.y),
                   (int) (zoom * r.width),
                   (int) (zoom * r.height));
      }
      trans.draw(g, x, y, obs, zoom);
    }
    else {
      getInner().draw(g, x, y, obs, zoom);
    }
  }

  public String getName() {
    if (invisibleToMe()) {
      return "";
    }
    else if (invisibleToOthers()) {
      return getInner().getName() + "(" + command + ")";
    }
    else {
      return getInner().getName();
    }
  }

  public KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      commands = new KeyCommand[1];
      commands[0] = new KeyCommand(command,
                                   KeyStroke.getKeyStroke(hideKey, InputEvent.CTRL_MASK),
                                   Decorator.getOutermost(this));
    }
    return commands;
  }

  public Command myKeyEvent(KeyStroke stroke) {
    if (KeyStroke.getKeyStroke(hideKey, InputEvent.CTRL_MASK) == stroke) {
      TrackPiece c = new TrackPiece(this);
      if (invisibleToOthers()) {
        hiddenBy = null;
      }
      else if (!invisibleToMe()) {
        hiddenBy = GameModule.getUserId();
      }
      c.finalize();
      return c;
    }
    return null;
  }

  public String getDescription() {
    return "Can be Invisible";
  }

  public HelpFile getHelpFile() {
    File dir = new File("docs");
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "Hideable.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  private static class Ed implements PieceEditor {
    private KeySpecifier hideKeyInput;
    private JTextField hideCommandInput;
    private ColorConfigurer colorConfig;
    private JPanel controls;

    public Ed(Hideable p) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      Box b = Box.createHorizontalBox();
      hideKeyInput = new KeySpecifier('H');
      hideKeyInput.setKey(p.hideKey);
      b.add(new JLabel("Key command:  "));
      b.add(hideKeyInput);
      controls.add(b);

      b = Box.createHorizontalBox();
      hideCommandInput = new JTextField(16);
      hideCommandInput.setMaximumSize(hideCommandInput.getPreferredSize());
      hideCommandInput.setText(p.command);
      b.add(new JLabel("Menu Text:  "));
      b.add(hideCommandInput);
      controls.add(b);

      colorConfig = new ColorConfigurer(null, "Background color", p.bgColor);
      controls.add(colorConfig.getControls());
    }

    public String getState() {
      return "null";
    }

    public String getType() {
      return ID + hideKeyInput.getKey() + ";" + hideCommandInput.getText()
        + (colorConfig.getValue() == null ? "" : ";" + colorConfig.getValueString());
    }

    public Component getControls() {
      return controls;
    }
  }
}
