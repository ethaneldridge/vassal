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

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.tools.DataArchive;
import VASSAL.tools.SequenceEncoder;

import java.util.List;
import javax.swing.*;
import javax.swing.event.ListSelectionListener;
import java.awt.*;
import java.awt.event.*;
import java.awt.geom.Area;
import java.io.File;
import java.net.MalformedURLException;
import java.util.Enumeration;
import java.util.Vector;
import java.util.ArrayList;

/**
 * The "Layer" trait. Contains a list of images that the user may cycle through.
 * The current image is superimposed over the inner piece.  The entire layer may
 * be activated or deactivated.
 */
public class Embellishment extends Decorator implements EditablePiece {
  public static final String ID = "emb;";

  protected String activateKey = "";
  protected String upKey, downKey;
  protected String upCommand, downCommand, activateCommand;
  protected String resetCommand;
  protected int resetLevel;
  protected char resetKey;

  protected int value = -1;  // Index of the image to draw.  Negative if inactive
  protected String activationStatus = "";
  protected int nValues;
  protected int xOff, yOff;
  protected String imageName[];
  protected String commonName[];
  protected Rectangle size[];
  protected boolean drawUnderneathWhenSelected = false;

  protected KeyCommand[] commands;

  public Embellishment() {
    this(ID + "A;Activate;];Increase;[;Decrease;0;0;", null);
  }

  public Embellishment(String type, GamePiece d) {
    mySetType(type);
    setInner(d);
  }

  public boolean isActive() {
    return value > 0;
  }

  public void setActive(boolean val) {
    value = val ? Math.abs(value) : -Math.abs(value);
  }

  public int getValue() {
    return Math.abs(value) - 1;
  }

  public void setValue(int val) {
    if (val >= nValues) {
      throw new IllegalArgumentException();
    }
    value = value > 0 ? val + 1 : -val - 1;
  }

  public void mySetType(String s) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ';');

    st.nextToken();
    SequenceEncoder.Decoder st2 = new SequenceEncoder.Decoder(st.nextToken(), ';');
    activateKey = st2.nextToken().toUpperCase();
    if (st2.hasMoreTokens()) {
      resetCommand = st2.nextToken();
      resetKey = st2.nextChar('\0');
      resetLevel = st2.nextInt(0);
    }
    else {
      resetKey = 0;
      resetCommand = "";
      resetLevel = 0;
    }

    activateCommand = st.nextToken();
    drawUnderneathWhenSelected = activateCommand.startsWith("_");
    if (drawUnderneathWhenSelected) {
      activateCommand = activateCommand.substring(1);
    }

    value = activateKey.length() > 0 ? -1 : 1;

    upKey = st.nextToken().toUpperCase();
    upCommand = st.nextToken();

    downKey = st.nextToken().toUpperCase();
    downCommand = st.nextToken();

    xOff = st.nextInt(0);
    yOff = st.nextInt(0);

    Vector v = new Vector();
    while (st.hasMoreTokens()) {
      v.addElement(st.nextToken());
    }
    nValues = v.size();
    imageName = new String[v.size()];
    commonName = new String[v.size()];
    size = new Rectangle[imageName.length];
    for (int i = 0; i < imageName.length; ++i) {
      String sub = (String) v.elementAt(i);
      SequenceEncoder.Decoder subSt = new SequenceEncoder.Decoder(sub, ',');
      imageName[i] = subSt.nextToken();
      if (subSt.hasMoreTokens()) {
        commonName[i] = subSt.nextToken();
      }
    }

    commands = null;
  }

  public String getName() {
    String name = null;
    if (value > 0 && commonName[value - 1] != null) {
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(commonName[value - 1], '+');
      String first = st.nextToken();
      if (st.hasMoreTokens()) {
        String second = st.nextToken();
        if (first.length() == 0) {
          name = piece.getName() + second;
        }
        else {
          name = first + piece.getName();
        }
      }
      else {
        name = first;
      }
    }
    else {
      name = piece.getName();
    }
    return name;
  }

  public void mySetState(String s) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ';');
    value = st.nextInt(1);
    activationStatus = st.nextToken(value < 0 ? "" : activateKey);
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(null, ';');
    SequenceEncoder se2 = new SequenceEncoder(activateKey, ';');
    se2.append(resetCommand).append(resetKey == 0 ? "" : String.valueOf(resetKey)).append(String.valueOf(resetLevel));
    se.append(se2.getValue())
        .append(drawUnderneathWhenSelected ? "_" + activateCommand : activateCommand)
        .append(upKey)
        .append(upCommand)
        .append(downKey)
        .append(downCommand)
        .append(xOff)
        .append(yOff);
    for (int i = 0; i < nValues; ++i) {
      if (commonName[i] != null) {
        SequenceEncoder sub = new SequenceEncoder(imageName[i], ',');
        se.append(sub.append(commonName[i]).getValue());
      }
      else {
        se.append(imageName[i]);
      }
    }
    return ID + se.getValue();
  }

  public String myGetState() {
    SequenceEncoder se = new SequenceEncoder(';');
    return se.append(String.valueOf(value)).append(activationStatus).getValue();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);

    if (value <= 0) {
      return;
    }
    try {
      Image im = getCurrentImage();
      if (im != null) {
        Rectangle r = getCurrentImageBounds();
        if (zoom == 1.0) {
          g.drawImage(im, x + r.x, y + r.y, obs);
        }
        else {
          Image scaled = GameModule.getGameModule().getDataArchive().getScaledImage(im, zoom);
          g.drawImage(scaled,
                      x + (int) (zoom * r.x),
                      y + (int) (zoom * r.y),
                      obs);
        }
      }
    }
    catch (java.io.IOException ex) {
    }
    if (drawUnderneathWhenSelected
        && Boolean.TRUE.equals(getProperty(Properties.SELECTED))) {
      piece.draw(g, x, y, obs, zoom);
    }
  }

  public KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      List l = new ArrayList();
      GamePiece outer = Decorator.getOutermost(this);
      if (activateKey.length() > 0) {
        l.add(new KeyCommand(activateCommand,
                             KeyStroke.getKeyStroke(activateKey.charAt(0), InputEvent.CTRL_MASK),
                             outer));
      }
      if (upCommand.length() > 0 && upKey.length() > 0 && nValues > 1) {
        l.add(new KeyCommand(upCommand,
                             KeyStroke.getKeyStroke(upKey.charAt(0), InputEvent.CTRL_MASK),
                             outer));
      }
      if (downCommand.length() > 0 && downKey.length() > 0 && nValues > 1) {
        l.add(new KeyCommand(downCommand,
                             KeyStroke.getKeyStroke(downKey.charAt(0), InputEvent.CTRL_MASK),
                             outer));
      }
      if (resetKey != '\0' && resetCommand.length() > 0) {
        l.add(new KeyCommand(resetCommand,
                             KeyStroke.getKeyStroke(resetKey, InputEvent.CTRL_MASK),
                             outer));
      }
      commands = (KeyCommand[]) l.toArray(new KeyCommand[l.size()]);
    }
    return commands;
  }

  public Command myKeyEvent(KeyStroke stroke) {
    char strokeChar = getMatchingActivationChar(stroke);
    ChangeTracker tracker = null;
    if (strokeChar != 0) {
      tracker = new ChangeTracker(this);
      int index = activationStatus.indexOf(strokeChar);
      if (index < 0) {
        activationStatus += strokeChar;
      }
      else {
        String before = activationStatus.substring(0, index);
        String after = activationStatus.substring(index + 1);
        activationStatus = before + after;
      }
      if (activationStatus.length() == activateKey.length()) {
        value = Math.abs(value);
      }
      else {
        value = -Math.abs(value);
      }
    }
    for (int i = 0; i < upKey.length(); ++i) {
      if (KeyStroke.getKeyStroke(upKey.charAt(i), InputEvent.CTRL_MASK).equals(stroke)) {
        if (tracker == null) {
          tracker = new ChangeTracker(this);
        }
        int val = Math.abs(value);
        if (++val > nValues)
          val = 1;
        value = value > 0 ? val : -val;
        break;
      }
    }
    for (int i = 0; i < downKey.length(); ++i) {
      if (KeyStroke.getKeyStroke(downKey.charAt(i), InputEvent.CTRL_MASK).equals(stroke)) {
        if (tracker == null) {
          tracker = new ChangeTracker(this);
        }
        int val = Math.abs(value);
        if (--val < 1)
          val = nValues;
        value = value > 0 ? val : -val;
        break;
      }
    }
    if (resetKey != '\0'
        && KeyStroke.getKeyStroke(resetKey, InputEvent.CTRL_MASK).equals(stroke)) {
      if (tracker == null) {
        tracker = new ChangeTracker(this);
      }
      setValue(resetLevel - 1);
    }
    return tracker != null ? tracker.getChangeCommand() : null;
  }

  private char getMatchingActivationChar(KeyStroke stroke) {
    for (int i = 0; i < activateKey.length(); ++i) {
      if (stroke.equals(KeyStroke.getKeyStroke(activateKey.charAt(i), InputEvent.CTRL_MASK))) {
        return activateKey.charAt(i);
      }
    }
    return (char) 0;
  }

  protected Image getCurrentImage() throws java.io.IOException {
    if (value > 0) {
      return GameModule.getGameModule() == null ? null
          : GameModule.getGameModule().getDataArchive()
          .getCachedImage(imageName[value - 1] + ".gif");
    }
    else {
      return null;
    }
  }

  public Rectangle boundingBox() {
    if (value > 0) {
      return getCurrentImageBounds().union(piece.boundingBox());
    }
    else {
      return piece.boundingBox();
    }
  }

  public Rectangle getCurrentImageBounds() {
    if (value > 0) {
      if (size[value - 1] == null) {
        try {
          Image im = getCurrentImage();
          if (im != null) {
            size[value - 1] = DataArchive.getImageBounds(im);
            size[value - 1].translate(xOff, yOff);
          }
          else {
            size[value - 1] = new Rectangle();
          }
        }
        catch (java.io.IOException e) {
          size[value - 1] = new Rectangle();
        }
      }
      return size[value - 1];
    }
    else {
      return new Rectangle();
    }
  }

  public Shape getShape() {
    if (value > 0) {
      if (Info.is2dEnabled()) {
        Area a = new Area(piece.getShape());
        a.add(new Area(getCurrentImageBounds()));
        return a;
      }
      else {
        return piece.getShape().getBounds().union(getCurrentImageBounds());
      }
    }
    else {
      return piece.getShape();
    }
  }

  public String getDescription() {
    if (imageName.length == 0
        || imageName[0] == null
        || imageName[0].length() == 0) {
      return "Layer";
    }
    else {
      return "Layer - " + imageName[0];
    }
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "Layer.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  /**
   * If the argument GamePiece contains a Layer whose "activate" command matches the given keystroke,
   * and whose active status matches the boolean argument, return that Layer
   */
  public static Embellishment getLayerWithMatchingActivateCommand(GamePiece piece, KeyStroke stroke, boolean active) {
    for (Embellishment layer = (Embellishment) Decorator.getDecorator(piece, Embellishment.class);
         layer != null;
         layer = (Embellishment) Decorator.getDecorator(layer.piece, Embellishment.class)
        ) {
      for (int i = 0; i < layer.activateKey.length(); ++i) {
        if (stroke.equals(KeyStroke.getKeyStroke(layer.activateKey.charAt(i), InputEvent.CTRL_MASK))) {
          if (active && layer.isActive()) {
            return layer;
          }
          else if (!active && !layer.isActive()) {
            return layer;
          }
          break;
        }
      }
    }
    return null;
  }

  private static class Ed implements PieceEditor {
    private MultiImagePicker images;
    private KeySpecifier activateKeyInput = new KeySpecifier('A');
    private KeySpecifier upKeyInput = new KeySpecifier(']');
    private KeySpecifier downKeyInput = new KeySpecifier('[');
    private JTextField activateCommand = new JTextField("Activate");
    private JTextField upCommand = new JTextField("Increase");
    private JTextField downCommand = new JTextField("Decrease");
    private JTextField xOffInput = new JTextField(2);
    private JTextField yOffInput = new JTextField(2);
    private JTextField levelNameInput = new JTextField(8);
    private JRadioButton prefix = new JRadioButton("is prefix");
    private JRadioButton suffix = new JRadioButton("is suffix");
    private JCheckBox alwaysActive = new JCheckBox("Always active");
    private JCheckBox drawUnderneath = new JCheckBox("Underneath when highlighted");
    private JTextField resetLevel = new JTextField(2);
    private JTextField resetCommand = new JTextField(8);
    private KeySpecifier resetKey = new KeySpecifier('R');

    private JPanel controls;
    private Vector names;
    private Vector isPrefix;
    private static final Integer NEITHER = new Integer(0);
    private static final Integer PREFIX = new Integer(1);
    private static final Integer SUFFIX = new Integer(2);

    public Ed(Embellishment e) {
      Box box;

      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.X_AXIS));

      JPanel p = new JPanel();
      p.setLayout(new GridLayout(4, 2));
      activateCommand.setMaximumSize(activateCommand.getPreferredSize());
      p.add(activateCommand);
      p.add(activateKeyInput);
      upCommand.setMaximumSize(upCommand.getPreferredSize());
      p.add(upCommand);
      p.add(upKeyInput);
      downCommand.setMaximumSize(downCommand.getPreferredSize());
      p.add(downCommand);
      p.add(downKeyInput);

      Box box2 = Box.createHorizontalBox();
      box2.add(new JLabel("Reset to level"));
      box2.add(resetLevel);
      box2.add(new JLabel("command"));
      box2.add(resetCommand);
      p.add(box2);
      p.add(resetKey);

      box = Box.createVerticalBox();
      alwaysActive.addItemListener
          (new ItemListener() {
            public void itemStateChanged(ItemEvent evt) {
              if (alwaysActive.isSelected()) {
                activateCommand.setText("");
                activateKeyInput.setKey("");
                activateCommand.setEnabled(false);
                activateKeyInput.setEnabled(false);
              }
              else {
                activateCommand.setText("Activate");
                activateKeyInput.setKey("A");
                activateCommand.setEnabled(true);
                activateKeyInput.setEnabled(true);
              }
            }
          });
      box.add(alwaysActive);
      box.add(drawUnderneath);
      box.add(p);

      box2 = Box.createHorizontalBox();
      xOffInput.setMaximumSize(xOffInput.getPreferredSize());
      xOffInput.setText("0");
      yOffInput.setMaximumSize(xOffInput.getPreferredSize());
      yOffInput.setText("0");
      box2.add(new JLabel("Offset: "));
      box2.add(xOffInput);
      box2.add(new JLabel(","));
      box2.add(yOffInput);
      box.add(box2);
      controls.add(box);

      images = new MultiImagePicker();
      controls.add(images);

      JPanel p2 = new JPanel();
      p2.setLayout(new GridLayout(2, 2));

      box = Box.createHorizontalBox();
      box.add(new JLabel("Level Name: "));
      levelNameInput.setMaximumSize(levelNameInput.getPreferredSize());
      levelNameInput.addKeyListener(new KeyAdapter() {
        public void keyReleased(KeyEvent evt) {
          changeLevelName();
        }
      });
      box.add(levelNameInput);
      p2.add(box);

      box = Box.createHorizontalBox();
      prefix.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          if (prefix.isSelected()) {
            suffix.setSelected(false);
          }
          changeLevelName();
        }
      }
      );
      suffix.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          if (suffix.isSelected()) {
            prefix.setSelected(false);
          }
          changeLevelName();
        }
      }
      );
      box.add(prefix);
      box.add(suffix);
      p2.add(box);

      JButton b = new JButton("Add Level");
      b.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          names.addElement(null);
          isPrefix.addElement(null);
          images.addEntry();
        }
      });
      p2.add(b);
      b = new JButton("Remove Level");
      b.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          int index = images.getList().getSelectedIndex();
          if (index >= 0) {
            names.removeElementAt(index);
            isPrefix.removeElementAt(index);
            images.removeEntryAt(index);
          }
        }
      });
      p2.add(b);

      controls.add(p2);

      images.getList().addListSelectionListener(new ListSelectionListener() {
        public void valueChanged
            (javax.swing.event.ListSelectionEvent evt) {
          updateLevelName();
        }
      });

      reset(e);
    }

    private void updateLevelName() {
      int index = images.getList().getSelectedIndex();
      if (index < 0) {
        levelNameInput.setText(null);
      }
      else {
        levelNameInput.setText((String) names.elementAt(index));
        prefix.setSelected(PREFIX.equals(isPrefix.elementAt(index)));
        suffix.setSelected(SUFFIX.equals(isPrefix.elementAt(index)));
      }
    }

    private void changeLevelName() {
      int index = images.getList().getSelectedIndex();
      if (index >= 0) {
        String s = levelNameInput.getText();
        names.setElementAt(s, index);
        if (prefix.isSelected()) {
          isPrefix.setElementAt(PREFIX, index);
        }
        else if (suffix.isSelected()) {
          isPrefix.setElementAt(SUFFIX, index);
        }
        else {
          isPrefix.setElementAt(NEITHER, index);
        }
      }
      else {
        names.setElementAt(null, index);
        isPrefix.setElementAt(NEITHER, index);
      }
    }

    public String getState() {
      return alwaysActive.isSelected() ? "1" : "-1";
    }

    public String getType() {
      SequenceEncoder imageList = new SequenceEncoder(';');
      int i = 0;
      for (Enumeration e = images.getImageNames();
           e.hasMoreElements();) {
        String imageName = (String) e.nextElement();
        String commonName = (String) names.elementAt(i);
        if (names.elementAt(i) != null
            && commonName != null
            && commonName.length() > 0) {
          SequenceEncoder sub = new SequenceEncoder(imageName, ',');
          if (PREFIX.equals(isPrefix.elementAt(i))) {
            commonName = new SequenceEncoder(commonName, '+').append("").getValue();
          }
          else if (SUFFIX.equals(isPrefix.elementAt(i))) {
            commonName = new SequenceEncoder("", '+').append(commonName).getValue();
          }
          else {
            commonName = new SequenceEncoder(commonName, '+').getValue();
          }
          imageList.append(sub.append(commonName).getValue());
        }
        else {
          imageList.append(imageName);
        }
        i++;
      }
      try {
        Integer.parseInt(xOffInput.getText());
      }
      catch (NumberFormatException xNAN) {
        xOffInput.setText("0");
      }
      try {
        Integer.parseInt(yOffInput.getText());
      }
      catch (NumberFormatException yNAN) {
        yOffInput.setText("0");
      }
      String command = activateCommand.getText();
      if (drawUnderneath.isSelected()) {
        command = "_" + command;
      }

      SequenceEncoder se2 = new SequenceEncoder(activateKeyInput.getKey(), ';');
      se2.append(resetCommand.getText()).append(resetKey.getKey()).append(resetLevel.getText());
      SequenceEncoder se = new SequenceEncoder(null, ';');
      se.append(se2.getValue())
          .append(command)
          .append(upKeyInput.getKey())
          .append(upCommand.getText())
          .append(downKeyInput.getKey())
          .append(downCommand.getText())
          .append(xOffInput.getText())
          .append(yOffInput.getText());

      String type = ID + se.getValue() + ';'
          + (imageList.getValue() == null ? "" : imageList.getValue());
      return type;
    }

    public Component getControls() {
      return controls;
    }

    public void reset(Embellishment e) {
      names = new Vector();
      isPrefix = new Vector();
      for (int i = 0; i < e.commonName.length; ++i) {
        String s = e.commonName[i];
        Integer is = NEITHER;
        if (s != null) {
          SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, '+');
          String first = st.nextToken();
          if (st.hasMoreTokens()) {
            String second = st.nextToken();
            if (first.length() == 0) {
              s = second;
              is = SUFFIX;
            }
            else {
              s = first;
              is = PREFIX;
            }
          }
          else {
            s = first;
          }
        }
        names.addElement(s);
        isPrefix.addElement(is);
      }

      alwaysActive.setSelected(e.activateKey.length() == 0);
      drawUnderneath.setSelected(e.drawUnderneathWhenSelected);

      images.clear();

      activateKeyInput.setKey(e.activateKey);
      activateCommand.setText(e.activateCommand);
      upKeyInput.setKey(e.upKey);
      upCommand.setText(e.upCommand);
      downKeyInput.setKey(e.downKey);
      downCommand.setText(e.downCommand);
      resetKey.setKey(e.resetKey != '\0' ? String.valueOf(e.resetKey) : "");
      resetCommand.setText(e.resetCommand);
      resetLevel.setText(String.valueOf(e.resetLevel));
      xOffInput.setText("" + e.xOff);
      yOffInput.setText("" + e.yOff);
      images.setImageList(e.imageName);

      updateLevelName();
    }

  }
}
