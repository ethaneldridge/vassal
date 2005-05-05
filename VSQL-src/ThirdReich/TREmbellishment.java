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
package ThirdReich;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.GridLayout;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.geom.Area;
import java.io.File;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Vector;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.event.ListSelectionListener;
import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.KeyModifiersConfigurer;
import VASSAL.counters.Decorator;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyCommand;
import VASSAL.counters.MultiImagePicker;
import VASSAL.counters.PieceEditor;
import VASSAL.counters.Properties;
import VASSAL.tools.DataArchive;
import VASSAL.tools.SequenceEncoder;

/**
 * The "Layer" trait. Contains a list of images that the user may cycle through.
 * The current image is superimposed over the inner piece. The entire layer may
 * be activated or deactivated.
 */
public class TREmbellishment extends Decorator implements EditablePiece {
  
  protected static final String NATIONALITY = "nationality";
  public static final String ID = "TRemb2;";
  protected String activateKey = "";
  protected String upKey, downKey;
  protected int activateModifiers, upModifiers, downModifiers;
  protected String upCommand, downCommand, activateCommand;
  protected String resetCommand;
  protected int resetLevel;
  protected boolean loopLevels;
  protected KeyStroke resetKey;
  protected int value = -1; // Index of the image to draw. Negative if inactive
  protected String activationStatus = "";
  protected int nValues;
  protected int xOff, yOff;
  protected String imageName[];
  protected String commonName[];
  protected Rectangle size[];
  protected boolean drawUnderneathWhenSelected = false;
  protected KeyCommand[] commands;

  public TREmbellishment() {
    this(ID + "Activate", null);
  }

  public TREmbellishment(String type, GamePiece d) {
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

  public Object getProperty(Object key) {
    if (NATIONALITY.equals(key)) {
      String name = "";
      if (value > 0) {
        name = commonName[value - 1].substring(1);
      }
      return name;
    }
    else {
      return super.getProperty(key);
    }
  }

  public void mySetType(String s) {
    s = s.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ';');
    activateCommand = st.nextToken("Activate");
    activateModifiers = st.nextInt(InputEvent.CTRL_MASK);
    activateKey = st.nextToken("A");
    upCommand = st.nextToken("Increase");
    upModifiers = st.nextInt(InputEvent.CTRL_MASK);
    upKey = st.nextToken("]");
    downCommand = st.nextToken("Decrease");
    downModifiers = st.nextInt(InputEvent.CTRL_MASK);
    downKey = st.nextToken("[");
    resetCommand = st.nextToken("Reset");
    resetKey = st.nextKeyStroke('R');
    resetLevel = st.nextInt(0);
    drawUnderneathWhenSelected = st.nextBoolean(false);
    xOff = st.nextInt(0);
    yOff = st.nextInt(0);
    imageName = st.nextStringArray(0);
    commonName = st.nextStringArray(imageName.length);
    loopLevels = st.nextBoolean(true);
    value = activateKey.length() > 0 ? -1 : 1;
    nValues = imageName.length;
    size = new Rectangle[imageName.length];
    commands = null;
  }

  public String getName() {
    String name = null;
    if (value > 0 && commonName[value - 1] != null && commonName[value - 1].length() > 0) {
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
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(activateCommand).append(activateModifiers).append(activateKey).append(upCommand).append(upModifiers)
        .append(upKey).append(downCommand).append(downModifiers).append(downKey).append(resetCommand).append(resetKey)
        .append(resetLevel).append(drawUnderneathWhenSelected).append(xOff).append(yOff).append(imageName).append(
            commonName).append(loopLevels);
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
          g.drawImage(scaled, x + (int) (zoom * r.x), y + (int) (zoom * r.y), obs);
        }
      }
    }
    catch (java.io.IOException ex) {
    }
    if (drawUnderneathWhenSelected && Boolean.TRUE.equals(getProperty(Properties.SELECTED))) {
      piece.draw(g, x, y, obs, zoom);
    }
  }

  public KeyCommand[] myGetKeyCommands() {
    KeyCommand[] c = new KeyCommand[commonName.length];
    GamePiece outer = Decorator.getOutermost(this);
    for (int i = 0; i < c.length; i++) {
      String command = commonName[i].substring(1);
      c[i] = new KeyCommand(command, KeyStroke.getKeyStroke(command.charAt(0), InputEvent.CTRL_MASK), outer);
    }
    return c;
  }

  public Command myKeyEvent(KeyStroke stroke) {
    ChangeTracker tracker = new ChangeTracker(this);
    for (int i = 0; i < commonName.length; i++) {
      if (stroke.equals(KeyStroke.getKeyStroke(commonName[i].charAt(1), InputEvent.CTRL_MASK))) {
        value = i + 1;
      }
    }
    return tracker.getChangeCommand();
  }

  protected Image getCurrentImage() throws java.io.IOException {
    if (value > 0) {
      return GameModule.getGameModule() == null ? null : GameModule.getGameModule().getDataArchive().getCachedImage(
          imageName[value - 1] + ".gif");
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
    if (imageName.length == 0 || imageName[0] == null || imageName[0].length() == 0) {
      return "3R4 Layer";
    }
    else {
      return "3R4 Layer - " + imageName[0];
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

  private static class Ed implements PieceEditor {
    private MultiImagePicker images;
    private JTextField activateKeyInput = new JTextField("A");
    private JTextField upKeyInput = new JTextField("]");
    private JTextField downKeyInput = new JTextField("[");
    private JTextField activateCommand = new JTextField("Activate");
    private KeyModifiersConfigurer activateModifiers = new KeyModifiersConfigurer(null, "key:  ");
    private JTextField upCommand = new JTextField("Increase");
    private KeyModifiersConfigurer upModifiers = new KeyModifiersConfigurer(null, "key:  ");
    private JTextField downCommand = new JTextField("Decrease");
    private KeyModifiersConfigurer downModifiers = new KeyModifiersConfigurer(null, "key:  ");
    private JTextField xOffInput = new JTextField(2);
    private JTextField yOffInput = new JTextField(2);
    private JTextField levelNameInput = new JTextField(8);
    private JRadioButton prefix = new JRadioButton("is prefix");
    private JRadioButton suffix = new JRadioButton("is suffix");
    private JCheckBox alwaysActive = new JCheckBox("Always active");
    private JCheckBox drawUnderneath = new JCheckBox("Underneath when highlighted");
    private JTextField resetLevel = new JTextField(2);
    private JTextField resetCommand = new JTextField(8);
    private JCheckBox loop = new JCheckBox("Loop through levels");
    private HotKeyConfigurer resetKey = new HotKeyConfigurer(null, "Keyboard:  ");
    private JPanel controls;
    private List names;
    private List isPrefix;
    private static final Integer NEITHER = new Integer(0);
    private static final Integer PREFIX = new Integer(1);
    private static final Integer SUFFIX = new Integer(2);

    public Ed(TREmbellishment e) {
      Box box;
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));
      JPanel p = new JPanel();
      p.setLayout(new GridLayout(4, 3));
      activateCommand.setMaximumSize(activateCommand.getPreferredSize());
      p.add(activateCommand);
      p.add(activateModifiers.getControls());
      p.add(activateKeyInput);
      upCommand.setMaximumSize(upCommand.getPreferredSize());
      p.add(upCommand);
      p.add(upModifiers.getControls());
      p.add(upKeyInput);
      downCommand.setMaximumSize(downCommand.getPreferredSize());
      p.add(downCommand);
      p.add(downModifiers.getControls());
      p.add(downKeyInput);
      Box resetControls = Box.createHorizontalBox();
      resetControls.add(new JLabel("Reset to level"));
      resetControls.add(resetLevel);
      p.add(resetControls);
      resetControls = Box.createHorizontalBox();
      resetControls.add(new JLabel("Command: "));
      resetControls.add(resetCommand);
      p.add(resetControls);
      p.add(resetKey.getControls());
      box = Box.createVerticalBox();
      alwaysActive.addItemListener(new ItemListener() {
        public void itemStateChanged(ItemEvent evt) {
          if (alwaysActive.isSelected()) {
            activateCommand.setText("");
            activateKeyInput.setText("");
            activateCommand.setEnabled(false);
            activateKeyInput.setEnabled(false);
          }
          else {
            activateCommand.setText("Activate");
            activateKeyInput.setText("A");
            activateCommand.setEnabled(true);
            activateKeyInput.setEnabled(true);
          }
        }
      });
      JPanel checkBoxes = new JPanel();
      checkBoxes.setLayout(new GridLayout(2, 2));
      checkBoxes.add(alwaysActive);
      checkBoxes.add(drawUnderneath);
      checkBoxes.add(loop);
      box.add(checkBoxes);
      box.add(p);
      Box offsetControls = Box.createHorizontalBox();
      xOffInput.setMaximumSize(xOffInput.getPreferredSize());
      xOffInput.setText("0");
      yOffInput.setMaximumSize(xOffInput.getPreferredSize());
      yOffInput.setText("0");
      offsetControls.add(new JLabel("Offset: "));
      offsetControls.add(xOffInput);
      offsetControls.add(new JLabel(","));
      offsetControls.add(yOffInput);
      checkBoxes.add(offsetControls);
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
      });
      suffix.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          if (suffix.isSelected()) {
            prefix.setSelected(false);
          }
          changeLevelName();
        }
      });
      box.add(prefix);
      box.add(suffix);
      p2.add(box);
      JButton b = new JButton("Add Level");
      b.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          names.add(null);
          isPrefix.add(null);
          images.addEntry();
        }
      });
      p2.add(b);
      b = new JButton("Remove Level");
      b.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          int index = images.getList().getSelectedIndex();
          if (index >= 0) {
            names.remove(index);
            isPrefix.remove(index);
            images.removeEntryAt(index);
          }
        }
      });
      p2.add(b);
      controls.add(p2);
      images.getList().addListSelectionListener(new ListSelectionListener() {
        public void valueChanged(javax.swing.event.ListSelectionEvent evt) {
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
        levelNameInput.setText((String) names.get(index));
        prefix.setSelected(PREFIX.equals(isPrefix.get(index)));
        suffix.setSelected(SUFFIX.equals(isPrefix.get(index)));
      }
    }

    private void changeLevelName() {
      int index = images.getList().getSelectedIndex();
      if (index >= 0) {
        String s = levelNameInput.getText();
        names.set(index, s);
        if (prefix.isSelected()) {
          isPrefix.set(index, PREFIX);
        }
        else if (suffix.isSelected()) {
          isPrefix.set(index, SUFFIX);
        }
        else {
          isPrefix.set(index, NEITHER);
        }
      }
      else {
        names.set(index, null);
        isPrefix.set(index, NEITHER);
      }
    }

    public String getState() {
      return alwaysActive.isSelected() ? "1" : "-1";
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      List imageNames = new ArrayList();
      List commonNames = new ArrayList();
      int i = 0;
      for (Enumeration e = images.getImageNames(); e.hasMoreElements();) {
        imageNames.add(e.nextElement());
        String commonName = (String) names.get(i);
        if (commonName != null && commonName.length() > 0) {
          if (PREFIX.equals(isPrefix.get(i))) {
            commonName = new SequenceEncoder(commonName, '+').append("").getValue();
          }
          else if (SUFFIX.equals(isPrefix.get(i))) {
            commonName = new SequenceEncoder("", '+').append(commonName).getValue();
          }
          else {
            commonName = new SequenceEncoder(commonName, '+').getValue();
          }
        }
        commonNames.add(commonName);
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
      se.append(activateCommand.getText()).append(activateModifiers.getValueString())
          .append(activateKeyInput.getText()).append(upCommand.getText()).append(upModifiers.getValueString()).append(
              upKeyInput.getText()).append(downCommand.getText()).append(downModifiers.getValueString()).append(
              downKeyInput.getText()).append(resetCommand.getText()).append((KeyStroke) resetKey.getValue()).append(
              resetLevel.getText()).append(drawUnderneath.isSelected()).append(xOffInput.getText()).append(
              yOffInput.getText()).append((String[]) imageNames.toArray(new String[imageNames.size()])).append(
              (String[]) commonNames.toArray(new String[commonNames.size()])).append(loop.isSelected());
      return ID + se.getValue();
    }

    public String oldgetType() {
      SequenceEncoder imageList = new SequenceEncoder(';');
      int i = 0;
      for (Enumeration e = images.getImageNames(); e.hasMoreElements();) {
        String imageName = (String) e.nextElement();
        String commonName = (String) names.get(i);
        if (names.get(i) != null && commonName != null && commonName.length() > 0) {
          SequenceEncoder sub = new SequenceEncoder(imageName, ',');
          if (PREFIX.equals(isPrefix.get(i))) {
            commonName = new SequenceEncoder(commonName, '+').append("").getValue();
          }
          else if (SUFFIX.equals(isPrefix.get(i))) {
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
      SequenceEncoder se2 = new SequenceEncoder(activateKeyInput.getText(), ';');
      se2.append(resetCommand.getText()).append((KeyStroke) resetKey.getValue()).append(resetLevel.getText());
      SequenceEncoder se = new SequenceEncoder(null, ';');
      se.append(se2.getValue()).append(command).append(upKeyInput.getText()).append(upCommand.getText()).append(
          downKeyInput.getText()).append(downCommand.getText()).append(xOffInput.getText()).append(yOffInput.getText());
      String type = ID + se.getValue() + ';' + (imageList.getValue() == null ? "" : imageList.getValue());
      return type;
    }

    public Component getControls() {
      return controls;
    }

    public void reset(TREmbellishment e) {
      names = new Vector();
      isPrefix = new Vector();
      for (int i = 0; i < e.commonName.length; ++i) {
        String s = e.commonName[i];
        Integer is = NEITHER;
        if (s != null && s.length() > 0) {
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
        names.add(s);
        isPrefix.add(is);
      }
      alwaysActive.setSelected(e.activateKey.length() == 0);
      drawUnderneath.setSelected(e.drawUnderneathWhenSelected);
      loop.setSelected(e.loopLevels);
      images.clear();
      activateKeyInput.setText(e.activateKey);
      activateCommand.setText(e.activateCommand);
      activateModifiers.setValue(new Integer(e.activateModifiers));
      upKeyInput.setText(e.upKey);
      upCommand.setText(e.upCommand);
      upModifiers.setValue(new Integer(e.upModifiers));
      downKeyInput.setText(e.downKey);
      downCommand.setText(e.downCommand);
      downModifiers.setValue(new Integer(e.downModifiers));
      resetKey.setValue(e.resetKey);
      resetCommand.setText(e.resetCommand);
      resetLevel.setText(String.valueOf(e.resetLevel));
      xOffInput.setText("" + e.xOff);
      yOffInput.setText("" + e.yOff);
      images.setImageList(e.imageName);
      /** Add at least one level if none defined */
      if (!images.getImageNames().hasMoreElements()) {
        names.add(null);
        isPrefix.add(null);
        images.addEntry();
      }
      updateLevelName();
    }
  }
}
