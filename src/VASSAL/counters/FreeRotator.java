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
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.Drawable;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.tools.RotateFilter;
import VASSAL.tools.SequenceEncoder;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.geom.AffineTransform;
import java.awt.image.FilteredImageSource;
import java.awt.image.ImageProducer;
import java.io.File;
import java.net.MalformedURLException;
import java.util.Hashtable;

/**
 * A Decorator that rotates a GamePiece to an arbitrary angle
 */
public class FreeRotator extends Decorator implements EditablePiece, MouseListener, MouseMotionListener, Drawable {
  public static final String ID = "rotate;";

  private KeyCommand setAngleCommand;
  private KeyCommand rotateCWCommand;
  private KeyCommand rotateCCWCommand;
  private KeyCommand[] commands;
  private char setAngleKey = 'R';
  private String setAngleText = "Rotate";
  private char rotateCWKey = ']';
  private String rotateCWText = "Rotate CW";
  private char rotateCCWKey = '[';
  private String rotateCCWText = "Rotate CCW";

  private double[] validAngles = new double[]{0.0};
  private int angleIndex = 0;

  private Hashtable images = new Hashtable();
  private Hashtable bounds = new Hashtable();
  private PieceImage unrotated;

  private double tempAngle;
  private boolean drawGhost;

  public FreeRotator() {
    this(ID + "6;];[", null);
  }

  public FreeRotator(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  public String getName() {
    return piece.getName();
  }

  public void setInner(GamePiece p) {
    unrotated = new PieceImage(p);
    super.setInner(p);
  }

  public Rectangle boundingBox() {
    if (getAngle() == 0.0) {
      return piece.boundingBox();
    }
    else if (Info.is2dEnabled()) {
      return AffineTransform.getRotateInstance(-Math.PI * getAngle() / 180.0).createTransformedShape(piece.boundingBox()).getBounds();
    }
    else {
      return getRotatedBounds();
    }
  }

  public double getAngle() {
    return validAngles[angleIndex];
  }

  public void setAngle(double angle) {
    validAngles[angleIndex] = angle;
  }

  public Rectangle getRotatedBounds() {
    Rectangle r = (Rectangle) bounds.get(new Double(getAngle()));
    if (r == null) {
      r = piece.boundingBox();
    }
    return r;
  }

  public Shape getShape() {
    if (getAngle() == 0.0) {
      return piece.getShape();
    }
    else if (Info.is2dEnabled()) {
      return AffineTransform.getRotateInstance(getAngleInRadians()).createTransformedShape(piece.getShape());
    }
    else {
      return getRotatedBounds();
    }
  }

  public double getAngleInRadians() {
    return -Math.PI * getAngle() / 180.0;
  }

  public void mySetType(String type) {
    type = type.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    validAngles = new double[Integer.parseInt(st.nextToken())];
    for (int i = 0; i < validAngles.length; ++i) {
      validAngles[i] = -i * (360.0 / validAngles.length);
    }
    if (validAngles.length == 1) {
      setAngleKey = st.nextChar('\0');
      if (st.hasMoreTokens()) {
        setAngleText = st.nextToken();
      }
    }
    else {
      String s = st.nextToken();
      rotateCWKey = s.length() > 0 ? s.charAt(0) : '\0';
      s = st.nextToken();
      rotateCCWKey = s.length() > 0 ? s.charAt(0) : '\0';
      if (st.hasMoreTokens()) {
        rotateCWText = st.nextToken();
        rotateCCWText = st.nextToken();
      }
    }
    commands = null;
  }

  public void draw(final Graphics g, final int x, final int y, final Component obs, final double zoom) {
    if (getAngle() == 0.0) {
      piece.draw(g, x, y, obs, zoom);
    }
    if (Info.is2dEnabled()) {
      Graphics2D g2d = (Graphics2D) g;
      AffineTransform oldT = g2d.getTransform();
      g2d.translate(x, y);
      g2d.transform(AffineTransform.getRotateInstance(getAngleInRadians()));
      g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR);
      piece.draw(g, 0, 0, obs, zoom);
      g2d.setClip(piece.getShape());
      g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
      piece.draw(g, 0, 0, obs, zoom);
      g2d.setClip(null);
      g2d.setTransform(oldT);
    }
    else {
      Image rotated = getRotatedImage(getAngle(), obs);
      Rectangle r = getRotatedBounds();
      g.drawImage(rotated,
                  x + (int) (zoom * r.x),
                  y + (int) (zoom * r.y),
                  (int) (zoom * r.width),
                  (int) (zoom * r.height),
                  obs);
    }
  }

  public void draw(Graphics g, Map map) {
    if (drawGhost
        && Info.is2dEnabled()
        && g instanceof Graphics2D) {
      Point p = map.componentCoordinates(getPosition());
      Graphics2D g2d = (Graphics2D) g;
      AffineTransform t = g2d.getTransform();
      g2d.transform(AffineTransform.getRotateInstance(-Math.PI * tempAngle / 180., p.x, p.y));
      g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5F));
      g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
      piece.draw(g, p.x, p.y, map.getView(), map.getZoom());
      g2d.setTransform(t);
    }
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append("" + validAngles.length);
    if (validAngles.length == 1) {
      se.append("" + setAngleKey);
      se.append(setAngleText);
    }
    else {
      se.append(rotateCWKey == 0 ? "" : "" + rotateCWKey)
          .append(rotateCCWKey == 0 ? "" : "" + rotateCCWKey)
          .append(rotateCWText)
          .append(rotateCCWText);
    }
    return ID + se.getValue();
  }

  public String myGetState() {
    if (validAngles.length == 1) {
      return "" + validAngles[0];
    }
    else {
      return "" + angleIndex;
    }
  }

  public void mySetState(String state) {
    if (validAngles.length == 1) {
      validAngles[0] = Double.valueOf(state).doubleValue();
    }
    else {
      angleIndex = Integer.parseInt(state);
    }
  }

  public KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      setAngleCommand = new KeyCommand
          (setAngleText,
           KeyStroke.getKeyStroke(setAngleKey,
                                  java.awt.event.InputEvent.CTRL_MASK),
           Decorator.getOutermost(this));
      rotateCWCommand = new KeyCommand
          (rotateCWText,
           KeyStroke.getKeyStroke(rotateCWKey,
                                  java.awt.event.InputEvent.CTRL_MASK),
           Decorator.getOutermost(this));

      rotateCCWCommand = new KeyCommand
          (rotateCCWText,
           KeyStroke.getKeyStroke(rotateCCWKey,
                                  java.awt.event.InputEvent.CTRL_MASK),
           Decorator.getOutermost(this));

      if (validAngles.length == 1) {
        if (setAngleText.length() > 0) {
          commands = new KeyCommand[]{setAngleCommand};
        }
        else {
          commands = new KeyCommand[0];
          setAngleCommand.setEnabled(false);
        }
        rotateCWCommand.setEnabled(false);
        rotateCCWCommand.setEnabled(false);
      }
      else {
        if (rotateCWText.length() > 0
            && rotateCCWText.length() > 0) {
          commands = new KeyCommand[]{rotateCWCommand, rotateCCWCommand};
        }
        else if (rotateCWText.length() > 0) {
          commands = new KeyCommand[]{rotateCWCommand};
          rotateCCWCommand.setEnabled(false);
        }
        else if (rotateCCWText.length() > 0) {
          commands = new KeyCommand[]{rotateCCWCommand};
          rotateCWCommand.setEnabled(false);
        }
        setAngleCommand.setEnabled(false);
      }
    }
    setAngleCommand.setEnabled(getMap() != null && validAngles.length == 1 && setAngleText.length() > 0);
    return commands;
  }

  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    Command c = null;
    if (setAngleCommand.matches(stroke)) {
      if (Info.is2dEnabled()) {
        getMap().pushMouseListener(this);
        getMap().addDrawComponent(this);
        getMap().getView().addMouseMotionListener(this);
        getMap().getView().setCursor(Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR));
      }
      else {
        ChangeTracker tracker = new ChangeTracker(this);
        String s = JOptionPane.showInputDialog(getMap().getView().getTopLevelAncestor(), setAngleText);
        if (s != null) {
          try {
            setAngle(-Double.valueOf(s).doubleValue());
            c = tracker.getChangeCommand();
          }
          catch (NumberFormatException ex) {
          }
        }
      }
    }
    else if (rotateCWCommand.matches(stroke)) {
      ChangeTracker tracker = new ChangeTracker(this);
      angleIndex = (angleIndex + 1) % validAngles.length;
      c = tracker.getChangeCommand();
    }
    else if (rotateCCWCommand.matches(stroke)) {
      ChangeTracker tracker = new ChangeTracker(this);
      angleIndex = (angleIndex - 1 + validAngles.length) % validAngles.length;
      c = tracker.getChangeCommand();
    }
    return c;
  }

  public void mouseClicked(MouseEvent e) {
  }

  public void mouseEntered(MouseEvent e) {
  }

  public void mouseExited(MouseEvent e) {
  }

  public void mousePressed(MouseEvent e) {
    drawGhost = true;
//    trans = new Transparent(this);
//    trans.setAlpha(0.5);
  }

  public void mouseReleased(MouseEvent e) {
    try {
      ChangeTracker tracker = new ChangeTracker(this);
      setAngle(tempAngle);
      GameModule.getGameModule().sendAndLog(tracker.getChangeCommand());
    }
    finally {
      getMap().getView().setCursor(null);
      getMap().removeDrawComponent(this);
      getMap().popMouseListener();
      getMap().getView().removeMouseMotionListener(this);
    }
  }

  public void mouseDragged(MouseEvent e) {
    if (drawGhost) {
      Point p = getMap().mapCoordinates(e.getPoint());
      Point p2 = getPosition();
      double myAngle;
      if (p.y == p2.y) {
        myAngle = p.x < p2.x ? Math.PI / 2 : -Math.PI / 2;
      }
      else {
        myAngle = Math.atan((float) (p.x - p2.x) / (float) (p2.y - p.y));
        if (p2.y < p.y) {
          myAngle += Math.PI;
        }
      }
      tempAngle = -180. * myAngle / Math.PI;
    }
    getMap().repaint();
  }

  public void mouseMoved(MouseEvent e) {
  }

  public Image getRotatedImage(double angle, Component obs) {
    if (unrotated.isChanged()) {
      images.clear();
      bounds.clear();
    }
    Image rotated = (Image) images.get(new Double(angle));
    if (rotated == null) {
      RotateFilter filter = new RotateFilter(angle);
      Rectangle rotatedBounds = piece.boundingBox();
      filter.transformSpace(rotatedBounds);
      ImageProducer producer = new FilteredImageSource
          (unrotated.getImage(obs).getSource(), filter);
      rotated = obs.createImage(producer);
      images.put(new Double(angle), rotated);
      bounds.put(new Double(angle), rotatedBounds);
    }
    return rotated;
  }

  public String getDescription() {
    return "Can Rotate";
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    File dir = new File("docs");
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "Rotate.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  private static class Ed implements PieceEditor,
      java.beans.PropertyChangeListener {
    private BooleanConfigurer anyConfig;
    private KeySpecifier anyKeyConfig;
    private IntConfigurer facingsConfig;
    private KeySpecifier cwKeyConfig;
    private KeySpecifier ccwKeyConfig;
    private JTextField anyCommand;
    private JTextField cwCommand;
    private JTextField ccwCommand;
    private Box anyControls;
    private Box cwControls;
    private Box ccwControls;

    private JPanel panel;

    public Ed(FreeRotator p) {
      cwKeyConfig = new KeySpecifier(p.rotateCWKey);
      ccwKeyConfig = new KeySpecifier(p.rotateCCWKey);
      anyConfig = new BooleanConfigurer
          (null, "Allow arbitrary rotations",
           new Boolean(p.validAngles.length == 1));
      anyKeyConfig = new KeySpecifier(p.setAngleKey);
      facingsConfig = new IntConfigurer
          (null, "Number of allowed facings :",
           new Integer(p.validAngles.length == 1 ? 6 : p.validAngles.length));

      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

      panel.add(facingsConfig.getControls());
      cwControls = Box.createHorizontalBox();
      cwControls.add(new JLabel("Command to rotate clockwise:"));
      cwControls.add(cwKeyConfig);
      cwControls.add(new JLabel("Menu text:"));
      cwCommand = new JTextField(12);
      cwCommand.setMaximumSize(cwCommand.getPreferredSize());
      cwCommand.setText(p.rotateCWText);
      cwControls.add(cwCommand);
      panel.add(cwControls);

      ccwControls = Box.createHorizontalBox();
      ccwControls.add(new JLabel("Command to rotate counterclockwise:"));
      ccwControls.add(ccwKeyConfig);
      ccwControls.add(new JLabel("Menu text:"));
      ccwCommand = new JTextField(12);
      ccwCommand.setMaximumSize(ccwCommand.getPreferredSize());
      ccwCommand.setText(p.rotateCCWText);
      ccwControls.add(ccwCommand);
      panel.add(ccwControls);

      panel.add(anyConfig.getControls());
      anyControls = Box.createHorizontalBox();
      anyControls.add(new JLabel("Command to rotate :"));
      anyControls.add(anyKeyConfig);
      anyControls.add(new JLabel("Menu text:"));
      anyCommand = new JTextField(12);
      anyCommand.setMaximumSize(anyCommand.getPreferredSize());
      anyCommand.setText(p.setAngleText);
      anyControls.add(anyCommand);
      panel.add(anyControls);

      anyConfig.addPropertyChangeListener(this);
      propertyChange(null);
    }

    public void propertyChange(java.beans.PropertyChangeEvent evt) {
      boolean any = Boolean.TRUE.equals(anyConfig.getValue());
      anyControls.setVisible(any);
      facingsConfig.getControls().setVisible(!any);
      cwControls.setVisible(!any);
      ccwControls.setVisible(!any);
      panel.revalidate();
    }

    public Component getControls() {
      return panel;
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      if (Boolean.TRUE.equals(anyConfig.getValue())) {
        se.append("1");
        se.append(anyKeyConfig.getKey());
        se.append(anyCommand.getText() == null ?
                  "" : anyCommand.getText().trim());
      }
      else {
        se.append(facingsConfig.getValueString());
        se.append(cwKeyConfig.getKey());
        se.append(ccwKeyConfig.getKey());
        se.append(cwCommand.getText() == null ?
                  "" : cwCommand.getText().trim());
        se.append(ccwCommand.getText() == null ?
                  "" : ccwCommand.getText().trim());
      }
      return ID + se.getValue();
    }

    public String getState() {
      return "0";
    }
  }
}
