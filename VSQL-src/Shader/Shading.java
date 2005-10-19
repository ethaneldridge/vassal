/*
 * $Id$
 * 
 * Copyright (c) 2000-2005 by Rodney Kinney, Brent Easton
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License (LGPL) as published by
 * the Free Software Foundation.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, copies are available at
 * http://www.opensource.org.
 */
package Shader;

import java.awt.Component;
import java.awt.Point;
import java.awt.Shape;
import java.awt.Window;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import VASSAL.build.module.Map;
import VASSAL.command.Command;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.counters.Decorator;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyCommand;
import VASSAL.counters.PieceEditor;
import VASSAL.counters.PieceFilter;
import VASSAL.counters.PropertiesPieceFilter;
import VASSAL.tools.PlayerIdFormattedString;
import VASSAL.tools.SequenceEncoder;

/**
 * A Decorator that marks a GamePiece as a Shade Generator. Specifies the
 * type of Shade, the range and how to activate the Shade.
 * @author Brent Easton
 *
 */
public class Shading extends Decorator implements EditablePiece {

  public static final String ID = "shade;";
  public static final String SHADE_TYPE = "shadeType";
  public static final String SHADE_SHAPE = "shadeShape";

  public static final String RANGE_MARKER = "Use Marker Value";
  public static final String RANGE_SHADE = "Use default range from Shade";
  public static final String RANGE_FIXED = "Fixed";
  public static final String RANGE_GRID = "Grid Elements";
  public static final String RANGE_PIXELS = "Pixels";

  public static final String ALWAYS_ON = "Always On";
  public static final String BY_COMMAND = "By Command";
  public static final String BY_FILTER = "By Matching Properties";

  public static final String RANGE = "_Range";
  public static final String SHAPE = "_Shape";
  public static final String ACTIVE = "_Active";

  protected String shade = "";
  protected String activation;
  protected boolean activated = false;
  protected String enableCommand = "";
  protected String disableCommand = "";
  protected String toggleCommand = "";
  protected KeyStroke enableKey = null;
  protected KeyStroke disableKey = null;
  protected KeyStroke toggleKey = null;
  protected String propertyFilter = "";
  protected String rangeType = "";
  protected String rangeSource = "";
  protected int fixedRange = 0;
  protected String markerName = "";

  // Cache details of last built shade shape
  protected Area shape = null;
  protected Area transformedShape = null;
  protected double lastZoom = -1.0;
  protected int lastRange = -1;
  protected Map lastMap = null;
  protected Point lastPosition = null;

  protected KeyCommand[] commands;

  public Shading() {
    this(ID, null);
  }

  public Shading(String type, GamePiece p) {
    mySetType(type);
    setInner(p);
  }

  public void mySetType(String s) {
    s = s.substring(ID.length());
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
    shade = sd.nextToken("");
    activation = sd.nextToken(BY_COMMAND);
    activated = activation.equals(ALWAYS_ON);
    propertyFilter = sd.nextToken("");
    enableCommand = sd.nextToken("Enable");
    disableCommand = sd.nextToken("Disable");
    toggleCommand = sd.nextToken("Toggle");
    enableKey = sd.nextKeyStroke('E');
    disableKey = sd.nextKeyStroke('D');
    toggleKey = sd.nextKeyStroke('T');
    rangeType = sd.nextToken(RANGE_GRID);
    rangeSource = sd.nextToken(RANGE_FIXED);
    markerName = sd.nextToken("");
    fixedRange = sd.nextInt(0);
  }

  public void draw(java.awt.Graphics g, int x, int y, java.awt.Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  public String getName() {
    return piece.getName();
  }

  public java.awt.Rectangle boundingBox() {
    return piece.boundingBox();
  }

  public Shape getShape() {
    return piece.getShape();
  }

  /**
   * Respond to Property requests. Replace <name> with the configured
   * name of this Shade.
   *   <name>_Active - Return "true" if this Shade is currently showing.
   *   <name>_Shape - Return the shape of the Shade generated by this piece
   *   <name>_Range - Return the range of the Shade generated by this piece
   */
  public Object getProperty(Object key) {

    if (key.equals(shade + ACTIVE)) {
      return getActivated() ? "true" : "false";
    }
    else if (key.equals(shade + SHAPE)) {
      if (getActivated()) {
        return getShadeShape();
      }
      else {
        return null;
      }
    }
    else if (key.equals(shade + RANGE)) {
      return getRange() + "";
    }
    else {
      return super.getProperty(key);
    }
  }
  
  /**
   * Is this Shading visible?
   * @return Shading activation status
   */
  protected boolean getActivated() {
    if (activation.equals(BY_FILTER)) {
      GamePiece outer = Decorator.getOutermost(this);
      PieceFilter filter = PropertiesPieceFilter.parse(new PlayerIdFormattedString(propertyFilter).getText(outer));
      return filter.accept(outer);
    }
    else {
      return activated;
    }
  }
  
  /**
   * Counter has changed state in some way that affects the display of
   * shade, so tell the Shade object to rebuild itself.
   */
  protected void markShadeDirty() {
   Map m = getMap();
    if (m != null && m instanceof ShadeableMap) {
      ((ShadeableMap) m).markShadeDirty(shade);
    }
  }

  /**
   * Return the Shape of the this shading if activated
   */
  protected Object getShadeShape() {
    if (getActivated()) {

      Map map = getMap();
      int range = getRange();
      double z = map.getZoom();
      Point p = getPosition();

      if (map != null) {
        if (range != lastRange || p != lastPosition || z != lastZoom) {

          if (rangeType.equals(RANGE_PIXELS)) {
            shape = new Area(new Ellipse2D.Double(-range, -range, range * 2, range * 2));
          }
          else if (rangeType.equals(RANGE_GRID)) {
            shape = ((ShadeableMap) map).getGridRangeShape(p, range);
          }

          transformedShape = new Area(shape);
          transformedShape.transform(AffineTransform.getTranslateInstance(p.x * z, p.y * z));

          lastZoom = z;
          lastRange = range;
          lastMap = map;
          lastPosition = p;
        }
        return transformedShape;
      }
    }
    return null;
  }

  /**
   * Calculate the range of this Shading.
   * @return range
   */
  protected int getRange() {
    int range = 0;
    if (rangeSource.equals(RANGE_FIXED)) {
      range = fixedRange;
    }
    else if (rangeSource.equals(RANGE_MARKER)) {
      try {
        range = Integer.parseInt((String) Decorator.getOutermost(this).getProperty(markerName));
      }
      catch (Exception e) {

      }
    }
    else if (rangeSource.equals(RANGE_SHADE)) {
      Map map = getMap();
      if (map != null && map instanceof ShadeableMap) {
        Shade s = ((ShadeableMap) map).getShade(shade);
        if (s != null) {
          return s.getDefaultRange();
        }
      }
    }

    return range;
  }

  /**
   * If a setProperty call changed the Activation status of this unit,
   * force the Shade to redraw. 
   */
  public void setProperty(Object key, Object value) {
    boolean oldActivated = getActivated();
    super.setProperty(key, value);
    if (getActivated() != oldActivated) {
      markShadeDirty();
    }
  }

  public String myGetState() {
    return "";
  }

  public void mySetState(String state) {
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(',');
    se.append(shade).append(activation).append(propertyFilter).append(enableCommand).append(disableCommand).append(
        toggleCommand).append(enableKey).append(disableKey).append(toggleKey).append(rangeType).append(rangeSource)
        .append(markerName).append(fixedRange);
    return ID + se.getValue();
  }

  protected KeyCommand[] myGetKeyCommands() {
    if (commands == null && activation.equals(BY_COMMAND)) {
      List l = new ArrayList();
      GamePiece outer = Decorator.getOutermost(this);

      if (enableKey != null && enableCommand.length() > 0) {
        l.add(new KeyCommand(enableCommand, enableKey, outer));
      }

      if (disableKey != null && disableCommand.length() > 0) {
        l.add(new KeyCommand(disableCommand, disableKey, outer));
      }

      if (toggleKey != null && toggleCommand.length() > 0) {
        l.add(new KeyCommand(toggleCommand, toggleKey, outer));
      }

      commands = (KeyCommand[]) l.toArray(new KeyCommand[l.size()]);
    }
    if (commands == null) {
      commands = new KeyCommand[0];
    }
    return commands;
  }

  /**
   * If this, or any inner keyCommand changes the activation status,
   * (i.e. changing a property used by a BY_FILTER activation),
   * force the Shade to redraw.
   */
  public Command keyEvent(KeyStroke stroke) {
    boolean oldActivated = getActivated();
    Command c = super.keyEvent(stroke);
    if (getActivated() != oldActivated) {
      markShadeDirty();
    }
    return c;
  }
  
  public Command myKeyEvent(KeyStroke stroke) {
    if (activation.equals(BY_COMMAND)) {
      if (enableKey != null && enableKey.equals(stroke)) {
        setActivated(true);
      }
      else if (disableKey != null && disableKey.equals(stroke)) {
        setActivated(false);
      }
      else if (toggleKey != null && toggleKey.equals(stroke)) {
        setActivated(!activated);
      }
    }
    return null;
  }
  
  public void setActivated(boolean b) {
    if (b != activated) {
      activated = b;
    }
  }

  public String getDescription() {
    String s = "Shading";
    if (shade.length() > 0) {
      s += " - " + shade;
    }
    return s;
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return null;
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  protected static class Ed implements PieceEditor, PropertyChangeListener {
    protected StringConfigurer shadeType;
    protected StringEnumConfigurer activationType;
    protected StringConfigurer propertyFilter;
    protected StringConfigurer enableCommand;
    protected HotKeyConfigurer enableStroke;
    protected StringConfigurer disableCommand;
    protected HotKeyConfigurer disableStroke;
    protected StringConfigurer toggleCommand;
    protected HotKeyConfigurer toggleStroke;
    protected StringEnumConfigurer rangeType;
    protected StringEnumConfigurer rangeSource;
    protected IntConfigurer fixedRange;
    protected StringConfigurer markerName;
    protected Component fixedRangeControls;

    protected Box commandBox;
    protected Box filterBox;
    protected Box rangeBox;
    protected Box markerBox;

    protected JPanel panel;

    protected Ed(Shading m) {

      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

      shadeType = new StringConfigurer(null, "Shade Type Name:  ", m.shade);
      panel.add(shadeType.getControls());

      activationType = new StringEnumConfigurer(null, "Activation Method:  ", new String[] { ALWAYS_ON, BY_COMMAND,
          BY_FILTER });
      activationType.setValue(m.activation);
      activationType.addPropertyChangeListener(this);
      panel.add(activationType.getControls());

      commandBox = Box.createVerticalBox();
      Box box = Box.createHorizontalBox();
      enableCommand = new StringConfigurer(null, "Enable Command:  ", m.enableCommand);
      enableStroke = new HotKeyConfigurer(null, "  KeyStroke:  ", m.enableKey);
      box.add(enableCommand.getControls());
      box.add(enableStroke.getControls());
      commandBox.add(box);

      box = Box.createHorizontalBox();
      disableCommand = new StringConfigurer(null, "Disable Command:  ", m.disableCommand);
      disableStroke = new HotKeyConfigurer(null, "  KeyStroke:  ", m.disableKey);
      box.add(disableCommand.getControls());
      box.add(disableStroke.getControls());
      commandBox.add(box);

      box = Box.createHorizontalBox();
      toggleCommand = new StringConfigurer(null, "Toggle Command:  ", m.toggleCommand);
      toggleStroke = new HotKeyConfigurer(null, "  KeyStroke:  ", m.toggleKey);
      box.add(toggleCommand.getControls());
      box.add(toggleStroke.getControls());
      commandBox.add(box);

      panel.add(commandBox);

      filterBox = Box.createHorizontalBox();
      propertyFilter = new StringConfigurer(null, "Matching Properties:  ", m.propertyFilter);
      filterBox.add(propertyFilter.getControls());
      panel.add(filterBox);

      rangeType = new StringEnumConfigurer(null, "Range Type:  ", new String[] { RANGE_GRID, RANGE_PIXELS });
      rangeType.setValue(m.rangeType);
      panel.add(rangeType.getControls());

      rangeSource = new StringEnumConfigurer(null, "Range Source:  ", new String[] { RANGE_FIXED, RANGE_MARKER, RANGE_SHADE });
      rangeSource.setValue(m.rangeSource);
      rangeSource.addPropertyChangeListener(this);
      panel.add(rangeSource.getControls());

      rangeBox = Box.createHorizontalBox();
      fixedRange = new IntConfigurer(null, "Range:  ", new Integer(m.fixedRange));
      fixedRangeControls = fixedRange.getControls();
      rangeBox.add(fixedRange.getControls());
      panel.add(rangeBox);

      markerBox = Box.createHorizontalBox();
      markerName = new StringConfigurer(null, "Marker Name:  ", m.markerName);
      markerBox.add(markerName.getControls());
      panel.add(markerBox);

      adjustVisibility();

    }

    public void propertyChange(PropertyChangeEvent arg0) {
      adjustVisibility();
    }

    public void adjustVisibility() {
      commandBox.setVisible(activationType.getValueString().equals(BY_COMMAND));
      filterBox.setVisible(activationType.getValueString().equals(BY_FILTER));
      rangeBox.setVisible(rangeSource.getValueString().equals(RANGE_FIXED));
      markerBox.setVisible(rangeSource.getValueString().equals(RANGE_MARKER));
      Window w = SwingUtilities.getWindowAncestor(panel);
      if (w != null) {
        w.pack();
      }
    }

    public Component getControls() {
      return panel;
    }

    public String getState() {
      return "";
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(',');
      se.append(shadeType.getValueString()).append(activationType.getValueString()).append(
          propertyFilter.getValueString()).append(enableCommand.getValueString()).append(
          disableCommand.getValueString()).append(toggleCommand.getValueString()).append(
          (KeyStroke) enableStroke.getValue()).append((KeyStroke) disableStroke.getValue()).append(
          (KeyStroke) toggleStroke.getValue()).append(rangeType.getValueString()).append(rangeSource.getValueString())
          .append(markerName.getValueString()).append(fixedRange.getValueString());
      return ID + se.getValue();
    }

  }
}
