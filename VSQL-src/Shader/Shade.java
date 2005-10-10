/*
 * $Id$
 * 
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.TexturePaint;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Stack;

public class Shade extends AbstractConfigurable {

  public static final String NAME = "name";
  public static final String TYPE = "type";
  public static final String PATTERN = "pattern";
  public static final String COLOR = "color";
  public static final String IMAGE = "image";
  public static final String OPACITY = "opacity";

  public static final String RANGE_TYPE = "rangeType";
  public static final String RANGE_SOURCE = "rangeSource";
  public static final String RANGE = "range";
  public static final String MARKER = "marker";

  public static final String BG_TYPE = "Background";
  public static final String FG_TYPE = "Foreground";
  
  public static final String RANGE_MARKER = "Use Marker Value";
  public static final String RANGE_FIXED = "Fixed";
  public static final String RANGE_GRID = "Grid Elements";
  public static final String RANGE_PIXELS = "Pixels";

  public static final String TYPE_25_PERCENT = "25%";
  public static final String TYPE_50_PERCENT = "50%"; 
  public static final String TYPE_75_PERCENT = "75%"; 
  public static final String TYPE_SOLID = "100% (Solid)"; 
  public static final String TYPE_IMAGE = "Custom Image";

  protected String rangeType = RANGE_GRID;
  protected String rangeSource = RANGE_FIXED;
  protected int range = 3;
  protected String rangeMarker = "";
  protected int builtRange = 3;
  protected String imageName = "";
  protected Color color = Color.BLACK;
  protected String type = FG_TYPE;
  protected String pattern = TYPE_25_PERCENT;
  protected int opacity = 100;

  //protected ShadeableMap map;
  protected Area shape;
  protected BufferedImage shading = null;
  protected Rectangle shadeRect = new Rectangle();

  public Shade() {
    super();
  }

  public void draw(Graphics g, Map map) {
    
    buildShading();
    
    Area myShape = getShape(map);
    
    /* 
     * Translate, Zoom and draw the shade
     */
    Graphics2D g2 = (Graphics2D) g;
//    double z = map.getZoom();
//
//    AffineTransform zoom = AffineTransform.getScaleInstance(z, z);
//    Area theShape = new Area(myhape);
//    theShape.transform(zoom);
    
    //AffineTransform saveXform = g2.getTransform();
    g2.setPaint(new TexturePaint(shading, shadeRect));
    g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, opacity / 100.0f));
    g2.fill(myShape);
    //g2.setTransform(saveXform);
    
  }
  
  public Area getShape(Map map) {
    
    Area area = new Area();
    
    GamePiece pieces[] = map.getPieces();
    for (int i = 0; i < pieces.length; i++) {
      checkPiece(area, pieces[i], map);
    }
    
    return area;
  }

  protected void checkPiece(Area area, GamePiece piece, Map map) {
    if (piece instanceof Stack) {
      Stack s = (Stack) piece;
      for (int i = 0; i < s.getPieceCount(); i++) {
        checkPiece(area, s.getPieceAt(i), map);
      }
    }
//    else {
//      String val = (String) piece.getProperty(marker);
//      if (val != null && val.equals(markerValue)) {
//        area.add(getShadeShape(piece, map));
//      }
//    }
  }

  protected Area getShadeShape(GamePiece piece, Map map) {

    if (shape == null || getRange() != builtRange) {
      buildShape(piece.getPosition(), map);
    }
    /* 
     * Translate, Zoom and draw the shade
     */
    Point p = piece.getPosition();
    double z = map.getZoom();

    Area theShape = new Area(shape);
    theShape.transform(AffineTransform.getScaleInstance(z, z));
    theShape.transform(AffineTransform.getTranslateInstance(p.x * z, p.y * z));
    return theShape;
    
//    AffineTransform saveXform = g2.getTransform();
//    g2.translate(p.x * z, p.y * z);
//    g2.setPaint(new TexturePaint(shading, shadeRect));
//    g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, opacity / 100.0f));
//    g2.fill(theShape);
//    g2.setTransform(saveXform);
  }

  /*
   * Build the repeating rectangle used to generate the shade.
   */
  protected void buildShading() {
    if (shading != null) {
      return;
    }
    
    if (pattern.equals(TYPE_IMAGE)) {
      try {
        shading = (BufferedImage) GameModule.getGameModule().getDataArchive().getCachedImage(imageName);
      }
      catch (IOException ex) {
      }
    }
    else {
      shading = new BufferedImage(2, 2, BufferedImage.TYPE_4BYTE_ABGR);
      Graphics2D g2 = shading.createGraphics();
      g2.setColor(color);
      if (pattern.equals(TYPE_25_PERCENT)) {
        g2.drawLine(0, 0, 0, 0);
      }
      else if (pattern.equals(TYPE_50_PERCENT)) {
        g2.drawLine(0, 0, 0, 0);
        g2.drawLine(1, 1, 1, 1);
      }
      else if (pattern.equals(TYPE_75_PERCENT)) {
        g2.drawLine(0, 0, 1, 0);
        g2.drawLine(1, 1, 1, 1);
      }
      else if (pattern.equals(TYPE_SOLID)) {
        g2.drawLine(0, 0, 1, 0);
        g2.drawLine(0, 1, 1, 1);
      }
    }

    shadeRect = new Rectangle(0, 0, shading.getWidth(), shading.getHeight());
  }
  
  protected int getRange() {
    return range;
  }
  
  /*
   * Build the shape of the shade as an Area of the correct size centred on (0, 0)
   */
  protected void buildShape(Point p, Map map) {
    if (rangeType.equals(RANGE_PIXELS)) {
      shape = new Area(new Ellipse2D.Double(-range, -range, range*2, range*2));
    }
    else if (rangeType.equals(RANGE_GRID)) {
      shape = ((ShadeableMap) map).getGridRangeShape(p, range);
    }
    builtRange = getRange();
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Name:  ", "Type:  ", "Shade Pattern:  ", "Color:  ", "Image:  ", "Opacity(%):  ",
        "Range Type:  ", "Range Source:  ", "Range:  ", "Marker:  " };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, TypePrompt.class, PatternPrompt.class, Color.class, Image.class, Integer.class,
        RangeTypeConfig.class, RangeSourceConfig.class, Integer.class, String.class };
  }

  public static class TypePrompt extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { FG_TYPE, BG_TYPE };
    }
  }
  
  public static class PatternPrompt extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { TYPE_25_PERCENT, TYPE_50_PERCENT, TYPE_75_PERCENT, TYPE_SOLID, TYPE_IMAGE };
    }
  }

  public static class RangeTypeConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { RANGE_GRID, RANGE_PIXELS };
    }
  }
  
  public static class RangeSourceConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { RANGE_FIXED, RANGE_MARKER };
    }
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, TYPE, PATTERN, COLOR, IMAGE, OPACITY, RANGE_TYPE, RANGE_SOURCE, RANGE, MARKER };
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (TYPE.equals(key)) {
      type = (String) value;
    }
    else if (PATTERN.equals(key)) {
      pattern = (String) value;
    }
    else if (COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      color = (Color) value;
    }
    else if (IMAGE.equals(key)) {
      if (value instanceof File) {
        value = ((File) value).getName();
      }
      imageName = (String) value;
    }
    else if (OPACITY.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      opacity = ((Integer) value).intValue();
      if (opacity < 0 || opacity > 100) {
        opacity = 100;
      }
    }
    else if (RANGE_TYPE.equals(key)) {
      rangeType = (String) value;
    }
    else if (RANGE_SOURCE.equals(key)) {
      rangeSource = (String) value;
    }
    else if (RANGE.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      range = ((Integer) value).intValue();
    }
    else if (MARKER.equals(key)) {
      rangeMarker = (String) value;
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (TYPE.equals(key)) {
      return type + "";
    }
    else if (PATTERN.equals(key)) {
      return pattern + "";
    }
    else if (COLOR.equals(key)) {
      return ColorConfigurer.colorToString(color);
    }
    else if (IMAGE.equals(key)) {
      return imageName + "";
    }
    else if (OPACITY.equals(key)) {
      return opacity + "";
    }
    else if (RANGE_TYPE.equals(key)) {
      return rangeType;
    }
    else if (RANGE_SOURCE.equals(key)) {
      return rangeSource;
    }
    else if (RANGE.equals(key)) {
      return range + "";
    }
    else if (MARKER.equals(key)) {
      return rangeMarker;
    }
    else {
      return null;
    }
  }

  public VisibilityCondition getAttributeVisibility(String name) {

   if (COLOR.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return !pattern.equals(TYPE_IMAGE);
        }
      };
    }
   else if (IMAGE.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return pattern.equals(TYPE_IMAGE);
        }
      };
    }
    else if (RANGE.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return rangeSource.equals(RANGE_FIXED);
        }
      };
    }
    else if (MARKER.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return rangeSource.equals(RANGE_MARKER);
        }
      };
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  public static String getConfigureTypeName() {
    return "Shade";
  }

  public void removeFrom(Buildable parent) {
    ((MapShader) parent).removeShade(this);
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void addTo(Buildable parent) {
    ((MapShader) parent).addShade(this);
  }

}