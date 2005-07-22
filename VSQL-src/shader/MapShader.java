/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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
 
package shader;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.TexturePaint;
import java.awt.event.ActionListener;
import java.awt.geom.Area;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.swing.KeyStroke;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.Drawable;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.command.Command;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.LaunchButton;

public class MapShader extends AbstractConfigurable implements Drawable, GameComponent {

  public static final String VERSION = "1.0";
  
  public static final String NAME = "name";
  public static final String HOT_KEY = "hotkey";
  public static final String ICON = "icon";
  public static final String BUTTON_TEXT = "buttonText";
  public static final String TYPE = "type";
  public static final String COLOR = "color";
  public static final String IMAGE = "image";
  public static final String OPACITY = "opacity";
  public static final String DEFAULT = "default";
  
  public static final String TYPE_STD = "Standard";
  public static final String TYPE_IMAGE = "Custom Image";
  public static final String ON = "On";
  public static final String OFF = "Off";
  
  protected LaunchButton launch;
  protected BufferedImage shading = null;
  protected Rectangle shadeRect;
  protected String imageName = "";
  protected Color color = Color.BLACK;
  protected String type = TYPE_STD;
  protected boolean shadingVisible;
  protected int opacity = 100;
  protected String defaultShading = ON;
  protected Map map;
  
  public MapShader() {
    
    ActionListener al = new ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent e) {
        toggleShading();
      }
    };
    launch = new LaunchButton("Shade", BUTTON_TEXT, HOT_KEY, ICON, al);
    launch.setToolTipText(getConfigureName());
    launch.setEnabled(false);
    
    buildShading();
    reset();
  }
  
  public void buildShading() {
    if (type.equals(TYPE_STD)) {
      shading = new BufferedImage(2, 2, BufferedImage.TYPE_4BYTE_ABGR);
      Graphics2D g2 = shading.createGraphics();
      g2.setColor(color);
      g2.drawLine(0, 0, 0, 0);
      //g2.drawLine(1, 1, 1, 1);
    }
    else {
      try {
        shading = (BufferedImage) GameModule.getGameModule().getDataArchive().getCachedImage(imageName);
      }
      catch (IOException ex) {
      }
    }
    shadeRect = new Rectangle(0,0,shading.getWidth(),shading.getHeight());   
    if (map != null) {
      map.repaint();
    }
  }
  
  public void reset() {
    shadingVisible = false;
  }

  protected void toggleShading() {
    shadingVisible = !shadingVisible;
    map.repaint();
  }
  
  public void draw(Graphics g, Map map) {
    if (shadingVisible && shading != null) {
      Graphics2D g2 = (Graphics2D) g;
      g2.setPaint(new TexturePaint(shading, shadeRect));
      g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, opacity / 100.0f));
      shadeHex(g2, 70, 70);
    }
  }
  
    protected void shadeHex(Graphics2D g2, int x, int y) {
    
    Point p = map.snapTo(new Point(x, y));
    Point p2 = map.snapTo(new Point(x+40, y+40));
    Board b = map.findBoard(p);
    MapGrid m = b.getGrid();
    ShadeableHexGrid h = (ShadeableHexGrid) m;
    Area hex = h.getHexShape(p.x, p.y, map.getZoom(), b.isReversed());
    Area hex2 = h.getHexShape(p2.x, p2.y, map.getZoom(), b.isReversed());
    hex.add(hex2);
    
    g2.fill(hex);
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Name:  ", "Button text:  ", "Button Icon:  ", "Hotkey:  ", 
        "Shading Type:  ", "Color:  ", "Image:  ", "Opacity(%):  ", "Default Shading:  " };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, String.class, IconConfig.class, KeyStroke.class, TypeConfig.class, Color.class, Image.class, Integer.class, DefaultConfig.class };
  }

  public static class TypeConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{TYPE_STD, TYPE_IMAGE};
    }
  }
  
  public static class DefaultConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ ON, OFF };
    }
  }
  
  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, ((MapShader) c).launch.getAttributeValueString(ICON));
    }
  }
  
  public String[] getAttributeNames() {
    return new String[] { NAME, BUTTON_TEXT, ICON, HOT_KEY, TYPE, COLOR, IMAGE, OPACITY, DEFAULT };
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
      launch.setToolTipText((String) value);
    }
    else if (TYPE.equals(key)) {
      type = (String) value;
    }
    else if (COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      color = (Color) value;
      buildShading();
    }
    else if (IMAGE.equals(key)) {
      if (value instanceof File) {
        value = ((File) value).getName();
      }
      imageName = (String) value;
      buildShading();
    }
    else if (OPACITY.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      opacity = ((Integer) value).intValue();
      if (opacity < 0 || opacity > 100) {
        opacity = 100;
      }
      buildShading();
    }
    else if (DEFAULT.equals(key)) {
      defaultShading = (String) value;
    }
    else {
      launch.setAttribute(key, value);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName() + "";
    }
    else if (TYPE.equals(key)) {
      return type + ""; 
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
    else if (DEFAULT.equals(key)) {
      return defaultShading + "";
    }
    else {
      return launch.getAttributeValueString(key);
    }

  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (IMAGE.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return type.equals(TYPE_IMAGE);
        }
      };
    }  
    else if (COLOR.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return type.equals(TYPE_STD);
        }
      };
    } 
    else {
      return super.getAttributeVisibility(name);
    }
  }
  
  public static String getConfigureTypeName() {
    return "MapShader v"+VERSION;
  }
  
  public void removeFrom(Buildable parent) {
    ((ShadeableMap) parent).removeShader(this);
    GameModule.getGameModule().getToolBar().remove(launch);
    GameModule.getGameModule().getGameState().removeGameComponent(this);

  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[] { Shader.class };
  }

  public void addTo(Buildable parent) {
    ((ShadeableMap) parent).addShader(this);
    GameModule.getGameModule().getToolBar().add(launch);
    launch.setAlignmentY(0.0F);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    map = (Map) parent;
  }
  
  public void setup(boolean gameStarting) {
    launch.setEnabled(gameStarting);
  }

  public Command getRestoreCommand() {
    return null;
  }

}
