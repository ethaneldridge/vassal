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
import java.awt.Rectangle;
import java.awt.TexturePaint;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;

import javax.swing.KeyStroke;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.Drawable;
import VASSAL.command.Command;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.tools.LaunchButton;

public class Shader extends AbstractConfigurable implements Drawable, GameComponent {

  public static final String VERSION = "1.0";
  public static final String NAME = "name";
  public static final String HOT_KEY = "hotkey";
  public static final String ICON = "icon";
  public static final String BUTTON_TEXT = "buttonText";
  public static final String COLOR = "color";
  
  protected LaunchButton launch;
  protected BufferedImage shading;
  protected Rectangle shadeRect;
  protected Color color;
  protected boolean shadingVisible;
  protected Map map;
  
  public Shader() {
    
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
    shading = new BufferedImage(2, 2, BufferedImage.TYPE_4BYTE_ABGR);
    Graphics2D g2 = shading.createGraphics();
    g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5F));
    g2.setColor(color);
    g2.drawLine(0, 0, 0, 0);
    g2.drawLine(1, 1, 1, 1);
    shadeRect = new Rectangle(0,0,2,2);
  }
  
  public void reset() {
    shadingVisible = false;
  }

  protected void toggleShading() {
    shadingVisible = !shadingVisible;
    map.repaint();
  }
  
  public void draw(Graphics g, Map map) {
    if (shadingVisible) {
      Graphics2D g2 = (Graphics2D) g;
      g2.setPaint(new TexturePaint(shading, shadeRect));
      g2.fillRect(50, 50, 100, 100);
    }
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Name:  ", "Button text:  ", "Button Icon:  ", "Hotkey:  ", "Color:  " };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, String.class, IconConfig.class, KeyStroke.class, Color.class };
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, ((Shader) c).launch.getAttributeValueString(ICON));
    }
  }
  
  public String[] getAttributeNames() {
    return new String[] { NAME, BUTTON_TEXT, ICON, HOT_KEY, COLOR };
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
      launch.setToolTipText((String) value);
    }
    else if (COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      color = (Color) value;
      buildShading();
      map.repaint();
    }
    else {
      launch.setAttribute(key, value);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName() + "";
    }
    else if (COLOR.equals(key)) {
      return ColorConfigurer.colorToString(color);
    }
    else {
      return launch.getAttributeValueString(key);
    }

  }

  public static String getConfigureTypeName() {
    return "Shader v"+VERSION;
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
    return new Class[0];
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
