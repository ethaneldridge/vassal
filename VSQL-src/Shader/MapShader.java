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

import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionListener;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;

import javax.swing.KeyStroke;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.command.Command;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.tools.LaunchButton;

public class MapShader extends AbstractConfigurable implements GameComponent {

  public static final String VERSION = "1.0";

  public static final String NAME = "name";
  public static final String HOT_KEY = "hotkey";
  public static final String ICON = "icon";
  public static final String BUTTON_TEXT = "buttonText";

  protected LaunchButton launch;

  protected boolean shadingVisible;
  protected Map map;
  
  protected ArrayList shade = new ArrayList();
  protected Area clip = new Area();
  
  protected boolean dirty = true;

  public MapShader() {

    ActionListener al = new ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent e) {
        toggleShading();
      }
    };
    launch = new LaunchButton("Shade", BUTTON_TEXT, HOT_KEY, ICON, al);
    launch.setEnabled(false);
    setConfigureName("MapShader");
    reset();
  }

  public void reset() {
    shadingVisible = false;
  }

  protected void toggleShading() {
    shadingVisible = !shadingVisible;
    map.repaint();
  }
  
  public void draw(Graphics g, Rectangle visibleRect, Map m) {
    if (shadingVisible) {
      if (dirty) {
        buildShader();
      }
      drawShader(g, m, visibleRect);
    }
  }

  /**
   * 
   */
  protected void drawShader(Graphics g, Map m, Rectangle visibleRect) {
    
    Shape oldClip = g.getClip();
    Area theClip = new Area(clip);
    theClip.transform(AffineTransform.getScaleInstance(m.getZoom(), m.getZoom()));
    theClip.intersect(new Area(visibleRect));
    g.setClip(theClip);
    
    Iterator it = shade.iterator();
    while (it.hasNext()) {
      Shade s = (Shade) it.next();
      s.draw(g, m);
    }
    
    g.setClip(oldClip);
  }

  /**
   * 
   */
  protected void buildShader() {
      
      /**
       * Build a clipping region excluding boards that do no want to be Shaded.
       */
      Enumeration e = map.getAllBoards();
      while (e.hasMoreElements()) {
        Board b = (Board) e.nextElement();
        boolean doShade = true;
        if (doShade) {
          clip.add(new Area(b.bounds()));
        }
      }
      dirty = false;
    
  }

  public void addShade(Shade s) {
    shade.add(s);
  }
  
  public void removeShade(Shade s) {
    shade.remove(s);
  }
  
  /*
   * update() is called by the Map when a piece is added or moved on the map
   * to indicate that the shader needs to be rebuilt.
   */
  protected void update() {
    dirty = true;
  }
  
  /*
   * -----------------------------------------------------------------------
   * GameComponent Implementation
   * -----------------------------------------------------------------------
   */
  public void setup(boolean gameStarting) {
    launch.setEnabled(gameStarting);
  }

  public Command getRestoreCommand() {
    return null;
  }

  /*
   * -----------------------------------------------------------------------
   * AbstractConfigurable Implementation
   * -----------------------------------------------------------------------
   */
  public String[] getAttributeDescriptions() {
    return new String[] { "Name:  ", "Button text:  ", "Button Icon:  ", "Hotkey:  " };
    
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, String.class, IconConfig.class, KeyStroke.class};
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, ((MapShader) c).launch.getAttributeValueString(ICON));
    }
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, BUTTON_TEXT, ICON, HOT_KEY };
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
      launch.setToolTipText((String) value);
    }
    else {
      launch.setAttribute(key, value);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName() + "";
    }
    else {
      return launch.getAttributeValueString(key);
    }

  }

  public static String getConfigureTypeName() {
    return "MapShader v" + VERSION;
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
    return new Class[] { Shade.class };
  }

  public void addTo(Buildable parent) {
    ((ShadeableMap) parent).addShader(this);
    GameModule.getGameModule().getToolBar().add(launch);
    launch.setAlignmentY(0.0F);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    map = (Map) parent;
  }

}
