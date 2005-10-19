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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
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
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Stack;
import VASSAL.tools.LaunchButton;

public class MapShader extends AbstractConfigurable implements GameComponent {

  public static final String VERSION = "1.1";

  public static final String NAME = "name";
  public static final String ALWAYS_ON = "alwaysOn";
  public static final String HOT_KEY = "hotkey";
  public static final String ICON = "icon";
  public static final String BUTTON_TEXT = "buttonText";
  public static final String BOARDS = "boards";
  public static final String BOARD_LIST = "boardList";

  public static final String ALL_BOARDS = "Yes";
  public static final String EXC_BOARDS = "No, exclude Boards in list";
  public static final String INC_BOARDS = "No, only shade Boards in List";

  protected LaunchButton launch;
  protected JPopupMenu popup = null;  
  protected boolean alwaysOn = false;
  protected String boardSelection = ALL_BOARDS;
  protected String[] boardList = new String[0];
  protected boolean shadingVisible;
  protected Map map;

  protected ArrayList shade = new ArrayList();
  protected Area boardClip = null;

  public static class BoardPrompt extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { ALL_BOARDS, EXC_BOARDS, INC_BOARDS };
    }
  }

  public MapShader() {

    ActionListener al = new ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent e) {
        apply();
      }
    };
    launch = new LaunchButton("Shade", BUTTON_TEXT, HOT_KEY, ICON, al);
    launch.setEnabled(false);
    setLaunchButtonVisibility();
    setConfigureName("MapShader");
    reset();
  }

  protected void apply() {
    buildPopup();
    if (popup == null) {
      toggleShading();
    }
    else {
      popup.show(launch, 0, 0);
    }
  }

  public void reset() {
    shadingVisible = isAlwaysOn();
  }

  protected void toggleShading() {
    setShadingVisibility(!shadingVisible);
  }
  
  public void setShadingVisibility(boolean b) { 
    shadingVisible = b;
    map.repaint();
  }

  public void draw(Graphics g, Rectangle visibleRect, Map m) {
    if (shadingVisible) {
      buildBoardClip();
      drawShader(g, m, visibleRect);
    }
  }
  
  protected boolean isAlwaysOn() {
    return alwaysOn;
  }
  
  protected Map getMap() {
    return map;
  }

  /**
   *  
   */
  protected void drawShader(Graphics g, Map m, Rectangle visibleRect) {

    Shape oldClip = g.getClip();
    Area theClip;

    if (boardClip == null) {
      theClip = new Area(visibleRect);
    }
    else {
      theClip = new Area(boardClip);
      theClip.transform(AffineTransform.getScaleInstance(m.getZoom(), m.getZoom()));
      theClip.intersect(new Area(visibleRect));
    }
    g.setClip(theClip);

    Iterator it = shade.iterator();
    while (it.hasNext()) {
      Shade s = (Shade) it.next();
      s.draw(g, m, visibleRect);
    }

    g.setClip(oldClip);

  }

  /**
   * Build a clipping region excluding boards that do no want to be Shaded.
   */
  protected void buildBoardClip() {

    if (boardClip == null) {
      boardClip = new Area();
      Enumeration e = map.getAllBoards();
      while (e.hasMoreElements()) {
        Board b = (Board) e.nextElement();
        String boardName = b.getName();
        boolean doShade = false;
        if (boardSelection.equals(ALL_BOARDS)) {
          doShade = true;
        }
        else if (boardSelection.equals(EXC_BOARDS)) {
          doShade = true;
          for (int i = 0; i < boardList.length && doShade; i++) {
            doShade = !boardList[i].equals(boardName);
          }
        }
        else if (boardSelection.equals(INC_BOARDS)) {
          for (int i = 0; i < boardList.length && !doShade; i++) {
            doShade = boardList[i].equals(boardName);
          }
        }
        if (doShade) {
          boardClip.add(new Area(b.bounds()));
        }
      }
    }
  }

  public void addShade(Shade s) {
    shade.add(s);
    setLaunchButtonVisibility();
  }

  public void removeShade(Shade s) {
    shade.remove(s);
    setLaunchButtonVisibility();
  }

  public Shade getShade(String shadeName) {
    Iterator it = shade.iterator();
    while (it.hasNext()) {
      Shade s = (Shade) it.next();
      if (s.getConfigureName().equals(shadeName)) {
        return s;
      }
    }
    return null;
  }
  /**
   * Mark specified Shade dirty, needing to be rebuilt.
   */
  public void dirtyShade(String shadeName) {
    Iterator it = shade.iterator();
    while (it.hasNext()) {
      Shade s = (Shade) it.next();
      if (shadeName == null || s.getConfigureName().equals(shadeName)) {
        s.setDirty(true);
      }
    }
  }
  
  public void dirtyAllShade() {
    dirtyShade((String) null);
  }
  
  /**
   * Mark dirty shade generated by given piece or all pieces in Stack
   */
  public void dirtyShade(GamePiece piece) {
    if (piece instanceof Stack) {
      for (int i = 0; i < ((Stack) piece).getPieceCount(); i++) {
        dirtyShade(((Stack) piece).getPieceAt(i));
      }
      return;
    }
    Iterator it = shade.iterator();
    while (it.hasNext()) {
      Shade shade = (Shade) it.next();
      String shadeName = shade.getConfigureName();
      if ("true".equals(piece.getProperty(shadeName + Shading.ACTIVE))) {
        shade.setDirty(true);
      }
    }
  }
  
  /**
   * The launch button is visible if
   * a) The Shader is not 'Always on' or
   * b) Any Shade component has a variable Default range (popup != null)
   */
  
  public void setLaunchButtonVisibility() {
    buildPopup();
    launch.setVisible(!isAlwaysOn() || popup != null);
  }

  /**
   * Build a popup menu if any of the Shade components has a variable
   * default range.
   */
  protected void buildPopup() {
    popup = null;
    Iterator it = shade.iterator();
    while (it.hasNext()) {
      Shade shade = (Shade) it.next();
      if (shade.isVariableDefaultRange()) {
        addItem(shade);
      }
    }
  }
  
  protected void addItem(Shade shade) {
    JMenuItem item;
    if (popup == null) {
      popup = new JPopupMenu();
      if (!isAlwaysOn()) {
        item = new JMenuItem("Show " + getConfigureName());
        item.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent arg0) {
            setShadingVisibility(true);            
          }});
        popup.add(item);
        item = new JMenuItem("Hide " + getConfigureName());
        item.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent arg0) {
            setShadingVisibility(false);            
          }});
        popup.add(item);
      }
    }
    item = new JMenuItem("Set " + shade.getDefaultRangePrompt());
    item.addActionListener(shade);
    popup.add(item);
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
    return new String[] { "Name:  ", "Shading Always On?", "Button text:  ", "Button Icon:  ", "Hotkey:  ",
        "All boards in map get Shaded?  ", "Board List:  " };

  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, Boolean.class, String.class, IconConfig.class, KeyStroke.class,
        BoardPrompt.class, String[].class };
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, ((MapShader) c).launch.getAttributeValueString(ICON));
    }
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, ALWAYS_ON, BUTTON_TEXT, ICON, HOT_KEY, BOARDS, BOARD_LIST };
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
      launch.setToolTipText((String) value);
    }
    else if (ALWAYS_ON.equals(key)) {
      if (value instanceof String) {
        value = new Boolean((String) value);
      }
      alwaysOn = ((Boolean) value).booleanValue();
      setLaunchButtonVisibility();
      reset();
    }
    else if (BOARDS.equals(key)) {
      boardSelection = (String) value;
    }
    else if (BOARD_LIST.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      boardList = (String[]) value;
    }
    else {
      launch.setAttribute(key, value);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName() + "";
    }
    else if (ALWAYS_ON.equals(key)) {
      return String.valueOf(isAlwaysOn());
    }
    else if (BOARDS.equals(key)) {
      return boardSelection + "";
    }
    else if (BOARD_LIST.equals(key)) {
      return StringArrayConfigurer.arrayToString(boardList);
    }
    else {
      return launch.getAttributeValueString(key);
    }

  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (ICON.equals(name) || HOT_KEY.equals(name) || BUTTON_TEXT.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return !isAlwaysOn();
        }
      };
    }
    else if (BOARD_LIST.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return !boardSelection.equals(ALL_BOARDS);
        }
      };
    }
    else {
      return super.getAttributeVisibility(name);
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
