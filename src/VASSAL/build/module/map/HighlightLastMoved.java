/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney
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
package VASSAL.build.module.map;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.Map;
import VASSAL.counters.ColoredBorder;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Stack;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.SingleChildInstance;

import java.awt.*;
import java.awt.event.MouseListener;
import java.awt.event.MouseEvent;
import java.util.HashMap;
import java.io.File;
import java.net.MalformedURLException;

public class HighlightLastMoved extends AbstractConfigurable implements Drawable, MouseListener {
  public static final String COLOR = "color";
  public static final String THICKNESS = "thickness";

  protected ColoredBorder highlighter = new ColoredBorder();
  protected GamePiece lastMoved;
  protected static java.util.Map instances = new HashMap();

  public String[] getAttributeDescriptions() {
    return new String[]{"Color", "Thickness"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{Color.class, Integer.class};
  }

  public String[] getAttributeNames() {
    return new String[]{COLOR, THICKNESS};
  }

  public void setAttribute(String key, Object value) {
    if (COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      highlighter.setColor((Color) value);
    }
    else if (THICKNESS.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      highlighter.setThickness(((Integer) value).intValue());
    }
  }

  public String getAttributeValueString(String key) {
    if (COLOR.equals(key)) {
      return ColorConfigurer.colorToString(highlighter.getColor());
    }
    else if (THICKNESS.equals(key)) {
      return String.valueOf(highlighter.getThickness());
    }
    else {
      return null;
    }
  }

  public void addTo(Buildable parent) {
    Map map = (Map) parent;
    map.addDrawComponent(this);
    map.addLocalMouseListener(this);
    instances.put(map, this);
    validator = new SingleChildInstance(map, getClass());
  }

  public void removeFrom(Buildable parent) {
    Map map = (Map) parent;
    map.removeDrawComponent(this);
    map.removeLocalMouseListener(this);
    instances.remove(map);
  }

  public void draw(Graphics g, Map map) {
    if (lastMoved != null) {
      highlighter.draw(lastMoved, g, lastMoved.getPosition().x, lastMoved.getPosition().y, map.getView(), map.getZoom());
    }
  }

  public static void setLastMoved(GamePiece p, Map m) {
    HighlightLastMoved h = (HighlightLastMoved) instances.get(m);
    if (h != null) {
      h.setLastMoved(p);
    }
  }

  public void setLastMoved(GamePiece p) {
    if (p.getParent() instanceof Stack) {
      lastMoved = p.getParent();
    }
    else {
      lastMoved = p;
    }
  }

  public boolean drawAboveCounters() {
    return true;
  }

  public void mouseClicked(MouseEvent e) {
  }

  public void mousePressed(MouseEvent e) {
  }

  public void mouseReleased(MouseEvent e) {
    lastMoved = null;
  }

  public void mouseEntered(MouseEvent e) {
  }

  public void mouseExited(MouseEvent e) {
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "Map.htm"),"#LastMoveHighlighter");
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public static String getConfigureTypeName() {
    return "Last Move Highlighter";
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }
}
