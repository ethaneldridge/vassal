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
/*
 * Created by IntelliJ IDEA.
 * User: rkinney
 * Date: Jun 11, 2002
 * Time: 9:30:01 PM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.build.module;

import VASSAL.build.*;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.StringArrayConfigurer;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyListener;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.KeyEvent;
import java.util.Vector;
import java.util.Enumeration;
import java.io.File;
import java.net.MalformedURLException;

/**
 * A Map that may be configured to be visible only a particular side.
 * If visible to all, the map will respond to key/mouse events
 * only from the player playing the assigned side
 */
public class PrivateMap extends Map {
  private String[] owners;
  private boolean visibleToAll;
  private Map surrogate;

  public static final String VISIBLE = "visible";
  public static final String SIDE = "side";
  public static final String USE_BOARDS = "useBoards";

  public String[] getAttributeNames() {
    String[] s1 = new String[]{SIDE, VISIBLE, USE_BOARDS};
    String[] s2 = super.getAttributeNames();
    String[] s = new String[s1.length + s2.length];
    System.arraycopy(s1, 0, s, 0, s1.length);
    System.arraycopy(s2, 0, s, s1.length, s2.length);
    return s;
  }

  public String[] getAttributeDescriptions() {
    String[] s1 = new String[]{"Belongs to side", "Visible to all players", "Use same boards as this map"};
    String[] s2 = super.getAttributeDescriptions();
    String[] s = new String[s1.length + s2.length];
    System.arraycopy(s1, 0, s, 0, s1.length);
    System.arraycopy(s2, 0, s, s1.length, s2.length);
    return s;
  }

  public Class[] getAttributeTypes() {
    Class[] c1 = new Class[]{String[].class, Boolean.class, String.class};
    Class[] c2 = super.getAttributeTypes();
    Class[] c = new Class[c1.length + c2.length];
    System.arraycopy(c1, 0, c, 0, c1.length);
    System.arraycopy(c2, 0, c, c1.length, c2.length);
    return c;
  }

  public void setAttribute(String key, Object value) {
    if (VISIBLE.equals(key)) {
      if (value instanceof String) {
        value = new Boolean((String) value);
      }
      visibleToAll = ((Boolean) value).booleanValue();
    }
    else if (SIDE.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      owners = (String[]) value;
    }
    else if (USE_BOARDS.equals(key)) {
      Enumeration e = GameModule.getGameModule().getComponents(Map.class);
      while (e.hasMoreElements()) {
        Map m = (Map) e.nextElement();
        if (m.getMapName().equals(value)) {
          surrogate = m;
          break;
        }
      }
    }
    else {
      super.setAttribute(key, value);
    }
  }

  public String getAttributeValueString(String key) {
    if (VISIBLE.equals(key)) {
      return "" + visibleToAll;
    }
    else if (SIDE.equals(key)) {
      return StringArrayConfigurer.arrayToString(owners);
    }
    else if (USE_BOARDS.equals(key)) {
      return surrogate == null ? null :
        surrogate.getMapName();
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  protected JFrame createtParentFrame() {
    return new JFrame() {
      public void setVisible(boolean show) {
        super.setVisible(show && (visibleToAll
                                  || isAccessibleTo(PlayerRoster.getMySide())));
      }
    };
  }

  public boolean shouldDockIntoMainWindow() {
    return false;
  }

  /** Return true if the player playing the given side can access this map
   * @see PlayerRoster
   */
  public boolean isAccessibleTo(String playerSide) {
    for (int i = 0; i < owners.length; ++i) {
      if (owners[i].equals(playerSide)) {
        return true;
      }
    }
    return false;
  }

  public void setup(boolean show) {
    super.setup(show);
    if (!show) {
      ((View) theMap).clearListeners();
    }
    else if (isAccessibleTo(PlayerRoster.getMySide())) {
      ((View) theMap).useListeners();
    }
  }

  public void setBoards(Enumeration boardList) {
    if (surrogate != null) {
      boardList = surrogate.getAllBoards();
      edgeBuffer = surrogate.getEdgeBuffer();
    }
    super.setBoards(boardList);
  }

  public JComponent getView() {
    if (theMap == null) {
      theMap = new View(this);
      scroll =
        new JScrollPane(theMap,
                        JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                        JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
      scroll.unregisterKeyboardAction(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_DOWN, 0));
      scroll.unregisterKeyboardAction(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_UP, 0));
    }
    return theMap;
  }

  public static String getConfigureTypeName() {
    return "Private Window";
  }

  public HelpFile getHelpFile() {
    File dir = new File("docs");
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "PrivateWindow.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public void build(org.w3c.dom.Element el) {
    surrogate = null;
    super.build(el);
  }

  public void addTo(Buildable b) {
    if (!PlayerRoster.isActive()) {
      throw new IllegalBuildException("Must define player sides first");
    }
    super.addTo(b);
  }

  public static class View extends Map.View {
    private Vector keyListeners = new Vector();
    private Vector mouseListeners = new Vector();
    private Vector mouseMotionListeners = new Vector();

    public View(PrivateMap m) {
      super(m);
    }

    public synchronized void addKeyListener(KeyListener l) {
      keyListeners.addElement(l);
    }

    public synchronized void addMouseListener(MouseListener l) {
      mouseListeners.addElement(l);
    }

    public synchronized void addMouseMotionListener(MouseMotionListener l) {
      mouseMotionListeners.addElement(l);
    }

    protected void clearListeners() {
      for (Enumeration e = keyListeners.elements(); e.hasMoreElements();) {
        removeKeyListener((KeyListener) e.nextElement());
      }
      for (Enumeration e = mouseListeners.elements(); e.hasMoreElements();) {
        removeMouseListener((MouseListener) e.nextElement());
      }
      for (Enumeration e = mouseMotionListeners.elements(); e.hasMoreElements();) {
        removeMouseMotionListener((MouseMotionListener) e.nextElement());
      }
    }

    protected void useListeners() {
      for (Enumeration e = keyListeners.elements(); e.hasMoreElements();) {
        super.addKeyListener((KeyListener) e.nextElement());
      }
      for (Enumeration e = mouseListeners.elements(); e.hasMoreElements();) {
        super.addMouseListener((MouseListener) e.nextElement());
      }
      for (Enumeration e = mouseMotionListeners.elements(); e.hasMoreElements();) {
        super.addMouseMotionListener((MouseMotionListener) e.nextElement());
      }
    }
  }
}
