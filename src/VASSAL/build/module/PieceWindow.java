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
package VASSAL.build.module;

import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.Widget;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.widget.*;
import VASSAL.counters.GamePiece;
import VASSAL.preferences.PositionOption;
import VASSAL.preferences.VisibilityOption;
import VASSAL.tools.LaunchButton;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.net.MalformedURLException;

/**
 * A window from which players can create new {@link GamePiece}s by
 * clicking and dragging from the PieceWindow.  The actual GamePieces
 * are contained in {@link PieceSlot} components.  PieceWindow extends
 * {@link Widget}, so it may be composed of various tabs, lists, etc.  */
public class PieceWindow extends Widget {
  private JFrame frame;
  private String id;
  private LaunchButton launch;
  public final String WINDOW_NAME = "entryName";
  public static final String HOTKEY = "hotkey";

  public PieceWindow() {
    frame = new JFrame();
    frame.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
    frame.setTitle(getConfigureName());
    addPropertyChangeListener(new java.beans.PropertyChangeListener() {
      public void propertyChange(java.beans.PropertyChangeEvent e) {
        if (Configurable.NAME_PROPERTY
          .equals(e.getPropertyName())) {
          frame.setTitle((String) e.getNewValue());
        }
      }
    });
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        frame.setVisible(!frame.isVisible());
      }
    };
    launch = new LaunchButton("Pieces",WINDOW_NAME,HOTKEY,al);
    launch.setToolTipText("Show/Hide the Pieces window");
  }

  public HelpFile getHelpFile() {
    File dir = new File("docs");
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "PieceWindow.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public void build(org.w3c.dom.Element e) {
    super.build(e);
    rebuild();
  }

  public java.awt.Component getComponent() {
    return frame;
  }

  public static String getConfigureTypeName() {
    return "Game Piece Palette";
  }

  /**
   * A PieceWindow may contain a {@link TabWidget}, a {@link
   * PanelWidget}, a {@link BoxWidget}, a {@link ListWidget}, or a
   * {@link PieceSlot} */
  public Class[] getAllowableConfigureComponents() {
    return new Class[]{TabWidget.class, PanelWidget.class, BoxWidget.class, ListWidget.class, PieceSlot.class};
  }

  public void add(Buildable b) {
    if (b instanceof Widget) {
      frame.getContentPane().add(((Widget) b).getComponent());
    }
    super.add(b);
  }

  public void remove(Buildable b) {
    if (b instanceof Widget) {
      frame.getContentPane().remove(((Widget) b).getComponent());
    }
    super.remove(b);
  }

  /**
   * Each instanceof PieceWindow has a unique String identifier
   *
   * @return the identifier for this PieceWindow
   */
  public String getId() {
    return id;
  }

  /**
   * Each instanceof PieceWindow has a unique String identifier
   */
  public void setId(String s) {
    id = s;
  }

  /**
   * Expects to be added to a {@link GameModule}.  When added, sets
   * the containing window to visible */
  public void addTo(Buildable parent) {
    int count = 0;
    for (java.util.Enumeration e =
      GameModule.getGameModule().getComponents(PieceWindow.class);
         e.hasMoreElements();) {
      count++;
      e.nextElement();
    }
    setId("PieceWindow" + count);

    String key = PositionOption.key + getId();
    final PositionOption pos = new VisibilityOption(key, frame);
    GameModule.getGameModule().getPrefs().addOption(pos);
    GameModule.getGameModule().getToolBar().add(launch);
  }

  public void removeFrom(Buildable parent) {
    frame.setVisible(false);
    GameModule.getGameModule().getToolBar().remove(launch);
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Name"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class};
  }

  public String[] getAttributeNames() {
    return new String[]{WINDOW_NAME, HOTKEY};
  }

  public void setAttribute(String name, Object value) {
    launch.setAttribute(name,value);
    if (WINDOW_NAME.equals(name)) {
      String s = (String) value;
      setConfigureName(s);
      launch.setToolTipText("Show/Hide the " + s + " window");
    }
  }

  public String getAttributeValueString(String name) {
    return launch.getAttributeValueString(name);
  }
}
