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
package VASSAL.build.module.map;

import VASSAL.build.*;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.PlayerRoster;
import VASSAL.build.module.PrivateMap;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.*;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceFinder;
import VASSAL.preferences.PositionOption;
import VASSAL.tools.LaunchButton;
import org.w3c.dom.Element;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

/**
 * This is scaled version of a {@link Map} that gives an overview.
 * Users can navigate around the Map by clicking on the GlobalMap,
 * which draws a square indicating the current viewable area in the
 * map window */
public class GlobalMap extends JPanel implements MouseListener,
  AutoConfigurable, GameComponent, Drawable {
  private Map map;
  private String boundsKey;
  private double scale = 0.19444444;      // Zoom factor
  private Color rectColor = Color.black;
  private JScrollPane scroll;
  private LaunchButton launch;

  private JFrame f;
  private BooleanConfigurer visibility;
  private ComponentListener visListener;

  public GlobalMap() {
    setSize(350, 125);
    f = new JFrame();
    scroll = new JScrollPane(this);
    f.getContentPane().add(scroll);
    f.setDefaultCloseOperation(javax.swing.WindowConstants.HIDE_ON_CLOSE);
    visListener = new ComponentAdapter() {
      public void componentHidden(ComponentEvent e) {
        if (visibility != null) {
          visibility.setValue(Boolean.FALSE);
        }
      }

      public void componentShown(ComponentEvent e) {
        if (visibility != null) {
          visibility.setValue(Boolean.TRUE);
        }
      }
    };
    launch = new LaunchButton(null,null,HOTKEY,new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        f.setVisible(!f.isVisible());
      }
    });
    launch.setToolTipText("Show/Hide overview window");
    launch.setAttribute(HOTKEY,KeyStroke.getKeyStroke(KeyEvent.VK_O,KeyEvent.CTRL_MASK+KeyEvent.SHIFT_MASK));
    URL imageURL = getClass().getResource("/images/overview.gif");
    if (imageURL != null) {
      launch.setIcon(new ImageIcon(imageURL));
    }
    else {
      launch.setText("overview");
    }

    addMouseListener(this);
  }

  /**
   * Expects to be added to a {@link Map}.  Adds itself as a {@link
   * GameComponent} and a {@link Drawable} component */
  public void addTo(Buildable b) {
    map = (Map) b;

    boundsKey = "BoundsOfGlobalMap" + map.getId();
    GameModule.getGameModule().getPrefs().addOption
      (new PositionOption(boundsKey, f));

    String visibilityKey = "GlobalMap" + map.getId()+"Visible";
    visibility = new BooleanConfigurer(visibilityKey,null,Boolean.TRUE);
    GameModule.getGameModule().getPrefs().addOption(null,visibility);

    GameModule.getGameModule().getGameState().addGameComponent(this);

    map.addDrawComponent(this);

    map.getToolBar().add(launch);
  }

  public void add(Buildable b) {
    throw new IllegalBuildException("Cannot contain children");
  }

  public void remove(Buildable b) {
    throw new IllegalBuildException("Cannot contain children");
  }

  public void removeFrom(Buildable b) {
    map = (Map) b;
    map.removeDrawComponent(this);
    map.getToolBar().remove(launch);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    f.dispose();
  }

  public void build(Element e) {
    AutoConfigurable.Util.buildAttributes(e, this);
  }

  private static final String SCALE = "scale";
  private static final String COLOR = "color";
  private static final String HOTKEY = "hotkey";

  public String[] getAttributeNames() {
    return new String[]{SCALE, COLOR,HOTKEY};
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    return null;
  }

  public void setAttribute(String key, Object value) {
    if (SCALE.equals(key)) {
      if (value instanceof String) {
        value = Double.valueOf((String) value);
      }
      scale = ((Double) value).doubleValue();
    }
    else if (COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      rectColor = (Color) value;
    }
    else {
      launch.setAttribute(key,value);
    }
  }

  public String getAttributeValueString(String key) {
    if (SCALE.equals(key)) {
      return "" + scale;
    }
    else if (COLOR.equals(key)) {
      return ColorConfigurer.colorToString(rectColor);
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Scale factor",
                        "Visible rectangle highlight color",
                        "Hotkey to show/hide"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{Double.class, Color.class, KeyStroke.class};
  }

  public void draw(Graphics g, Map m) {
    repaint();
  }

  public void paint(Graphics g) {
    g.clearRect(0, 0, getSize().width, getSize().height);
    map.drawBoards(g, -Math.round((float)scale * map.getEdgeBuffer().width),
             -Math.round((float)scale * map.getEdgeBuffer().height), scale, this);
    GamePiece stack[] = map.getPieces();
    for (int i = 0; i < stack.length; i++) {
      int x = Math.round((stack[i].getPosition().x - map.getEdgeBuffer().width) * (float)scale);
      int y = Math.round((stack[i].getPosition().y - map.getEdgeBuffer().height) * (float)scale);
      stack[i].draw(g, x, y, this, scale);
    }

    // Draw a rectangle indicating the present viewing area
    g.setColor(rectColor);

    Rectangle r = map.getView().getVisibleRect();
    Point ul = map.mapCoordinates(r.getLocation());
    ul.translate(-map.getEdgeBuffer().width,
                 -map.getEdgeBuffer().height);
    int x0 = (int) (scale * ul.x);
    int y0 = (int) (scale * ul.y);
    int w = (int) (scale * r.width / map.getZoom());
    int h = (int) (scale * r.height / map.getZoom());
    g.drawRect(x0, y0, w, h);
    g.drawRect(x0 - 1, y0 - 1, w + 2, h + 2);
  }

  public void mousePressed(MouseEvent e) {
  }

  public void mouseEntered(MouseEvent e) {
  }

  public void mouseExited(MouseEvent e) {
  }

  public void mouseClicked(MouseEvent e) {
  }

  public void mouseReleased(MouseEvent e) {
    map.centerAt(new Point(map.getEdgeBuffer().width + (int) (e.getX() / scale),
                           map.getEdgeBuffer().height + (int) (e.getY() / scale)));
    map.repaint();
  }

  public String getToolTipText(MouseEvent e) {
    Point p = new Point(map.getEdgeBuffer().width + (int) (e.getX() / scale),
                        map.getEdgeBuffer().height + (int) (e.getY() / scale));
    GamePiece piece = map.findPiece(p, PieceFinder.MOVABLE);
    return piece == null ? null : piece.getName();
  }

  public Command getRestoreCommand() {
    return null;
  }

  public Dimension getPreferredSize() {
    return new Dimension((int) ((map.mapSize().width
                                 - 2 * map.getEdgeBuffer().width) * scale),
                         (int) ((map.mapSize().height
                                 - 2 * map.getEdgeBuffer().height) * scale));
  }

  public void setup(boolean show) {
    boolean visible = show
      && map.getAllBoards().hasMoreElements()
      && visibility.booleanValue().booleanValue();
    if (map instanceof PrivateMap
      && !((PrivateMap) map).isAccessibleTo(PlayerRoster.getMySide())) {
      visible = false;
    }
    if (visible) {
      f.setTitle(map.getMapName() + " overview");
      scroll.getViewport().setPreferredSize(getPreferredSize());
      f.pack();
      Rectangle r = (Rectangle) GameModule.getGameModule().getPrefs()
        .getValue(boundsKey);
      if (r != null) {
        f.setLocation(r.x, r.y);
      }
    }
    f.removeComponentListener(visListener);
    f.setVisible(visible);
    f.addComponentListener(visListener);

    if (show) {
      revalidate();
    }
  }

  public void setTitle(String s) {
    f.setTitle(s);
  }

  public static String getConfigureTypeName() {
    return "Overview Window";
  }

  public String getConfigureName() {
    return null;
  }

  public Configurer getConfigurer() {
    return new AutoConfigurer(this);
  }

  public Configurable[] getConfigureComponents() {
    return new Configurable[0];
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void addPropertyChangeListener(java.beans.PropertyChangeListener l) {
  }

  public HelpFile getHelpFile() {
    File dir = new File("docs");
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "Map.htm"), "#OverviewWindow");
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public org.w3c.dom.Element getBuildElement(org.w3c.dom.Document doc) {
    return AutoConfigurable.Util.getBuildElement(doc, this);
  }
}


