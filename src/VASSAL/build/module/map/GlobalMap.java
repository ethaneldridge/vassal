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
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.AutoConfigurer;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Properties;
import VASSAL.tools.LaunchButton;
import VASSAL.Info;
import org.w3c.dom.Element;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Enumeration;

/**
 * This is scaled version of a {@link Map} that gives an overview.
 * Users can navigate around the Map by clicking on the GlobalMap,
 * which draws a square indicating the current viewable area in the
 * map window */
public class GlobalMap extends JPanel implements MouseListener,
    AutoConfigurable, GameComponent, Drawable {
  private Map map;
  private double scale = 0.19444444;      // Zoom factor
  private Color rectColor = Color.black;
  private LaunchButton launch;

  private JWindow f;
  private CounterDetailViewer mouseOverViewer;

  public GlobalMap() {
    ActionListener al = new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            setWindowVisible(!f.isVisible());
          }
        };
    launch = new LaunchButton(null,null,HOTKEY,al);
    launch.setToolTipText("Show/Hide overview window");
    launch.setAttribute(HOTKEY, KeyStroke.getKeyStroke(KeyEvent.VK_O, KeyEvent.CTRL_MASK + KeyEvent.SHIFT_MASK));
    URL imageURL = getClass().getResource("/images/overview.gif");
    if (imageURL != null) {
      launch.setIcon(new ImageIcon(imageURL));
    }
    else {
      launch.setText("overview");
    }

    addMouseListener(this);
  }

  private void initWindow() {
    if (f == null) {
      Component ancestor = map.getView().getTopLevelAncestor();
      JFrame owner = ancestor instanceof JFrame ? (JFrame) ancestor : null;
      f = new JWindow(owner);
      f.getContentPane().add(this);
      if (Info.is2dEnabled()) {
        map.getView().addHierarchyBoundsListener(new HierarchyBoundsAdapter() {
          public void ancestorMoved(HierarchyEvent e) {
            adjustWindowLocation();
          }
        });
      }
      else {
        map.getView().addComponentListener(new ComponentAdapter() {
          public void componentMoved(ComponentEvent e) {
            adjustWindowLocation();
          }
        });
      }
    }
  }

  /**
   * Expects to be added to a {@link Map}.  Adds itself as a {@link
   * GameComponent} and a {@link Drawable} component */
  public void addTo(Buildable b) {
    map = (Map) b;

    mouseOverViewer = new CounterViewer();

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
    if (f != null) {
      f.dispose();
    }
  }

  public void build(Element e) {
    AutoConfigurable.Util.buildAttributes(e, this);
  }

  private static final String SCALE = "scale";
  private static final String COLOR = "color";
  private static final String HOTKEY = "hotkey";

  public String[] getAttributeNames() {
    return new String[]{SCALE, COLOR, HOTKEY};
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
      launch.setAttribute(key, value);
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
    map.drawBoards(g, -Math.round((float) scale * map.getEdgeBuffer().width),
                   -Math.round((float) scale * map.getEdgeBuffer().height), scale, this);
    GamePiece stack[] = map.getPieces();
    for (int i = 0; i < stack.length; i++) {
      Point p = componentCoordinates(stack[i].getPosition());
      stack[i].draw(g, p.x, p.y, this, scale);
    }
    for (Enumeration e = map.getComponents(DrawPile.class); e.hasMoreElements();) {
      DrawPile deck = (DrawPile) e.nextElement();
      Point p = componentCoordinates(deck.getPosition());
      deck.draw(g, p.x, p.y, this, scale);
    }
    mouseOverViewer.draw(g, map);

    // Draw a rectangle indicating the present viewing area
    g.setColor(rectColor);

    Rectangle r = map.getView().getVisibleRect();
    Point ul = map.mapCoordinates(r.getLocation());
    ul.translate(-map.getEdgeBuffer().width,
                 -map.getEdgeBuffer().height);
    ul = componentCoordinates(ul);
    int w = (int) (scale * r.width / map.getZoom());
    int h = (int) (scale * r.height / map.getZoom());
    g.drawRect(ul.x, ul.y, w, h);
    g.drawRect(ul.x - 1, ul.y - 1, w + 2, h + 2);
  }

  public void mousePressed(MouseEvent e) {
  }

  public void mouseEntered(MouseEvent e) {
  }

  public void mouseExited(MouseEvent e) {
  }

  public void mouseClicked(MouseEvent e) {
  }

  /**
   * Transform a point from Map coordinates to coordinates in the overview window
   * @param p
   * @return
   */
  public Point componentCoordinates(Point p) {
    p = new Point(p.x - map.getEdgeBuffer().width,
                  p.y - map.getEdgeBuffer().height);
    p.x *= scale;
    p.y *= scale;
    return p;
  }

  /**
   * Transform a point from coordinates in the overview window to Map coordinates
   * @param p
   * @return
   */
  public Point mapCoordinates(Point p) {
    p = new Point((int) Math.round(p.x / scale),
                  (int) Math.round(p.y / scale));
    p.translate(map.getEdgeBuffer().width,
                map.getEdgeBuffer().height);
    return p;
  }

  public void mouseReleased(MouseEvent e) {
    map.centerAt(mapCoordinates(e.getPoint()));
    map.repaint();
  }

  public String getToolTipText(MouseEvent e) {
    return null;
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
    initWindow();
    if (show) {
      f.setSize(getPreferredSize());
    }
    else {
      f.setVisible(false);
    }

    if (show && map.getComponents(CounterDetailViewer.class).hasMoreElements()) {
      addMouseMotionListener(mouseOverViewer);
      f.addKeyListener(mouseOverViewer);
    }
    else {
      removeMouseMotionListener(mouseOverViewer);
      f.removeKeyListener(mouseOverViewer);
    }
  }

  public void setWindowVisible(final boolean visible) {
    if (visible) {
      adjustWindowLocation();
    }
    f.setVisible(visible);
  }

  protected void adjustWindowLocation() {
    if (map.getView().isShowing()) {
      Point p = map.getView().getLocationOnScreen();
      p.translate(map.getView().getVisibleRect().x,map.getView().getVisibleRect().y);
      f.setLocation(p);
    }
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
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
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

  private class CounterViewer extends CounterDetailViewer {
    public CounterViewer() {
      this.map = GlobalMap.this.map;
    }

    public void draw(Graphics g, Map map) {
      if (currentMousePosition != null) {
        this.draw(g, currentMousePosition.getPoint(), GlobalMap.this);
      }
    }

    protected GamePiece findPieceAtMousePosition() {
      Point oldPoint = currentMousePosition.getPoint();
      Point mapPoint = GlobalMap.this.map.componentCoordinates(mapCoordinates(oldPoint));

      currentMousePosition.translatePoint(mapPoint.x - oldPoint.x, mapPoint.y - oldPoint.y);
      GamePiece p = super.findPieceAtMousePosition();
      currentMousePosition.translatePoint(oldPoint.x - mapPoint.x, oldPoint.y - mapPoint.y);
      return p;
    }

    protected boolean shouldBeVisible() {
      return currentPiece != null
          && !Boolean.TRUE.equals(currentPiece.getProperty(Properties.IMMOBILE));
    }
  }
}


