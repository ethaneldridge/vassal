/*
 * $Id$
 *
 * Copyright (c) 2003 by David Sullivan and Rodney Kinney
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
import VASSAL.build.GameModule;
import VASSAL.build.IllegalBuildException;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.counters.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.awt.event.KeyListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.net.MalformedURLException;
import java.util.Enumeration;
import java.util.Vector;

/**
 * This is a {@link Drawable} class that draws the counters horizontally
 * when the mouse is held over a stack with the control key down.
 *
 * @author       David Sullivan
 * @version      1.0
 */
public class CounterDetailViewer extends AbstractConfigurable implements Drawable, MouseMotionListener, Runnable, KeyListener {

  public static final String USE_KEYBOARD = "ShowCounterDetails";

  public static final String DELAY = "delay";
  public static final String ALWAYS_SHOW_LOC = "alwaysshowloc";
  public static final String SHOW_GRAPH = "showgraph";
  public static final String SHOW_GRAPH_SINGLE = "showgraphsingle";
  public static final String SHOW_TEXT = "showtext";
  public static final String SHOW_TEXT_SINGLE = "showtextsingle";
  public static final String SHOW_REF = "showref";
  public static final String ZOOM_LEVEL = "zoomlevel";

  protected Map map;
  protected Thread delayThread;
  protected int delay = 700;
  protected long expirationTime;
  protected boolean graphicsVisible = false;
  protected boolean textVisible = false;
  protected MouseEvent currentMousePosition;
  protected GamePiece currentPiece;
  protected PieceVisitorDispatcher enumBuilder;

  protected boolean alwaysShowLoc = false;
  protected boolean showGraph = true;
  protected boolean showGraphSingle = false;
  protected boolean showText = false;
  protected boolean showTextSingle = false;
  protected boolean showRef = false;
  protected double zoomLevel = 1.0;

  public CounterDetailViewer() {
    enumBuilder = createEnumBuilder();
  }

  public void addTo(Buildable b) {
    map = (Map) b;
    Enumeration e = map.getComponents(getClass());
    while (e.hasMoreElements()) {
      if (e.nextElement() != this) {
        throw new IllegalBuildException("Mouse-over Stack Viewer already enabled");
      }
    }
    map.addDrawComponent(this);
    GameModule.getGameModule().getPrefs().addOption(
        "General",
        new BooleanConfigurer(USE_KEYBOARD, "Use CTRL-space to view stack details", Boolean.FALSE));

    map.getView().addMouseMotionListener(this);
    map.getView().addKeyListener(this);
  }

  private DeckVisitorDispatcher createEnumBuilder() {
    DeckVisitor v = new DeckVisitor() {
      public Object visitDeck(Deck d) {
        return new Enumeration() {
          public boolean hasMoreElements() {
            return false;
          }

          public Object nextElement() {
            return null;
          }
        };
      }

      public Object visitStack(Stack s) {
        return s.getPieces();
      }

      public Object visitDefault(GamePiece p) {
        return new Enumeration() {
          boolean finished = false;

          public boolean hasMoreElements() {
            return !finished;
          }

          public Object nextElement() {
            finished = true;
            return currentPiece;
          }
        };
      }
    };
    return new DeckVisitorDispatcher(v);
  }

  public void draw(Graphics g, Map map) {
    if (currentMousePosition != null) {
      draw(g, currentMousePosition.getPoint(), map.getView());
    }
  }

  public void draw(Graphics g, Point pt, JComponent comp) {

//    if (currentPiece == null) {
//      return;
//    }
    if (!graphicsVisible && !textVisible) {
      return;
    }

    PieceIterator pi;

    if (graphicsVisible) {
      pi = PieceIterator.visible(buildPieceEnum());
      drawGraphics(g, pt, comp, pi);
    }

    if (textVisible) {
      pi = PieceIterator.visible(buildPieceEnum());
      drawText(g, pt, comp, pi);
    }
  }

  private Enumeration buildPieceEnum() {
    return (Enumeration) enumBuilder.accept(currentPiece);
  }

  protected void drawGraphics(Graphics g, Point pt, JComponent comp, PieceIterator pi) {

    Rectangle bounds = new Rectangle(pt.x, pt.y, 0, 0);
    Vector v = new Vector();
    while (pi.hasMoreElements()) {
      GamePiece piece = pi.nextPiece();
      v.addElement(piece);
      Rectangle pieceBounds = piece.getShape().getBounds();
      bounds.width += pieceBounds.width;
      bounds.height = Math.max(bounds.height, pieceBounds.height);
    }

    if (bounds.width > 0) {

      Color outline = map.getHighlighter() instanceof ColoredBorder ? ((ColoredBorder) map.getHighlighter()).getColor() : Color.black;
      Color background = new Color(255 - outline.getRed(), 255 - outline.getGreen(), 255 - outline.getBlue());

      Rectangle visibleRect = comp.getVisibleRect();
      bounds.x = Math.min(bounds.x, visibleRect.x + visibleRect.width - bounds.width);
      bounds.y = Math.min(bounds.y, visibleRect.y + visibleRect.height - bounds.height);
      g.setColor(background);
      g.fillRect(bounds.x - 1, bounds.y - 1, bounds.width + 2, bounds.height + 2);
      g.setColor(outline);
      g.drawRect(bounds.x - 2, bounds.y - 2, bounds.width + 3, bounds.height + 3);
      g.drawRect(bounds.x - 3, bounds.y - 3, bounds.width + 5, bounds.height + 5);
      pi = new PieceIterator(v.elements());
      while (pi.hasMoreElements()) {
        // Draw the next piece
        // pt is the location of the left edge of the piece
        GamePiece piece = pi.nextPiece();
        Rectangle pieceBounds = piece.getShape().getBounds();
        piece.draw(g, bounds.x - pieceBounds.x, bounds.y - pieceBounds.y, comp, 1.0);

        bounds.translate(pieceBounds.width, 0);
      }
    }
  }

  protected void drawText(Graphics g, Point pt, JComponent comp, PieceIterator pi) {
    /*
     * Label with the location
     * If the counter viewer is being displayed, then place the location name just above the
     * left hand end of the counters.
     * If no counter viewer (i.e. single piece or expanded stack), then place the location
     * name above the centre of the first piece in the stack.
     */
    String locationName = null;

    while (pi.hasMoreElements()) {
      GamePiece piece = pi.nextPiece();
      if (locationName == null) {
        locationName = map.locationName(piece.getPosition());
      }
    }

    if (locationName == null) {
      Point mapPt = map.mapCoordinates(currentMousePosition.getPoint());
      Point snapPt = map.snapTo(mapPt);
      locationName = map.locationName(snapPt);
    }
    drawLabel(g, pt, locationName);
  }

  protected void drawLabel(Graphics g, Point pt, String locationName) {

    if (locationName != null) {
      Color outline = map.getHighlighter() instanceof ColoredBorder ? ((ColoredBorder) map.getHighlighter()).getColor() : Color.black;
      Color background = new Color(255 - outline.getRed(), 255 - outline.getGreen(), 255 - outline.getBlue());

      Labeler.drawLabel(g, locationName,
                        pt.x, pt.y - 5,
                        new Font("Dialog", Font.PLAIN, 9),
                        Labeler.RIGHT, Labeler.BOTTOM,
                        outline, background, outline);
    }
  }

  public void run() {
    while (System.currentTimeMillis() < expirationTime) {
      try {
        Thread.sleep(Math.max(0, expirationTime - System.currentTimeMillis()));
      }
      catch (InterruptedException e) {
      }
    }
    showDetails();
  }

  protected void showDetails() {
    currentPiece = findPieceAtMousePosition();
    /*
     * Visibility Rules:
     *   Stack         - Depends on setting of showGraphics/showText
     *   Single Unit   - Depends on setting of showGraphics/showText and showGraphicsSingle/showTextSingle
     *                   and stack must not be expanded.
     *   Empty space   - Depends on setting of
     */
    if (currentPiece == null) {
      textVisible = (showRef && map.getZoom() < zoomLevel);
      graphicsVisible = false;
    }
    else {
      if (map.getZoom() < 0.75) {
        boolean val = !Boolean.TRUE.equals(currentPiece.getProperty(Properties.IMMOBILE));
        graphicsVisible = (showGraph && val);
        textVisible = (showText && val);
      }
      else if (currentPiece instanceof Stack) {
        Stack s = (Stack) currentPiece;
        if (s.topPiece() == s.bottomPiece()) {
          graphicsVisible = (showGraph && showGraphSingle);
          textVisible = (showText && showTextSingle);
        }
        else {
          graphicsVisible = (showGraph && !s.isExpanded());
          textVisible = (showText && !s.isExpanded());
        }
      }
    }

    map.repaint();
  }

  protected GamePiece findPieceAtMousePosition() {
    GamePiece p = map.findPiece(map.mapCoordinates(currentMousePosition.getPoint()), PieceFinder.MOVABLE);
    if (p != null && p.getParent() != null) {
      p = p.getParent();
    }
    return p;
  }

  public void mouseMoved(MouseEvent e) {

    // clear details when mouse moved
    if (graphicsVisible || textVisible) {
      graphicsVisible = false;
      textVisible = false;
      map.repaint();
    }
    else {
      // set the timer
      currentMousePosition = e;
      // quit if not active
      if (Boolean.FALSE.equals(GameModule.getGameModule().getPrefs().getValue(USE_KEYBOARD))) {
        expirationTime = System.currentTimeMillis() + delay;
        if (delayThread == null || !delayThread.isAlive()) {
          delayThread = new Thread(this);
          delayThread.start();
        }
      }
    }
  }

  public void mouseDragged(MouseEvent e) {
    mouseMoved(e);
  }

  public void keyTyped(KeyEvent e) {
  }

  public void keyPressed(KeyEvent e) {
    if (e.getKeyCode() == KeyEvent.VK_SPACE
        && e.isControlDown()
        && Boolean.TRUE.equals(GameModule.getGameModule().getPrefs().getValue(USE_KEYBOARD))) {
      showDetails();
    }
  }

  public void keyReleased(KeyEvent e) {
    if (graphicsVisible || textVisible) {
      graphicsVisible = false;
      textVisible = false;
      map.repaint();
    }
  }

  public String[] getAttributeNames() {
    return new String[]{DELAY, SHOW_GRAPH, SHOW_GRAPH_SINGLE, SHOW_TEXT, SHOW_TEXT_SINGLE, SHOW_REF, ZOOM_LEVEL};
  }

  public String[] getAttributeDescriptions() {
    return new String[]{
      "Delay before display (ms)",
      "Display Graphics?",
      "Display Graphics for single counter?",
      "Display Text?",
      "Display Text for single counter?",
      "Display Text for empty grid?",
      "When zoom level less than"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{Integer.class, Boolean.class, Boolean.class, Boolean.class, Boolean.class, Boolean.class, Double.class};
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "Map.htm"), "#StackViewer");
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public void removeFrom(Buildable parent) {
    map.removeDrawComponent(this);
    map.getView().removeMouseMotionListener(this);
  }

  public void setAttribute(String name, Object value) {
    if (DELAY.equals(name)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      if (value != null) {
        delay = ((Integer) value).intValue();
      }
    }
    else if (SHOW_GRAPH.equals(name)) {
      if (value instanceof Boolean) {
        showGraph = ((Boolean) value).booleanValue();
      }
      else if (value instanceof String) {
        showGraph = "true".equals(value);
      }
    }
    else if (SHOW_GRAPH_SINGLE.equals(name)) {
      if (value instanceof Boolean) {
        showGraphSingle = ((Boolean) value).booleanValue();
      }
      else if (value instanceof String) {
        showGraphSingle = "true".equals(value);
      }
    }
    else if (SHOW_TEXT.equals(name)) {
      if (value instanceof Boolean) {
        showText = ((Boolean) value).booleanValue();
      }
      else if (value instanceof String) {
        showText = "true".equals(value);
      }
    }
    else if (SHOW_TEXT_SINGLE.equals(name)) {
      if (value instanceof Boolean) {
        showTextSingle = ((Boolean) value).booleanValue();
      }
      else if (value instanceof String) {
        showTextSingle = "true".equals(value);
      }
    }
    else if (SHOW_REF.equals(name)) {
      if (value instanceof Boolean) {
        showRef = ((Boolean) value).booleanValue();
      }
      else if (value instanceof String) {
        showRef = "true".equals(value);
      }
    }
    else if (ZOOM_LEVEL.equals(name)) {
      if (value instanceof String) {
        value = new Double((String) value);
      }
      zoomLevel = ((Double) value).doubleValue();
    }
  }


  public String getAttributeValueString(String name) {
    if (DELAY.equals(name)) {
      return "" + delay;
    }
    else if (SHOW_GRAPH.equals(name)) {
      return "" + showGraph;
    }
    else if (SHOW_GRAPH_SINGLE.equals(name)) {
      return "" + showGraphSingle;
    }
    else if (SHOW_TEXT.equals(name)) {
      return "" + showText;
    }
    else if (SHOW_TEXT_SINGLE.equals(name)) {
      return "" + showTextSingle;
    }
    else if (SHOW_REF.equals(name)) {
      return "" + showRef;
    }
    else if (ZOOM_LEVEL.equals(name)) {
      return "" + zoomLevel;
    }
    else
      return null;
  }

  public static String getConfigureTypeName() {
    return "Mouse-over Stack Viewer";
  }

}
