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

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.*;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.command.AddPiece;
import VASSAL.command.Command;
import VASSAL.command.MoveTracker;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.*;
import VASSAL.preferences.PositionOption;
import VASSAL.tools.ComponentSplitter;
import VASSAL.tools.KeyStrokeSource;
import VASSAL.tools.LaunchButton;
import org.w3c.dom.Element;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.net.MalformedURLException;
import java.util.Enumeration;
import java.util.Vector;

/**
 * The Map is the main component for displaying and containing {@link
 * GamePiece}s during play.  Pieces are displayed on a Map and moved
 * by clicking and dragging.  Keyboard events are forwarded to
 * selected pieces.  Multiple map windows are supported in a single
 * game, with dragging between windows allowed.
 *
 * A Map may contain many different {@link Buildable} subcomponents.
 * Components which are added directly to a Map are contained in the
 * <code>VASSAL.build.module.map</code> package */
public class Map extends AbstractConfigurable implements GameComponent,
    FocusListener, MouseListener, MouseMotionListener,
    Configurable {
  /*
   ** The map consists of the empty board and an array of stacks
   ** Movement of a stack is accomplished by clicking and dragging
   ** Alterations to a stack are done by keyboard commands
   */

  private String mapID = "";
  private String mapName = "";

  protected JPanel theMap;

  private Vector drawComponents = new Vector();

  protected JScrollPane scroll;
  protected ComponentSplitter.SplitPane mainWindowDock;
  protected BoardPicker picker;
  protected JToolBar toolBar = new JToolBar();
  protected Zoomer zoom;
  protected StackMetrics metrics;
  protected Dimension edgeBuffer = new Dimension(0, 0);
  protected LaunchButton launchButton;
  protected boolean useLaunchButton = false;
  protected String markMovedOption;

  protected MouseListener multicaster = null;
  protected Vector mouseListenerStack;

  protected Vector boards = new Vector();

  protected int maxStacks = 100, moreStacks = 25;
  protected GamePiece stack[] = new GamePiece[maxStacks];
  protected int nstacks;

  protected Highlighter highlighter = new ColoredBorder();

  private boolean clearFirst = false;  //  Whether to clear the display before drawing the map
  private boolean hideCounters = false;//  Option to hide counters to see map
  private boolean allowMultiple = false;
  private VisibilityCondition visibilityCondition;

  public Map() {
    getView();
    theMap.addMouseListener(this);
    theMap.addMouseMotionListener(this);
    theMap.addFocusListener(this);
    toolBar.setFloatable(false);
  }

  public static final String NAME = "mapName";
  public static final String MARK_MOVED = "markMoved";
  public static final String EDGE_WIDTH = "edgeWidth";
  public static final String EDGE_HEIGHT = "edgeHeight";
  public static final String HIGHLIGHT_COLOR = "color";
  public static final String HIGHLIGHT_THICKNESS = "thickness";
  public static final String ALLOW_MULTIPLE = "allowMultiple";
  public static final String USE_LAUNCH_BUTTON = "launch";
  public static final String BUTTON_NAME = "buttonName";
  public static final String HOTKEY = "hotkey";

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setMapName((String) value);
    }
    else if (MARK_MOVED.equals(key)) {
      markMovedOption = (String) value;
    }
    else if ("edge".equals(key)) { // Backward-compatible
      String s = (String) value;
      int i = s.indexOf(",");
      if (i > 0) {
        edgeBuffer = new Dimension(Integer.parseInt(s.substring(0, i)),
                                   Integer.parseInt(s.substring(i + 1)));
      }
    }
    else if (EDGE_WIDTH.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      try {
        edgeBuffer = new Dimension(((Integer) value).intValue(), edgeBuffer.height);
      }
      catch (NumberFormatException ex) {
      }
    }
    else if (EDGE_HEIGHT.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      try {
        edgeBuffer = new Dimension(edgeBuffer.width, ((Integer) value).intValue());
      }
      catch (NumberFormatException ex) {
      }
    }
    else if (ALLOW_MULTIPLE.equals(key)) {
      if (value instanceof String) {
        value = new Boolean((String) value);
      }
      allowMultiple = ((Boolean) value).booleanValue();
      if (picker != null) {
        picker.setAllowMultiple(allowMultiple);
      }
    }
    else if (HIGHLIGHT_COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      if (value != null) {
        ((ColoredBorder) highlighter).setColor((Color) value);
      }
    }
    else if (HIGHLIGHT_THICKNESS.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      if (highlighter instanceof ColoredBorder) {
        ((ColoredBorder) highlighter).setThickness
            (((Integer) value).intValue());
      }
    }
    else if (USE_LAUNCH_BUTTON.equals(key)) {
      if (value instanceof String) {
        value = new Boolean((String) value);
      }
      launchButton.setVisible(((Boolean) value).booleanValue());
    }
    else {
      launchButton.setAttribute(key, value);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getMapName();
    }
    else if (MARK_MOVED.equals(key)) {
      return markMovedOption;
    }
    else if (EDGE_WIDTH.equals(key)) {
      return "" + edgeBuffer.width;
    }
    else if (EDGE_HEIGHT.equals(key)) {
      return "" + edgeBuffer.height;
    }
    else if (ALLOW_MULTIPLE.equals(key)) {
      return "" + picker.isAllowMultiple();
    }
    else if (HIGHLIGHT_COLOR.equals(key)) {
      if (highlighter instanceof ColoredBorder) {
        return ColorConfigurer.colorToString(((ColoredBorder) highlighter).getColor());
      }
      else {
        return null;
      }
    }
    else if (HIGHLIGHT_THICKNESS.equals(key)) {
      if (highlighter instanceof ColoredBorder) {
        return "" + ((ColoredBorder) highlighter).getThickness();
      }
      else {
        return null;
      }
    }
    else if (USE_LAUNCH_BUTTON.equals(key)) {
      return "" + launchButton.isVisible();
    }
    else {
      return launchButton.getAttributeValueString(key);
    }
  }

  public void build(Element e) {
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        if (mainWindowDock == null
            && launchButton.isEnabled()
          && theMap.getTopLevelAncestor() != null) {
          theMap.getTopLevelAncestor().setVisible(!theMap.getTopLevelAncestor().isVisible());
        }
      }
    };
    launchButton = new LaunchButton("Map", BUTTON_NAME, HOTKEY, al);
    launchButton.setEnabled(false);
    launchButton.setVisible(false);
    if (e != null) {
      super.build(e);
    }
    else {
      getBoardPicker();

      getStackMetrics();

      addChild(new ForwardToKeyBuffer());
      addChild(new Scroller());
      addChild(new ForwardToChatter());
      addChild(new MenuDisplayer());
      addChild(new MapCenterer());
      addChild(new StackExpander());
      addChild(new PieceMover());
      addChild(new KeyBufferer());
      addChild(new ImageSaver());
      addChild(new CounterDetailViewer());
    }
    setup(false);
  }

  private void addChild(Buildable b) {
    add(b);
    b.addTo(this);
  }

  /**
   * Every map must include a {@link BoardPicker} as one of its
   * build components */
  public void setBoardPicker(BoardPicker picker) {
    this.picker = picker;
    if (picker != null) {
      picker.setAllowMultiple(allowMultiple);
    }
  }

  /**
   * Every map must include a {@link BoardPicker} as one of its
   * build components
   * @return the BoardPicker for this map*/
  public BoardPicker getBoardPicker() {
    if (picker == null) {
      picker = new BoardPicker();
      picker.build(null);
      add(picker);
      picker.addTo(this);
    }
    return picker;
  }

  /**
   * A map may include a {@link Zoomer} as one of its build components
   */
  public void setZoomer(Zoomer z) {
    zoom = z;
  }

  /**
   * A map may include a {@link Zoomer} as one of its build components
   * @return the Zoomer for this map
   */
  public Zoomer getZoomer() {
    return zoom;
  }

  /**
   * Every map must include a {@link StackMetrics} as one of its
   * build components, which governs the stacking behavior of
   * GamePieces on the map */
  public void setStackMetrics(StackMetrics sm) {
    metrics = sm;
  }

  /**
   * Every map must include a {@link StackMetrics} as one of its
   * build components, which governs the stacking behavior of
   * GamePieces on the map
   * @return the StackMetrics for this map */
  public StackMetrics getStackMetrics() {
    if (metrics == null) {
      metrics = new StackMetrics();
      metrics.build(null);
      add(metrics);
      metrics.addTo(this);
    }
    return metrics;
  }

  /**
   * @return the current zoom factor for the map
   */
  public double getZoom() {
    return zoom == null ? 1.0 : zoom.getZoomFactor();
  }

  /**
   * @return the toolbar for this map's window
   */
  public JToolBar getToolBar() {
    return toolBar;
  }

  /**
   * Add a {@link Drawable} component to this map
   *
   * @see #paint
   */
  public void addDrawComponent(Drawable theComponent) {
    drawComponents.addElement(theComponent);
  }

  /**
   * Remove a {@link Drawable} component from this map
   *
   * @see #paint
   */
  public void removeDrawComponent(Drawable theComponent) {
    drawComponents.removeElement(theComponent);
  }

  /**
   * Expects to be added to a {@link GameModule}.  Determines a
   * unique id for this map.  Registers itself as {@link
   * KeyStrokeSource}.  Registers itself as a {@link GameComponent}.
   * Registers itself as a drop target and drag source
   *
   * @see #getId
   * @see DragBuffer */
  public void addTo(Buildable b) {
    int mapCount = 0;
    for (Enumeration e =
        GameModule.getGameModule().getComponents(Map.class);
         e.hasMoreElements();) {
      mapCount++;
      e.nextElement();
    }
    setID("Map" + mapCount);

    DragBuffer.getBuffer().addDropTarget(theMap, this);
    DragBuffer.getBuffer().addDragSource(theMap);

    GameModule.getGameModule().getGameState().addGameComponent(this);
    GameModule.getGameModule().addKeyStrokeSource
        (new KeyStrokeSource(theMap, JComponent.WHEN_FOCUSED));
    GameModule.getGameModule().getToolBar().add(launchButton);

    if (shouldDockIntoMainWindow()) {
      JPanel root = new JPanel(new BorderLayout());
//      root.add(toolBar, BorderLayout.NORTH);
      root.add(scroll, BorderLayout.CENTER);
      ComponentSplitter splitter = new ComponentSplitter();
      mainWindowDock = splitter.splitBottom(splitter.getSplitAncestor(GameModule.getGameModule().getControlPanel(), -1), root, true);
    }
  }

  public void removeFrom(Buildable b) {
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    Window w = SwingUtilities.getWindowAncestor(theMap);
    if (w != null) {
      w.dispose();
    }
    GameModule.getGameModule().getToolBar().remove(launchButton);
  }

  /**
   * Set the boards for this map.  Each map may contain more than
   * one {@link Board} */
  public void setBoards(Enumeration boardList) {
    boards.removeAllElements();
    boards = null;
    System.gc();
    boards = new Vector();
    while (boardList.hasMoreElements()) {
      boards.addElement(boardList.nextElement());
    }
    setBoardBoundaries();
  }

  public Command getRestoreCommand() {
    return null;
  }

  /**
   * @return the {@link Board} on this map containing the argument point
   */
  public Board findBoard(Point p) {
    for (int i = 0; i < boards.size(); ++i) {
      Board b = (Board) boards.elementAt(i);
      if (b.bounds().contains(p))
        return b;
    }
    return null;
  }

  /**
   * Return the board with the given name
   * @param name
   * @return null if no such board found
   */
  public Board getBoardByName(String name) {
    Board board = null;
    if (name != null) {
      for (Enumeration e = getAllBoards(); e.hasMoreElements();) {
        Board b = (Board) e.nextElement();
        if (name.equals(b.getName())) {
          board = b;
          break;
        }
      }
    }
    return board;
  }

  public Dimension getPreferredSize() {
    return new Dimension((int) (getZoom() * mapSize().width),
                         (int) (getZoom() * mapSize().height));
  }

  /**
   * @return the size of the map in pixels at 100% zoom, including the edge buffer
   */
  public Dimension mapSize() {
    Rectangle r = new Rectangle(0, 0);
    for (int i = 0; i < boards.size(); ++i) {
      Board b = (Board) boards.elementAt(i);
      r = r.union(b.bounds());
    }
    r.width += edgeBuffer.width;
    r.height += edgeBuffer.height;
    r.width = Math.max(r.width, 200);
    r.height = Math.max(r.height, 200);
    return r.getSize();
  }

  /**
   * @return the nearest allowable point according to the {@link MapGrid} on the {@link Board} at this point
   *
   * @see Board#snapTo
   * @see MapGrid#snapTo */
  public Point snapTo(Point p) {
    Point snap = new Point(p);

    Board b = findBoard(p);
    if (b == null)
      return snap;

    Rectangle r = b.bounds();
    snap.translate(-r.x, -r.y);
    snap = b.snapTo(snap);
    snap.translate(r.x, r.y);
    return snap;
  }

  /** The buffer of empty space around the boards in the Map window,
   * in component coordinates at 100% zoom
   */
  public Dimension getEdgeBuffer() {
    return new Dimension(edgeBuffer);
  }

  /**
   * Translate a point from component coordinates (i.e. x,y position
   * on the JPanel) to map coordinates, i.e. accounting for zoom factor
   *
   * @see #componentCoordinates */
  public Point mapCoordinates(Point p1) {
    Point p = new Point(p1.x, p1.y);
    p.x /= getZoom();
    p.y /= getZoom();
    return p;
  }

  public Rectangle mapRectangle(Rectangle r) {
    r = new Rectangle(r);
    r.x /= getZoom();
    r.y /= getZoom();
    r.width /= getZoom();
    r.height /= getZoom();
    return r;
  }

  /**
   * Translate a point from map coordinates to component coordinates
   *
   * @see #mapCoordinates
   */
  public Point componentCoordinates(Point p1) {
    Point p = new Point(p1.x, p1.y);
    p.x *= getZoom();
    p.y *= getZoom();
    return p;
  }

  public Rectangle componentRectangle(Rectangle r) {
    r = new Rectangle(r);
    r.x *= getZoom();
    r.y *= getZoom();
    r.width *= getZoom();
    r.height *= getZoom();
    return r;
  }

  /**
   * @return a String name for the given location on the map
   *
   * @see Board#locationName
   */
  public String locationName(Point p) {
    String name = "offboard";
    Board b = findBoard(p);
    if (b != null) {
      name = b.locationName(new Point(p.x - b.bounds().x,
                                      p.y - b.bounds().y));
      if (name != null
          && boards.size() > 1
          && b.getName() != null) {
        name = b.getName() + name;
      }
    }
    return name;
  }

  public void focusGained(FocusEvent e) {
  }

  /**
   * When focus is lost, deselect all selected counters on this map.
   */
  public void focusLost(FocusEvent fe) {
    for (Enumeration e = KeyBuffer.getBuffer().getPieces();
         e.hasMoreElements();) {
      GamePiece p = (GamePiece) e.nextElement();
      if (p.getMap() == this) {
        KeyBuffer.getBuffer().remove(p);
      }
    }
    theMap.repaint();
  }

  /**
   * Because MouseEvents are received in component coordinates, it
   * is inconvenient for MouseListeners on the map to have to
   * translate to map coordinates.  MouseListeners added with this
   * method will receive mouse events with points already translated
   * into map coordinates */
  public void addLocalMouseListener(MouseListener l) {
    multicaster = AWTEventMulticaster.add(multicaster, l);
  }

  public void removeLocalMouseListener(MouseListener l) {
    multicaster = AWTEventMulticaster.remove(multicaster, l);
  }

  /**
   * MouseListeners on a map may be pushed and popped onto a stack.
   * Only the top listener on the stack receives mouse events */
  public void pushMouseListener(MouseListener l) {
    if (mouseListenerStack == null) {
      mouseListenerStack = new Vector();
    }
    mouseListenerStack.addElement(l);
  }

  /**
   * MouseListeners on a map may be pushed and popped onto a stack.
   * Only the top listener on the stack receives mouse events */
  public void popMouseListener() {
    mouseListenerStack.removeElement(mouseListenerStack.lastElement());
  }

  public void mouseEntered(MouseEvent e) {
  }

  public void mouseExited(MouseEvent e) {
  }

  /**
   * Mouse events are first translated into map coordinates.
   * Then the event is forwarded to the top MouseListener in the
   * stack, if any, otherwise forwarded to all LocalMouseListeners
   *
   * @see #pushMouseListener
   * @see #popMouseListener
   * @see #addLocalMouseListener*/
  public void mouseClicked(MouseEvent e) {
    if (mouseListenerStack != null && mouseListenerStack.size() > 0) {
      Point p = mapCoordinates(e.getPoint());
      e.translatePoint(p.x - e.getX(), p.y - e.getY());
      ((MouseListener) mouseListenerStack.lastElement()).mouseClicked(e);
    }
    else if (multicaster != null) {
      Point p = mapCoordinates(e.getPoint());
      e.translatePoint(p.x - e.getX(), p.y - e.getY());
      multicaster.mouseClicked(e);
    }
  }

  /**
   * Mouse events are first translated into map coordinates.
   * Then the event is forwarded to the top MouseListener in the
   * stack, if any, otherwise forwarded to all LocalMouseListeners
   *
   * @see #pushMouseListener
   * @see #popMouseListener
   * @see #addLocalMouseListener*/
  public void mousePressed(MouseEvent e) {
    if (mouseListenerStack != null && mouseListenerStack.size() > 0) {
      Point p = mapCoordinates(e.getPoint());
      e.translatePoint(p.x - e.getX(), p.y - e.getY());
      ((MouseListener) mouseListenerStack.lastElement()).mousePressed(e);
    }
    else if (multicaster != null) {
      Point p = mapCoordinates(e.getPoint());
      e.translatePoint(p.x - e.getX(), p.y - e.getY());
      multicaster.mousePressed(e);
    }
  }

  /**
   * Mouse events are first translated into map coordinates.
   * Then the event is forwarded to the top MouseListener in the
   * stack, if any, otherwise forwarded to all LocalMouseListeners
   *
   * @see #pushMouseListener
   * @see #popMouseListener
   * @see #addLocalMouseListener*/
  public void mouseReleased(MouseEvent e) {
    Point p = e.getPoint();
    p.translate(theMap.getLocation().x, theMap.getLocation().y);

    if (theMap.getBounds().contains(p)) {
      if (mouseListenerStack != null && mouseListenerStack.size() > 0) {
        p = mapCoordinates(e.getPoint());
        e.translatePoint(p.x - e.getX(), p.y - e.getY());
        ((MouseListener) mouseListenerStack.lastElement()).mouseReleased(e);
      }
      else if (multicaster != null) {
        p = mapCoordinates(e.getPoint());
        e.translatePoint(p.x - e.getX(), p.y - e.getY());
        multicaster.mouseReleased(e);
      }
      // Request Focus so that keyboard input will be recognized
      theMap.requestFocus();
    }

    // Clicking with mouse always repaints the map
    clearFirst = true;
    theMap.repaint();
  }

  /**
   * Mouse motion events are not forwarded to LocalMouseListeners or
   * to listeners on the stack */
  public void mouseMoved(MouseEvent e) {
  }

  /**
   * Mouse motion events are not forwarded to LocalMouseListeners or
   * to listeners on the stack
   *
   * The map scrolls when dragging the mouse near the edge */
  public void mouseDragged(MouseEvent e) {
    if (!e.isMetaDown()) {
      Point p = new Point
          (e.getX() - scroll.getViewport().getViewPosition().x,
           e.getY() - scroll.getViewport().getViewPosition().y);
      int xBuf = 15;
      int yBuf = 15;
      int dx = 0, dy = 0;
      if (p.x < xBuf
          && p.x >= 0)
        dx = -1;
      if (p.x >= scroll.getViewport().getSize().width - xBuf
          && p.x < scroll.getViewport().getSize().width)
        dx = 1;
      if (p.y < yBuf
          && p.y >= 0)
        dy = -1;
      if (p.y >= scroll.getViewport().getSize().height - yBuf
          && p.y < scroll.getViewport().getSize().height)
        dy = 1;

      if (dx != 0 || dy != 0) {
        scroll(dx * 25, dy * 25);
      }
    }
  }

  public void repaint(boolean cf) {
    clearFirst = cf;
    theMap.repaint();
  }


  /**
   * Painting the map is done in three steps: 1) draw each of the
   * {@link Board}s on the map.  2) draw all of the counters on the
   * map.  3) draw all of the {@link Drawable} components on the map
   *
   * @see #addDrawComponent
   * @see #setBoards
   * @see #addPiece
   */
  public void paint(Graphics g) {
    paint(g, 0, 0);
  }

  public void paintRegion(Graphics g, Rectangle visibleRect) {
    clearMapBorder(g); // To avoid ghost pieces around the edge

    drawBoardsInRegion(g, visibleRect);
    drawPiecesInRegion(g, visibleRect);
    drawDrawable(g);
  }

  public void drawBoardsInRegion(Graphics g, Rectangle visibleRect) {
    for (int i = 0; i < boards.size(); ++i) {
      Board b = (Board) boards.elementAt(i);
      b.drawRegion(g, componentCoordinates(b.bounds().getLocation()), visibleRect, getZoom(), theMap);
    }
  }


  public void repaint() {
    theMap.repaint();
  }

  public void drawPiecesInRegion(Graphics g, Rectangle visibleRect) {
    if (!hideCounters) {
      for (int i = 0; i < nstacks; ++i) {
        Point pt = componentCoordinates(stack[i].getPosition());
        if (stack[i] instanceof Stack) {
          getStackMetrics().draw((Stack) stack[i], pt, g, this, getZoom(), visibleRect);
        }
        else {
          stack[i].draw(g, pt.x, pt.y, theMap, getZoom());
          if (Boolean.TRUE.equals(stack[i].getProperty(Properties.SELECTED))) {
            highlighter.draw
                (stack[i], g, pt.x, pt.y, theMap, getZoom());
          }
        }
      }
    }
  }

  public void drawPieces(Graphics g, int xOffset, int yOffset) {
    if (!hideCounters) {
      for (int i = 0; i < nstacks; ++i) {
        Point pt = componentCoordinates(stack[i].getPosition());
        stack[i].draw(g, pt.x - xOffset, pt.y - yOffset, theMap, getZoom());
        if (Boolean.TRUE.equals(stack[i].getProperty(Properties.SELECTED))) {
          highlighter.draw
              (stack[i], g, pt.x - xOffset, pt.y - yOffset, theMap, getZoom());
        }
      }
    }
  }

  public void drawDrawable(Graphics g) {
    for (Enumeration e = drawComponents.elements();
         e.hasMoreElements();) {
      ((Drawable) e.nextElement()).draw(g, this);
    }
  }

  /**
   * Paint the map at the given offset, i.e. such that (xOffset, yOffset)
   * is in the upper left corner
   */
  public void paint(Graphics g, int xOffset, int yOffset) {
    clearMapBorder(g); // To avoid ghost pieces around the edge

    Point p = componentCoordinates(new Point(-xOffset, -yOffset));
    drawBoards(g, p.x, p.y, getZoom(), theMap);
    drawPieces(g, xOffset, yOffset);
    drawDrawable(g);
  }

  public Highlighter getHighlighter() {
    return highlighter;
  }

  public void setHighlighter(Highlighter h) {
    highlighter = h;
  }

  /**
   * @return an Enumeration of all {@link Board}s on the map */
  public Enumeration getAllBoards() {
    return boards.elements();
  }

  /**
   * Returns the boundingBox of a GamePiece accounting for
   * the offset of a piece within its parent stack
   *
   * @see GamePiece#boundingBox
   */
  public Rectangle boundingBoxOf(GamePiece p) {
    if (p.getMap() != this) {
      throw new RuntimeException("Piece is not on this map");
    }
    Rectangle r = p.boundingBox();
    if (Boolean.TRUE.equals(p.getProperty(Properties.INVISIBLE_TO_ME))) {
      r = new Rectangle(p.getPosition(), new Dimension(0, 0));
    }
    else {
      if (Boolean.TRUE.equals(p.getProperty(Properties.SELECTED))) {
        r = r.union(highlighter.boundingBox(p));
      }
      if (p.getParent() != null) {
        Point pt = getStackMetrics().relativePosition(p.getParent(), p);
        r.translate(pt.x, pt.y);
      }
    }
    return r;
  }

  /**
   * Returns the selection bounding box of a GamePiece accounting for
   * the offset of a piece within a stack
   *
   * @see GamePiece#selectionBounds
   */
  public Rectangle selectionBoundsOf(GamePiece p) {
    if (p.getMap() != this) {
      throw new RuntimeException("Piece is not on this map");
    }
    Rectangle r = p.selectionBounds();
    if (Boolean.TRUE.equals(p.getProperty(Properties.INVISIBLE_TO_ME))) {
      r = new Rectangle(p.getPosition(), new Dimension(0, 0));
    }
    else if (p.getParent() != null) {
      Point pt = getStackMetrics().relativePosition(p.getParent(), p);
      r.translate(pt.x, pt.y);
    }
    return r;
  }

  /**
   * @return an array of all GamePieces on the map.  This is a
   * read-only copy.  Altering the array does not alter the pieces
   * on the map.  */
  public GamePiece[] getPieces() {
    GamePiece p[] = new GamePiece[nstacks];
    System.arraycopy(stack, 0, p, 0, nstacks);
    return p;
  }

  protected void clearMapBorder(Graphics g) {
    if (clearFirst || boards.size() == 0) {
      g.clearRect(0, 0, theMap.getSize().width, theMap.getSize().height);
      clearFirst = false;
    }
    else {

      Dimension buffer = new Dimension((int) (getZoom() * edgeBuffer.width),
                                       (int) (getZoom() * edgeBuffer.height));
      g.clearRect(0, 0, buffer.width, theMap.getSize().height);
      g.clearRect(0, 0, theMap.getSize().width, buffer.height);
      g.clearRect(theMap.getSize().width - buffer.width, 0,
                  buffer.width, theMap.getSize().height);
      g.clearRect(0, theMap.getSize().height - buffer.height,
                  theMap.getSize().width, buffer.height);
    }
  }

  /**
   * Adjusts the bounds() rectangle to account for the Board's
   * relative position to other boards.  In other words, if Board A
   * is N pixels wide and Board B is to the right of Board A, then
   * the origin of Board B will be adjusted N pixels to the right.
   */
  protected void setBoardBoundaries() {
    for (int n = 0; n < boards.size(); ++n) {
      Board board = (Board) boards.elementAt(n);
      board.setLocation(0, 0);
      for (int i = 0; i < boards.size(); i++) {
        Board b = (Board) boards.elementAt(i);
        if (b.relativePosition().x < board.relativePosition().x

            && b.relativePosition().y == board.relativePosition().y) {
          board.translate(b.bounds().width, 0);
        }
        if (b.relativePosition().y < board.relativePosition().y
            && b.relativePosition().x == board.relativePosition().x) {
          board.translate(0, b.bounds().height);
        }
      }
      board.translate(edgeBuffer.width, edgeBuffer.height);
    }
    theMap.revalidate();
  }

  /**
   * Draw the boards of the map at the given point and zoom factor
   * onto the given Graphics object */
  public void drawBoards(Graphics g, int xoffset, int yoffset,
                         double zoom, Component obs) {
    for (int i = 0; i < boards.size(); ++i) {
      Board b = (Board) boards.elementAt(i);
      b.draw(g,
             xoffset + Math.round(b.bounds().x * (float) zoom),
             yoffset + Math.round(b.bounds().y * (float) zoom), zoom, obs);
    }
  }

  /**
   * Repaint the given area, specified in map coordinates
   */
  public void repaint(Rectangle r) {
    r.setLocation(componentCoordinates(new Point(r.x, r.y)));
    r.setSize((int) (r.width * getZoom()),
              (int) (r.height * getZoom()));
    theMap.repaint(r.x, r.y, r.width, r.height);
  }

  /**
   * @param show if true, enable drawing of GamePiece.  If false,
   * don't draw GamePiece when painting the map */
  public void setPiecesVisible(boolean show) {
    hideCounters = !show;
  }

  public boolean isPiecesVisible() {
    return !hideCounters;
  }

  /**
   * @return the top-level window containing this map
   */
  protected Window createParentFrame() {
    if (GlobalOptions.getInstance().isUseSingleWindow()) {
      JDialog d = new JDialog(GameModule.getGameModule().getFrame());
      d.setDefaultCloseOperation
          (WindowConstants.DO_NOTHING_ON_CLOSE);
      d.setTitle(getDefaultWindowTitle());
      return d;
    }
    else {
      JFrame d = new JFrame();
      d.setDefaultCloseOperation
          (WindowConstants.DO_NOTHING_ON_CLOSE);
      d.setTitle(getDefaultWindowTitle());
      return d;
    }
  }

  public boolean shouldDockIntoMainWindow() {
    boolean shouldDock = false;
    if (GlobalOptions.getInstance().isUseSingleWindow()
      && !launchButton.isVisible()) {
      shouldDock = true;
      for (Enumeration e = GameModule.getGameModule().getComponents(Map.class); e.hasMoreElements();) {
        Map m = (Map) e.nextElement();
        if (m == this) {
          break;
        }
        if (m.shouldDockIntoMainWindow()) {
          shouldDock = false;
          break;
        }
      }
    }
    return shouldDock;
  }

  /**
   * When a game is started, create a top-level window, if none
   * exists.  When a game is ended, remove all boards from the map
   *
   * @see GameComponent */
  public void setup(boolean show) {
    if (show) {
      if (shouldDockIntoMainWindow()) {
        mainWindowDock.showComponent();
        if (toolBar.getParent() == null) {
          GameModule.getGameModule().getToolBar().addSeparator();
          GameModule.getGameModule().getToolBar().add(toolBar);
        }
        toolBar.setVisible(true);
      }
      else {
        if (SwingUtilities.getWindowAncestor(theMap) == null) {
          final Window topWindow = createParentFrame();
          topWindow.addWindowListener
              (new WindowAdapter() {
                public void windowClosing(WindowEvent e) {
                  if (launchButton.isVisible()) {
                    topWindow.setVisible(false);
                  }
                  else {
                    GameModule.getGameModule()
                        .getGameState().setup(false);
                  }
                }
              });
          ((RootPaneContainer)topWindow).getContentPane().add("North", getToolBar());
          ((RootPaneContainer)topWindow).getContentPane().add("Center", scroll);
          topWindow.setSize(600, 400);
          PositionOption option = new PositionOption(PositionOption.key + getId(), topWindow);
          GameModule.getGameModule().getPrefs().addOption(option);
        }
        theMap.getTopLevelAncestor().setVisible(!launchButton.isVisible());
        theMap.revalidate();
      }
    }
    else {
      nstacks = 0;
      boards.removeAllElements();
      System.gc();
      if (mainWindowDock != null) {
        mainWindowDock.hideComponent();
        toolBar.setVisible(false);
      }
      else if (theMap.getTopLevelAncestor() != null) {
        theMap.getTopLevelAncestor().setVisible(false);
      }
    }
    launchButton.setEnabled(show);
  }

  public void appendToTitle(String s) {
    if (mainWindowDock == null) {
      Component c = theMap.getTopLevelAncestor();
      if (s == null) {
        if (c instanceof JFrame) {
          ((JFrame)c).setTitle(getDefaultWindowTitle());
        }
        if (c instanceof JDialog) {
          ((JDialog)c).setTitle(getDefaultWindowTitle());
        }
      }
      else {
        if (c instanceof JFrame) {
          ((JFrame)c).setTitle(((JFrame)c).getTitle() + s);
        }
        if (c instanceof JDialog) {
          ((JDialog)c).setTitle(((JDialog)c).getTitle() + s);
        }
      }
    }
  }

  protected String getDefaultWindowTitle() {
    return getMapName().length() > 0 ? getMapName()
        : GameModule.getGameModule().getGameName() + " map";
  }

  public GamePiece findPiece(Point pt, PieceFinder finder) {
    for (int i = nstacks - 1; i >= 0; --i) {
      GamePiece p = finder.select(this, stack[i], pt);
      if (p != null) {
        return p;
      }
    }
    return null;
  }

  /**
   * Place a piece at the destination point.  If necessary,
   * remove the piece from its parent Stack or Map
   *
   * @return a {@link Command} that reproduces this action
   */
  public Command placeAt(GamePiece piece, Point pt) {
    Command c = null;
    if (GameModule.getGameModule().getGameState().getPieceForId(piece.getId()) == null) {
      piece.setPosition(pt);
      addPiece(piece);
      GameModule.getGameModule().getGameState().addPiece(piece);
      c = new AddPiece(piece);
    }
    else {
      MoveTracker tracker = new MoveTracker(piece);
      piece.setPosition(pt);
      addPiece(piece);
      c = tracker.getMoveCommand();
    }
    return c;
/*
    Command comm = null;
    String oldState = null;
    String oldParentState = null;
    Stack oldParent = null;
    if (GameModule.getGameModule().getGameState()
      .getPieceForId(piece.getId()) != null) {
      oldState = piece.getState();
    }
    if (piece.getParent() != null) {
      oldParent = piece.getParent();
      oldParentState = piece.getParent().getState();
    }
    piece.setPosition(pt);
    addPiece(piece);
    if (oldState != null) {
      comm = new ChangePiece(piece.getId(), oldState, piece.getState());
    }
    else {
      GameModule.getGameModule().getGameState().addPiece(piece);
      comm = new AddPiece(piece);
    }
    if (oldParent != null) {
      if (oldParent.getPieceCount() == 0) {
        removePiece(oldParent);
        oldParent.setMap(null);
        comm = comm.append(new RemovePiece(oldParent));
      }
      else {
        comm = comm.append(new ChangePiece(oldParent.getId(), oldParentState, oldParent.getState()));
      }
    }
    return comm;
*/
  }

  /**
   * Move a piece to the destination point.  If a piece is at the point
   * (i.e. has a location exactly equal to it), merge with the piece
   * by forwarding to {@link StackMetrics#merge}.
   * Otherwise, place by forwarding to placeAt()
   *
   * @see StackMetrics#merge
   */
  public Command placeOrMerge(GamePiece p, Point pt) {
    for (int i = 0; i < nstacks; ++i) {
      if (stack[i].getPosition().equals(pt)) {
        return getStackMetrics().merge(stack[i], p);
      }
    }
    return placeAt(p, pt);
  }

  /**
   * Adds a GamePiece to this map.  Removes the piece from its
   * parent Stack and from its current map, if different from this
   * map */
  public void addPiece(GamePiece p) {
    if (indexOf(p) >= 0) {
      p.setMap(this); // Moves Immobilized pieces to the back
      return;
    }
    if (p.getParent() != null) {
      p.getParent().remove(p);
    }
    if (p.getMap() != null
        && p.getMap() != this) {
      p.getMap().removePiece(p);
    }
    if (nstacks >= maxStacks) {
      maxStacks += moreStacks;
      GamePiece oldStack[] = stack;
      stack = new GamePiece[maxStacks];
      System.arraycopy(oldStack, 0, stack, 0, nstacks);
    }
    stack[nstacks++] = p;
    p.setParent(null);
    p.setMap(this);
    //	repaint(boundingBoxOf(p));
    theMap.repaint();
  }

  /**
   * Reorder the argument GamePiece to the new index.  When painting
   * the map, pieces are drawn in order of index */
  public void reposition(GamePiece s, int pos) {
    int index = indexOf(s);
    if (index < 0) {
      throw new IllegalArgumentException("Piece is not on this map");
    }
    for (int i = index; i < nstacks - 1; ++i) {
      stack[i] = stack[i + 1];
    }
    for (int i = nstacks - 1; i > pos; --i) {
      stack[i] = stack[i - 1];
    }
    stack[pos] = s;
  }

  /**
   * Returns the index of a piece.  When painting the map, pieces
   * are drawn in order of index */
  public int indexOf(GamePiece s) {
    for (int i = 0; i < nstacks; ++i)
      if (stack[i] == s)
        return (i);
    return (-1);
  }

  /**
   * Removes a piece from the map
   */
  public void removePiece(GamePiece s) {
    int n = indexOf(s);
    if (n > -1) {
      Rectangle r = boundingBoxOf(s);
      removePiece(n);
      repaint(r);
    }
  }

  private void removePiece(int gone) {
    for (int i = gone; i < nstacks - 1; ++i)
      stack[i] = stack[i + 1];
    nstacks--;
  }

  /** Center the map at given map coordinates within its JScrollPane
   container*/
  public void centerAt(Point p) {
    centerAt(p, 0, 0);
  }

  /**
   * Center the map at the given map coordinates, if the point is not
   * already within (dx,dy) of the center
   */
  public void centerAt(Point p, int dx, int dy) {
    if (scroll != null) {
      p = componentCoordinates(p);
      p.translate(-scroll.getViewport().getSize().width / 2,
                  -scroll.getViewport().getSize().height / 2);
      Rectangle r = new Rectangle(p, scroll.getViewport().getSize());
      r.width = dx > r.width ? 0 : r.width - dx;
      r.height = dy > r.height ? 0 : r.height - dy;
      theMap.scrollRectToVisible(r);
    }
  }

  /** Ensure that the given region (in map coordinates) is visible */
  public void ensureVisible(Rectangle r) {
    if (scroll != null) {
      Point p = componentCoordinates(r.getLocation());
      r = new Rectangle(p.x, p.y, (int) (getZoom() * r.width), (int) (getZoom() * r.height));
      theMap.scrollRectToVisible(r);
    }
  }

  /**
   * Scrolls the map in the containing JScrollPane
   * @param dx number of pixels to scroll horizontally
   * @param dy number of pixels to scroll vertically
   */
  public void scroll(int dx, int dy) {
    Rectangle r = new Rectangle(scroll.getViewport().getViewRect());
    r.translate(dx, dy);
    r = r.intersection(new Rectangle(new Point(0, 0),
                                     getPreferredSize()));
    theMap.scrollRectToVisible(r);
  }

  public static String getConfigureTypeName() {
    return "Map Window";
  }

  public String getMapName() {
    return mapName;
  }

  public void setMapName(String s) {
    mapName = s;
    setConfigureName(mapName);
    launchButton.setToolTipText(s != null ? "Show/hide " + s + " window" : "Show/hide Map window");
  }

  public HelpFile getHelpFile() {
    File dir = new File("docs");
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "Map.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Map Name", "Mark pieces that move (if they possess the proper trait)", "Horizontal Padding", "Vertical Padding", "Can contain multiple boards",
                        "Border color for selected counters", "Border thickness for selected counters",
                        "Include toolbar button to show/hide", "Toolbar button name", "Hotkey"};
  }

  public String[] getAttributeNames() {
    return new String[]{NAME, MARK_MOVED, EDGE_WIDTH, EDGE_HEIGHT, ALLOW_MULTIPLE, HIGHLIGHT_COLOR, HIGHLIGHT_THICKNESS, USE_LAUNCH_BUTTON, BUTTON_NAME, HOTKEY};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class, GlobalOptions.Prompt.class, Integer.class, Integer.class, Boolean.class, Color.class, Integer.class, Boolean.class, String.class, KeyStroke.class};
  }

  public Class[] getAllowableConfigureComponents() {
    Class[] c = {GlobalMap.class, LOS_Thread.class,
                 Zoomer.class, CounterDetailViewer.class, ImageSaver.class, TextSaver.class, DrawPile.class, MassKeyCommand.class};
    return c;
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (visibilityCondition == null) {
      visibilityCondition = new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return launchButton.isVisible();
        }
      };
    }
    if (HOTKEY.equals(name)) {
      return visibilityCondition;
    }
    else if (BUTTON_NAME.equals(name)) {
      return visibilityCondition;
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  /**
   * Each Map must have a unique String id
   */
  public void setID(String id) {
    mapID = id;
  }

  public static Map getMapById(String id) {
    Map map = null;
    for (Enumeration e = GameModule.getGameModule().getComponents(Map.class);
         e.hasMoreElements();) {
      Map m = (Map) e.nextElement();
      if (m.getId().equals(id)) {
        map = m;
        break;
      }
    }
    return map;
  }

  /**
   * Each Map must have a unique String id
   *
   * @return the id for this map
   */
  public String getId() {
    return mapID;
  }

  /** Return the AWT component representing the map */
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

  /**
   * The component that represents the map itself
   */
  public static class View extends JPanel {
    protected Map map;

    public View(Map m) {
      map = m;
    }

    public void paint(Graphics g) {
      Rectangle r = getVisibleRect();
      g.clearRect(r.x,r.y,r.width,r.height);
      map.paintRegion(g, r);
//      map.paint(g);
    }

    public void update(Graphics g) {
      // To avoid flicker, don't clear the display first *
      paint(g);
    }

    public boolean isManagingFocus() {
      return true;
    }

    public Dimension getPreferredSize() {
      return map.getPreferredSize();
    }

    public Map getMap() {
      return map;
    }
  }
}
