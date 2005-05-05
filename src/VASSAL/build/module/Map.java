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

import VASSAL.Info;
import VASSAL.build.*;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.*;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.command.AddPiece;
import VASSAL.command.Command;
import VASSAL.command.MoveTracker;
import VASSAL.configure.*;
import VASSAL.counters.*;
import VASSAL.preferences.PositionOption;
import VASSAL.tools.ComponentSplitter;
import VASSAL.tools.KeyStrokeSource;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.UniqueIdManager;
import org.w3c.dom.Element;

import javax.swing.*;
import java.awt.*;
import java.awt.dnd.*;
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
    FocusListener, MouseListener, MouseMotionListener, DropTargetListener,
    Configurable, UniqueIdManager.Identifyable {

  private String mapID = "";
  private String mapName = "";

  private static final String MAIN_WINDOW_HEIGHT = "mainWindowHeight";
  private static UniqueIdManager idMgr = new UniqueIdManager("Map");

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
  protected String markMovedOption = GlobalOptions.ALWAYS;
  protected String markUnmovedIcon;

  protected MouseListener multicaster = null;
  protected Vector mouseListenerStack = new Vector();

  protected Vector boards = new Vector();

  protected PieceCollection pieces = new DefaultPieceCollection();

  protected Highlighter highlighter = new ColoredBorder();

  private boolean clearFirst = false;  //  Whether to clear the display before drawing the map
  private boolean hideCounters = false;//  Option to hide counters to see map
  private boolean allowMultiple = false;
  private VisibilityCondition visibilityCondition;
  private DragGestureListener dragGestureListener;
  private String moveWithinFormat;
  private String moveToFormat;
  private String createFormat;
  private String changeFormat = "$" + MESSAGE + "$";

  public Map() {
    getView();
    theMap.addMouseListener(this);
    theMap.addMouseMotionListener(this);
    theMap.addFocusListener(this);
    toolBar.setFloatable(false);
  }

  public static final String NAME = "mapName";
  public static final String MARK_MOVED = "markMoved";
  public static final String MARK_UNMOVED_ICON = "markUnmovedIcon";
  public static final String EDGE_WIDTH = "edgeWidth";
  public static final String EDGE_HEIGHT = "edgeHeight";
  public static final String HIGHLIGHT_COLOR = "color";
  public static final String HIGHLIGHT_THICKNESS = "thickness";
  public static final String ALLOW_MULTIPLE = "allowMultiple";
  public static final String USE_LAUNCH_BUTTON = "launch";
  public static final String BUTTON_NAME = "buttonName";
  public static final String ICON = "icon";
  public static final String HOTKEY = "hotkey";
  public static final String SUPPRESS_AUTO = "suppressAuto";
  public static final String MOVE_WITHIN_FORMAT = "moveWithinFormat";
  public static final String MOVE_TO_FORMAT = "moveToFormat";
  public static final String CREATE_FORMAT = "createFormat";
  public static final String CHANGE_FORMAT = "changeFormat";

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setMapName((String) value);
    }
    else if (MARK_MOVED.equals(key)) {
      markMovedOption = (String) value;
    }
    else if (MARK_UNMOVED_ICON.equals(key)) {
      markUnmovedIcon = (String) value;
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
    else if (SUPPRESS_AUTO.equals(key)) {
      if (value instanceof String) {
        value = new Boolean((String) value);
      }
      if (Boolean.TRUE.equals(value)) {
        moveWithinFormat = "";
      }
    }
    else if (MOVE_WITHIN_FORMAT.equals(key)) {
      moveWithinFormat = (String) value;
    }
    else if (MOVE_TO_FORMAT.equals(key)) {
      moveToFormat = (String) value;
    }
    else if (CREATE_FORMAT.equals(key)) {
      createFormat = (String) value;
    }
    else if (CHANGE_FORMAT.equals(key)) {
      changeFormat = (String) value;
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
    else if (MARK_UNMOVED_ICON.equals(key)) {
      return markUnmovedIcon;
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
    else if (MOVE_WITHIN_FORMAT.equals(key)) {
      return getMoveWithinFormat();
    }
    else if (MOVE_TO_FORMAT.equals(key)) {
      return getMoveToFormat();
    }
    else if (CREATE_FORMAT.equals(key)) {
      return getCreateFormat();
    }
    else if (CHANGE_FORMAT.equals(key)) {
      return getChangeFormat();
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
    launchButton = new LaunchButton("Map", BUTTON_NAME, HOTKEY, ICON, al);
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
    if (this.picker != null) {
      GameModule.getGameModule().removeCommandEncoder(picker);
      GameModule.getGameModule().getGameState().addGameComponent(picker);
    }
    this.picker = picker;
    if (picker != null) {
      picker.setAllowMultiple(allowMultiple);
      GameModule.getGameModule().addCommandEncoder(picker);
      GameModule.getGameModule().getGameState().addGameComponent(picker);
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
    idMgr.add(this);

    validator = new CompoundValidityChecker
        (new MandatoryComponent(this, BoardPicker.class),
         new MandatoryComponent(this, StackMetrics.class))
        .append(idMgr);

    if (Info.isDndEnabled()) {
      DragGestureListener dgl = new DragGestureListener() {
        public void dragGestureRecognized(DragGestureEvent dge) {
          if (mouseListenerStack.size() == 0
              && dragGestureListener != null) {
            dragGestureListener.dragGestureRecognized(dge);
          }
        }
      };
      DragSource.getDefaultDragSource().createDefaultDragGestureRecognizer(theMap, DnDConstants.ACTION_MOVE, dgl);
      theMap.setDropTarget(PieceMover.DragHandler.makeDropTarget(theMap, DnDConstants.ACTION_MOVE, this));
    }
    else {
      DragBuffer.getBuffer().addDropTarget(theMap, this);
      DragBuffer.getBuffer().addDragSource(theMap);
    }

    GameModule.getGameModule().getGameState().addGameComponent(this);
    GameModule.getGameModule().getToolBar().add(launchButton);

    if (shouldDockIntoMainWindow()) {
      IntConfigurer config = new IntConfigurer(MAIN_WINDOW_HEIGHT, null, new Integer(-1));
      GameModule.getGameModule().getGlobalPrefs().addOption(null, config);
      JPanel root = new JPanel(new BorderLayout());
      root.add(scroll, BorderLayout.CENTER);
      ComponentSplitter splitter = new ComponentSplitter();
      mainWindowDock = splitter.splitBottom(splitter.getSplitAncestor(GameModule.getGameModule().getControlPanel(), -1), root, true);
      GameModule.getGameModule().addKeyStrokeSource
          (new KeyStrokeSource(theMap, JComponent.WHEN_FOCUSED));
    }
    else {
      GameModule.getGameModule().addKeyStrokeSource
          (new KeyStrokeSource(theMap, JComponent.WHEN_IN_FOCUSED_WINDOW));
    }
  }

  public void removeFrom(Buildable b) {
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    Window w = SwingUtilities.getWindowAncestor(theMap);
    if (w != null) {
      w.dispose();
    }
    GameModule.getGameModule().getToolBar().remove(launchButton);
    idMgr.remove(this);
    if (picker != null) {
      GameModule.getGameModule().removeCommandEncoder(picker);
      GameModule.getGameModule().getGameState().addGameComponent(picker);
    }
  }

  /**
   * Set the boards for this map.  Each map may contain more than
   * one {@link Board} */
  public synchronized void setBoards(Enumeration boardList) {
    boards.removeAllElements();
    System.gc();
    while (boardList.hasMoreElements()) {
      Board board = (Board) boardList.nextElement();
      board.setMap(this);
      boards.addElement(board);
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
  public synchronized Dimension mapSize() {
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
   * @return the nearest allowable point according to the {@link VASSAL.build.module.map.boardPicker.board.MapGrid} on the {@link Board} at this point
   *
   * @see Board#snapTo
   * @see VASSAL.build.module.map.boardPicker.board.MapGrid#snapTo */
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

  /**
   *
   * @param p
   * @return false if this point is in a no-go area
   */
  public boolean isSnappable(Point p) {
    return true;
    // TODO re-enable this when the Board class is updated
/*
    Board b = findBoard(p);
    if (b == null) {
      return true;
    }
    return b.isSnappable(p);
*/
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
    String loc = getDeckNameAt(p);
    if (loc == null) {
      Board b = findBoard(p);
      if (b != null) {
        loc = b.locationName(new Point(p.x - b.bounds().x,
                                       p.y - b.bounds().y));
      }
    }
    if (loc == null) {
      loc = "offboard";
    }
    return loc;
  }

  /**
   * @return a String name for the given location on the map. Include Map name
   *         if requested. Report deck name instead of location if point is inside
   *         the bounds of a deck. Do not include location if this map is not visible
   *         to all players.
   */
  public String getFullLocationName(Point p, boolean includeMap) {
    String loc = "";

    if (includeMap && getMapName() != null && getMapName().length() > 0) {
      loc = "[" + getMapName() + "]";
    }

    if (isVisibleToAll() && p != null) {
      String pos = getDeckNameContaining(p);
      if (pos == null) {
        if (locationName(p) != null) {
          loc = locationName(p) + loc;
        }
      }
      else {
        loc = pos;
      }
    }
    return loc;
  }

  /**
   * Is this map visible to all players
   */
  public boolean isVisibleToAll() {
    if (this instanceof PrivateMap) {
      if (!getAttributeValueString(PrivateMap.VISIBLE).equals("true")) {
        return false;
      }
    }
    return true;
  }

  /**
   * Return the name of the deck whose bounding box contains p
   */
  public String getDeckNameContaining(Point p) {
    String deck = null;
    if (p != null) {
      Enumeration e = getComponents(DrawPile.class);
      while (e.hasMoreElements()) {
        DrawPile d = (DrawPile) e.nextElement();
        if (d.boundingBox().contains(p)) {
          deck = d.getConfigureName();
          break;
        }
      }
    }
    return deck;
  }

  /**
   * Return the name of the deck whose position is p
   * @param p
   * @return
   */
  public String getDeckNameAt(Point p) {
    String deck = null;
    if (p != null) {
      Enumeration e = getComponents(DrawPile.class);
      while (e.hasMoreElements()) {
        DrawPile d = (DrawPile) e.nextElement();
        if (d.getPosition().equals(p)) {
          deck = d.getConfigureName();
          break;
        }
      }
    }
    return deck;
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
    if (mouseListenerStack.size() > 0) {
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
    if (mouseListenerStack.size() > 0) {
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
      if (mouseListenerStack.size() > 0) {
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
   * This listener will be notified when a drag event is initiated, assuming that no MouseListeners are on the stack.
   * @see #pushMouseListener
   * @param dragGestureListener
   */
  public void setDragGestureListener(DragGestureListener dragGestureListener) {
    this.dragGestureListener = dragGestureListener;
  }

  public DragGestureListener getDragGestureListener() {
    return dragGestureListener;
  }

  public void dragEnter(DropTargetDragEvent dtde) {
  }

  public void dragOver(DropTargetDragEvent dtde) {
    scrollAtEdge(dtde.getLocation(), 15);
  }

  public void dropActionChanged(DropTargetDragEvent dtde) {
  }

  public void dragExit(DropTargetEvent dte) {
  }

  public void drop(DropTargetDropEvent dtde) {
    if (dtde.getDropTargetContext().getComponent() == theMap) {
      MouseEvent evt = new MouseEvent(theMap, MouseEvent.MOUSE_RELEASED, System.currentTimeMillis(), 0, dtde.getLocation().x, dtde.getLocation().y, 1, false);
      theMap.dispatchEvent(evt);
    }
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
      scrollAtEdge(e.getPoint(), 15);
    }
  }

  /**
   * Scoll map so that the argument point is at least a certain distance from the visible edte
   * @param evtPt
   */
  public void scrollAtEdge(Point evtPt, int dist) {
    Point p = new Point
        (evtPt.x - scroll.getViewport().getViewPosition().x,
         evtPt.y - scroll.getViewport().getViewPosition().y);
    int dx = 0, dy = 0;
    if (p.x < dist
        && p.x >= 0)
      dx = -1;
    if (p.x >= scroll.getViewport().getSize().width - dist
        && p.x < scroll.getViewport().getSize().width)
      dx = 1;
    if (p.y < dist
        && p.y >= 0)
      dy = -1;
    if (p.y >= scroll.getViewport().getSize().height - dist
        && p.y < scroll.getViewport().getSize().height)
      dy = 1;

    if (dx != 0 || dy != 0) {
      scroll(2 * dist * dx, 2 * dist * dy);
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
      GamePiece[] stack = pieces.getPieces();
      for (int i = 0; i < stack.length; ++i) {
        Point pt = componentCoordinates(stack[i].getPosition());
        if (stack[i].getClass() == Stack.class) {
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
      GamePiece[] stack = pieces.getPieces();
      for (int i = 0; i < stack.length; ++i) {
        Point pt = componentCoordinates(stack[i].getPosition());
        stack[i].draw(g, pt.x + xOffset, pt.y + yOffset, theMap, getZoom());
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

    drawBoards(g, xOffset, yOffset, getZoom(), theMap);
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

  public int getBoardCount() {
    return boards.size();
  }

  /**
   * Returns the boundingBox of a GamePiece accounting for
   * the offset of a piece within its parent stack.
   * Return null if this piece is not on the map
   *
   * @see GamePiece#boundingBox
   */
  public Rectangle boundingBoxOf(GamePiece p) {
    Rectangle r = null;
    if (p.getMap() == this) {
      r = p.boundingBox();
      Point pos = p.getPosition();
      r.translate(pos.x, pos.y);
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
   * @see GamePiece#getShape
   */
  public Rectangle selectionBoundsOf(GamePiece p) {
    if (p.getMap() != this) {
      throw new RuntimeException("Piece is not on this map");
    }
    Rectangle r = p.getShape().getBounds();
    r.translate(p.getPosition().x, p.getPosition().y);
    if (p.getParent() != null) {
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
    return pieces.getPieces();
  }

  public void setPieceCollection(PieceCollection pieces) {
    this.pieces = pieces;
  }

  public PieceCollection getPieceCollection() {
    return pieces;
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
        int height = ((Integer) GameModule.getGameModule().getGlobalPrefs().getValue(MAIN_WINDOW_HEIGHT)).intValue();
        if (height > 0) {
          Container top = mainWindowDock.getTopLevelAncestor();
          top.setSize(top.getSize().width, height);
        }
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
          ((RootPaneContainer) topWindow).getContentPane().add("North", getToolBar());
          ((RootPaneContainer) topWindow).getContentPane().add("Center", scroll);
          topWindow.setSize(600, 400);
          PositionOption option = new PositionOption(PositionOption.key + getIdentifier(), topWindow);
          GameModule.getGameModule().getPrefs().addOption(option);
        }
        theMap.getTopLevelAncestor().setVisible(!launchButton.isVisible());
        theMap.revalidate();
      }
    }
    else {
      pieces.clear();
      boards.removeAllElements();
      System.gc();
      if (mainWindowDock != null) {
        if (mainWindowDock.getHideableComponent().isVisible()) {
          GameModule.getGameModule().getGlobalPrefs().getOption(MAIN_WINDOW_HEIGHT).setValue(new Integer(mainWindowDock.getTopLevelAncestor().getSize().height));
        }
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
          ((JFrame) c).setTitle(getDefaultWindowTitle());
        }
        if (c instanceof JDialog) {
          ((JDialog) c).setTitle(getDefaultWindowTitle());
        }
      }
      else {
        if (c instanceof JFrame) {
          ((JFrame) c).setTitle(((JFrame) c).getTitle() + s);
        }
        if (c instanceof JDialog) {
          ((JDialog) c).setTitle(((JDialog) c).getTitle() + s);
        }
      }
    }
  }

  protected String getDefaultWindowTitle() {
    return getMapName().length() > 0 ? getMapName()
        : GameModule.getGameModule().getGameName() + " map";
  }

  public GamePiece findPiece(Point pt, PieceFinder finder) {
    GamePiece[] stack = pieces.getPieces();
    for (int i = stack.length - 1; i >= 0; --i) {
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
  }

  /**
   * Apply the provided {@link PieceVisitorDispatcher}
   * to all pieces on this map.  Returns the first non-null {@link Command}
   * returned by <code>commandFactory</code>
   *
   * @param commandFactory
   *
   */
  public Command apply(PieceVisitorDispatcher commandFactory) {
    GamePiece[] stack = pieces.getPieces();
    Command c = null;
    for (int i = 0; i < stack.length && c == null; ++i) {
      c = (Command) commandFactory.accept(stack[i]);
    }
    return c;

  }

  /**
   * Move a piece to the destination point.  If a piece is at the point
   * (i.e. has a location exactly equal to it), merge with the piece
   * by forwarding to {@link StackMetrics#merge}.
   * Otherwise, place by forwarding to placeAt()
   *
   * @see StackMetrics#merge
   */
  public Command placeOrMerge(final GamePiece p, final Point pt) {
    PieceVisitorDispatcher dispatch = new DeckVisitorDispatcher(new DeckVisitor() {
      public Object visitDeck(Deck d) {
        if (d.getPosition().equals(pt)) {
          return getStackMetrics().merge(d, p);
        }
        else {
          return null;
        }
      }

      public Object visitStack(Stack s) {
        if (s.getPosition().equals(pt)
            && getStackMetrics().isStackingEnabled()
            && !Boolean.TRUE.equals(p.getProperty(Properties.NO_STACK))
            && s.topPiece() != null) {
          return getStackMetrics().merge(s, p);
        }
        else {
          return null;
        }
      }

      public Object visitDefault(GamePiece piece) {
        if (piece.getPosition().equals(pt)
            && getStackMetrics().isStackingEnabled()
            && !Boolean.TRUE.equals(p.getProperty(Properties.NO_STACK))
            && !Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))
            && !Boolean.TRUE.equals(piece.getProperty(Properties.NO_STACK))) {
          return getStackMetrics().merge(piece, p);
        }
        else {
          return null;
        }
      }
    });
    Command c = apply(dispatch);
    if (c == null) {
      c = placeAt(p, pt);
    }
    return c;
  }

  /**
   * Adds a GamePiece to this map.  Removes the piece from its
   * parent Stack and from its current map, if different from this
   * map */
  public void addPiece(GamePiece p) {
    if (indexOf(p) < 0) {
      if (p.getParent() != null) {
        p.getParent().remove(p);
        p.setParent(null);
      }
      if (p.getMap() != null
          && p.getMap() != this) {
        p.getMap().removePiece(p);
      }
      pieces.add(p);
      p.setMap(this);
      theMap.repaint();
    }
  }

  /**
   * Reorder the argument GamePiece to the new index.  When painting
   * the map, pieces are drawn in order of index
   * @deprecated use {@link PieceCollection#moveToFront} */
  public void reposition(GamePiece s, int pos) {
  }

  /**
   * Returns the index of a piece.
   * When painting the map, pieces are drawn in order of index
   * Return -1 if the piece is not on this map
   */
  public int indexOf(GamePiece s) {
    return pieces.indexOf(s);
  }

  /**
   * Removes a piece from the map
   */
  public void removePiece(GamePiece p) {
    pieces.remove(p);
    theMap.repaint();
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
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "Map.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Map Name",
                        "Mark pieces that move (if they possess the proper trait)",
                        "\"Mark unmoved\" button icon",
                        "Horizontal Padding",
                        "Vertical Padding",
                        "Can contain multiple boards",
                        "Border color for selected counters",
                        "Border thickness for selected counters",
                        "Include toolbar button to show/hide",
                        "Toolbar button name",
                        "Toolbar button icon",
                        "Hotkey",
                        "Auto-report format for movement within this map",
                        "Auto-report format for movement to this map",
                        "Auto-report format for units created in this map",
                        "Auto-report format for units modified on this map"};
  }

  public String[] getAttributeNames() {
    return new String[]{NAME,
                        MARK_MOVED,
                        MARK_UNMOVED_ICON,
                        EDGE_WIDTH,
                        EDGE_HEIGHT,
                        ALLOW_MULTIPLE,
                        HIGHLIGHT_COLOR,
                        HIGHLIGHT_THICKNESS,
                        USE_LAUNCH_BUTTON,
                        BUTTON_NAME,
                        ICON,
                        HOTKEY,
                        MOVE_WITHIN_FORMAT,
                        MOVE_TO_FORMAT,
                        CREATE_FORMAT,
                        CHANGE_FORMAT,
                        SUPPRESS_AUTO};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class,
                       GlobalOptions.Prompt.class,
                       UnmovedIconConfig.class,
                       Integer.class,
                       Integer.class,
                       Boolean.class,
                       Color.class,
                       Integer.class,
                       Boolean.class,
                       String.class,
                       IconConfig.class,
                       KeyStroke.class,
                       MoveWithinFormatConfig.class,
                       MoveToFormatConfig.class,
                       CreateFormatConfig.class,
                       ChangeFormatConfig.class};
  }

  public static final String LOCATION = "location";
  public static final String OLD_LOCATION = "previousLocation";
  public static final String OLD_MAP = "previousMap";
  public static final String MAP_NAME = "mapName";
  public static final String PIECE_NAME = "pieceName";
  public static final String MESSAGE = "message";

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "/images/map.gif");
    }
  }

  public static class UnmovedIconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "/images/unmoved.gif");
    }
  }

  public static class MoveWithinFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[]{PIECE_NAME,
                                                                           LOCATION,
                                                                           MAP_NAME,
                                                                           OLD_LOCATION});
    }
  }

  public static class MoveToFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[]{PIECE_NAME,
                                                                           LOCATION,
                                                                           OLD_MAP,
                                                                           MAP_NAME,
                                                                           OLD_LOCATION});
    }
  }

  public static class CreateFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[]{PIECE_NAME,
                                                                           MAP_NAME,
                                                                           LOCATION});
    }
  }

  public static class ChangeFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[]{MESSAGE,
                                                                           ReportState.COMMAND_NAME,
                                                                           ReportState.OLD_UNIT_NAME,
                                                                           ReportState.NEW_UNIT_NAME,
                                                                           ReportState.MAP_NAME,
                                                                           ReportState.LOCATION_NAME});
    }
  }

  public String getCreateFormat() {
    if (createFormat != null) {
      return createFormat;
    }
    else {
      String val = "$" + PIECE_NAME + "$ created in $" + LOCATION + "$";
      if (boards.size() > 0) {
        Board b = (Board) boards.firstElement();
        if (b.getGrid() == null
            || b.getGrid().getGridNumbering() == null) {
          val = "";
        }
      }
      return val;
    }
  }

  public String getChangeFormat() {
    return changeFormat;
  }

  public String getMoveToFormat() {
    if (moveToFormat != null) {
      return moveToFormat;
    }
    else {
      String val = "$" + PIECE_NAME + "$" + " moves $" + OLD_LOCATION + "$ -> $" + LOCATION + "$ *";
      if (boards.size() > 0) {
        Board b = (Board) boards.firstElement();
        if (b.getGrid() == null
            || b.getGrid().getGridNumbering() != null) {
          val = "";
        }
      }
      return val;
    }
  }

  public String getMoveWithinFormat() {
    if (moveWithinFormat != null) {
      return moveWithinFormat;
    }
    else {
      String val = "$" + PIECE_NAME + "$" + " moves $" + OLD_LOCATION + "$ -> $" + LOCATION + "$ *";
      if (boards.size() > 0) {
        Board b = (Board) boards.firstElement();
        if (b.getGrid() == null) {
          val = "";
        }
      }
      return val;
    }
  }

  public Class[] getAllowableConfigureComponents() {
    Class[] c = {GlobalMap.class, LOS_Thread.class, HidePiecesButton.class,
                 Zoomer.class, CounterDetailViewer.class, LayeredPieceCollection.class, ImageSaver.class,
                 TextSaver.class, DrawPile.class, SetupStack.class, MassKeyCommand.class, PieceRecenterer.class};
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
  public void setId(String id) {
    mapID = id;
  }

  public static Map getMapById(String id) {
    return (Map) idMgr.findInstance(id);
  }

  /**
   * Each Map must have a unique String id
   *
   * @return the id for this map
   */
  public String getId() {
    return mapID;
  }

  /**
   * Make a best gues for a unique identifier for the target.
   * Use {@link VASSAL.tools.UniqueIdManager.Identifyable#getConfigureName} if non-null, otherwise
   * use {@link VASSAL.tools.UniqueIdManager.Identifyable#getId}
   * @return
   */
  public String getIdentifier() {
    return UniqueIdManager.getIdentifier(this);
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
      g.clearRect(r.x, r.y, r.width, r.height);
      map.paintRegion(g, r);
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
