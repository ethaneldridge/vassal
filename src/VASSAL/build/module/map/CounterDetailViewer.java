/*
 * $Id$
 *
 * Copyright (c) 2003-2006 by David Sullivan, Rodney Kinney and Brent Easton.
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

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.io.File;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JComponent;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.SingleChildInstance;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.ColoredBorder;
import VASSAL.counters.Deck;
import VASSAL.counters.DeckVisitor;
import VASSAL.counters.DeckVisitorDispatcher;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Labeler;
import VASSAL.counters.PieceFilter;
import VASSAL.counters.PieceFinder;
import VASSAL.counters.PieceIterator;
import VASSAL.counters.Properties;
import VASSAL.counters.PropertiesPieceFilter;
import VASSAL.counters.Stack;
import VASSAL.tools.FormattedString;

/**
 * This is a {@link Drawable}class that draws the counters horizontally when
 * the mouse is held over a stack with the control key down.
 * 
 * @author David Sullivan
 * @version 1.0
 */
public class CounterDetailViewer extends AbstractConfigurable implements Drawable, MouseMotionListener, MouseListener,
    Runnable, KeyListener {

  public static final String VIEWER_VERSION = "2";
  public static final String USE_KEYBOARD = "ShowCounterDetails";
  public static final String PREFERRED_DELAY = "PreferredDelay";

  public static final String DELAY = "delay";
  public static final String ALWAYS_SHOW_LOC = "alwaysshowloc";
  public static final String GRAPHICS_DISPLAY = "graphicsDisplay";
  public static final String SHOW_GRAPH = "showgraph";
  public static final String SHOW_GRAPH_SINGLE = "showgraphsingle";

  public static final String SHOW_TEXT = "showtext";
  public static final String SHOW_TEXT_SINGLE = "showtextsingle";
  public static final String SHOW_REF = "showref";
  public static final String ZOOM_LEVEL = "zoomlevel";
  public static final String GRAPHICS_ZOOM_LEVEL = "graphicsZoom";
  public static final String BORDER_WIDTH = "borderWidth";
  public static final String SHOW_NOSTACK = "showNoStack";
  public static final String SHOW_DECK = "showDeck";
  public static final String DISPLAY = "display";
  public static final String LAYER_LIST = "layerList";
  public static final String SUMMARY_REPORT_FORMAT = "summaryReportFormat";
  public static final String COUNTER_REPORT_FORMAT = "counterReportFormat";
  public static final String EMPTY_HEX_REPORT_FORMAT = "emptyHexReportForma";
  public static final String VERSION = "version";
  public static final String FG_COLOR = "fgColor";
  public static final String BG_COLOR = "bgColor";
  public static final String FONT_SIZE = "fontSize";
  public static final String PROPERTY_FILTER = "propertyFilter";

  public static final String TOP_LAYER = "from top-most layer only";
  public static final String ALL_LAYERS = "from all layers";
  public static final String INC_LAYERS = "from listed layers only";
  public static final String EXC_LAYERS = "from layers other than those listed";
  public static final String FILTER = "by using a property filter";

  public static final String NEVER = "never";
  public static final String COUNTER1 = "when mouse moves over 1 or more displayable pieces";
  public static final String COUNTER2 = "when mouse moves over 2 or more displayable pieces";

  public static final String SUM = "sum(propertyName)";

  protected Map map;
  protected Thread delayThread;
  protected int delay = 700;
  protected long expirationTime;
  protected boolean graphicsVisible = false;
  protected boolean textVisible = false;
  protected MouseEvent currentMousePosition;
  protected GamePiece currentPiece;
  protected GamePiece topPiece;
  protected Point currentLocation;
  protected Point currentMouseLocation;

  protected String graphicsOptions = COUNTER2;
  protected boolean alwaysShowLoc = false;
  protected boolean showGraph = true;
  protected boolean showGraphSingle = false;
  protected boolean showText = false;
  protected boolean showTextSingle = false;
  protected boolean showRef = false;
  protected boolean showDeck = false;
  protected double zoomLevel = 1.0;
  protected double graphicsZoomLevel = 1.0;
  protected int borderWidth = 0;
  protected boolean showNoStack = false;
  protected String displayWhat = TOP_LAYER;
  protected String[] displayLayers = new String[0];
  protected SummingFormattedString summaryReportFormat = new SummingFormattedString("$" + BasicPiece.LOCATION_NAME
      + "$");
  protected FormattedString counterReportFormat = new FormattedString("");
  protected FormattedString emptyHexReportFormat = new FormattedString("$" + BasicPiece.LOCATION_NAME + "$");
  protected String version = "";
  protected Color fgColor = Color.black;
  protected Color bgColor = Color.white;
  protected int fontSize = 9;
  protected String propertyFilterString = "";
  protected PieceFilter propertyFilter = null;

  protected Rectangle bounds;
  protected boolean mouseInView = true;
  protected int[] sumPropertyTotals = new int[0];
  protected ArrayList displayablePieces = null;
  protected boolean emptyHex = false;

  public CounterDetailViewer() {
  }

  public void addTo(Buildable b) {
    map = (Map) b;
    validator = new SingleChildInstance(map, getClass());
    map.addDrawComponent(this);
    GameModule.getGameModule().getPrefs().addOption("General",
        new BooleanConfigurer(USE_KEYBOARD, "Use CTRL-space to view stack details", Boolean.FALSE));
    GameModule.getGameModule().getPrefs().addOption("General",
        new IntConfigurer(PREFERRED_DELAY, "Delay before automatic stack display (ms)", new Integer(delay)));
    
    map.getView().addMouseMotionListener(this);
    map.getView().addMouseListener(this);
    map.getView().addKeyListener(this);
  }

  public void draw(Graphics g, Map map) {
    if (currentMouseLocation != null && map.getView().getVisibleRect().contains(map.componentCoordinates(currentLocation))) {
      draw(g, currentMouseLocation, map.getView());
    }
  }

  public boolean drawAboveCounters() {
    return true;
  }

  public void draw(Graphics g, Point pt, JComponent comp) {

    if (!graphicsVisible && !textVisible) {
      return;
    }

    bounds = new Rectangle((int) (pt.x * map.getZoom()), (int) (pt.y * map.getZoom()), 0, 0);

    if (graphicsVisible) {
      drawGraphics(g, pt, comp, displayablePieces);
    }

    if (textVisible) {
      drawText(g, pt, comp, displayablePieces);
    }
  }

  // Required for backward compatibility
  protected void drawGraphics(Graphics g, Point pt, JComponent comp, PieceIterator pi) {
    ArrayList a = new ArrayList();
    while (pi.hasMoreElements()) {
      a.add(pi.nextPiece());
    }
    drawGraphics(g, pt, comp, pi);
  }

  protected void drawGraphics(Graphics g, Point pt, JComponent comp, ArrayList pieces) {

    for (int i = 0; i < pieces.size(); i++) {
      GamePiece piece = (GamePiece) pieces.get(i);
      Rectangle pieceBounds = piece.getShape().getBounds();
      bounds.width += (int) (pieceBounds.width * graphicsZoomLevel) + borderWidth;
      bounds.height = Math.max(bounds.height, (int) (pieceBounds.height * graphicsZoomLevel) + borderWidth * 2);
    }
    bounds.width += borderWidth;
    bounds.y -= bounds.height;

    if (bounds.width > 0) {

      Rectangle visibleRect = comp.getVisibleRect();
      bounds.x = Math.min(bounds.x, visibleRect.x + visibleRect.width - bounds.width);
      if (bounds.x < visibleRect.x) bounds.x = visibleRect.x;
      bounds.y = Math.min(bounds.y, visibleRect.y + visibleRect.height - bounds.height) - (isTextUnderCounters()? 15 : 0);
      int minY = visibleRect.y + (textVisible ? g.getFontMetrics().getHeight()+6 : 0); 
      if (bounds.y < minY) bounds.y = minY;

      g.setColor(bgColor);
      g.fillRect(bounds.x - 1, bounds.y - 1, bounds.width + 2, bounds.height + 2);
      g.setColor(fgColor);
      g.drawRect(bounds.x - 2, bounds.y - 2, bounds.width + 3, bounds.height + 3);
      g.drawRect(bounds.x - 3, bounds.y - 3, bounds.width + 5, bounds.height + 5);
      Shape oldClip = g.getClip();

      int borderOffset = borderWidth;
      double graphicsZoom = graphicsZoomLevel;
      for (int i = 0; i < pieces.size(); i++) {
        // Draw the next piece
        // pt is the location of the left edge of the piece
        GamePiece piece = (GamePiece) pieces.get(i);
        Rectangle pieceBounds = piece.getShape().getBounds();
        g.setClip(bounds.x - 3, bounds.y - 3, bounds.width + 5, bounds.height + 5);
        piece.draw(g, bounds.x - (int) (pieceBounds.x * graphicsZoom) + borderOffset, bounds.y
            - (int) (pieceBounds.y * graphicsZoom) + borderWidth, comp, graphicsZoom);
        g.setClip(oldClip);

        if (isTextUnderCounters()) {
          String text = counterReportFormat.getText(piece);
          int x = bounds.x - (int) (pieceBounds.x * graphicsZoom) + borderOffset;
          int y = bounds.y + bounds.height + 10;
          drawLabel(g, new Point(x, y), text, Labeler.CENTER, Labeler.CENTER);
        }

        bounds.translate((int) (pieceBounds.width * graphicsZoom), 0);
        borderOffset += borderWidth;
      }
      
    }
  }
  
  protected boolean isTextUnderCounters() {
    return textVisible && counterReportFormat.getFormat().length() > 0;
  }

  // Required for backward compatibility
  protected void drawText(Graphics g, Point pt, JComponent comp, PieceIterator pi) {
    ArrayList a = new ArrayList();
    while (pi.hasMoreElements()) {
      a.add(pi.nextPiece());
    }
    drawText(g, pt, comp, pi);
  }

  protected void drawText(Graphics g, Point pt, JComponent comp, ArrayList pieces) {
    /*
     * Label with the location If the counter viewer is being displayed, then
     * place the location name just above the left hand end of the counters. If
     * no counter viewer (i.e. single piece or expanded stack), then place the
     * location name above the centre of the first piece in the stack.
     */
    String report = "";
    int x = (int) ((bounds.x - bounds.width));
    int y = (int) (bounds.y) - 5;

    if (currentPiece == null || displayablePieces.size() == 0 || emptyHex) {
      Point mapPt = map.mapCoordinates(currentMousePosition.getPoint());
      Point snapPt = map.snapTo(mapPt);
      String locationName = map.locationName(snapPt);
      emptyHexReportFormat.setProperty(BasicPiece.LOCATION_NAME, locationName.equals("offboard") ? "" : locationName);
      emptyHexReportFormat.setProperty(BasicPiece.CURRENT_MAP, map.getMapName());
      Board b = map.findBoard(snapPt);
      String boardName = (b == null) ? "" : b.getName();
      emptyHexReportFormat.setProperty(BasicPiece.CURRENT_BOARD, boardName);
      Zone z = map.findZone(snapPt);
      String zone = (z == null) ? "" : z.getName();
      emptyHexReportFormat.setProperty(BasicPiece.CURRENT_ZONE, zone);
      report = emptyHexReportFormat.getText();
      x -= g.getFontMetrics().stringWidth(report) / 2;
    }
    else {
      String locationName = (String) topPiece.getProperty(BasicPiece.LOCATION_NAME);
      emptyHexReportFormat.setProperty(BasicPiece.LOCATION_NAME, locationName.equals("offboard") ? "" : locationName);
      report = summaryReportFormat.getText(topPiece, sumPropertyTotals);
      x += borderWidth*pieces.size()+2;
    }
    
    if (report.length() > 0) {
      drawLabel(g, new Point(x, y), report, Labeler.RIGHT, Labeler.BOTTOM);
    }
  }

  // Required for backward compatibility
  protected void drawLabel(Graphics g, Point pt, String label) {
    drawLabel(g, pt, label, Labeler.RIGHT, Labeler.BOTTOM);
  }

  protected void drawLabel(Graphics g, Point pt, String label, int hAlign, int vAlign) {
    
    if (label != null) {
      Graphics2D g2d = ((Graphics2D) g);
      g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_OFF);
      Labeler.drawLabel(g, label, pt.x, pt.y, new Font("Dialog", Font.PLAIN, fontSize), hAlign, vAlign, fgColor,
          bgColor, fgColor);
      g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
    }
  }

  /*
   * Thread code running in background to show the view after the mouse has been
   * stationery for the specified time.
   */
  public void run() {

    while (System.currentTimeMillis() < expirationTime) {
      try {
        Thread.sleep(Math.max(0, expirationTime - System.currentTimeMillis()));
      }
      catch (InterruptedException e) {
      }
    }
    /*
     * Show the viewer only if the mouse is still on the map
     */
    if (mouseInView) {
      showDetails();
    }
  }

  protected void showDetails() {

    currentPiece = findPieceAtMousePosition();
    topPiece = (currentPiece instanceof Stack) ? ((Stack) currentPiece).topPiece() : currentPiece;
    currentMouseLocation = map.mapCoordinates(currentMousePosition.getPoint());
    
    if (topPiece == null) {
      currentLocation = currentMousePosition.getPoint();
    }
    else {
      currentLocation = topPiece.getPosition();
    }
    displayablePieces = getDisplayablePieces();
    emptyHex = false;

    /*
     * Visibility Rules: Stack - Depends on setting of showGraphics/showText
     * Single Unit - Depends on setting of showGraphics/showText and
     * showGraphicsSingle/showTextSingle and stack must not be expanded. Empty
     * space - Depends on setting of
     */

    if (currentPiece == null || displayablePieces.size() == 0 ||
        graphicsOptions.equals(NEVER) ||
        (displayablePieces.size() == 1 && graphicsOptions.equals(COUNTER2))) {
      textVisible = (showRef && emptyHexReportFormat.getFormat().length() > 0);
      graphicsVisible = false;
      emptyHex = true;
    }
    else {
      boolean hasReport = summaryReportFormat.getFormat().length() > 0 || counterReportFormat.getFormat().length() > 0;

      if (map.getZoom() < zoomLevel) {
        boolean isNotTerrain = !Boolean.TRUE.equals(topPiece.getProperty(Properties.TERRAIN));
        graphicsVisible = (!graphicsOptions.equals(NEVER) && isNotTerrain);
      }
      else  {
        if (displayablePieces.size() == 1) {
          graphicsVisible = (graphicsOptions.equals(COUNTER1));
        }
        else {
          if (currentPiece instanceof Stack) {
            graphicsVisible = (!graphicsOptions.equals(NEVER) && !((Stack) currentPiece).isExpanded());
          }
          else {
            graphicsVisible = showNoStack;
          }
        }
      }
      if (graphicsVisible) {
        textVisible = hasReport;
      }
      else {
        emptyHex = !graphicsVisible;
        textVisible = true;
      }
    }

    map.repaint();
  }

  /*
   * Build an ArrayList of pieces to be displayed in order from bottom up, based
   * on selection criteria setup in config.
   */
  protected ArrayList getDisplayablePieces() {

    GamePiece[] allPieces = map.getPieces(); // All pieces from bottom up

    Visitor visitor = new Visitor(new Filter(), summaryReportFormat.getSummingPropertyNames(), map);
    DeckVisitorDispatcher dispatcher = new DeckVisitorDispatcher(visitor);

    /*
     * Process pieces from the top down to make it easier to check for top layer
     * only.
     */
    for (int i = allPieces.length - 1; i >= 0; i--) {
      dispatcher.accept(allPieces[i]);
    }

    sumPropertyTotals = visitor.getPropertyTotals();

    return visitor.getPieces();
  }

  /*
   * Utility class to select the pieces we wish to view.
   */
  protected class Filter implements PieceFilter {

    protected int topLayer;

    public Filter() {
      topLayer = -1;
    }

    public boolean accept(GamePiece piece) {
      return accept(piece, 0, "");
    }

    public boolean accept(GamePiece piece, int layer, String layerName) {

      // Is this piece in the same location as the piece we found under the cursor?
      if (!currentLocation.equals(piece.getPosition())) {
        return false;
      }

      // Is it visible to us?
      if (Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))) {
        return false;
      }

      // If it Does Not Stack, do we want to see it?
      if (Boolean.TRUE.equals(piece.getProperty(Properties.NO_STACK))) {
        return showNoStack;
      }

      // Deck?
      if (piece.getParent() instanceof Deck && !showDeck) {
        return false;
      }
      
      // Select by property filter
      if (displayWhat.equals(FILTER)) {
        return propertyFilter.accept(piece);
      }

      // Looking at All Layers accepts anything.
      else if (displayWhat.equals(ALL_LAYERS)) {
        return true;
      }
      else {

        if (topLayer < 0) {
          topLayer = layer;
        }

        // Pieces are passed to us top down, so only display the top-most layer
        if (displayWhat.equals(TOP_LAYER)) {
          return layer == topLayer;
        }

        // Include pieces on named layers only
        else if (displayWhat.equals(INC_LAYERS)) {
          for (int i = 0; i < displayLayers.length; i++) {
            if (layerName.equals(displayLayers[i])) {
              return true;
            }
          }
        }

        // Exclude pieces from named layers.
        else if (displayWhat.equals(EXC_LAYERS)) {
          for (int i = 0; i < displayLayers.length; i++) {
            if (layerName.equals(displayLayers[i])) {
              return false;
            }
          }
          return true;
        }
      }

      // Ignore anything else
      return false;
    }

  }

  /*
   * Utility class to visit Map pieces, apply the filter and return a list of
   * pieces we are interested in.
   */
  protected static class Visitor implements DeckVisitor {
    protected ArrayList pieces;
    protected Filter filter = null;
    protected String[] sumPropertyNames;
    protected int[] sumPropertyTotals;
    protected PieceCollection collection;
    protected int lastLayer = -1;
    protected int insertPos = 0;

    public Visitor(Filter filter, String[] propertyNames, Map map) {
      collection = map.getPieceCollection();
      pieces = new ArrayList();
      this.filter = filter;
      if (propertyNames == null) {
        sumPropertyNames = new String[0];
        sumPropertyTotals = new int[0];
      }
      else {
        sumPropertyNames = propertyNames;
        sumPropertyTotals = new int[propertyNames.length];
        for (int i = 0; i < sumPropertyTotals.length; i++) {
          sumPropertyTotals[i] = 0;
        }
      }
    }

    public Object visitDeck(Deck d) {
      GamePiece top = d.topPiece();
      if (top != null) {
        if (!Boolean.TRUE.equals(top.getProperty(Properties.OBSCURED_TO_ME)) &&
             !Boolean.TRUE.equals(top.getProperty(Properties.INVISIBLE_TO_ME))) {
          visitDefault(top);
        }     
      }
      return null;
    }
    
    public Object visitStack(Stack s) {
      for (Enumeration e = s.getPieces(); e.hasMoreElements();) {
        apply((GamePiece) e.nextElement());
      }
      return null;
    }

    public Object visitDefault(GamePiece p) {
      apply(p);
      return null;
    }

    /*
     * Insert accepted pieces into the start of the array since we are being
     * passed pieces from the top down. Sum any required property values as we
     * go.
     */
    protected void apply(GamePiece p) {

      int layer = 0;
      String layerName = "";

      try {
        if (collection instanceof CompoundPieceCollection) {
          layer = ((CompoundPieceCollection) collection).getLayerForPiece(p);
          layerName = ((CompoundPieceCollection) collection).getLayerNameForPiece(p);
        }
      } 
      catch (Exception ex) {
        
      }

      if (filter == null || filter.accept(p, layer, layerName)) {

        if (layer != lastLayer) {
          insertPos = 0;
          lastLayer = layer;
        }

        pieces.add(insertPos++, p);
        for (int i = 0; i < sumPropertyNames.length; i++) {
          try {
            sumPropertyTotals[i] += Integer.parseInt((String) p.getProperty(sumPropertyNames[i]));
          }
          catch (Exception ex) {
          }
        }
      }
    }

    public ArrayList getPieces() {
      return pieces;
    }

    public int[] getPropertyTotals() {
      return sumPropertyTotals;
    }

  }
  
  protected GamePiece findPieceAtMousePosition() {
    GamePiece p = map.findPiece(map.mapCoordinates(currentMousePosition.getPoint()), deckPieceFinder);
    if (p != null && p.getParent() != null) {
      p = p.getParent();
    }
    return p;
  }

  /*
   * Define a PieceFinder that will also find the top face up piece
   * in a stack
   */
  public static final DeckPiece deckPieceFinder = new DeckPiece();
  
  public static class DeckPiece extends PieceFinder.Movable {
    public Object visitDeck(Deck d) {
      GamePiece top = d.topPiece();
      if (top != null &&
          !Boolean.TRUE.equals(top.getProperty(Properties.OBSCURED_TO_ME))
          && !Boolean.TRUE.equals(top.getProperty(Properties.INVISIBLE_TO_ME))) {
        Rectangle r = (Rectangle) d.getShape();
        r.x += d.getPosition().x;
        r.y += d.getPosition().y;
        if (r.contains(pt)) {
          return top;
        }
      }
      return null;
    }
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
        restartDelay();
         // Reset thread
        // timer
        if (delayThread == null || !delayThread.isAlive()) {
          delayThread = new Thread(this);
          delayThread.start();
        }
      }
    }
  }
  
  protected void restartDelay() {
    expirationTime = System.currentTimeMillis() + getPreferredDelay();
  }
  
  protected int getPreferredDelay() {
    return ((Integer) GameModule.getGameModule().getPrefs().getValue(PREFERRED_DELAY)).intValue();
  }

  public void mouseDragged(MouseEvent e) {
    mouseMoved(e);
  }

  public void mouseClicked(MouseEvent e) {
  }

  public void mouseEntered(MouseEvent e) {
    mouseInView = true;
  }

  public void mouseExited(MouseEvent e) {
    mouseInView = false;
  }

  public void mousePressed(MouseEvent e) {
    restartDelay();
  }

  public void mouseReleased(MouseEvent e) {
    mouseInView = true;
    restartDelay();
  }

  public void keyTyped(KeyEvent e) {
  }

  public void keyPressed(KeyEvent e) {
    if (e.getKeyCode() == KeyEvent.VK_SPACE && e.isControlDown()
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

  /*
   * Compatibility. If this component has not yet been saved by this version of
   * vassal, convert the old-style options to new and update the version.
   */
  public Configurer getConfigurer() {

    // New version 2 viewer being created
    if (map == null) {
      version = VIEWER_VERSION;
    }
    // Previous version needing upgrading?
    else if (!version.equals(VIEWER_VERSION)) {
      upgrade();
    }
    return super.getConfigurer();
  }

  protected void upgrade() {

    if (!showGraph && !showText) {
      graphicsOptions = NEVER;
    }
    else if (showGraphSingle) {
      graphicsOptions = COUNTER1;
    }
    else {
      graphicsOptions = COUNTER2;
    }

    fgColor = map.getHighlighter() instanceof ColoredBorder ? ((ColoredBorder) map.getHighlighter()).getColor()
        : Color.black;

    bgColor = new Color(255 - fgColor.getRed(), 255 - fgColor.getGreen(), 255 - fgColor.getBlue());

    version = VIEWER_VERSION;
  }

  public String[] getAttributeNames() {
    return new String[] {
        VERSION,
        DELAY,
        GRAPHICS_ZOOM_LEVEL,
        ZOOM_LEVEL,
        FG_COLOR,
        BG_COLOR,
        GRAPHICS_DISPLAY,
        SHOW_GRAPH,
        SHOW_GRAPH_SINGLE,
        BORDER_WIDTH,
        SHOW_TEXT,
        SHOW_TEXT_SINGLE,
        FONT_SIZE,
        SUMMARY_REPORT_FORMAT,
        COUNTER_REPORT_FORMAT,
        SHOW_NOSTACK,
        SHOW_DECK,
        DISPLAY,
        LAYER_LIST,
        PROPERTY_FILTER,
        SHOW_REF,
        EMPTY_HEX_REPORT_FORMAT };
  }

  public String[] getAttributeDescriptions() {
    return new String[] {
        "Version", // Not displayed
        "Recommended Delay before display (ms):  ",
        "Zoom level for piece display:  ",
        "Always display when zoom level less than:  ",
        "Borders and font color:  ",
        "Background color:  ",

        "Display stack viewer:  ",
        "Display piece images?",
        "Display unit graphics for single counter?", // Obsolete
        "Width of border around each displayed piece:  ",

        "Display text?",
        "Display text report for single counter?",// Obsolete
        "Font size:  ",
        "Summary text above pieces:  ",
        "Text below each piece:  ",

        "Include non-stacking pieces in display?",
        "Include top face-up piece in Deck in display?",
        "Select pieces to display:  ",
        "Listed layers",
        "Piece selection property filter:  ",

        "Display a message when no pieces under mouse?",
        "Format of 'no pieces' message:  ", };

  }

  public Class[] getAttributeTypes() {
    return new Class[] {
        String.class,
        Integer.class,
        Double.class,
        Double.class,
        Color.class,
        Color.class,
        TextConfig.class,
        Boolean.class,
        Boolean.class,
        Integer.class,
        Boolean.class,
        Boolean.class,
        Integer.class,
        ReportFormatConfig.class,
        CounterFormatConfig.class,
        Boolean.class,
        Boolean.class,
        DisplayConfig.class,
        String[].class,
        String.class,
        Boolean.class,
        EmptyFormatConfig.class, };
  }

  public static class DisplayConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { TOP_LAYER, ALL_LAYERS, INC_LAYERS, EXC_LAYERS, FILTER };
    }
  }

  public static class TextConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { NEVER, COUNTER1, COUNTER2 };
    }
  }

  public static class EmptyFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, new String[] {
          BasicPiece.LOCATION_NAME,
          BasicPiece.CURRENT_MAP,
          BasicPiece.CURRENT_BOARD,
          BasicPiece.CURRENT_ZONE });
    }
  }

  public static class ReportFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, new String[] {
          BasicPiece.LOCATION_NAME,
          BasicPiece.CURRENT_MAP,
          BasicPiece.CURRENT_BOARD,
          BasicPiece.CURRENT_ZONE,
          SUM });
    }
  }

  public static class CounterFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, new String[] { BasicPiece.PIECE_NAME });
    }
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
    else if (GRAPHICS_ZOOM_LEVEL.equals(name)) {
      if (value instanceof String) {
        value = new Double((String) value);
      }
      graphicsZoomLevel = ((Double) value).doubleValue();
    }
    else if (BORDER_WIDTH.equals(name)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      borderWidth = ((Integer) value).intValue();
    }
    else if (SHOW_NOSTACK.equals(name)) {
      if (value instanceof Boolean) {
        showNoStack = ((Boolean) value).booleanValue();
      }
      else if (value instanceof String) {
        showNoStack = "true".equals(value);
      }
    }
    else if (SHOW_DECK.equals(name)) {
      if (value instanceof Boolean) {
        showDeck = ((Boolean) value).booleanValue();
      }
      else if (value instanceof String) {
        showDeck = "true".equals(value);
      }
    }
    else if (DISPLAY.equals(name)) {
      displayWhat = (String) value;
    }
    else if (LAYER_LIST.equals(name)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      displayLayers = (String[]) value;
    }
    else if (EMPTY_HEX_REPORT_FORMAT.equals(name)) {
      emptyHexReportFormat.setFormat((String) value);
    }
    else if (SUMMARY_REPORT_FORMAT.equals(name)) {
      summaryReportFormat.setFormat((String) value);
    }
    else if (COUNTER_REPORT_FORMAT.equals(name)) {
      counterReportFormat.setFormat((String) value);
    }
    else if (GRAPHICS_DISPLAY.equals(name)) {
      graphicsOptions = (String) value;
    }
    else if (VERSION.equals(name)) {
      version = (String) value;
    }
    else if (FG_COLOR.equals(name)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      if (value != null) {
        fgColor = (Color) value;
      }
    }
    else if (BG_COLOR.equals(name)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      if (value != null) {
        bgColor = (Color) value;
      }
    }
    else if (FONT_SIZE.equals(name)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      if (value != null) {
        fontSize = ((Integer) value).intValue();
      }
    }
    else if (PROPERTY_FILTER.equals(name)) {
      propertyFilterString = (String) value;
      propertyFilter = PropertiesPieceFilter.parse(propertyFilterString);
    }
  }

  public String getAttributeValueString(String name) {
    if (DELAY.equals(name)) {
      return String.valueOf(delay);
    }
    else if (SHOW_GRAPH.equals(name)) {
      return String.valueOf(showGraph);
    }
    else if (SHOW_GRAPH_SINGLE.equals(name)) {
      return String.valueOf(showGraphSingle);
    }
    else if (SHOW_TEXT.equals(name)) {
      return String.valueOf(showText);
    }
    else if (SHOW_TEXT_SINGLE.equals(name)) {
      return String.valueOf(showTextSingle);
    }
    else if (SHOW_REF.equals(name)) {
      return String.valueOf(showRef);
    }
    else if (ZOOM_LEVEL.equals(name)) {
      return String.valueOf(zoomLevel);
    }
    else if (GRAPHICS_ZOOM_LEVEL.equals(name)) {
      return String.valueOf(graphicsZoomLevel);
    }
    else if (BORDER_WIDTH.equals(name)) {
      return String.valueOf(borderWidth);
    }
    else if (SHOW_NOSTACK.equals(name)) {
      return String.valueOf(showNoStack);
    }
    else if (SHOW_DECK.equals(name)) {
      return String.valueOf(showDeck);
    }
    else if (DISPLAY.equals(name)) {
      return displayWhat;
    }
    else if (LAYER_LIST.equals(name)) {
      return StringArrayConfigurer.arrayToString(displayLayers);
    }
    else if (EMPTY_HEX_REPORT_FORMAT.equals(name)) {
      return emptyHexReportFormat.getFormat();
    }
    else if (SUMMARY_REPORT_FORMAT.equals(name)) {
      return summaryReportFormat.getFormat();
    }
    else if (COUNTER_REPORT_FORMAT.equals(name)) {
      return counterReportFormat.getFormat();
    }
    else if (GRAPHICS_DISPLAY.equals(name)) {
      return graphicsOptions;
    }
    else if (VERSION.equals(name)) {
      return version;
    }
    else if (FG_COLOR.equals(name)) {
      return ColorConfigurer.colorToString(fgColor);
    }
    else if (BG_COLOR.equals(name)) {
      return ColorConfigurer.colorToString(bgColor);
    }
    else if (FONT_SIZE.equals(name)) {
      return String.valueOf(fontSize);
    }
    else if (PROPERTY_FILTER.equals(name)) {
      return propertyFilterString;
    }
    else
      return null;
  }

  public static String getConfigureTypeName() {
    return "Mouse-over Stack Viewer";
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (BORDER_WIDTH.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return !graphicsOptions.equals(NEVER) && showGraph;
        }
      };
    }
    else if (FONT_SIZE.equals(name) || SUMMARY_REPORT_FORMAT.equals(name) || COUNTER_REPORT_FORMAT.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return !graphicsOptions.equals(NEVER) & showText;
        }
      };
    }
    else if (SHOW_GRAPH.equals(name) || SHOW_TEXT.equals(name) || SHOW_NOSTACK.equals(name) || SHOW_DECK.equals(name) || DISPLAY.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return !graphicsOptions.equals(NEVER);
        }
      };
    }
    else if (LAYER_LIST.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return !graphicsOptions.equals(NEVER) && (displayWhat.equals(INC_LAYERS) || displayWhat.equals(EXC_LAYERS));
        }
      };
    }
    else if (PROPERTY_FILTER.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return !graphicsOptions.equals(NEVER) && displayWhat.equals(FILTER);
        }
      };
    }
    else if (EMPTY_HEX_REPORT_FORMAT.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return showRef;
        }
      };
    }
    /*
     * The following fields are not to be displayed. They are either obsolete
     * and maintained for backward compatibility
     */
    else if (VERSION.equals(name) || SHOW_TEXT_SINGLE.equals(name) || SHOW_GRAPH_SINGLE.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return false;
        }
      };
    }
    return null;
  }

  /*
   * Enhanced FormattedString class to support summing properties accross a
   * number of counters using format $sum(property_name)$
   */
  protected class SummingFormattedString extends FormattedString {

    protected String[] summingPropertyNames;
    protected int[] summingTotals;

    public SummingFormattedString(String format) {
      setFormat(format);
    }

    public void setFormat(String format) {
      ArrayList names = new ArrayList();

      Pattern pattern = Pattern.compile("\\$sum\\(.*?\\)\\$");
      Matcher matcher = pattern.matcher(format);

      while (matcher.find()) {
        int start = matcher.start();
        int end = matcher.end();
        String sum = format.substring(start, end);
        names.add(sum.substring(5, sum.length() - 2));
      }

      if (names.size() == 0) {
        summingPropertyNames = new String[0];
        summingTotals = new int[0];
      }
      else {
        summingPropertyNames = (String[]) names.toArray(summingPropertyNames);
        summingTotals = new int[names.size()];
      }

      super.setFormat(format);
    }

    public String getText(GamePiece piece, int[] totals) {
      String format = getFormat();
      String origFormat = format;

      for (int i = 0; i < summingPropertyNames.length; i++) {
        format = format.replaceAll("\\$sum\\(" + summingPropertyNames[i] + "\\)\\$", String.valueOf(totals[i]));
      }
      setFormat(format);

      String result = super.getText(piece);

      setFormat(origFormat);
      return result;
    }

    public String[] getSummingPropertyNames() {
      return summingPropertyNames;
    }

  }

}
