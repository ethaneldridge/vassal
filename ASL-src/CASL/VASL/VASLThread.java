/*
 * $Id$
 *
 * Copyright (c) 2001 David Sullivan
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
package CASL.VASL;

import CASL.Map.*;
import CASL.Scenario.*;
import CASL.Unit.*;
import CASL.Unit.*;
import VASL.build.module.map.ASLThread;
import VASL.build.module.map.HindranceKeeper;
import VASL.build.module.map.boardPicker.ASLBoard;
import VASL.counters.ASLProperties;
import VASL.counters.TextInfo;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.ColorConfigurer;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceIterator;
import VASSAL.tools.BackgroundTask;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.util.Enumeration;
import java.util.StringTokenizer;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;
import java.io.IOException;

/**
* Extends the LOS thread to take advantage of CASL's LOS logic and report 
*/
public class VASLThread
  extends ASLThread
  implements KeyListener, GameComponent {

  public static final String ENABLED = "LosCheckEnabled";
  // status flag

  private int status = LOADING;
  private static final int LOADING = 1; // Initialization Thread still running
  private static final int LOADED = 2; // CASL map successfully loaded
  private static final int ERROR = 3; // Error loading CASL map
  private static final int DISABLED = 4; // Permanently disabled, due to incompatible Java version
  // board stuff
  private GameMap CASLMap;
  private int mapWidth = 0;		// VASL map dimensions (i.e. number of rows and columns of boards)
  private int mapHeight = 0;

  // LOS stuff
  private LOSResult result;
  private String resultsString;
  private boolean useAuxSourceLOSPoint;
  private boolean useAuxTargetLOSPoint;
  private Location source;
  private Location target;
  private Scenario scenario;
  private ASLBoard upperLeftBoard;

  // movement stuff
  private Image unitImage;
  private MovementLogic moveLogic;
  private Unit unit;
  private int unitSize = 48;
  private MovementResult moveResult;

  // LOS colors
  private Color LOSColor;
  private Color hindranceColor;
  private Color blockedColor;

  // Displayed in top left corner of map while loading
  private Image loadingStatus = null;

  // Thread to initialize CASL map in background;
  private Thread initThread;

  public VASLThread() {
    // ensure correct Java version
    if (System.getProperty("java.version").startsWith("1.1")) {
      System.err.println("LOS checking turned off:  Java version " + System.getProperty("java.version"));
      status = DISABLED;
      freeResources();
    }
    else {
//            moveLogic = new MovementLogic();
//            unit = new Infantry((Location) null);
    }
  }

  /** Invoked when the user hits the "LOS" button */
  protected void launch() {
    if (!isPreferenceEnabled()) {
      super.launch();
    }
    else {
      switch (status) {
        case LOADING:
          if (initThread != null && !visible) {
            try {
              initThread.join();
            }
            catch (InterruptedException e) {
            }
            launchTruLOS();
          }
          break;
        case LOADED:
          if (!visible) {
            launchTruLOS();
          }
          break;
        default:
          super.launch();
      }
    }
  }

  /** register mouseListener that calculates true LOS */
  private void launchTruLOS() {
    super.launch();
    LOSColor = (Color) GameModule.getGameModule().getPrefs().getValue("threadColor");
    hindranceColor = (Color) GameModule.getGameModule().getPrefs().getValue("hindranceThreadColor");
    blockedColor = (Color) GameModule.getGameModule().getPrefs().getValue("blockedThreadColor");
    map.getView().requestFocus();
  }

  /** Initialize the CASL Map.  Return null if successful, otherwise return a String describing the error preventing load */
  protected String initCaslMap() {
    if (DISABLED == status) {
      return null;
    }
    // hide while loading
    visible = false;

    // if there are any unexpected exceptions, turn off LOS checking
    try {
      // get the board list
      Enumeration boardList = map.getAllBoards();

      // determine the VASL map dimensions
      while (boardList.hasMoreElements()) {
        ASLBoard b = (ASLBoard) boardList.nextElement();
        mapWidth = Math.max(b.relativePosition().x, mapWidth);
        mapHeight = Math.max(b.relativePosition().y, mapHeight);
      }
      mapWidth++;
      mapHeight++;
      // reset the enumerator
      boardList = map.getAllBoards();
      // create the necessary LOS variables
      result = new LOSResult();
      scenario = new Scenario();
      resultsString = "";

      // create the map
      CASLMap = new GameMap(mapWidth * 32 + 1, mapHeight * 10);

      // load the CASL maps
      boolean mapFound = false;
      while (boardList.hasMoreElements()) {

        ASLBoard b = (ASLBoard) boardList.nextElement();
        String boardName = b.getName().startsWith("r") ? b.getName().substring(1) : b.getName();

        // set the upper left board
        if (b.relativePosition().x == 0 && b.relativePosition().y == 0) {

          upperLeftBoard = b;
        }

        // load the map files
        GameMap newCASLMap;
        try {
            newCASLMap = CASL.Map.Map.readMap(VASSAL.tools.DataArchive.getFileStream(b.getFile(), "bd" + boardName + ".map"));
        }
        catch (IOException e) {
          freeResources();
          return "Board "+boardName+" does not support LOS checking";
        }

        if (newCASLMap == null) {
          freeResources();
          return "Could not read bd" + boardName + ".map.  LOS Checking disabled";
        }

        else {
          mapFound = true;
          // reverse if necessary
          if (b.isReversed()) {
            newCASLMap.flip();
          }

          // add to map
          if (!CASLMap.insertGEOMap(newCASLMap, CASLMap.getHex(b.relativePosition().x * 32, b.relativePosition().y * 10))) {
            System.err.println("LOS checking turned off... Error building map");
            newCASLMap = null;
            freeResources();
            return "Real LOS disabled! Error building map";
          }

          // clean up to try to reuse the same memory
          newCASLMap = null;
          System.gc();
        }
      }

      // found no boards?
      if (!mapFound) {
        System.err.println("LOS checking turned off... No board found");
        freeResources();
        return "LOS checking turned off... No board found";
      }
    }
      // give up with any exception
    catch (Exception e) {
      freeResources();
      e.printStackTrace();
      return "LOS checking turned off... " + e.getMessage();
    }
    return null;
  }

  public void addTo(Buildable buildable) {
    super.addTo(buildable);
    if (status != DISABLED) {
      // add the key listener
      map.getView().addKeyListener(this);
      // add additional thread colors
      final BooleanConfigurer enable = new BooleanConfigurer(ENABLED, "Enable LOS checking", Boolean.TRUE);
      final JCheckBox enableBox = findBox(enable.getControls());
      final ColorConfigurer hindrance = new ColorConfigurer("hindranceThreadColor", "Hindrance Thread Color", Color.red);
      final ColorConfigurer blocked = new ColorConfigurer("blockedThreadColor", "Blocked Thread Color", Color.blue);
      final BooleanConfigurer verbose = new BooleanConfigurer("verboseLOS", "Verbose LOS mode");
      GameModule.getGameModule().getPrefs().addOption(getAttributeValueString("label"), enable);
      GameModule.getGameModule().getPrefs().addOption(getAttributeValueString("label"), hindrance);
      GameModule.getGameModule().getPrefs().addOption(getAttributeValueString("label"), blocked);
      GameModule.getGameModule().getPrefs().addOption(getAttributeValueString("label"), verbose);
      java.awt.event.ItemListener l = new java.awt.event.ItemListener() {
        public void itemStateChanged(java.awt.event.ItemEvent evt) {
          enableAll(hindrance.getControls(), enableBox.isSelected());
          enableAll(blocked.getControls(), enableBox.isSelected());
          enableAll(verbose.getControls(), enableBox.isSelected());
        }
      };
      enableBox.addItemListener(l);
      enableAll(hindrance.getControls(), Boolean.TRUE.equals(enable.getValue()));
      enableAll(blocked.getControls(), Boolean.TRUE.equals(enable.getValue()));
      enableAll(verbose.getControls(), Boolean.TRUE.equals(enable.getValue()));
      // hook for game opening/closing
      GameModule.getGameModule().getGameState().addGameComponent(this);
    }
  }

  private JCheckBox findBox(Component c) {
    JCheckBox val = null;
    if (c instanceof JCheckBox) {
      val = (JCheckBox) c;
    }
    if (c instanceof Container) {
      if (c instanceof Container) {
        for (int i = 0; i < ((Container) c).getComponentCount(); ++i) {
          val = findBox(((Container) c).getComponent(i));
          if (val != null) {
            break;
          }
        }
      }
    }
    return val;
  }

  private void enableAll(Component c, boolean enable) {
    c.setEnabled(enable);
    if (c instanceof Container) {
      for (int i = 0; i < ((Container) c).getComponentCount(); ++i) {
        enableAll(((Container) c).getComponent(i), enable);
      }
    }
  }

  public void mousePressed(MouseEvent e) {
    super.mousePressed(e);
    if (!isEnabled()) {
      return;
    }
    result.setClear();
    // get the map point
    Point p = mapMouseToCASLCoordinates(e.getPoint());
    if (p == null || !CASLMap.onMap(p.x, p.y)) return;
    // get the nearest location
    source = CASLMap.gridToHex(p.x, p.y).nearestLocation(p.x, p.y);
    useAuxSourceLOSPoint = useAuxLOSPoint(source, p.x, p.y);
    // if Ctrl click, use upper location
    if (e.isControlDown()) {
      while (source.getUpLocation() != null) {
        source = source.getUpLocation();
      }
    }
    // make the souce and the target the same
    target = source;
    useAuxTargetLOSPoint = useAuxSourceLOSPoint;
    // update the scenario
    resetScenario();
  }

  public void mouseDragged(MouseEvent e) {
    super.mouseDragged(e);
    if (!isEnabled()) {
      return;
    }
    // get the map point, ensure the point is on the CASL map
    Point p = mapMouseToCASLCoordinates(map.mapCoordinates(e.getPoint()));
    if (p == null || !CASLMap.onMap(p.x, p.y)) return;
    Location newLocation = CASLMap.gridToHex(p.x, p.y).nearestLocation(p.x, p.y);
    boolean useAuxNewLOSPoint = useAuxLOSPoint(newLocation, p.x, p.y);
    // are we really in a new location?
    if (target == newLocation && useAuxTargetLOSPoint == useAuxNewLOSPoint) {
      return;
    }
    target = newLocation;
    useAuxTargetLOSPoint = useAuxNewLOSPoint;
    // if Ctrl click, use upper location
    if (e.isControlDown()) {
      while (target.getUpLocation() != null) {
        target = target.getUpLocation();
      }
    }
    doLOS();
  }

  private boolean isEnabled() {
    return visible
      && status == LOADED
      && isPreferenceEnabled();
  }

  private boolean isPreferenceEnabled() {
    return Boolean.TRUE.equals(GameModule.getGameModule().getPrefs().getValue(ENABLED));
  }
  /*
  private void drawUnit(Graphics g) {
    if (map.getZoom() == 1.0 && unit != null && unitImage != null) {
      g.setFont(RANGE_FONT);
      Point unitPoint = mapCASLPointToScreen(unit.getLocation().getUnitLocationPoint());
      g.setColor((Color) GameModule.getGameModule().getPrefs().getValue("ge"));
      g.fillRect(unitPoint.x - unitSize / 2, unitPoint.y - unitSize / 2, unitSize, unitSize);
      g.drawImage(unitImage, unitPoint.x - unitSize / 2, unitPoint.y - unitSize / 2, unitSize, unitSize, map.getView());
      if (moveResult == null) {
        drawString(g, unitPoint.x - unitSize / 2, unitPoint.y + unitSize / 2 + g.getFontMetrics().getHeight(), "Move me!");
      }
      else {
        drawString(g, unitPoint.x - unitSize / 2, unitPoint.y + unitSize / 2 + g.getFontMetrics().getHeight(), moveResult.getMF() + " MF");
        // draw the move/error message
        if (moveResult.isLegal()) {
          StringTokenizer t = new StringTokenizer(moveResult.getMovementMessage(), "\n");
          int count = 1;
          while (t.hasMoreTokens()) {
            drawString(
              g,
              unitPoint.x - unitSize / 2,
              unitPoint.y + unitSize / 2 + g.getFontMetrics().getHeight() * (count + 1),
              t.nextToken()
            );
            count++;
          }
        }
        else {
          drawString(
            g,
            unitPoint.x - unitSize / 2,
            unitPoint.y + unitSize / 2 + g.getFontMetrics().getHeight(),
            moveResult.getErrorMessage()
          );
        }
      }
    }
  }
  */

  public void draw(Graphics g, VASSAL.build.module.Map m) {
    if (!isPreferenceEnabled()) {
      super.draw(g,m);
    }
    else if (LOADING == status) {
      if (loadingStatus == null) {
        JLabel l = new JLabel("Loading LOS data ...");
        l.setSize(l.getPreferredSize());
        l.setFont(new Font("Dialog", 0, 11));
        l.setForeground(Color.black);
        Color bg = new Color(200, 200, 255);
        l.setBackground(bg);
        loadingStatus = map.getView().createImage(l.getWidth(), l.getHeight());
        Graphics gg = loadingStatus.getGraphics();
        gg.setColor(bg);
        gg.fillRect(0, 0, l.getWidth(), l.getHeight());
        l.paint(gg);
      }
      g.drawImage(loadingStatus, map.getView().getVisibleRect().x, map.getView().getVisibleRect().y, map.getView());
    }
    else if (visible && status == LOADED) {
      //drawUnit(g);
      if (source != null) {
        // source LOS point
        Point sourceLOSPoint;
        if (useAuxSourceLOSPoint) {
          sourceLOSPoint = new Point(source.getAuxLOSPoint());
        }
        else {
          sourceLOSPoint = new Point(source.getLOSPoint());
        }
        sourceLOSPoint = mapCASLPointToScreen(sourceLOSPoint);
        // target LOS point
        Point targetLOSPoint;
        if (useAuxTargetLOSPoint) {
          targetLOSPoint = new Point(target.getAuxLOSPoint());
        }
        else {
          targetLOSPoint = new Point(target.getLOSPoint());
        }
        targetLOSPoint = mapCASLPointToScreen(targetLOSPoint);
        // transform the blocked-at point
        Point b = null;
        if (result.isBlocked()) {
          b = new Point(result.getBlockedAtPoint());
          b = mapCASLPointToScreen(b);
        }
        // transform the hindrance point
        Point h = null;
        if (result.hasHindrance()) {
          h = new Point(result.firstHindranceAt());
          h = mapCASLPointToScreen(h);
        }
        // draw the LOS thread
        if (result.isBlocked()) {
          if (result.hasHindrance()) {
            g.setColor(LOSColor);
            g.drawLine(
              sourceLOSPoint.x,
              sourceLOSPoint.y,
              h.x,
              h.y);
            g.setColor(hindranceColor);
            g.drawLine(
              h.x,
              h.y,
              b.x,
              b.y);
            g.setColor(blockedColor);
            g.drawLine(
              b.x,
              b.y,
              targetLOSPoint.x,
              targetLOSPoint.y);
          }
          else {
            g.setColor(LOSColor);
            g.drawLine(
              sourceLOSPoint.x,
              sourceLOSPoint.y,
              b.x,
              b.y);
            g.setColor(blockedColor);
            g.drawLine(
              b.x,
              b.y,
              targetLOSPoint.x,
              targetLOSPoint.y);
          }
        }
        else if (result.hasHindrance()) {
          g.setColor(LOSColor);
          g.drawLine(
            sourceLOSPoint.x,
            sourceLOSPoint.y,
            h.x,
            h.y);
          g.setColor(hindranceColor);
          g.drawLine(
            h.x,
            h.y,
            targetLOSPoint.x,
            targetLOSPoint.y);
        }
        else {
          g.setColor(LOSColor);
          g.drawLine(
            sourceLOSPoint.x,
            sourceLOSPoint.y,
            targetLOSPoint.x,
            targetLOSPoint.y);
        }
        // use the draw range property to turn all text on/off
        boolean verbose = Boolean.TRUE.equals(GameModule.getGameModule().getPrefs().getValue("verboseLOS"));
        if (drawRange) {
          // determine if the text should be above or below the location
          boolean shiftSourceText = sourceLOSPoint.y > targetLOSPoint.y;
          int shift = g.getFontMetrics().getHeight();
          // draw the source elevation
          switch (source.getBaseHeight() + source.getHex().getBaseHeight()) {
            case -1:
            case -2:
              g.setColor(Color.red);
              break;
            case 0:
              g.setColor(Color.gray);
              break;
            case 1:
              g.setColor(Color.darkGray);
              break;
            case 2:
              g.setColor(Color.black);
              break;
            default:
              g.setColor(Color.white);
          }
          g.setFont(RANGE_FONT);
          if (verbose) {
            drawString(g,
                       sourceLOSPoint.x - 20,
                       sourceLOSPoint.y + (shiftSourceText ? shift : 0) - g.getFontMetrics().getDescent(),
                       source.getName() + "  (Level " + ((int) source.getBaseHeight() + source.getHex().getBaseHeight() + ")"));
          }
          else if (source.getBaseHeight() != 0) {
            drawString(g,
                       sourceLOSPoint.x - 20,
                       sourceLOSPoint.y + (shiftSourceText ? shift : 0) - g.getFontMetrics().getDescent(),
                       "Level " + ((int) source.getBaseHeight() + source.getHex().getBaseHeight()));
          }
          // draw the target elevation
          switch (target.getBaseHeight() + target.getHex().getBaseHeight()) {
            case -1:
            case -2:
              g.setColor(Color.red);
              break;
            case 0:
              g.setColor(Color.gray);
              break;
            case 1:
              g.setColor(Color.darkGray);
              break;
            case 2:
              g.setColor(Color.black);
              break;
            default:
              g.setColor(Color.white);
          }
          if (verbose) {
            drawString(g,
                       targetLOSPoint.x - 20,
                       targetLOSPoint.y + (shiftSourceText ? 0 : shift) - g.getFontMetrics().getDescent(),
                       target.getName() + "  (Level " + ((int) target.getBaseHeight() + target.getHex().getBaseHeight() + ")"));
          }
          else if (target.getBaseHeight() != 0) {
            drawString(g,
                       targetLOSPoint.x - 20,
                       targetLOSPoint.y + (shiftSourceText ? 0 : shift) - g.getFontMetrics().getDescent(),
                       "Level " + ((int) target.getBaseHeight() + target.getHex().getBaseHeight()));
          }
          // draw the results string
          if (verbose) {
            g.setColor(Color.black);
            if (shiftSourceText) {
              drawString(g, targetLOSPoint.x - 20, targetLOSPoint.y - shift, resultsString);
            }
            else {
              drawString(g, targetLOSPoint.x - 20, targetLOSPoint.y + shift * 2 - 2, resultsString);
            }
          }
        }
      }
    }
    else {
      super.draw(g, m);
    }
  }

  /******************************
   Keyboard methods
   ******************************/
  public void keyTyped(KeyEvent e) {
  }

  public void keyReleased(KeyEvent e) {
  }

  public void keyPressed(KeyEvent e) {
    if (!isEnabled()) {
      return;
    }
    int code = e.getKeyCode();
    String modifiers = e.getKeyModifiersText(e.getModifiers());
    // move up
    if (code == KeyEvent.VK_KP_UP || code == KeyEvent.VK_UP) {
      // move the source up
      if (modifiers.equals("Ctrl") && source != null) {
        if (source.getUpLocation() != null) {
          source = source.getUpLocation();
          doLOS();
          map.repaint();
        }
      }
      // move the target up
      else if (modifiers.equals("") && target != null) {
        if (target.getUpLocation() != null) {
          target = target.getUpLocation();
          doLOS();
          map.repaint();
        }
      }
    }
    // move down
    else if (code == KeyEvent.VK_KP_DOWN || code == KeyEvent.VK_DOWN) {
      // move the source down
      if (modifiers.equals("Ctrl") && source != null) {
        if (source.getDownLocation() != null) {
          source = source.getDownLocation();
          doLOS();
          map.repaint();
        }
      }
      // move the target down
      else if (modifiers.equals("") && target != null) {
        if (target.getDownLocation() != null) {
          target = target.getDownLocation();
          doLOS();
          map.repaint();
        }
      }
    }
    //moveUnit(e);
  }

  private void moveUnit(KeyEvent e) {
    int code = e.getKeyCode();
    // do movement if not zoomed
    if (map.getZoom() == 1.0 &&
      (code == KeyEvent.VK_NUMPAD0 ||
      code == KeyEvent.VK_NUMPAD1 ||
      code == KeyEvent.VK_NUMPAD2 ||
      code == KeyEvent.VK_NUMPAD3 ||
      code == KeyEvent.VK_NUMPAD4 ||
      code == KeyEvent.VK_NUMPAD5 ||
      code == KeyEvent.VK_NUMPAD6 ||
      code == KeyEvent.VK_NUMPAD7 ||
      code == KeyEvent.VK_NUMPAD8 ||
      code == KeyEvent.VK_NUMPAD9)) {
      doMovement(e);
      map.repaint();
      e.consume();
      // force the map to scroll if necessary
      Point p = mapCASLPointToScreen(unit.getLocation().getLOSPoint());
      if (!map.getView().getVisibleRect().contains(p)) {
        map.centerAt(p);
      }
    }
  }

  /******************************
   Private methods
   ******************************/
  private boolean useAuxLOSPoint(Location l, int x, int y) {
    Point LOSPoint = l.getLOSPoint();
    Point AuxLOSPoint = l.getAuxLOSPoint();
    // use the closest LOS point
    if (Point.distance(x, y, LOSPoint.x, LOSPoint.y) > Point.distance(x, y, AuxLOSPoint.x, AuxLOSPoint.y)) {
      return true;
    }
    return false;
  }

  private void freeResources() {
    // release all resource
    CASLMap = null;
    result = null;
    source = null;
    target = null;
    scenario = null;
    System.gc();
  }

  private void drawString(Graphics g, int x, int y, String s) {
    int border = 1;
    // paint the background
    g.setColor(Color.black);
    g.fillRect(
      x - border,
      y - border - g.getFontMetrics().getHeight() + g.getFontMetrics().getDescent(),
      g.getFontMetrics().stringWidth(s) + border * 2,
      g.getFontMetrics().getHeight() + border * 2);

    // draw the string
    g.setColor(Color.white);
    g.drawString(s, x, y);
  }

  private void setResultsString() {
  }

  private void doLOS() {
    // do the LOS
    CASLMap.LOS(source, useAuxSourceLOSPoint, target, useAuxTargetLOSPoint, result, scenario);
    // set the result string
    if (result.isBlocked()) {
      resultsString =
        "Range: " + result.getRange() +
        "  Blocked in " + CASLMap.gridToHex(result.getBlockedAtPoint().x, result.getBlockedAtPoint().y).getName() +
        " ( " + result.getReason() + ")";
    }
    else {
      resultsString =
        "Range: " + result.getRange() +
        (result.getHindrance() > 0 ? ("  Hindrances: " + result.getHindrance()) : "");
    }
  }

  private void resetScenario() {
    // remove all of the old smoke, vehicles
    CASLMap.removeAllSmoke();
    scenario = new Scenario();
    if (!map.isPiecesVisible() && Boolean.TRUE.equals(GameModule.getGameModule().getPrefs().getValue(HindranceKeeper.DRAW_HINDRANCES))) {
      // get all of the game pieces
      GamePiece[] p = map.getPieces();
      // add each of the pieces to the scenario
      for (int i = 0; i < p.length; ++i) {
        if (p[i] instanceof VASSAL.counters.Stack) {
          for (PieceIterator pi = new PieceIterator(((VASSAL.counters.Stack) p[i]).getPieces()); pi.hasMoreElements();) {
            loadPiece((GamePiece) pi.nextPiece());
          }
        }
        else {
          loadPiece(p[i]);
        }
      }
    }
  }

  private void loadPiece(GamePiece piece) {
    // determine what hex the piece is in
    Point p = map.mapCoordinates(new Point(piece.getPosition()));
    p.translate(-map.getEdgeBuffer().width, -map.getEdgeBuffer().height);

    if (!CASLMap.onMap(p.x, p.y)) return;
    Hex h = CASLMap.gridToHex(p.x, p.y);
    // add the piece to the scenario/map
    if ((piece.getProperty(ASLProperties.HINDRANCE) != null && !Boolean.TRUE.equals(piece.getProperty(VASSAL.counters.Properties.INVISIBLE_TO_ME)))) {
      // smoke
      if (piece.getName().equals("+3 Smoke")) {
        CASLMap.addSmoke(new Smoke(Smoke.SMOKE, h.getCenterLocation()));
      }
      else if (piece.getName().equals("+3 Smoke2 Smoke")) {
        CASLMap.addSmoke(new Smoke(Smoke.SMOKE, h.getCenterLocation(), true));
      }
      else if (piece.getName().equals("+2 Smoke (Grey)")) {
        CASLMap.addSmoke(new Smoke(Smoke.SMOKE, h.getCenterLocation(), true));
      }
      else if (piece.getName().equals("+2 WP")) {
        CASLMap.addSmoke(new Smoke(Smoke.WHITE_PHOSPHORUS, h.getCenterLocation()));
      }
      else if (piece.getName().equals("+1 WP (Grey)")) {
        CASLMap.addSmoke(new Smoke(Smoke.WHITE_PHOSPHORUS, h.getCenterLocation(), true));
      }
      else if (piece.getName().equals("Smoke grenades")) {
        CASLMap.addSmoke(new Smoke(Smoke.SMOKE_GRENADES, h.getCenterLocation()));
      }
      else if (piece.getName().equals("WP grenades")) {
        CASLMap.addSmoke(new Smoke(Smoke.WHITE_PHOSPHORUS_SMOKE_GRENADES, h.getCenterLocation()));
      }
      else if (piece.getName().equals("Blaze")) {
        CASLMap.addSmoke(new Smoke(Smoke.SMOKE, h.getCenterLocation()));
      }
      else if (piece.getName().equals("Blazing Building")) {
        CASLMap.addSmoke(new Smoke(Smoke.SMOKE, h.getCenterLocation()));
      }
      else if (piece.getName().equals("Wreck")) {
        scenario.addUnit((CASL.Unit.Unit) new CASL.Unit.Vehicle(h.getCenterLocation()), Scenario.ALLIES);
      }
      // vehicle hindrances
      else if (Decorator.getDecorator(piece, TextInfo.class) != null) {
        scenario.addUnit((CASL.Unit.Unit) new CASL.Unit.Vehicle(h.getCenterLocation()), Scenario.ALLIES);
      }
      else if (piece.getName().equals("Stone Rubble")) {
        CASLMap.setGridTerrain((Shape) h.getHexBorder(), CASLMap.getTerrain(Terrain.STONE_RUBBLE));
        CASLMap.setHexTerrain((Shape) h.getHexBorder(), CASLMap.getTerrain(Terrain.STONE_RUBBLE));
      }
      else if (piece.getName().equals("Wooden Rubble")) {
        CASLMap.setGridTerrain((Shape) h.getHexBorder(), CASLMap.getTerrain(Terrain.WOODEN_RUBBLE));
        CASLMap.setHexTerrain((Shape) h.getHexBorder(), CASLMap.getTerrain(Terrain.WOODEN_RUBBLE));
      }
    }
  }

  public void setup(boolean flag) {
    // game closing - close LOS and free resources
    if (!flag) {
      CASLMap = null;
      visible = false;
      freeResources();
      initThread = null;
    }
    // game opening - start a new Thread to load CASL Map
    else if (isPreferenceEnabled()) {
      if (initThread == null
        && status != DISABLED) {
        status = LOADING;
        initThread = new BackgroundTask() {
          String error = null;

          public void doFirst() {
            error = initCaslMap();
          }

          public void doLater() {
            if (error != null) {
              GameModule.getGameModule().warn(error);
              status = ERROR;
            }
            else {
              GameModule.getGameModule().warn("");
              status = LOADED;
            }
            map.repaint();
          }
        }.start();
      }
    }
    else {
      super.setup(flag);
    }
  }

  public Command getRestoreCommand() {
    return null;
  }

  private Point mapCASLPointToScreen(Point p) {
    Point temp = map.componentCoordinates(new Point(p));
    temp.translate((int) (map.getEdgeBuffer().width * map.getZoom()), (int) (map.getEdgeBuffer().height * map.getZoom()));
    // adjust for board cropping
    if (upperLeftBoard != null) {
      int deltaX=0, deltaY=0;
      Rectangle crop = upperLeftBoard.getCropBounds();
      if (upperLeftBoard.isReversed()) {
        if (crop.width >= 0) {
          deltaX = upperLeftBoard.getUncroppedSize().width - crop.x - crop.width;
        }
        if (crop.height >= 0) {
          deltaY = upperLeftBoard.getUncroppedSize().height - crop.y - crop.height;
        }
      }
      else {
        deltaX = crop.x;
        deltaY = crop.y;
      }
      temp.translate((int) (-deltaX * map.getZoom()), (int) (-deltaY * map.getZoom()));
    }
    return temp;
  }

  private Point mapMouseToCASLCoordinates(Point p) {
    ASLBoard b = (ASLBoard) map.findBoard(p);
    // ensure we are on a board
    if (b == null) return null;
    p = b.uncroppedCoordinates(p);
    // Now we need to adjust for cropping of the boards to the left and
    // above the target board
    for (Enumeration e = map.getAllBoards(); e.hasMoreElements();) {
      ASLBoard b2 = (ASLBoard) e.nextElement();
      if (b2.relativePosition().y == b.relativePosition().y
        && b2.relativePosition().x < b.relativePosition().x) {
        p.translate(b2.getUncroppedSize().width - b2.bounds().width, 0);
      }
      else if (b2.relativePosition().x == b.relativePosition().x
        && b2.relativePosition().y < b.relativePosition().y) {
        p.translate(0, b2.getUncroppedSize().height - b2.bounds().height);
      }
    }
    // remove edge buffer
    p.translate(-map.getEdgeBuffer().width, -map.getEdgeBuffer().height);
    return p;
  }

  private void doMovement(KeyEvent e) {
    MovementResult newMove = getMovementResult(e, moveResult);
    // legal?
    if (newMove != null && newMove.isLegal() && newMove.getEndLocation() != null) {
      // set the current movement result
      moveResult = newMove;
      // if unit has moved into the center of a new hex containing a shellhole,
      // ask if they want to move directly into that shellhole
      if (moveResult.crossedHexside() &&
        moveResult.getEndLocation().isCenterLocation() &&
        moveResult.getEndLocation().getDownLocation() != null &&
        moveResult.getEndLocation().getDownLocation().getTerrain().getType() == Terrain.SHELL_HOLES) {
        int response = JOptionPane.showConfirmDialog(null, "Move directly INTO the shellholes?", "Select move Option", JOptionPane.YES_NO_OPTION);
        if (response == JOptionPane.YES_OPTION) {
          moveResult.setEndLocation(moveResult.getEndLocation().getDownLocation());
        }
      }
      // if unit has moved into the center of a new hex containing rice paddies,
      // ask if they want to move directly into the rice paddy
      else if (moveResult.crossedHexside() &&
        moveResult.getEndLocation().isCenterLocation() &&
        moveResult.getEndLocation().getTerrain().isRicePaddy()) {
        int response = JOptionPane.showConfirmDialog(null, "Move directly INTO the rice paddy?", "Select move option", JOptionPane.YES_NO_OPTION);
        if (response == JOptionPane.NO_OPTION) {
          moveResult.setEndLocation(moveResult.getEndLocation().getUpLocation());
        }
      }
      // apply movement logic
      moveLogic.moveUnit(unit, moveResult);
      // error?
      if (!moveResult.isLegal()) {
        if (moveResult.getEnterLocation() != null) {
          unit.setLocation(moveResult.getEndLocation());
        }
      }
      else {
        // move the unit
        unit.setLocation(moveResult.getEndLocation());
      }
    }
  }

  private MovementResult getMovementResult(KeyEvent e, MovementResult previousMove) {
    int code = e.getKeyCode();
    String modifiers = e.getKeyModifiersText(e.getModifiers());
    // north
    if (code == KeyEvent.VK_NUMPAD8) {
      if (modifiers.equals(""))
        return CASLMap.getMovementResult(unit.getLocation(), 0, 0, previousMove);
      else if (modifiers.equals("Alt"))
        return CASLMap.getMovementResult(unit.getLocation(), 0, 2, previousMove);
      else if (modifiers.equals("Ctrl"))
        return CASLMap.getMovementResult(unit.getLocation(), 0, 1, previousMove);
      // bad modifier string, ignore
      else
        return null;
    }
    // south
    else if (code == KeyEvent.VK_NUMPAD5) {
      if (modifiers.equals(""))
        return CASLMap.getMovementResult(unit.getLocation(), 3, 0, previousMove);
      else if (modifiers.equals("Alt"))
        return CASLMap.getMovementResult(unit.getLocation(), 3, 2, previousMove);
      else if (modifiers.equals("Ctrl"))
        return CASLMap.getMovementResult(unit.getLocation(), 3, 1, previousMove);
      // bad modifier string, ignore
      else
        return null;
    }
    // northwest
    else if (code == KeyEvent.VK_NUMPAD7) {
      if (modifiers.equals(""))
        return CASLMap.getMovementResult(unit.getLocation(), 5, 0, previousMove);
      else if (modifiers.equals("Alt"))
        return CASLMap.getMovementResult(unit.getLocation(), 5, 2, previousMove);
      else if (modifiers.equals("Ctrl"))
        return CASLMap.getMovementResult(unit.getLocation(), 5, 1, previousMove);
      // bad modifier string, ignore
      else
        return null;
    }
    // northeast
    else if (code == KeyEvent.VK_NUMPAD9) {
      if (modifiers.equals(""))
        return CASLMap.getMovementResult(unit.getLocation(), 1, 0, previousMove);
      else if (modifiers.equals("Alt"))
        return CASLMap.getMovementResult(unit.getLocation(), 1, 2, previousMove);
      else if (modifiers.equals("Ctrl"))
        return CASLMap.getMovementResult(unit.getLocation(), 1, 1, previousMove);
      // bad modifier string, ignore
      else
        return null;
    }
    // southwest
    else if (code == KeyEvent.VK_NUMPAD4) {
      if (modifiers.equals(""))
        return CASLMap.getMovementResult(unit.getLocation(), 4, 0, previousMove);
      else if (modifiers.equals("Alt"))
        return CASLMap.getMovementResult(unit.getLocation(), 4, 2, previousMove);
      else if (modifiers.equals("Ctrl"))
        return CASLMap.getMovementResult(unit.getLocation(), 4, 1, previousMove);
      // bad modifier string, ignore
      else
        return null;
    }
    // southeast
    else if (code == KeyEvent.VK_NUMPAD6) {
      if (modifiers.equals(""))
        return CASLMap.getMovementResult(unit.getLocation(), 2, 0, previousMove);
      else if (modifiers.equals("Alt"))
        return CASLMap.getMovementResult(unit.getLocation(), 2, 2, previousMove);
      else if (modifiers.equals("Ctrl"))
        return CASLMap.getMovementResult(unit.getLocation(), 2, 1, previousMove);
      // bad modifier string, ignore
      else
        return null;
    }
    // up/down
    else if (code == KeyEvent.VK_NUMPAD2) {
      // up or down?
      if (modifiers.equals("Alt"))
        return CASLMap.getMovementResult(unit.getLocation(), 7, 0, previousMove);
      else if (modifiers.equals("")) return CASLMap.getMovementResult(unit.getLocation(), 6, 0, previousMove);
    }
    // ignore other keys
    return null;
  }
}

