/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney, Jim Urbas
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

import VASSAL.build.AbstractBuildable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.counters.*;
import VASSAL.tools.Sort;
import VASSAL.Info;

import javax.swing.*;
import java.awt.*;
import java.awt.datatransfer.StringSelection;
import java.awt.dnd.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.net.URL;
import java.util.Enumeration;
import java.util.ArrayList;
import java.util.Iterator;

import java.awt.image.*;


/**
 * This is a MouseListener that moves pieces onto a Map window
 */
public class PieceMover extends AbstractBuildable implements
    MouseListener, GameComponent, PieceFinder, Sort.Comparator {

  /** The Preferences key for autoreporting moves.  */
  public static final String AUTO_REPORT = "autoReport";

  protected static final String OFFMAP = "offmap";

  protected Map map;
  protected Point dragBegin;
  private JButton markUnmovedButton;

  public void addTo(Buildable b) {
    map = (Map) b;
    map.addLocalMouseListener(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    if (Info.isDndEnabled()) {
      map.setDragGestureListener(DragHandler.getTheDragHandler());
    }
  }

  public void setup(boolean gameStarting) {
    if (gameStarting) {
      initButton();
    }
  }

  public Command getRestoreCommand() {
    return null;
  }

  private void initButton() {
    String value = getMarkOption();
    if (GlobalOptions.PROMPT.equals(value)) {
      BooleanConfigurer config = new BooleanConfigurer(Map.MARK_MOVED, "Mark Moved Pieces", Boolean.TRUE);
      GameModule.getGameModule().getPrefs().addOption(config);
    }
    if (!GlobalOptions.NEVER.equals(value)) {
      if (markUnmovedButton == null) {
        markUnmovedButton = new JButton();
        URL icon = getClass().getResource("/images/unmoved.gif");
        if (icon != null) {
          markUnmovedButton.setIcon(new ImageIcon(icon));
          ;
        }
        else {
          markUnmovedButton.setText("Mark Unmoved");
        }
        markUnmovedButton.setAlignmentY(0.0F);
        markUnmovedButton.setToolTipText("Mark all pieces on this map as not moved");
        markUnmovedButton.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            GamePiece[] p = map.getPieces();
            Command c = new NullCommand();
            for (int i = 0; i < p.length; ++i) {
              c.append(markMoved(p[i], false));
            }
            GameModule.getGameModule().sendAndLog(c);
            map.repaint();
          }
        });
        map.getToolBar().add(markUnmovedButton);
      }
    }
    else if (markUnmovedButton != null) {
      map.getToolBar().remove(markUnmovedButton);
      markUnmovedButton = null;
    }
  }

  private String getMarkOption() {
    String value = map.getAttributeValueString(Map.MARK_MOVED);
    if (value == null) {
      value = GlobalOptions.getInstance().getAttributeValueString(GlobalOptions.MARK_MOVED);
    }
    return value;
  }

  public String[] getAttributeNames() {
    return new String[0];
  }

  public String getAttributeValueString(String key) {
    return null;
  }

  public void setAttribute(String key, Object value) {
  }

  protected boolean isMultipleSelectionEvent(MouseEvent e) {
    return e.isShiftDown();
  }

  /** Invoked after a piece has been moved */
  protected Command movedPiece(GamePiece p, Point loc) {
    if (p instanceof Stack) {
      GamePiece top = ((Stack) p).topPiece();
      if (top != null) {
        KeyBuffer.getBuffer().add(top);
      }
    }
    if (!loc.equals(p.getPosition())) {
      return markMoved(p, true);
    }
    else {
      return null;
    }
  }

  public Command markMoved(GamePiece p, boolean hasMoved) {
    if (GlobalOptions.NEVER.equals(getMarkOption())) {
      hasMoved = false;
    }
    Command c = new NullCommand();
    if (!hasMoved || shouldMarkMoved()) {
      if (p instanceof Stack) {
        for (Enumeration e = ((Stack) p).getPieces(); e.hasMoreElements();) {
          c.append(markMoved((GamePiece) e.nextElement(), hasMoved));
        }
      }
      else if (p.getProperty(Properties.MOVED) != null) {
        if (p.getId() != null) {
          ChangeTracker comm = new ChangeTracker(p);
          p.setProperty(Properties.MOVED, hasMoved ? Boolean.TRUE : Boolean.FALSE);
          c = comm.getChangeCommand();
        }
      }
    }
    return c;
  }

  protected boolean shouldMarkMoved() {
    String option = getMarkOption();
    if (GlobalOptions.ALWAYS.equals(option)) {
      return true;
    }
    else if (GlobalOptions.NEVER.equals(option)) {
      return false;
    }
    else {
      return Boolean.TRUE.equals(GameModule.getGameModule().getPrefs().getValue(Map.MARK_MOVED));
    }
  }

  /** moves pieces in DragBuffer to point p by generating a Command for each element in Dragbuffer
   *
   * @param map Map
   * @param p Point mouse released
   */
  public Command movePieces(Map map, Point p) {
    DragBuffer.getBuffer().sort(this);

    ArrayList originMaps = new ArrayList(); // List of maps, different from the destination maps, that the dragged pieces came from

    PieceIterator it = DragBuffer.getBuffer().getIterator();

    if (!it.hasMoreElements()) {
      return null;
    }

    GamePiece mergeWith = map.findPiece(p, this);

    GamePiece bottom = it.nextPiece();
    KeyBuffer.getBuffer().clear();

    if (!Boolean.TRUE.equals(bottom.getProperty(Properties.IGNORE_GRID))) {
      p = map.snapTo(p);
    }

    String myId = GameModule.getUserId();
    GameModule.setUserId("yendoR117");  // Don't report my hidden or concealed units
    String origin = map.locationName(bottom.getPosition());
    StringBuffer moved = new StringBuffer();
    if (bottom.getMap() == map) {
      moved.append(bottom.getName());
    }
    else if (bottom.getMap() != null) {
      originMaps.add(bottom.getMap());
    }
    else {
      moved.append(bottom.getName());
      origin = OFFMAP;
    }
    GameModule.setUserId(myId);

    Command comm = new NullCommand();
    String destination;

    //String origin = map.locationName(bottom.getPosition()); // Moved up

    if (mergeWith == null) {
      comm = comm.append(movedPiece(bottom, p));
      comm = comm.append(map.placeAt(bottom, p));
      if (!(bottom instanceof Stack) && !Boolean.TRUE.equals(bottom.getProperty(Properties.NO_STACK))) {
        Stack parent = map.getStackMetrics().createStack(bottom);
        if (parent != null) {
          comm = comm.append(map.placeAt(parent, p));
        }
      }
      destination = map.locationName(p);
    }
    else {
      comm = comm.append(movedPiece(bottom, mergeWith.getPosition()));
      comm = comm.append(map.getStackMetrics().merge(mergeWith, bottom));
      destination = map.locationName(mergeWith.getPosition());
    }

    while (it.hasMoreElements()) {
      GamePiece next = it.nextPiece();
      GameModule.setUserId("yendoR117");  // Don't report my hidden or concealed units
      //if (next.getMap() == map) { // Make sure moved from offmap are reported
      if (next.getMap() == map || OFFMAP.equals(origin)) {
        if (next.getName().length() > 0) {
          moved.append(',');
          moved.append(next.getName());
        }
      }
      else if (next.getMap() != null) {
        originMaps.add(next.getMap());
      }
      GameModule.setUserId(myId);
      String nextOrigin = map.locationName(next.getPosition());
      if (nextOrigin == null || !nextOrigin.equals(origin)) {
        origin = null;
      }
      comm = comm.append(movedPiece(next, bottom.getPosition()));
      comm = comm.append(map.getStackMetrics().merge(bottom, next));
      bottom = next;
    }
    String s = moved.toString();
    if (comm != null && !comm.isNull() && destination != null &&
        s.length() > 0 && GlobalOptions.getInstance().autoReportEnabled()) {
      if (origin == null) {
        s = "* " + s + " moves " + destination + " * ";
      }
      else {
        s = "* " + s + " moves " + origin + " -> " + destination + " * ";
      }
      Command report = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), s);
      report.execute();
      comm = comm.append(report);
    }
    GameModule.setUserId(myId);
    for (Iterator iterator = originMaps.iterator(); iterator.hasNext();) {
      ((Map) iterator.next()).repaint();
    }
    return comm;
  }

  /**
   * This listener is used for faking drag-and-drop on Java 1.1 systems
   * @param e
   */
  public void mousePressed(MouseEvent e) {
    if (canHandleEvent(e)) {
      selectMovablePieces(e.getPoint());
      if (!Info.isDndEnabled()
          && DragBuffer.getBuffer().getIterator().hasMoreElements()) {
        map.getView().setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
      }
    }
  }

  /** Place the clicked-on piece into the {@link DragBuffer} */
  protected void selectMovablePieces(Point point) {

    GamePiece p = map.findPiece(point, PieceFinder.MOVABLE);
    dragBegin = point;

    if (p == null) {
      DragBuffer.getBuffer().clear();
    }
    else if (Boolean.TRUE.equals(p.getProperty(Properties.IMMOBILE))) {
      DragBuffer.getBuffer().clear();

      if (KeyBuffer.getBuffer().contains(p)) {
        DragBuffer.getBuffer().add(p);
      }
      else {
        p = null;
      }
    }
    else if (Boolean.TRUE.equals(p.getProperty(Properties.NO_STACK))) {
      DragBuffer.getBuffer().clear();
      DragBuffer.getBuffer().add(p);
    }
    else {
      DragBuffer.getBuffer().clear();
      if (KeyBuffer.getBuffer().contains(p)) {
        DragBuffer.getBuffer().add(p);
        // If clicking on a selected piece, put all selected pieces into the drag buffer
        for (Enumeration enum = KeyBuffer.getBuffer().getPieces(); enum.hasMoreElements();) {
          GamePiece piece = (GamePiece) enum.nextElement();
          if (piece != p && piece.getParent() != p) {
            DragBuffer.getBuffer().add(piece);
          }
        }
      }
      else {
        // Otherwise, only put the clicked-on piece into the drag buffer
        DragBuffer.getBuffer().add(p);
      }
    }

    // show/hide selection boxes
    map.repaint();
  }

  protected boolean canHandleEvent(MouseEvent e) {
    return !e.isShiftDown()
        && !e.isControlDown()
        && !e.isMetaDown()
        && e.getClickCount() < 2
        && !e.isConsumed();
  }

  /**
   * Return true if this point is "close enough" to
   * the point at which the user pressed the mouse
   * to be considered a mouse click (such that no moves are done)
   */
  public boolean isClick(Point pt) {
    boolean isClick = false;
    if (dragBegin != null) {
      Board b = map.findBoard(pt);
      if (b != null && b.getGrid() != null) {
        if (map.snapTo(pt).equals(map.snapTo(dragBegin))) {
          isClick = true;
        }
      }
      else {
        if (Math.abs(pt.x - dragBegin.x) <= 5
            && Math.abs(pt.y - dragBegin.y) <= 5) {
          isClick = true;
        }
      }
    }
    return isClick;
  }

  public void mouseReleased(MouseEvent e) {
    if (canHandleEvent(e)) {
      if (!isClick(e.getPoint())) {
        performDrop(e.getPoint());
      }
    }
    dragBegin = null;
    map.getView().setCursor(null);
  }

  protected void performDrop(Point p) {
    Command move = movePieces(map, p);
    GameModule.getGameModule().sendAndLog(move);
    if (move != null) {
      DragBuffer.getBuffer().clear();
    }
  }

  public void mouseEntered(MouseEvent e) {
  }

  public void mouseExited(MouseEvent e) {
  }

  public void mouseClicked(MouseEvent e) {
  }

  /**
   * Implements PieceFinder to determine which piece to stack with when
   * completing a drag
   */
  public GamePiece select(Map map, GamePiece piece, Point pt) {
    if (!map.getStackMetrics().isStackingEnabled()) {
      return null;
    }
    GamePiece selected = null;
    Board b = map.findBoard(pt);
    if (b == null || b.getGrid() == null) {
      selected = PieceFinder.MOVABLE.select(map, piece, pt);
    }
    else {
      Point snap = map.snapTo(pt);
      if (piece instanceof Stack) {
        Stack s = (Stack) piece;
        if (s.isExpanded()) {
          Shape[] bounds = new Shape[s.getPieceCount()];
          map.getStackMetrics().getContents(s, null, bounds, null, s.getPosition().x, s.getPosition().y);
          for (Enumeration e = s.getPiecesInVisibleOrder(); e.hasMoreElements();) {
            GamePiece child = (GamePiece) e.nextElement();
            if (bounds[s.indexOf(child)].contains(pt)) {
              selected = child;
              break;
            }
          }
        }
        else if (s.getPosition().equals(snap) && s.topPiece() != null) {
          selected = s;
        }
      }
      else if (piece.getPosition().equals(snap) && !Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))) {
        selected = piece;
      }
    }
    if (selected != null && Boolean.TRUE.equals(selected.getProperty(Properties.NO_STACK))) {
      selected = null;
    }
    if (selected != null && DragBuffer.getBuffer().contains(selected)) {
      /* If clicking and dragging to within the outline of the same piece,
      ignore this piece if it's a stack or the only visible piece in a stack */
      if (selected.getParent() == null) {
        selected = null;
      }
      else {
        Enumeration e = selected.getParent().getPiecesInVisibleOrder();
        if (e.hasMoreElements() && e.nextElement() == selected && !e.hasMoreElements()) {
          selected = null;
        }
      }
    }
    return selected;
  }

  /**
   * Implement Comparator to sort the contents of the darg bufeer before completing the drag.
   * This sorts the contents to be in the same order as the piece were in their original parent stack.
   */
  public int compare(Object o1, Object o2) {
    GamePiece p1 = (GamePiece) o1;
    GamePiece p2 = (GamePiece) o2;
    int result = 0;
    if (p1.getMap() == null && p2.getMap() == null) {
      return 0;
    }
    else if (p1.getMap() == null) {
      return 1;
    }
    else if (p2.getMap() == null) {
      return -1;
    }
    if (p1.getMap() != p2.getMap()) {
      result = new Sort.Alpha().compare(p1.getMap().getId(), p2.getMap().getId());
    }
    else {
      Stack s1 = p1 instanceof Stack ? (Stack) p1 : p1.getParent();
      Stack s2 = p2 instanceof Stack ? (Stack) p2 : p2.getParent();
      result = p1.getMap().indexOf(s1) - p2.getMap().indexOf(s2);
      if (result == 0) { // Pieces must be in the same stack
        result = s1.indexOf(p1) - s2.indexOf(p2);
      }
    }
    return result;
  }

  /** Implements a psudo-cursor that follows the mouse cursor when user drags gamepieces.
   * Supports map zoom by resizing cursor when it enters a drop target of type Map.View
   *
   * @author Jim Urbas
   * @version 0.4.2
   *
   */
  // NOTE: DragSource.isDragImageSupported() returns false for j2sdk1.4.2_02 on Windows 2000

  static public class DragHandler implements DragGestureListener, DragSourceListener,
      DragSourceMotionListener, DropTargetListener {

    final int CURSOR_ALPHA = 127;     // psuedo cursor is 50% transparent
    final int EXTRA_BORDER = 4;       // psuedo cursor is includes a 4 pixel border

    static private DragHandler theDragHandler = null; // singleton pattern

    private JLabel dragCursor;    // An image label. Lives on current DropTarget's LayeredPane.
    private Point drawOffset = new Point(); // translates event coords to local drawing coords

    Rectangle boundingBox;  // image bounds
    private int dragPieceOffCenterX; // How far drag STARTED from gamepiece's center
    private int dragPieceOffCenterY; // "
    private double dragPieceOffCenterZoom = 1.0;  // zoom at start of drag
    private int cursorOffCenterX;    // How far cursor is CURRENTLY off-center, a function of dragPieceOffCenter{X,Y,Zoom}
    private int cursorOffCenterY;    // "
    double dragCursorZoom = 1.0; // Current cursor scale (zoom)

    Component dragWin; // the component that initiated the drag operation
    Component dropWin; // the drop target the mouse is currently over
    JLayeredPane drawWin; // the component that owns our psuedo-cursor

    // Seems there can be only one DropTargetListener a drop target.  After we process a drop target
    // event, we manually pass the event on to this listener.
    java.util.Map dropTargetListeners = new java.util.HashMap();


    /** returns the singleton DragHandler instance */
    static public DragHandler getTheDragHandler() {
      if (theDragHandler == null) {
        theDragHandler = new DragHandler();
      }
      return theDragHandler;
    }


    /** Creates a new DropTarget and hooks us into the beginning of a
     * DropTargetListener chain. DropTarget events are not multicast; there
     * can be only one "true" listener */
    static public DropTarget makeDropTarget(Component theComponent, int dndContants, DropTargetListener dropTargetListener) {
      if (dropTargetListener != null) {
        DragHandler.getTheDragHandler().dropTargetListeners.put(theComponent, dropTargetListener);
      }
      DropTarget dropTarget = new DropTarget(theComponent, dndContants, DragHandler.getTheDragHandler());
      return dropTarget;
    }

    static public void removeDropTarget(Component theComponent) {
      DragHandler.getTheDragHandler().dropTargetListeners.remove(theComponent);
    }

    protected DropTargetListener getListener(DropTargetEvent event) {
      Component component = event.getDropTargetContext().getComponent();
      return (DropTargetListener) dropTargetListeners.get(component);
    }

    /** CTOR */
    private DragHandler() {
      if (theDragHandler != null) {
        throw new java.lang.RuntimeException("There can be no more than one DragHandler!");
      }
    }


    /** Moves the drag cursor on the current draw window */
    protected void moveDragCursor(int dragX, int dragY) {
      if (drawWin != null) {
        dragCursor.setLocation(dragX - drawOffset.x, dragY - drawOffset.y);
        if (dropWin instanceof Map.View) {
          Map map = ((Map.View) dropWin).getMap();
        }
      }
    }


    /** Removes the drag cursor from the current draw window */
    private void removeDragCursor() {
      if (drawWin != null) {
        if (dragCursor != null) {
          dragCursor.setVisible(false);
          drawWin.remove(dragCursor);
        }
        drawWin = null;
      }
    }

    /** calculates the offset between cursor dragCursor positions */
    private void calcDrawOffset() {
      if (drawWin != null) {
        // drawOffset accounts for difference betwen event point (screen coords)
        // and Layered Pane position, boundingBox and off-center drag
        drawOffset.x = -boundingBox.x - cursorOffCenterX + EXTRA_BORDER;
        drawOffset.y = -boundingBox.y - cursorOffCenterY + EXTRA_BORDER;
        SwingUtilities.convertPointToScreen(drawOffset, drawWin);
      }
    }

    /** creates or moves cursor object to given JLayeredPane. Usually called by setDrawWinToOwnerOf() */
    private void setDrawWin(JLayeredPane newDrawWin) {
      if (newDrawWin != drawWin) {
        // remove cursor from old window
        if (dragCursor.getParent() != null) {
          dragCursor.getParent().remove(dragCursor);
        }
        drawWin = newDrawWin;

        calcDrawOffset();
        dragCursor.setVisible(false);
        drawWin.add(dragCursor, JLayeredPane.DRAG_LAYER);
      }
    }

    /** creates or moves cursor object to given window. Called when drag operation begins in a window
     *  or the cursor is dragged over a new drop-target window */
    public void setDrawWinToOwnerOf(Component newDropWin) {
      if (newDropWin != null) {
        JRootPane rootWin = SwingUtilities.getRootPane(newDropWin);
        if (rootWin != null) {
          setDrawWin(rootWin.getLayeredPane());
        }
      }
    }

    /** Installs the cursor image into our dragCursor JLabel. Sets current zoom.
     * Should be called at beginning of drag and whenever zoom changes.
     *  INPUT:
     *    DragBuffer.getBuffer
     *  OUTPUT:
     *    dragCursorZoom
     *    cursorOffCenterX
     *    cursorOffCenterY
     *    boundingBox
     */
    private void makeDragCursor(double zoom) {
      // create the cursor if necessary
      if (dragCursor == null) {
        dragCursor = new JLabel();
        dragCursor.setVisible(false);
      }

      dragCursorZoom = zoom;
      cursorOffCenterX = (int) (dragPieceOffCenterX / dragPieceOffCenterZoom * zoom + 0.5);
      cursorOffCenterY = (int) (dragPieceOffCenterY / dragPieceOffCenterZoom * zoom + 0.5);

      // get the piece(s) our cursor will be based on
      GamePiece piece = DragBuffer.getBuffer().getIterator().nextPiece();
      // Record sizing info and resize our cursor
      boundingBox = Decorator.getOutermost(piece).getShape().getBounds();
      boundingBox.width *= zoom;
      boundingBox.height *= zoom;
      boundingBox.x *= zoom;
      boundingBox.y *= zoom;
      calcDrawOffset();

      int width = boundingBox.width + EXTRA_BORDER * 2;
      int height = boundingBox.height + EXTRA_BORDER * 2;

      BufferedImage cursorImage = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR);
      piece.draw(cursorImage.createGraphics(), EXTRA_BORDER - boundingBox.x, EXTRA_BORDER - boundingBox.y, dragCursor, zoom);
      dragCursor.setSize(width, height);

      // Make bitmap 50% transparent
      WritableRaster alphaRaster = cursorImage.getAlphaRaster();
      int size = width * height;
      int[] alphaArray = new int[size];
      alphaArray = alphaRaster.getPixels(0, 0, width, height, alphaArray);
      for (int i = 0; i < size; ++i) {
        if (alphaArray[i] == 255)
          alphaArray[i] = CURSOR_ALPHA;
      }

      // ... feather the cursor, since traits can extend arbitraily far out from bounds
      final int FEATHER_WIDTH = EXTRA_BORDER;
      for (int f = 0; f < FEATHER_WIDTH; ++f) {
        int alpha = CURSOR_ALPHA * (f + 1) / FEATHER_WIDTH;
        int limRow = (f + 1) * width - f; // for horizontal runs
        for (int i = f * (width + 1); i < limRow; ++i) {
          if (alphaArray[i] > 0) // North
            alphaArray[i] = alpha;
          if (alphaArray[size - i - 1] > 0) // South
            alphaArray[size - i - 1] = alpha;
        }
        int limVert = size - (f + 1) * width; // for vertical runs
        for (int i = (f + 1) * width + f; i < limVert; i += width) {
          if (alphaArray[i] > 0) // West
            alphaArray[i] = alpha;
          if (alphaArray[size - i - 1] > 0) // East
            alphaArray[size - i - 1] = alpha;
        }
      }
      // ... apply the alpha to the image
      alphaRaster.setPixels(0, 0, width, height, alphaArray);

      // store the bitmap in the cursor
      dragCursor.setIcon(new ImageIcon(cursorImage));
    }


    /////////////////////////////////////////////////////////////////////////////////////
    // DRAG GESTURE LISTENER INTERFACE
    //
    // EVENT uses SCALED, DRAG-SOURCE coordinate system.
    // PIECE uses SCALED, OWNER (arbitrary) coordinate system
    //
    /////////////////////////////////////////////////////////////////////////////////////

    /** Fires after user begins moving the mouse several pixels over a map. */
    public void dragGestureRecognized(DragGestureEvent dge) {
      if (DragBuffer.getBuffer().getIterator().hasMoreElements()) {
        if (Info.is2dEnabled()) {

          Map map = dge.getComponent() instanceof Map.View ? ((Map.View) dge.getComponent()).getMap() : null;
          GamePiece piece = DragBuffer.getBuffer().getIterator().nextPiece();
          Point mousePosition = map == null ? dge.getDragOrigin() : map.componentCoordinates(dge.getDragOrigin());
          Point piecePosition = map == null ? piece.getPosition() : map.componentCoordinates(piece.getPosition());

          // If DragBuffer holds a piece with invalid coordinates (for example, a card drawn from a deck),
          // drag from center of piece
          if (piecePosition.x <= 0 || piecePosition.y <= 0) {
            piecePosition = mousePosition;
          }

          // Pieces in an expanded stack need to be offset
          if (piece.getParent() != null && piece.getParent().isExpanded() && map != null) {
            StackMetrics metrics = map == null ? piece.getParent().getDefaultMetrics() : map.getStackMetrics();
            Point offset = piece.getMap().getStackMetrics().relativePosition(piece.getParent(), piece);
            piecePosition.translate(offset.x, offset.y);
          }

          dragPieceOffCenterX = piecePosition.x - mousePosition.x; // dragging from UL results in positive offsets
          dragPieceOffCenterY = piecePosition.y - mousePosition.y;
          dragPieceOffCenterZoom = map == null ? 1.0 : map.getZoom();

          dragWin = dge.getComponent();
          drawWin = null;
          dropWin = null;

          makeDragCursor(dragPieceOffCenterZoom);

          setDrawWinToOwnerOf(dragWin);

          SwingUtilities.convertPointToScreen(mousePosition, drawWin);
          moveDragCursor(mousePosition.x, mousePosition.y);
        }

        // begin dragging
        dge.startDrag(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR), new StringSelection(""), this); //DEBUG
        dge.getDragSource().addDragSourceMotionListener(this);
      }
    }

    /////////////////////////////////////////////////////////////////////////////////////
    // DRAG SOURCE LISTENER INTERFACE
    //
    /////////////////////////////////////////////////////////////////////////////////////
    public void dragDropEnd(DragSourceDropEvent e) {
      removeDragCursor();
    }

    public void dragEnter(DragSourceDragEvent e) {
    }

    public void dragExit(DragSourceEvent e) {
    }

    public void dragOver(DragSourceDragEvent e) {
    }

    public void dropActionChanged(DragSourceDragEvent e) {
    }

    /////////////////////////////////////////////////////////////////////////////////////
    // DRAG SOURCE MOTION LISTENER INTERFACE
    //
    // EVENT uses UNSCALED, SCREEN coordinate system
    //
    /////////////////////////////////////////////////////////////////////////////////////

    // Used to check for real mouse movement.
    // Warning: dragMouseMoved fires 8 times for each point on development system (Win2k)
    Point lastDragLocation = new Point();

    /** Moves cursor after mouse */
    public void dragMouseMoved(DragSourceDragEvent event) {

      if (!event.getLocation().equals(lastDragLocation)) {
        lastDragLocation = event.getLocation();
        moveDragCursor(event.getX(), event.getY());
        if (!dragCursor.isVisible()) {
          dragCursor.setVisible(true);
        }
      }
    }

    /////////////////////////////////////////////////////////////////////////////////////
    // DROP TARGET INTERFACE
    //
    // EVENT uses UNSCALED, DROP-TARGET coordinate system
    /////////////////////////////////////////////////////////////////////////////////////

    /** switches current drawWin when mouse enters a new DropTarget */
    public void dragEnter(DropTargetDragEvent event) {
      Component newDropWin = event.getDropTargetContext().getComponent();
      if (newDropWin != dropWin) {
        double newZoom = newDropWin instanceof Map.View ? ((Map.View) newDropWin).getMap().getZoom() : 1.0;
        if (Math.abs(newZoom - dragCursorZoom) > 0.01) {
          makeDragCursor(newZoom);
        }
        setDrawWinToOwnerOf(event.getDropTargetContext().getComponent());
        dropWin = newDropWin;
      }

      DropTargetListener forward = getListener(event);
      if (forward != null)
        forward.dragEnter(event);
    }

    /** Last event of the drop operation. We adjust the drop point for off-center drag,
     *  remove the cursor, and pass the event along listener chain.
     */
    public void drop(DropTargetDropEvent event) {
      removeDragCursor();

      // EVENT uses UNSCALED, DROP-TARGET coordinate system
      event.getLocation().translate(cursorOffCenterX, cursorOffCenterY);

      DropTargetListener forward = getListener(event);
      if (forward != null)
        forward.drop(event);
    }

    /** ineffectual. Passes event along listener chain */
    public void dragExit(DropTargetEvent event) {
      DropTargetListener forward = getListener(event);
      if (forward != null)
        forward.dragExit(event);
    }

    /** ineffectual. Passes event along listener chain */
    public void dragOver(DropTargetDragEvent event) {
      DropTargetListener forward = getListener(event);
      if (forward != null)
        forward.dragOver(event);
    }

    /** ineffectual. Passes event along listener chain */
    public void dropActionChanged(DropTargetDragEvent event) {
      DropTargetListener forward = getListener(event);
      if (forward != null)
        forward.dropActionChanged(event);
    }
  }
}
