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
import java.awt.image.BufferedImage;
import java.awt.image.WritableRaster;
import java.awt.datatransfer.StringSelection;
import java.awt.dnd.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.net.URL;
import java.util.Enumeration;

/**
 * This is a MouseListener that moves pieces on a Map window
 */
public class PieceMover extends AbstractBuildable implements
    MouseListener, GameComponent, PieceFinder, Sort.Comparator {
  /* Test cases for moving pieces:
  - Simple click on piece -> no move
  - Click and drag piece to within its own outline:  move if to new grid snap point or > 5 pixels away
  */
  /**
   * The Preferences key for autoreporting moves.
   */
  public static final String AUTO_REPORT = "autoReport";

  protected Map map;
  protected Point dragBegin;
  private JButton markUnmovedButton;

  public void addTo(Buildable b) {
    map = (Map) b;
    map.addLocalMouseListener(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    if (Info.isDndEnabled()) {
      DragHandler dh = new DragHandler();
      map.setDragGestureListener(dh);
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
    if (!hasMoved
        || shouldMarkMoved()) {
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


  public Command movePieces(Map m, Point p) {
    DragBuffer.getBuffer().sort(this);

    PieceIterator it = DragBuffer.getBuffer().getIterator();

    if (!it.hasMoreElements()) {
      return null;
    }

    GamePiece mergeWith = m.findPiece(p, this);

    GamePiece bottom = it.nextPiece();
    KeyBuffer.getBuffer().clear();

    if (!Boolean.TRUE.equals(bottom.getProperty(Properties.IGNORE_GRID))) {
      p = m.snapTo(p);
    }

    String myId = GameModule.getUserId();
    GameModule.setUserId("yendoR117");  // Don't report my hidden or concealed units
    StringBuffer moved = new StringBuffer();
    if (bottom.getMap() == m) {
      moved.append(bottom.getName());
    }
    GameModule.setUserId(myId);

    Command comm = new NullCommand();
    String destination;
    String origin = m.locationName(bottom.getPosition());
    if (mergeWith == null) {
      comm = comm.append(movedPiece(bottom, p));
      comm = comm.append(m.placeAt(bottom, p));
      if (!(bottom instanceof Stack)
          && !Boolean.TRUE.equals(bottom.getProperty(Properties.NO_STACK))) {
        Stack parent = m.getStackMetrics().createStack(bottom);
        if (parent != null) {
          comm = comm.append(m.placeAt(parent, p));
        }
      }
      destination = m.locationName(p);
    }
    else {
      comm = comm.append(movedPiece(bottom, mergeWith.getPosition()));
      comm = comm.append(m.getStackMetrics().merge(mergeWith, bottom));
      destination = m.locationName(mergeWith.getPosition());
    }

    while (it.hasMoreElements()) {
      GamePiece next = it.nextPiece();
      GameModule.setUserId("yendoR117");  // Don't report my hidden or concealed units
      if (next.getMap() == m && next.getName().length() > 0) {
        moved.append(',');
        moved.append(next.getName());
      }
      GameModule.setUserId(myId);
      String nextOrigin = m.locationName(next.getPosition());
      if (nextOrigin == null
          || !nextOrigin.equals(origin)) {
        origin = null;
      }
      comm = comm.append(movedPiece(next, bottom.getPosition()));
      comm = comm.append(m.getStackMetrics().merge(bottom, next));
      bottom = next;
    }
    String s = moved.toString();
    if (comm != null
        && !comm.isNull()
        && destination != null
        && s.length() > 0
        && GlobalOptions.getInstance().autoReportEnabled()) {
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
    return comm;
  }

  /**
   * This listener is used for faking drag-and-drop on Java 1.1 systems
   * @param e
   */
  public void mousePressed(MouseEvent e) {
    if (canHandleEvent(e)) {
      selectMovablePieces(e.getPoint());
      if (!Info.isDndEnabled() && DragBuffer.getBuffer().getIterator().hasMoreElements()) {
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
          if (piece != p
              && piece.getParent() != p) {
            DragBuffer.getBuffer().add(piece);
          }
        }
      }
      else {
        // Otherwise, only put the clicked-on piece into the drag buffer
        DragBuffer.getBuffer().add(p);
      }
    }
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
    if (b == null
        || b.getGrid() == null) {
      selected = PieceFinder.MOVABLE.select(map, piece, pt);
    }
    else {
      Point snap = map.snapTo(pt);
      if (piece instanceof Stack) {
        Stack s = (Stack) piece;
        if (s.isExpanded()) {
          Shape[] bounds = new Shape[s.getPieceCount()];
          map.getStackMetrics().getContents(s, null, bounds, null, s.getPosition().x, s.getPosition().y);
          for (Enumeration e = s.getPiecesInVisibleOrder();
               e.hasMoreElements();) {
            GamePiece child = (GamePiece) e.nextElement();
            if (bounds[s.indexOf(child)].contains(pt)) {
              selected = child;
              break;
            }
          }
        }
        else if (s.getPosition().equals(snap)
            && s.topPiece() != null) {
          selected = s;
        }
      }
      else if (piece.getPosition().equals(snap)
          && !Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))) {
        selected = piece;
      }
    }
    if (selected != null
        && Boolean.TRUE.equals(selected.getProperty(Properties.NO_STACK))) {
      selected = null;
    }
    if (selected != null
        && DragBuffer.getBuffer().contains(selected)) {
      /* If clicking and dragging to within the outline of the same piece,
      ignore this piece if it's a stack or the only visible piece in a stack */
      if (selected.getParent() == null) {
        selected = null;
      }
      else {
        Enumeration e = selected.getParent().getPiecesInVisibleOrder();
        if (e.hasMoreElements()
            && e.nextElement() == selected
            && !e.hasMoreElements()) {
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
    if (p1.getMap() == null
        && p2.getMap() == null) {
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

  private class DragHandler implements DragSourceListener, DragGestureListener {

    JLabel dragCursor;

    Point cursorOffset = new Point(); // translates event coords to local drawing coords


    public void dragGestureRecognized( DragGestureEvent dge ) {
      System.out.println( "PieceMover.DragHandler.dragGestureRecognized()" );

      if ( DragBuffer.getBuffer().getIterator().hasMoreElements() ) {

        // get the piece(s) our cursor will be based on
        GamePiece piece = DragBuffer.getBuffer().getIterator().nextPiece();

        // create the cursor if necessary
        JFrame gmFrame = GameModule.getGameModule().getFrame();
        if ( dragCursor == null ) {
          dragCursor = new JLabel();
          dragCursor.setVisible(false);
          gmFrame.getLayeredPane().add( dragCursor, javax.swing.JLayeredPane.DRAG_LAYER );
        }

        // Record sizing info and resize our cursor
        Rectangle box  = piece.boundingBox();
        int width      = box.width  + 4;
        int height     = box.height + 4;
        Point piecePos = piece.getPosition();    // local coordinate system, center of piece
        Point mousePos = dge.getDragOrigin();    // local coordinate system
        cursorOffset.x = width  / 2 + gmFrame.getX() + gmFrame.getContentPane().getX() - piecePos.x + mousePos.x + 4;
        cursorOffset.y = height / 2 + gmFrame.getY() + gmFrame.getContentPane().getY() - piecePos.y + mousePos.y;
        dragCursor.setSize( width, height );

        // render the pieces into a buffered bitmap
        BufferedImage cursorImage = new BufferedImage( width, height, BufferedImage.TYPE_4BYTE_ABGR );
        piece.draw( cursorImage.createGraphics(), width / 2, height / 2, dragCursor, 1.0 );

        // Make bitmap 50% transparent
        WritableRaster alphaRaster = cursorImage.getAlphaRaster();
        int size = width * height;
        int[] alphaArray = new int[ size ];
        alphaArray = alphaRaster.getPixels( 0, 0, width, height, alphaArray );
        for ( int i = 0; i < size; ++i ) {
          if ( alphaArray[ i ] == 255 )
            alphaArray[ i ] = 127;
        }
        alphaRaster.setPixels( 0, 0, width, height, alphaArray );

        // store the bitmap in the cursor
        dragCursor.setIcon( new ImageIcon( cursorImage ));

        // begin dragging
        dge.startDrag( Cursor.getPredefinedCursor( Cursor.HAND_CURSOR ), new StringSelection(""), this );
      }
    }


    public void dragEnter( DragSourceDragEvent dsde ) {
      dragCursor.setLocation( dsde.getX() - cursorOffset.x, dsde.getY() - cursorOffset.y );
      dragCursor.setVisible(true);
    }

    public void dragOver( DragSourceDragEvent dsde ) {
      dragCursor.setLocation( dsde.getX() - cursorOffset.x, dsde.getY() - cursorOffset.y );
    }

    public void dropActionChanged( DragSourceDragEvent dsde ) {
    }

    public void dragExit( DragSourceEvent dse ) {
      dragCursor.setVisible(false);
    }

    public void dragDropEnd( DragSourceDropEvent dsde ) {
      map.repaint();
    }
  }
}
