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

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.*;
import VASSAL.build.module.Map;
import VASSAL.configure.*;
import VASSAL.counters.*;
import VASSAL.command.*;
import VASSAL.Info;

import java.awt.*;
import java.awt.geom.AffineTransform;
import javax.swing.*;
import java.awt.event.KeyEvent;
import java.util.Enumeration;
import java.util.Vector;
import java.net.MalformedURLException;
import java.io.File;

/**
 * Encapsulates information on how to draw expanded and unexpanded
 * views of a stack
 */
public class StackMetrics extends AbstractConfigurable {
  protected int exSepX, exSepY;
  protected int unexSepX, unexSepY;
  protected boolean disabled = false;

  protected KeyStroke topKey = KeyStroke.getKeyStroke(KeyEvent.VK_UP, 0);
  protected KeyStroke bottomKey = KeyStroke.getKeyStroke(KeyEvent.VK_DOWN, 0);
  protected KeyStroke upKey = KeyStroke.getKeyStroke(KeyEvent.VK_RIGHT, 0);
  protected KeyStroke downKey = KeyStroke.getKeyStroke(KeyEvent.VK_LEFT, 0);

  protected PieceFilter unselectedVisible;
  protected PieceFilter selectedVisible;

  protected Color blankColor;

  public static final String EXSEP_X = "exSepX";
  public static final String EXSEP_Y = "exSepY";
  public static final String UNEXSEP_X = "unexSepX";
  public static final String UNEXSEP_Y = "unexSepY";
  public static final String DISABLED = "disabled";
  public static final String TOP_KEY = "top";
  public static final String BOTTOM_KEY = "bottom";
  public static final String UP_KEY = "up";
  public static final String DOWN_KEY = "down";
  public static final String COLOR = "color";

  public void setAttribute(String name, Object value) {
    if (EXSEP_X.equals(name)) {
      if (value instanceof String) {
        try {
          exSepX = Integer.parseInt((String) value);
        }
        catch (NumberFormatException NaN) {
        }
      }
      else if (value != null) {
        exSepX = ((Integer) value).intValue();
      }
      //	    exSepX = new Double((String)value).doubleValue();
    }
    else if (EXSEP_Y.equals(name)) {
      if (value instanceof String) {
        try {
          exSepY = Integer.parseInt((String) value);
        }
        catch (NumberFormatException NaN) {
        }
      }
      else if (value != null) {
        exSepY = ((Integer) value).intValue();
      }
      //	    exSepY = new Double((String)value).doubleValue();
    }
    else if (UNEXSEP_X.equals(name)) {
      if (value instanceof String) {
        try {
          unexSepX = Integer.parseInt((String) value);
        }
        catch (NumberFormatException NaN) {
        }
      }
      else if (value != null) {
        unexSepX = ((Integer) value).intValue();
      }
      //	    unexSepX = new Double((String)value).doubleValue();
    }
    else if (UNEXSEP_Y.equals(name)) {
      if (value instanceof String) {
        try {
          unexSepY = Integer.parseInt((String) value);
        }
        catch (NumberFormatException NaN) {
        }
      }
      else if (value != null) {
        unexSepY = ((Integer) value).intValue();
      }
      //	    unexSepY = new Double((String)value).doubleValue();
    }
    else if (DISABLED.equals(name)) {
      if (value instanceof String) {
        value = new Boolean((String) value);
      }
      disabled = ((Boolean) value).booleanValue();
    }
    else if (TOP_KEY.equals(name)) {
      topKey = HotKeyConfigurer.decode((String) value);
    }
    else if (BOTTOM_KEY.equals(name)) {
      bottomKey = HotKeyConfigurer.decode((String) value);
    }
    else if (UP_KEY.equals(name)) {
      upKey = HotKeyConfigurer.decode((String) value);
    }
    else if (DOWN_KEY.equals(name)) {
      downKey = HotKeyConfigurer.decode((String) value);
    }
    else if (COLOR.equals(name)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      blankColor = (Color) value;
    }
  }

  public String getAttributeValueString(String name) {
    if (EXSEP_X.equals(name)) {
      return "" + exSepX;
    }
    else if (EXSEP_Y.equals(name)) {
      return "" + exSepY;
    }
    else if (UNEXSEP_X.equals(name)) {
      return "" + unexSepX;
    }
    else if (UNEXSEP_Y.equals(name)) {
      return "" + unexSepY;
    }
    else if (DISABLED.equals(name)) {
      return "" + disabled;
    }
    else if (TOP_KEY.equals(name)) {
      return HotKeyConfigurer.encode(topKey);
    }
    else if (BOTTOM_KEY.equals(name)) {
      return HotKeyConfigurer.encode(bottomKey);
    }
    else if (UP_KEY.equals(name)) {
      return HotKeyConfigurer.encode(upKey);
    }
    else if (DOWN_KEY.equals(name)) {
      return HotKeyConfigurer.encode(downKey);
    }
    else if (COLOR.equals(name)) {
      return blankColor == null ? null
          : ColorConfigurer.colorToString(blankColor);
    }
    return null;
  }

  public void addTo(Buildable b) {
    ((Map) b).setStackMetrics(this);
  }

  public StackMetrics() {
    this(false, 6, 18, 2, 4);
  }

  public StackMetrics(boolean dis,
                      int exSx, int exSy,
                      int unexSx, int unexSy) {
    disabled = dis;
    exSepX = exSx;
    exSepY = exSy;
    unexSepX = unexSx;
    unexSepY = unexSy;

    unselectedVisible = new PieceFilter() {
      public boolean accept(GamePiece piece) {
        return !Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))
            && !Boolean.TRUE.equals(piece.getProperty(Properties.SELECTED));
      }
    };
    selectedVisible = new PieceFilter() {
      public boolean accept(GamePiece piece) {
        return !Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))
            && Boolean.TRUE.equals(piece.getProperty(Properties.SELECTED));
      }
    };
  }

  /**
   * Different instances of StackMetrics may render a {@link Stack}
   * in different ways.  The default algorithm is: If not expanded,
   * all but the top visible GamePiece is drawn as a white square
   * with size given by {@link GamePiece#getShape}.  The
   * separation between GamePieces is given by {@link
   * #relativePosition}
   *
   * If expanded, all GamePieces are drawn with separation given by
   * {@link #relativePosition}.  GamePiece that are selected are
   * drawn in front of other GamePieces, even those above them in
   * the stack*/
  public void draw(Stack stack, Graphics g, int x, int y, Component obs, double zoom) {
    Highlighter highlighter = stack.getMap() == null ? BasicPiece.getHighlighter() : stack.getMap().getHighlighter();
    Point[] positions = new Point[stack.getPieceCount()];
    getContents(stack, positions, null, null, x, y);
    for (PieceIterator e = new PieceIterator(stack.getPieces(), unselectedVisible); e.hasMoreElements();) {
      GamePiece next = e.nextPiece();
      int index = stack.indexOf(next);
      int nextX = x + (int) (zoom * (positions[index].x - x));
      int nextY = y + (int) (zoom * (positions[index].y - y));
      if (stack.isExpanded() || !e.hasMoreElements()) {
        next.draw(g,
                  nextX,
                  nextY,
                  obs, zoom);
      }
      else {
        drawUnexpanded(next, g, nextX, nextY, obs, zoom);
      }
    }
    for (PieceIterator e = new PieceIterator(stack.getPieces(), selectedVisible); e.hasMoreElements();) {
      GamePiece next = e.nextPiece();
      int index = stack.indexOf(next);
      int nextX = x + (int) (zoom * (positions[index].x - x));
      int nextY = y + (int) (zoom * (positions[index].y - y));
      next.draw(g,
                nextX,
                nextY,
                obs, zoom);
      highlighter.draw
          (next, g,
           nextX,
           nextY,
           obs, zoom);
    }
  }

  /**
   * Draw only those pieces in the target stack whose boundingBoxes fall within the given visibleRect
   * This method is considerably faster than the other draw method.
   * @param stack
   * @param g
   * @param location the location of the stack in component coordinates
   * @param zoom
   * @param visibleRect the visible rectangle in component coordinates
   */
  public void draw(Stack stack, Point location, Graphics g, Map map, double zoom, Rectangle visibleRect) {
    Highlighter highlighter = map.getHighlighter();
    Point mapLocation = map.mapCoordinates(location);
    Rectangle region = visibleRect == null ? null : map.mapRectangle(visibleRect);
    Point[] positions = new Point[stack.getPieceCount()];
    Rectangle[] bounds = region == null ? null : new Rectangle[stack.getPieceCount()];
    getContents(stack, positions, null, bounds, mapLocation.x, mapLocation.y);
    for (PieceIterator e = new PieceIterator(stack.getPieces(), unselectedVisible); e.hasMoreElements();) {
      GamePiece next = e.nextPiece();
      int index = stack.indexOf(next);
      Point pt = map.componentCoordinates(positions[index]);
      if (bounds == null || isVisible(region, bounds[index])) {
        if (stack.isExpanded() || !e.hasMoreElements()) {
          next.draw(g,
                    pt.x,
                    pt.y,
                    map.getView(), zoom);
        }
        else {
          drawUnexpanded(next, g, pt.x, pt.y, map.getView(), zoom);
        }
      }
    }
    for (PieceIterator e = new PieceIterator(stack.getPieces(), selectedVisible); e.hasMoreElements();) {
      GamePiece next = e.nextPiece();
      int index = stack.indexOf(next);
      if (bounds == null || isVisible(region, bounds[index])) {
        Point pt = map.componentCoordinates(positions[index]);
        next.draw(g,
                  pt.x,
                  pt.y,
                  map.getView(), zoom);
        highlighter.draw
            (next, g,
             pt.x,
             pt.y,
             map.getView(), zoom);
      }
    }
  }

  private boolean isVisible(Rectangle region, Rectangle bounds) {
    boolean visible = true;
    if (region != null) {
      visible = region.intersects(bounds);
    }
    return visible;
  }

  /**
   * Draw a {@link GamePiece} that is not the top piece in an unexpanded {@link Stack}
   *
   * Default implementation is a white square with a black border
   */
  protected void drawUnexpanded(GamePiece p, Graphics g,
                                int x, int y, Component obs, double zoom) {
    if (blankColor == null) {
      p.draw(g, x, y, obs, zoom);
    }
    else if (Info.is2dEnabled()) {
      Graphics2D g2d = (Graphics2D) g;
      g.setColor(blankColor);
      Shape s = p.getShape();
      AffineTransform t = AffineTransform.getScaleInstance(zoom,zoom);
      t.translate(x/zoom,y/zoom);
      s = t.createTransformedShape(s);
      g2d.fill(s);
      g.setColor(Color.black);
      g2d.draw(s);
    }
    else {
      g.setColor(blankColor);
      Rectangle r = p.getShape().getBounds();
      r.setSize((int) (zoom * r.width), (int) (zoom * r.height));
      r.setLocation(x - r.width / 2, y - r.height / 2);
      g.fillRect(r.x, r.y, r.width, r.height);
      g.setColor(Color.black);
      g.drawRect(r.x, r.y, r.width, r.height);
    }
  }

  /**
   * Fill the argument arrays with the positions, selection bounds and bounding boxes of the pieces in the argument stack
   * @param parent The parent Stack
   * @param positions If non-null will contain a {@link Point} giving the position of each piece in <code>parent</code>
   * @param shapes If non-null will contain a {@link Shape} giving the shape of for each piece in <code>parent</code>
   * @param boundingBoxes If non-null will contain a {@link Rectangle} giving the bounding box for each piece in <code>parent</code>
   * @param x the x-location of the parent
   * @param y the y-location of the parent
   * @return the number of pieces processed in the stack
   */
  public int getContents(Stack parent, Point[] positions, Shape[] shapes, Rectangle[] boundingBoxes, int x, int y) {
    int count = parent.getPieceCount();
    if (positions != null) {
      count = Math.min(count, positions.length);
    }
    if (boundingBoxes != null) {
      count = Math.min(count, boundingBoxes.length);
    }
    if (shapes != null) {
      count = Math.min(count,shapes.length);
    }
    int dx = parent.isExpanded() ? exSepX : unexSepX;
    int dy = parent.isExpanded() ? exSepY : unexSepY;
    Point currentPos = null, nextPos = null;
    Rectangle currentSelBounds = null, nextSelBounds = null;
    for (int index = 0; index < count; ++index) {
      GamePiece child = parent.getPieceAt(index);
      if (Boolean.TRUE.equals(child.getProperty(Properties.INVISIBLE_TO_ME))) {
        Rectangle blank = new Rectangle(x, y, 0, 0);
        if (positions != null) {
          positions[index] = blank.getLocation();
        }
        if (boundingBoxes != null) {
          boundingBoxes[index] = blank;
        }
        if (shapes != null) {
          shapes[index] = blank;
        }
      }
      else {
        nextSelBounds = child.getShape().getBounds();
        nextPos = new Point(0,0);
        if (currentPos == null) {
          currentSelBounds = nextSelBounds;
          currentSelBounds.translate(x, y);
          currentPos = new Point(x, y);
          nextPos = currentPos;
        }
        else {
          nextPosition(currentPos, currentSelBounds, nextPos, nextSelBounds, dx, dy);
        }
        if (positions != null) {
          positions[index] = nextPos;
        }
        if (boundingBoxes != null) {
          Rectangle bbox = child.boundingBox();
          bbox.translate(nextPos.x, nextPos.y);
          boundingBoxes[index] = bbox;
        }
        if (shapes != null) {
          Shape s = child.getShape();
          s = AffineTransform.getTranslateInstance(nextPos.x,nextPos.y).createTransformedShape(s);
          shapes[index] = s;
        }
        currentPos = nextPos;
        currentSelBounds = nextSelBounds;
      }
    }
    return count;
  }

  private void nextPosition(Point currentPos, Rectangle currentBounds, Point nextPos, Rectangle nextBounds, int dx, int dy) {
    int deltaX,deltaY;
    if (dx > 0) {
      deltaX = currentBounds.x + dx - nextBounds.x;
    }
    else if (dx < 0) {
      deltaX = currentBounds.x + currentBounds.width - nextBounds.width + dx - nextBounds.x;
    }
    else {
      deltaX = currentPos.x - nextPos.x;
    }
    if (dy > 0) {
      deltaY = currentBounds.y + currentBounds.height - nextBounds.height - nextBounds.y - dy;
    }
    else if (dy < 0) {
      deltaY = currentBounds.y - dy - nextBounds.y;
    }
    else {
      deltaY = currentPos.y - nextPos.y;
    }
    nextBounds.translate(deltaX, deltaY);
    nextPos.translate(deltaX, deltaY);
  }

  public Point relativePosition(Stack parent, GamePiece c) {
    int index = parent.indexOf(c);
    if (index < 0) {
      throw new RuntimeException(c.getType() + " is not contained in " + parent.getId());
    }
    Point[] pos = new Point[index + 1];
    getContents(parent, pos, null, null, 0, 0);
    return pos[index];
  }

  public boolean isStackingEnabled() {
    return !disabled;
  }

  public void removeFrom(Buildable parent) {
    throw new IllegalBuildException("Required Component");
  }

  public String getConfigureName() {
    return null;
  }

  public static String getConfigureTypeName() {
    return "Stacking options";
  }

  public HelpFile getHelpFile() {
    File dir = new File("docs");
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "Map.htm"), "#StackingOptions");
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public String[] getAttributeNames() {
    String[] s = {DISABLED, EXSEP_X, EXSEP_Y, UNEXSEP_X, UNEXSEP_Y,
                  COLOR, TOP_KEY, BOTTOM_KEY, UP_KEY, DOWN_KEY};
    return s;
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Disable stacking",
                        "Horizontal separation when expanded",
                        "Vertical separation when expanded",
                        "Horizontal separation when not expanded",
                        "Vertical separation when not expanded",
                        "Color of pieces when not expanded"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{Boolean.class,
                       Integer.class,
                       Integer.class,
                       Integer.class,
                       Integer.class,
                       Color.class};
  }

  private VisibilityCondition cond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return !disabled;
    }
  };

  public VisibilityCondition getAttributeVisibility(String name) {
    if (name.equals(EXSEP_X)
        || name.equals(EXSEP_Y)
        || name.equals(UNEXSEP_X)
        || name.equals(UNEXSEP_Y)
        || name.equals(COLOR)) {
      return cond;
    }
    else {
      return null;
    }
  }

  public Stack createStack(GamePiece p) {
    if (isStackingEnabled()) {
      Stack s = new Stack(p);
      return s;
    }
    else {
      return null;
    }
  }

  public KeyStroke getMoveUpKey() {
    return upKey;
  }

  public KeyStroke getMoveDownKey() {
    return downKey;
  }

  public KeyStroke getMoveTopKey() {
    return topKey;
  }

  public KeyStroke getMoveBottomKey() {
    return bottomKey;
  }

  /**
   * Place a GamePiece on top of another GamePiece
   * Create/remove stacks as necessary
   * @return a Command that accomplishes this task
   */
  public Command merge(GamePiece fixed, GamePiece add) {
    Command comm;
    if (!isStackingEnabled()) {
      comm = fixed.getMap().placeAt(add, fixed.getPosition());
    }
    else if (fixed instanceof Stack && ((Stack) fixed).topPiece() != null) {
      comm = merge(((Stack) fixed).topPiece(), add);
    }
    else if (Boolean.TRUE.equals(add.getProperty(Properties.NO_STACK))
        || Boolean.TRUE.equals(fixed.getProperty(Properties.NO_STACK))) {
      comm = fixed.getMap().placeAt(add, fixed.getPosition());
    }
    else {
      MoveTracker tracker = new MoveTracker(add);
      comm = new NullCommand();
      Stack fixedParent = fixed.getParent();
      int index = fixedParent == null ? 1 : fixedParent.indexOf(fixed) + 1;
      if (add != fixed
          && add != fixed.getParent()) {
        boolean isNewPiece = GameModule.getGameModule().getGameState()
            .getPieceForId(add.getId()) == null;
        if (fixedParent == null) {
          if (fixed instanceof Stack) {
            fixedParent = (Stack) fixed;
          }
          else {
            fixedParent = createStack(fixed);
            GameModule.getGameModule().getGameState().addPiece(fixedParent);
            fixed.getMap().addPiece(fixedParent);
            comm = comm.append(new AddPiece(fixedParent));
          }
        }
        if (isNewPiece) {
          GameModule.getGameModule().getGameState().addPiece(add);
          comm = comm.append(new AddPiece(add));
        }
        if (add instanceof Stack) {
          Vector v = new Vector();
          for (Enumeration e = ((Stack) add).getPieces();
               e.hasMoreElements();) {
            v.addElement(e.nextElement());
          }
          for (Enumeration e = v.elements();
               e.hasMoreElements();) {
            GamePiece p = (GamePiece) e.nextElement();
            MoveTracker t = new MoveTracker(p);
            fixedParent.insert(p, index++);
            comm = comm.append(t.getMoveCommand());
          }
        }
        else {
          if (add.getParent() == fixedParent && fixedParent != null) {
            index--;
          }
          fixedParent.insert(add, index);
          comm = comm.append(tracker.getMoveCommand());
        }
      }
/*
      comm = new NullCommand();
      Stack fixedParent = fixed.getParent();
      String fixedParentOldState = fixedParent == null ? null : fixedParent.getState();
      int index = fixedParent == null ? 1 : fixedParent.indexOf(fixed) + 1;
      if (add != fixed
        && add != fixed.getParent()) {
        boolean isNewPiece = GameModule.getGameModule().getGameState()
          .getPieceForId(add.getId()) == null;
        if (isNewPiece) {
          GameModule.getGameModule().getGameState().addPiece(add);
          comm = comm.append(new AddPiece(add));
        }
        if (fixedParent == null) {
          fixedParent = createStack(fixed);
          GameModule.getGameModule().getGameState().addPiece(fixedParent);
          fixed.getMap().addPiece(fixedParent);
        }
        if (add instanceof Stack) {
          Vector v = new Vector();
          for (Enumeration e = ((Stack) add).getPieces();
               e.hasMoreElements();) {
            v.addElement(e.nextElement());
          }
          Command c = new RemovePiece(add);
          c.execute();
          comm = comm.append(c);
          for (Enumeration e = v.elements();
               e.hasMoreElements();) {
            GamePiece p = (GamePiece) e.nextElement();
            fixedParent.insert(p, index++);
          }
        }
        else {
          Stack removedFrom = add.getParent();
          String oldState = null;
          if (add.getParent() == fixedParent && fixedParent != null) {
            index--;
          }
          else if (removedFrom != null) {
            oldState = removedFrom.getState();
          }
          fixedParent.insert(add, index);
          if (oldState != null) {
            comm = comm.append(new ChangePiece(removedFrom.getId(),
                                               oldState, removedFrom.getState()));
          }
        }
        if (fixedParentOldState != null) {
          comm = comm.append(new ChangePiece(fixedParent.getId(),
                                             fixedParentOldState,
                                             fixedParent.getState()));
        }
        else if (fixedParent != null) {
          comm = comm.append(new AddPiece(fixedParent));
        }
      }
*/
    }
    return comm;
  }
}
