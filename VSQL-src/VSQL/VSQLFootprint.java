/*
 * $Id$
 * 
 * Copyright (c) 2005 by Brent Easton
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License (LGPL) as published by
 * the Free Software Foundation.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, copies are available at
 * http://www.opensource.org.
 */
package VSQL;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.InputEvent;
import java.util.Enumeration;
import java.util.Vector;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import CASL.Map.Hex;
import VASL.counters.MarkMoved;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyCommand;
import VASSAL.counters.KeySpecifier;
import VASSAL.counters.Labeler;
import VASSAL.counters.PieceEditor;
import VASSAL.counters.Properties;
import VASSAL.tools.SequenceEncoder;

public class VSQLFootprint extends MarkMoved {

  public static final String ID = "footprint;";
  public static final String VEHICLE_CA_PROPERTY = "VEHICLE_CA";

  protected static Image[] caImages = new Image[6];

  private char trailKey;
  private KeyCommand[] commands;
  private String command = "Movement Trail";
  protected boolean visible = false;
  protected int currentCA = 1;

  protected int minX, minY, maxX, maxY;
  protected Rectangle myBoundingBox;

  protected static final int CIRCLE_RADIUS = 10;
  protected static final Color CIRCLE_COLOR = Color.BLACK;
  protected static final Color FILL_COLOR = Color.WHITE;
  protected static final Color LINE_COLOR = Color.BLACK;
  protected static final float LINE_WIDTH = 1.0f;
  protected static final int BASE_FONT_SIZE = 14;

  protected static final String VSQL_TYPE_MARKER = "TYPE";
  protected static final String VSQL_VEHICLE_TYPE = "Vehicle";
  protected static final int EDGE_CLIP_LIMIT = 20;

  protected Vector pointList = new Vector();

  public class caPoint extends Point {
    protected int ca = 1;

    public caPoint(int x, int y, int c) {
      super(x, y);
      ca = c;
    }

    public caPoint(Point p, int c) {
      this(p.x, p.y, c);
    }
  }

  public VSQLFootprint() {
    this(ID + "fp;", null);
  }

  public VSQLFootprint(String type, GamePiece p) {
    mySetType(type);
    setInner(p);
  }

  protected Enumeration getPointList() {
    return pointList.elements();
  }

  /**
   * State a count of points, followed by a list of the points. Visibility
   * status of the trail is local to each player
   */
  public void mySetState(String newState) {
    pointList.clear();
    SequenceEncoder.Decoder ss = new SequenceEncoder.Decoder(newState, ';');
    int items = ss.nextInt(0);
    for (int i = 0; i < items; i++) {
      String point = ss.nextToken("");
      if (point.length() != 0) {
        SequenceEncoder.Decoder sp = new SequenceEncoder.Decoder(point, ',');
        int x = sp.nextInt(0);
        int y = sp.nextInt(0);
        int ca = sp.nextInt(1);
        pointList.addElement(new caPoint(x, y, ca));
      }
    }
    if (ss.hasMoreTokens()) {
      visible = (ss.nextToken("false").equals("true"));
    }
    else
      visible = false;
  }

  public String myGetState() {
    String s = pointList.size() + "";
    Enumeration e = getPointList();
    while (e.hasMoreElements()) {
      caPoint p = (caPoint) e.nextElement();
      s += ";" + p.x + "," + p.y + "," + p.ca;
    }
    s += ";" + visible;

    return s;
  }

  /**
   * Type is the character command that toggles footprint visiblity
   */
  public void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    trailKey = st.nextChar('T');
    commands = null;
  }

  public String myGetType() {
    return ID + trailKey;
  }

  /**
   * setMoved is called with an argument of true each time the piece is moved.
   *  
   */
  public void setMoved(boolean justMoved) {

    if (justMoved) {
      Point where = this.getPosition();
      if (pointList.size() == 0) {
        addPoint(where);
      }
      else {
        Point last = (Point) pointList.lastElement();
        if (!last.equals(where)) { // Don't add the same point twice
          addPoint(where);
        }
      }
    }
    else {
      pointList.clear();
      myBoundingBox = null;
    }
    redraw();
  }

  protected void addPoint(Point p) {
    pointList.addElement(new caPoint(p, currentCA));

    getMyBoundingBox();
    
    if (p.x+CIRCLE_RADIUS > maxX) maxX = p.x+CIRCLE_RADIUS;
    if (p.x-CIRCLE_RADIUS < minX) minX = p.x-CIRCLE_RADIUS;
    if (p.y+CIRCLE_RADIUS > maxY) maxY = p.y+CIRCLE_RADIUS;
    if (p.y-CIRCLE_RADIUS < minY) minY = p.y-CIRCLE_RADIUS;
    
    myBoundingBox = new Rectangle(minX, minY, maxX - minX, maxY - minY);

  }
  /**
   * Is this counter a vehicle?
   */
  protected String counterType = null;

  public boolean isVehicle() {
    if (counterType == null) {
      counterType = (String) Decorator.getOutermost(this).getProperty(VSQL_TYPE_MARKER);
    }
    return (counterType.equals(VSQL_VEHICLE_TYPE));
  }

  public void redraw() {
    piece.getMap().repaint();
  }

  public void setProperty(Object key, Object val) {
    if (Properties.MOVED.equals(key)) {
      setMoved(Boolean.TRUE.equals(val));
      piece.setProperty(key, val); // Pass on to MarkMoved
    }
    if (VEHICLE_CA_PROPERTY.equals(key)) {
      try {
        currentCA = Integer.parseInt((String) val);
      }
      catch (Exception e) {

      }
    }
    else {
      super.setProperty(key, val);
    }
  }

  public String getDescription() {
    return "Movement trail";
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {

    int x1, y1, x2, y2;
    int lastCA = 0;

    piece.draw(g, x, y, obs, zoom);

    /*
     * If we are asked to be drawn at a different zoom from the current map zoom
     * setting, then don't draw the trail as it will be in the wrong place.
     * (i.e. Mouse-over viewer)
     */
    double mapZoom = zoom;
    if (this.getMap() != null) {
      mapZoom = this.getMap().getZoom();
    }

    if (visible && (zoom == mapZoom)) {
      Graphics2D g2d = (Graphics2D) g;

      int mapHeight = getMap().mapSize().height;
      int mapWidth = getMap().mapSize().width;
      int edgeHeight = Integer.parseInt(getMap().getAttributeValueString(Map.EDGE_HEIGHT));
      int edgeWidth = Integer.parseInt(getMap().getAttributeValueString(Map.EDGE_WIDTH));

      int edgeClipHeight = (edgeHeight < EDGE_CLIP_LIMIT) ? edgeHeight : EDGE_CLIP_LIMIT;
      int edgeClipWidth = (edgeWidth < EDGE_CLIP_LIMIT) ? edgeWidth : EDGE_CLIP_LIMIT;

      int clipX = edgeWidth - edgeClipWidth;
      int clipY = edgeHeight - edgeClipHeight;
      int width = mapWidth - (2 * edgeWidth) + 2 * edgeClipWidth;
      int height = mapHeight - (2 * edgeHeight) + 2 * edgeClipHeight;

      Rectangle newClip = new Rectangle((int) (clipX * zoom), (int) (clipY * zoom), (int) (width * zoom),
          (int) (height * zoom));
      Rectangle mapRect = new Rectangle(edgeWidth, edgeHeight, mapWidth, mapHeight);
      Rectangle visibleRect = getMap().getView().getVisibleRect();

      Shape oldClip = g.getClip();
      g.setClip(newClip.intersection(visibleRect));

      //Composite oldComposite = g2d.getComposite();
      //g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER,
      // 0.5F));
      g2d.setStroke(new BasicStroke(LINE_WIDTH));
      g2d.setColor(LINE_COLOR);
      Enumeration e = getPointList();
      Point lastP = null;
      Point here = getPosition();
      while (e.hasMoreElements()) {
        caPoint cap = (caPoint) e.nextElement();
        Point p = cap.getLocation();

        if (lastP != null) {
          x1 = (int) (lastP.x * zoom);
          y1 = (int) (lastP.y * zoom);
          x2 = (int) (p.x * zoom);
          y2 = (int) (p.y * zoom);
          g.drawLine(x1, y1, x2, y2);
        }
        lastP = p;
      }
      if (lastP != null) {
        x1 = (int) (lastP.x * zoom);
        y1 = (int) (lastP.y * zoom);
        x2 = (int) (here.x * zoom);
        y2 = (int) (here.y * zoom);
        g.drawLine(x1, y1, x2, y2);
      }
      Font f = new Font("Dialog", Font.PLAIN, (int) (BASE_FONT_SIZE * zoom));
      int step = 0;
      e = getPointList();
      while (e.hasMoreElements()) {
        step += 1;
        caPoint cap = (caPoint) e.nextElement();
        Point p = cap.getLocation();

        if (mapRect.contains(p)) {
          x1 = (int) (p.x * zoom);
          y1 = (int) (p.y * zoom);
          x2 = (int) ((p.x - CIRCLE_RADIUS) * zoom);
          y2 = (int) ((p.y - CIRCLE_RADIUS) * zoom);
          int radius = (int) (CIRCLE_RADIUS * 2 * zoom);
          g.setColor(FILL_COLOR);
          g.fillOval(x2, y2, radius, radius);
          g.setColor(CIRCLE_COLOR);
          g.drawOval(x2, y2, radius, radius);
          //g.drawOval(p.x - CIRCLE_RADIUS + 1, p.y - CIRCLE_RADIUS + 1,
          // CIRCLE_RADIUS * 2 - 1, CIRCLE_RADIUS * 2 - 1);
          if (isVehicle() && lastCA > 0) {
            //Labeler.drawLabel(g, lastCA + "", x1, y1, f, Labeler.CENTER,
            // Labeler.CENTER, CIRCLE_COLOR, null, null);
            try {
              Image im = getCAImage(lastCA);
              if (zoom == 1.0) {
                g.drawImage(im, x2, y2, obs);
              }
              else {
                Image scaled = GameModule.getGameModule().getDataArchive().getScaledImage(im, zoom);
                g.drawImage(scaled, x2, y2, obs);
              }
            }
            catch (Exception ex) {

            }

          }
          lastCA = cap.ca;
        }
      }
      //g2d.setComposite(oldComposite);
      g.setClip(oldClip);

    }
  }

  protected Image getCAImage(int ca) throws java.io.IOException {

    if (caImages[ca - 1] == null) {
      caImages[ca - 1] = GameModule.getGameModule().getDataArchive().getCachedImage("ca_" + ca + ".gif");
    }
    return caImages[ca - 1];

  }

  public Rectangle boundingBox() {
    if (visible) {
      return piece.boundingBox().union(getMyBoundingBox());
    }
    else
      return piece.boundingBox();
  }

  public Rectangle getMyBoundingBox() {
    if (myBoundingBox == null) {
      
      Point p = piece.getPosition();
      myBoundingBox = new Rectangle(p.x, p.y, 60, 60);
      minX = myBoundingBox.x;
      minY = myBoundingBox.y;
      maxX = myBoundingBox.x + myBoundingBox.width;
      maxY = myBoundingBox.y + myBoundingBox.height;
    }
    return myBoundingBox;
  }

  public Shape getShape() {
    return piece.getShape();
  }

  public String getName() {
    return piece.getName();
  }

  public KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      commands = new KeyCommand[1];
      commands[0] = new KeyCommand(command, KeyStroke.getKeyStroke(trailKey, InputEvent.CTRL_MASK), Decorator
          .getOutermost(this));
    }
    return commands;
  }

  public Command myKeyEvent(KeyStroke stroke) {
    if (KeyStroke.getKeyStroke(trailKey, InputEvent.CTRL_MASK) == stroke) {
      ChangeTracker tracker = new ChangeTracker(this);
      visible = !visible;
      redraw();
      return tracker.getChangeCommand();
      //GameModule.getGameModule().sendAndLog(c);
      //return c;
    }
    return null;
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  private static class Ed implements PieceEditor {
    private KeySpecifier trailKeyInput;
    private JPanel controls;

    public Ed(VSQLFootprint p) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      Box b = Box.createHorizontalBox();
      trailKeyInput = new KeySpecifier('T');
      trailKeyInput.setKey(p.trailKey);
      b.add(new JLabel("Key command:  "));
      b.add(trailKeyInput);
      controls.add(b);

    }

    public String getState() {
      return "null";
    }

    public String getType() {
      return ID + trailKeyInput.getKey();
    }

    public Component getControls() {
      return controls;
    }
  }
}