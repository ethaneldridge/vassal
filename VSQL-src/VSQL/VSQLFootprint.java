package VSQL;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Composite;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
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

import VASL.counters.MarkMoved;
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
  private char trailKey;
  private KeyCommand[] commands;
  private String command = "Movement Trail";
  protected boolean visible = false;

  protected static final int CIRCLE_RADIUS = 10;
  protected static final Color CIRCLE_COLOR = Color.BLACK;
  protected static final Color FILL_COLOR = Color.WHITE;
  protected static final Color LINE_COLOR = Color.BLACK;
  protected static final float LINE_WIDTH = 1.0f;
  protected static final int BASE_FONT_SIZE = 14;

  protected Vector pointList = new Vector();

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
        pointList.addElement(new Point(x, y));
      }
    }
  }

  public String myGetState() {
    String s = pointList.size() + "";
    Enumeration e = getPointList();
    while (e.hasMoreElements()) {
      Point p = (Point) e.nextElement();
      s += ";" + p.x + "," + p.y;
    }

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
        pointList.addElement(where);
      }
      else {
        Point last = (Point) pointList.lastElement();
        if (!last.equals(where)) { // Don't add the same point twice
          pointList.addElement(where);
        }
      }
    }
    else {
      pointList.clear();
    }
    redraw();
  }

  public void redraw() {
    piece.getMap().repaint();
  }

  public void setProperty(Object key, Object val) {
    if (Properties.MOVED.equals(key)) {
      setMoved(Boolean.TRUE.equals(val));
      piece.setProperty(key, val); // Pass on to MarkMoved
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
      //Composite oldComposite = g2d.getComposite();
      //g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER,
      // 0.5F));
      g2d.setStroke(new BasicStroke(LINE_WIDTH));
      g2d.setColor(LINE_COLOR);
      Enumeration e = getPointList();
      Point lastP = null;
      Point here = getPosition();
      while (e.hasMoreElements()) {
        Point p = (Point) e.nextElement();

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
        Point p = (Point) e.nextElement();
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
        Labeler.drawLabel(g, step + "", x1, y1, f, Labeler.CENTER, Labeler.CENTER, CIRCLE_COLOR, null, null);
      }
      //g2d.setComposite(oldComposite);
    }
  }

  public Rectangle boundingBox() {
    return piece.boundingBox();
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