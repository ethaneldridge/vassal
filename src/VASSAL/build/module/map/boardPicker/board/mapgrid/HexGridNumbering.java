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
/*
 * Created by IntelliJ IDEA.
 * User: rkinney
 * Date: Jul 25, 2002
 * Time: 11:46:35 PM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.build.module.map.boardPicker.board.mapgrid;

import VASSAL.build.Buildable;
import VASSAL.build.module.map.boardPicker.board.HexGrid;
import VASSAL.counters.Labeler;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.AffineTransform;
import java.awt.event.*;

public class HexGridNumbering extends RegularGridNumbering {
  private HexGrid grid;
  private boolean stagger = true;

  public void addTo(Buildable parent) {
    grid = (HexGrid) parent;
    grid.setGridNumbering(this);
  }

  public static final String STAGGER = "stagger";

  public String[] getAttributeDescriptions() {
    String[] s = super.getAttributeDescriptions();
    String[] val = new String[s.length + 1];
    System.arraycopy(s, 0, val, 0, s.length);
    val[s.length] = "Odd-numbered rows numbered higher";
    return val;
  }

  public String[] getAttributeNames() {
    String[] s = super.getAttributeNames();
    String[] val = new String[s.length + 1];
    System.arraycopy(s, 0, val, 0, s.length);
    val[s.length] = STAGGER;
    return val;
  }

  public Class[] getAttributeTypes() {
    Class[] s = super.getAttributeTypes();
    Class[] val = new Class[s.length + 1];
    System.arraycopy(s, 0, val, 0, s.length);
    val[s.length] = Boolean.class;
    return val;
  }

  public void setAttribute(String key, Object value) {
    if (STAGGER.equals(key)) {
      if (value instanceof String) {
        value = new Boolean((String) value);
      }
      stagger = ((Boolean) value).booleanValue();
    }
    else {
      super.setAttribute(key, value);
    }
  }

  public String getAttributeValueString(String key) {
    if (STAGGER.equals(key)) {
      return "" + stagger;
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  public void draw(Graphics g, Rectangle bounds, Rectangle visibleRect, double scale, boolean reversed) {
    int size = (int) (scale * fontSize + 0.5);
    if (size < 5) {
      return;
    }

    Graphics2D g2d = (Graphics2D) g;
    AffineTransform oldT = g2d.getTransform();
    if (reversed) {
      AffineTransform t = AffineTransform.getRotateInstance(Math.PI,bounds.x+.5*bounds.width,bounds.y+.5*bounds.height);
      g2d.transform(t);
      visibleRect = t.createTransformedShape(visibleRect).getBounds();
    }

    if (!bounds.intersects(visibleRect)) {
       return;
    }

    Rectangle region = bounds.intersection(visibleRect);

    Shape oldClip = g.getClip();
    g.setClip(region.x, region.y, region.width, region.height);

    double deltaX = scale * grid.getHexWidth();
    double deltaY = scale * grid.getHexSize();

    if (grid.isSideways()) {
      bounds = new Rectangle(bounds.y, bounds.x, bounds.height, bounds.width);
      region = new Rectangle(region.y, region.x, region.height, region.width);
    }

    int minCol = 2 * (int) Math.floor((region.x - bounds.x - scale * grid.getOrigin().x) / (2 * deltaX));
    double xmin = bounds.x + scale * grid.getOrigin().x + deltaX * minCol;
    double xmax = region.x + region.width + deltaX;
    int minRow = (int) Math.floor((region.y - bounds.y - scale * grid.getOrigin().y) / deltaY);
    double ymin = bounds.y + scale * grid.getOrigin().y + deltaY * minRow;
    double ymax = region.y + region.height + deltaY;

    Font f = new Font("Dialog", Font.PLAIN, size);
    Point p = new Point();
    int alignment = Labeler.TOP;
    int offset = -(int) Math.round(deltaY / 2);
    if (grid.isSideways()) {
      alignment = Labeler.CENTER;
      offset = 0;
    }

    Point gridp = new Point();

    for (double x = xmin; x < xmax; x += 2 * deltaX) {
      for (double y = ymin; y < ymax; y += deltaY) {

        p.setLocation((int) Math.round(x), (int) Math.round(y) + offset);
        gridp = new Point(p.x,p.y-offset);
        grid.rotateIfSideways(p);

        // Convert from map co-ordinates to board co-ordinates
        gridp.translate(-bounds.x, -bounds.y);
        grid.rotateIfSideways(gridp);
        gridp.x = (int) Math.round(gridp.x / scale);
        gridp.y = (int) Math.round(gridp.y / scale);

        Labeler.drawLabel(g, getName(getRow(gridp), getColumn(gridp)),
                          p.x,
                          p.y,
                          f,
                          Labeler.CENTER,
                          alignment, color, null, null);

        p.setLocation((int) Math.round(x + deltaX), (int) Math.round(y + deltaY / 2) + offset);
        gridp = new Point(p);
        grid.rotateIfSideways(p);

        // Convert from map co-ordinates to board co-ordinates
        gridp.translate(-bounds.x, -bounds.y);
        grid.rotateIfSideways(gridp);
        gridp.x = (int) Math.round(gridp.x / scale);
        gridp.y = (int) Math.round(gridp.y / scale);

        Labeler.drawLabel(g, getName(getRow(gridp), getColumn(gridp)),
                          p.x,
                          p.y,
                          f,
                          Labeler.CENTER,
                          alignment, color, null, null);
      }
    }
    g.setClip(oldClip);
    g2d.setTransform(oldT);
  }


  public int getColumn(Point p) {

    int x = getRawColumn(p);

    if (vDescending && grid.isSideways()) {
      x = (getMaxRows() - x);
    }
    if (hDescending && !grid.isSideways()) {
      x = (getMaxColumns() - x);
    }

    return x;
  }

  public int getRawColumn(Point p) {
    p = new Point(p);
    grid.rotateIfSideways(p);
    int x = p.x - grid.getOrigin().x;

    x = (int) Math.floor(x / grid.getHexWidth() + 0.5);
    return x;
  }

  protected JComponent getGridVisualizer() {
    if (visualizer == null) {
      visualizer = new JPanel() {
        public void paint(Graphics g) {
          g.clearRect(0, 0, getWidth(), getHeight());
          Rectangle bounds = new Rectangle(0, 0, getWidth(), getHeight());
          grid.draw(g, bounds, bounds, 1.0, false);
          draw(g, bounds, bounds, 1.0, false);
        }

        public Dimension getPreferredSize() {
          return new Dimension(4 * (int) grid.getHexSize(), 4 * (int) grid.getHexWidth());
        }
      };
    }
    return visualizer;
  }

  public int getRow(Point p) {

    int ny = getRawRow(p);

    if (vDescending && !grid.isSideways()) {
      ny = (getMaxRows() - ny);
    }
    if (hDescending && grid.isSideways()) {
      ny = (getMaxColumns() - ny);
    }

    if (stagger) {
      if (grid.isSideways()) {
        if (getRawColumn(p) % 2 != 0) {
          if (hDescending) {
            ny--;
          }
          else {
            ny++;
          }
        }
      }
      else {
        if (getRawColumn(p) % 2 != 0) {
          if (vDescending) {
            ny--;
          }
          else {
            ny++;
          }
        }
      }
    }
    return ny;
  }

  protected int getRawRow(Point p) {
    p = new Point(p);
    grid.rotateIfSideways(p);
    Point origin = grid.getOrigin();
    double dx = grid.getHexWidth();
    double dy = grid.getHexSize();
    int nx = (int) Math.round((p.x - origin.x) / dx);
    int ny;
    if (nx % 2 == 0) {
      ny = (int) Math.round((p.y - origin.y) / dy);
    }
    else {
      ny = (int) Math.round((p.y - origin.y - dy / 2) / dy);
    }
    return ny;
  }

  public void removeFrom(Buildable parent) {
    grid.setGridNumbering(null);
  }

  protected int getMaxRows() {
    return (int) Math.floor(grid.getBoard().bounds().height / grid.getHexWidth() + 0.5);
  }

  protected int getMaxColumns() {
    return (int) Math.floor(grid.getBoard().bounds().width / grid.getHexSize() + 0.5);
  }

  public static void main(String[] args) {
    class TestPanel extends JPanel {
      private boolean reversed;
      private double scale = 1.0;
      private HexGrid grid;
      private HexGridNumbering numbering;

      private TestPanel() {
        setLayout(new BorderLayout());
        Box b = Box.createHorizontalBox();
        final JTextField tf = new JTextField("1.0");
        b.add(tf);
        tf.addKeyListener(new KeyAdapter() {
          public void keyReleased(KeyEvent e) {
            try {
              scale = Double.parseDouble(tf.getText());
              repaint();
            }
            catch (NumberFormatException e1) {
              e1.printStackTrace();
            }
          }
        });
        final JCheckBox reverseBox = new JCheckBox("Reversed");
        reverseBox.addItemListener(new ItemListener() {
          public void itemStateChanged(ItemEvent e) {
            reversed = reverseBox.isSelected();
            repaint();
          }
        });
        b.add(reverseBox);
        final JCheckBox sidewaysBox = new JCheckBox("Sideways");
        sidewaysBox.addItemListener(new ItemListener() {
          public void itemStateChanged(ItemEvent e) {
            grid.setAttribute(HexGrid.SIDEWAYS, sidewaysBox.isSelected() ? Boolean.TRUE : Boolean.FALSE);
            repaint();
          }
        });
        b.add(sidewaysBox);
        add(BorderLayout.NORTH, b);
        grid = new HexGrid();
        grid.setAttribute(HexGrid.COLOR, Color.black);
        numbering = new HexGridNumbering();
        numbering.setAttribute(HexGridNumbering.COLOR, Color.black);
        numbering.addTo(grid);
        JPanel p = new JPanel() {
          public void paint(Graphics g) {
            Rectangle r = new Rectangle(0,0,getWidth(),getHeight());
            g.clearRect(r.x,r.y,r.width,r.height);
            grid.draw(g, r, getVisibleRect(), scale, reversed);
            numbering.draw(g, getBounds(), getVisibleRect(), scale, reversed);
          }
        };
        Dimension d = new Dimension(4000,4000);
        p.setPreferredSize(d);
        add(BorderLayout.CENTER, new JScrollPane(p));
      }
    }
    JFrame f = new JFrame();
    f.getContentPane().add(new TestPanel());
    Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
    screenSize.height -= 100;
    screenSize.width -= 100;
    f.setSize(screenSize);
    f.setVisible(true);
    f.addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
        System.exit(0);
      }
    });
  }

}
