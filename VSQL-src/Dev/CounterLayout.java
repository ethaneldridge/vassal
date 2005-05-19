/*
 * $Id$
 * 
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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

package Dev;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Point;
import java.awt.Window;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Properties;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import org.w3c.dom.Element;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.StringConfigurer;

public class CounterLayout extends AbstractConfigurable {

  protected static final String NAME = "name";
  protected static final String WIDTH = "width";
  protected static final String HEIGHT = "height";
  protected static final String BGCOLOR = "bgColor";

  protected static final String N = "Top";
  protected static final String S = "Bottom";
  protected static final String E = "Right";
  protected static final String W = "Left";
  protected static final String NE = "Top Right";
  protected static final String NW = "Top Left";
  protected static final String SE = "Bottom Right";
  protected static final String SW = "Bottom Left";
  protected static final String CENTER = "Center";

  protected static final int POS_L = 15;
  protected static final int POS_R = 85;
  protected static final int POS_T = 15;
  protected static final int POS_B = 85;
  protected static final int POS_C = 50;

  public static final String[] LOCATIONS = new String[] { CENTER, N, S, E, W, NE, NW, SE, SW };
  public static final int[] X_POS = new int[] { POS_C, POS_C, POS_C, POS_R, POS_L, POS_R, POS_L, POS_R, POS_L };
  public static final int[] Y_POS = new int[] { POS_C, POS_T, POS_B, POS_C, POS_C, POS_T, POS_T, POS_B, POS_B };
  
  public static Point getPosition(String s, CounterLayout layout) {
    int x = X_POS[0];
    int y = Y_POS[0];

    
    for (int i = 0; i < LOCATIONS.length; i++) {
      if (s.equals(LOCATIONS[i])) {
        x = X_POS[i];
        y = Y_POS[i];
      }      
    }
    x = (int) x * layout.getLayoutWidth() / 100;
    y = (int) y * layout.getLayoutHeight() / 100;
    return new Point(x, y);
  }
  
  protected int width = 54;
  private int height = 54;
  protected ColorSwatch bgColor = new ColorSwatch();
  protected BufferedImage image;
  protected Properties props;
  protected ArrayList items = new ArrayList();
  protected JComponent visualizer;
  protected JPanel visPanel;

  public CounterLayout() {
    super();
    name = "";
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Name", "Counter Width", "Counter Height", "Background Color" };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, Integer.class, Integer.class, BgColorSwatchConfig.class };
  }

  public static class BgColorSwatchConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((CounterLayout) c).bgColor);
    }
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, WIDTH, HEIGHT, BGCOLOR };
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (WIDTH.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      setWidth(((Integer) value).intValue());
    }
    else if (HEIGHT.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      setHeight(((Integer) value).intValue());
    }
    else if (BGCOLOR.equals(key)) {
      if (value instanceof String) {
        value = GenericsContainer.getColorSwatch((String) value);
      }
      bgColor = (ColorSwatch) value;
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (WIDTH.equals(key)) {
      return getLayoutWidth() + "";
    }
    else if (HEIGHT.equals(key)) {
      return getLayoutHeight() + "";
    }
    else if (BGCOLOR.equals(key)) {
      return bgColor.getConfigureName();
    }
    else
      return null;
  }

  public void removeFrom(Buildable parent) {

  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[] { TextItem.class, SymbolItem.class };
  }

  public void addTo(Buildable parent) {

  }

  protected void setWidth(int width) {
    this.width = width;
  }

  protected int getLayoutWidth() {
    return width;
  }

  protected void setHeight(int height) {
    this.height = height;
  }

  protected int getLayoutHeight() {
    return height;
  }

  protected Color getBgColor() {
    return bgColor.getColor();
  }

  public void add(Buildable b) {
    super.add(b);

    if (b instanceof Item) {
      items.add(b);
    }

  }

  public void remove(Buildable b) {
    super.remove(b);

    if (b instanceof Item) {
      items.remove(b);
    }
  }

  public void build(Element e) {
    super.build(e);
  }

  public Configurer getConfigurer() {
    return new LayoutConfigurer(this);
  }

  public Image getImage(Properties p) {

    if (image == null) {
      buildImage(p);
    }

    return image;

  }

  public void refresh() {
    buildImage(props);
  }

  protected void buildImage(Properties p) {

    props = p;
    
    // Create our base image
    image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
    Graphics g = image.getGraphics();

    // Fill in the Background color
    g.setColor(bgColor.getColor());
    g.fillRect(0, 0, width, height);

    // layer each item over the top
    Iterator i = items.iterator();
    while (i.hasNext()) {
      Item item = (Item) i.next();
      item.draw(g, p);
    }

  }

  /**
   * 
   * Custom Configurer for Counter Layouts
   */
  protected class LayoutConfigurer extends Configurer {

    protected JPanel panel;
    protected JPanel itemListPanel;
    protected JPanel itemDispPanel;
    protected Component currentDispControls;
    protected JLabel visLabel;

    CounterLayout defn;
    StringConfigurer defName;
    NewIntConfigurer height, width;
    ColorSwatchConfigurer bgColor;

    protected LayoutConfigurer() {
      super(null, null);
    }

    protected LayoutConfigurer(CounterLayout def) {
      this();
      defn = def;
    }

    public Object getValue() {
      if (defn != null) {

        defn.name = defName.getValueString();
        defn.height = ((Integer) height.getValue()).intValue();
        defn.width = ((Integer) width.getValue()).intValue();
        defn.bgColor = bgColor.getValueColorSwatch();
      }
      return defn;
    }

    public void setValue(String s) {
    }

    public Component getControls() {
      if (panel == null) {

        panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

        defName = new StringConfigurer(null, "Layout Name:  ", defn.name);
        panel.add(defName.getControls());

        Box box = Box.createHorizontalBox();
        width = new NewIntConfigurer(null, "Width:  ", new Integer(defn.width));
        width.addPropertyChangeListener(new PropertyChangeListener() {
          public void propertyChange(PropertyChangeEvent evt) {
            setAttribute(WIDTH, width.getValue());
            refresh();
            visualizer.repaint();
          }
        });

        height = new NewIntConfigurer(null, "  Height:  ", new Integer(defn.height));
        height.addPropertyChangeListener(new PropertyChangeListener() {
          public void propertyChange(PropertyChangeEvent evt) {
            setAttribute(HEIGHT, height.getValue());
            refresh();
            visualizer.repaint();
          }
        });

        box.add(width.getControls());
        box.add(height.getControls());
        width.setColumns(3);
        height.setColumns(3);
        panel.add(box);

        bgColor = new ColorSwatchConfigurer(null, "Background Color:  ", defn.bgColor);
        bgColor.addPropertyChangeListener(new PropertyChangeListener() {
          public void propertyChange(PropertyChangeEvent evt) {
            setAttribute(BGCOLOR, bgColor.getValue());
            refresh();
            visualizer.repaint();
          }
        });
        panel.add(bgColor.getControls());

        itemListPanel = new JPanel();
        panel.add(itemListPanel);

        itemDispPanel = new JPanel();
        itemDispPanel.setBorder(BorderFactory.createLineBorder(Color.black));

        //itemDispPanel.add(((Item)
        // defn.items.get(0)).getConfigurer().getControls());
        panel.add(itemDispPanel);

        visPanel = new JPanel();
        visPanel.setSize(new Dimension(getLayoutWidth(), getLayoutHeight()));
        visPanel.setPreferredSize(new Dimension(getLayoutWidth() + 20, getLayoutHeight() + 20));
        visPanel.add(getVisualizer());
        panel.add(visPanel);

        Window w = SwingUtilities.getWindowAncestor(box);
        if (w != null) {
          w.pack();
        }
      }

      return panel;
    }

    public String getValueString() {

      return null;
    }
  }

  protected JComponent getVisualizer() {
    if (visualizer == null) {
      visualizer = new JPanel() {
        public void paint(Graphics g) {
          visualizer.setSize(new Dimension(getLayoutWidth(), getLayoutHeight()));
          visualizer.setPreferredSize(new Dimension(getLayoutWidth(), getLayoutHeight()));
          g.clearRect(0, 0, getLayoutWidth(), getLayoutHeight());
          BufferedImage bi = (BufferedImage) getImage(getTestProps());
          g.drawImage(getImage(null), 0, 0, this);
        }
      };
    }
    return visualizer;
  }

  protected Properties getTestProps() {
    Properties p = new Properties();
    Iterator i = items.iterator();
    while (i.hasNext()) {
      Item item = (Item) i.next();
      String name = item.getConfigureName();
      p.setProperty(name, name);
    }
    return p;
  }
  
  protected class NewIntConfigurer extends IntConfigurer {

    NewIntConfigurer(String name, String key, Integer i) {
      super(name, key, i);
    }

    public void setColumns(int cols) {
      nameField.setColumns(cols);
    }

  }

}