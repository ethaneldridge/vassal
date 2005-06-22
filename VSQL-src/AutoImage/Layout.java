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

package AutoImage;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.StringEnum;
import VASSAL.tools.SequenceEncoder;

public class Layout extends AbstractConfigurable implements Visualizable {

  protected static final String NAME = "name";
  protected static final String WIDTH = "width";
  protected static final String HEIGHT = "height";
  protected static final String BORDER = "border";
  protected static final String ITEMS = "layout";

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

  protected static final String BORDER_PLAIN = "Plain";
  protected static final String BORDER_FANCY = "Fancy";
  protected static final String BORDER_3D = "3D";
  protected static final String BORDER_NONE = "None";

  public static final String[] LOCATIONS = new String[] { CENTER, N, S, E, W, NE, NW, SE, SW };
  public static final int[] X_POS = new int[] { POS_C, POS_C, POS_C, POS_R, POS_L, POS_R, POS_L, POS_R, POS_L };
  public static final int[] Y_POS = new int[] { POS_C, POS_T, POS_B, POS_C, POS_C, POS_T, POS_T, POS_B, POS_B };

  public static Point getPosition(String s, Layout layout) {
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
  protected int height = 54;
  protected String border = BORDER_3D;
  protected BufferedImage image;
  protected ImageDefn imageDefn = new ImageDefn();
  protected ArrayList items = new ArrayList();

  public Layout() {
    super();
    name = "";
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Name", "Counter Width", "Counter Height", "Border Style", "" };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, Integer.class, Integer.class, BorderConfig.class, LayoutConfig.class };
  }

  public static class LayoutConfig implements ConfigurerFactory {
    protected static LayoutConfigurer configurer;

    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      configurer = new LayoutConfigurer(key, name, (Layout) c);
      return configurer;
    }

    public static void refresh() {
      if (configurer != null) {
        configurer.repack();
      }
    }
  }

  public static class BorderConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { BORDER_PLAIN, BORDER_FANCY, BORDER_3D, BORDER_NONE };
    }
  }

  public boolean isColoredBorder() {
    return border.equals(BORDER_PLAIN) || border.equals(BORDER_FANCY);
  }
  
  public String[] getAttributeNames() {
    return new String[] { NAME, WIDTH, HEIGHT, BORDER, ITEMS };
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
    else if (BORDER.equals(key)) {
      border = (String) value;
    }
    else if (ITEMS.equals(key)) {
      decodeItemList((String) value);
    }
    buildImage();
    LayoutConfig.refresh();
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
    else if (BORDER.equals(key)) {
      return border + "";
    }
    else if (ITEMS.equals(key)) {
      return encodeItemList();
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
    return new Class[] { ImageDefn.class };
  }

  public void addTo(Buildable parent) {

  }

  public Configurer getConfigurer() {
    return super.getConfigurer();
  }

  public static String getConfigureTypeName() {
    return "Layout";
  }

  public void setWidth(int width) {
    this.width = width;
  }

  public int getLayoutWidth() {
    return width;
  }

  public void setHeight(int height) {
    this.height = height;
  }

  public int getLayoutHeight() {
    return height;
  }

  public String getBorder() {
    return border;
  }

  public ArrayList getItems() {
    return items;
  }

  protected int getItemCount() {
    return items.size();
  }

  protected Item getItem(int n) {
    return (Item) items.get(n);
  }

  protected Item getItem(String name) {
    Iterator i = items.iterator();
    while (i.hasNext()) {
      Item item = (Item) i.next();
      if (item.getConfigureName().equals(name)) {
        return item;
      }
    }
    return null;
  }

  protected void removeItem(int n) {
    items.remove(n);
  }

  protected void addItem(Item i) {
    items.add(i);
  }

  public void add(Buildable b) {
    super.add(b);
  }

  public void remove(Buildable b) {
    super.remove(b);
  }

  public Image getVisualizerImage() {
    if (imageDefn == null) {
      imageDefn = new ImageDefn();
    }
    return getVisualizerImage(imageDefn);
  }

  public Image getVisualizerImage(ImageDefn defn) {
    if (image == null) {
      buildImage(defn);
    }
    return image;
  }

  public void refresh() {
    buildImage();
    LayoutConfig.refresh();
  }

  protected void buildImage() {
    if (imageDefn == null) {
      imageDefn = new ImageDefn(this);
    }
    buildImage(imageDefn);
  }

  protected void buildImage(ImageDefn defn) {

    imageDefn = defn;

    // Create our base image
    image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
    Graphics g = image.getGraphics();

    // Fill in the sample Background color
    Color bgColor = imageDefn.getBgColor().getColor();
    g.setColor(bgColor);

    if (getBorder().equals(BORDER_3D)) {
      g.fill3DRect(0, 0, width, height, true);
    }
    else {

      g.fillRect(0, 0, width, height);

      // Add Border
      if (getBorder().equals(BORDER_PLAIN) || getBorder().equals(BORDER_FANCY)) {
        g.setColor(imageDefn.getBorderColor().getColor());
        ((Graphics2D) g).setStroke(new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
        g.drawRect(0, 0, width - 1, height - 1);
        if (getBorder().equals(BORDER_FANCY)) {
          Color lt = new Color(bgColor.getRed() / 2, bgColor.getGreen() / 2, bgColor.getBlue() / 2);
          Color dk = new Color(bgColor.getRed() + (255 - bgColor.getRed()) / 2, bgColor.getGreen()
              + (255 - bgColor.getGreen()) / 2, bgColor.getBlue() + (255 - bgColor.getBlue()) / 2);
          g.setColor(dk);
          g.drawLine(1, 1, width - 3, 1);
          g.drawLine(1, 2, 1, height - 3);
          g.setColor(lt);
          g.drawLine(width - 2, 2, width - 2, height - 2);
          g.drawLine(2, height - 2, width - 3, height - 2);
        }
      }
    }

    // layer each item over the top
    Iterator i = items.iterator();
    while (i.hasNext()) {
      Item item = (Item) i.next();
      if (item != null) {
        item.draw(g, defn);
      }
    }

  }

  protected void decodeItemList(String string) {
    items.clear();
    if (string.length() > 0) {
      SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(string, ',');
      while (sd.hasMoreTokens()) {
        Item item = Item.decode(this, sd.nextToken());
        addItem(item);
      }
    }

  }

  protected String encodeItemList() {
    String[] list = new String[getItemCount()];
    Iterator it = items.iterator();
    int i = 0;
    while (it.hasNext()) {
      Item item = (Item) it.next();
      list[i++] = item.encode();
    }
    SequenceEncoder se = new SequenceEncoder('#');
    se.append(list);
    return se.getValue();
  }

  public int getVisualizerHeight() {
    return getLayoutHeight();
  }

  public int getVisualizerWidth() {
    return getLayoutWidth();
  }

  public void rebuildVisualizerImage(ImageDefn defn) {
    buildImage(defn);
  }

  public void rebuildVisualizerImage() {
    if (imageDefn == null) {
      imageDefn = new ImageDefn();
    }
    buildImage(imageDefn);
  }

  public void setImageDefn(ImageDefn d) {
    imageDefn = d;
  }

  //  public void highlightItem(int itemNo) {
  //    if (itemNo >= 0 && itemNo < items.size()) {
  //      ((Item) items.get(itemNo)).s
  //    }
  //  }

  public ImageDefn getGenericDefn(String defnName) {
    ImageDefn defn = null;
    Enumeration e = getBuildComponents();
    while (e.hasMoreElements() && defn == null) {
      Object o = e.nextElement();
      if (o instanceof ImageDefn) {
        if (((ImageDefn) o).getConfigureName().equals(defnName)) {
          return (ImageDefn) o;
        }
      }
    }
    return defn;
  }
}
