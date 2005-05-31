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

package Generic;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Iterator;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.tools.SequenceEncoder;

public class CounterLayout extends AbstractConfigurable implements Visualizable {

  protected static final String NAME = "name";
  protected static final String WIDTH = "width";
  protected static final String HEIGHT = "height";
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
  protected int height = 54;
  protected BufferedImage image;
  protected ColorScheme colorScheme;
  protected ArrayList items = new ArrayList();

  public CounterLayout() {
    super();
    name = "";
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Name", "Counter Width", "Counter Height", "" };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, Integer.class, Integer.class, LayoutConfig.class };
  }

  public static class LayoutConfig implements ConfigurerFactory {
    protected static LayoutConfigurer configurer;
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      configurer = new LayoutConfigurer(key, name, (CounterLayout) c);
      return configurer;
    }
    public static void refresh() {
      if (configurer != null) {
        configurer.repack();
      }
    }
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, WIDTH, HEIGHT, ITEMS };
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
    else if (ITEMS.equals(key)) {
      decodeItemList((String) value);
    }
    buildImage(null);
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
    return new Class[] {ColorScheme.class };
  }

  public void addTo(Buildable parent) {

  }

  public Configurer getConfigurer() {
    return super.getConfigurer();
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

    return getVisualizerImage(colorScheme);

  }
  
  public Image getVisualizerImage(ColorScheme scheme) {

    if (image == null) {
      buildImage(scheme);
    }

    return image;

  }

  public void refresh() {
    buildImage(colorScheme);
    LayoutConfig.refresh();
  }

  protected void buildImage(ColorScheme c) {

    colorScheme = c;

    // Create our base image
    image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
    Graphics g = image.getGraphics();

    // Fill in the sample Background color
    Color bgColor = colorScheme == null ? Color.WHITE : colorScheme.getBgColor();
    g.setColor(bgColor);
    g.fillRect(0, 0, width, height);

    // layer each item over the top
    Iterator i = items.iterator();
    while (i.hasNext()) {
      Item item = (Item) i.next();
      SchemeElement se = null;
      
      if (colorScheme != null) {
         se = colorScheme.getElement(item.getConfigureName());
      }
      
      if (se == null) {
        se = new SchemeElement(item.getConfigureName(), ColorSwatch.getBlack(), ColorSwatch.getClear());
      }
      
      item.draw(g, se);
    }

  }

  protected void decodeItemList(String string) {
    items.clear();
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(string, ',');
    while (sd.hasMoreTokens()) {
      Item item = Item.decode(this, sd.nextToken());
      addItem(item);
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

  public void rebuildVisualizerImage(ColorScheme c) {
    buildImage(c);
  }
  
  public void rebuildVisualizerImage() {
    buildImage(colorScheme);
  }
}
