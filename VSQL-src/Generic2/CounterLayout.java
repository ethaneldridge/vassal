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

 //TODO 
package Generic2;

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
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyCommand;
import VASSAL.tools.SequenceEncoder;

public class CounterLayout extends AbstractConfigurable implements Visualizable {

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
  protected boolean border = true;
  protected BufferedImage image;
  protected ImageDefn imageDefn = new ImageDefn();
  protected ArrayList items = new ArrayList();

  public CounterLayout() {
    super();
    name = "";
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Name", "Counter Width", "Counter Height", "Border?", ""};
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, Integer.class, Integer.class, Boolean.class, LayoutConfig.class };
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
      if (value instanceof String) {
        value = new Boolean((String) value);
      }
      border = ((Boolean) value).booleanValue();
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
    return new Class[] {ImageDefn.class };
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

  public boolean isBorder() {
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
    g.setColor(imageDefn.getBgColor().getColor());
    g.fillRect(0, 0, width, height);

    // Add Border
    if (isBorder()) {
      g.setColor(imageDefn.getBorderColor().getColor());
      ((Graphics2D) g).setStroke(new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
      g.drawRect(0, 0, width-1, height-1);      
    }
    
    // layer each item over the top
    Iterator i = items.iterator();
    while (i.hasNext()) {
      Item item = (Item) i.next();
      
//      if (colorScheme != null) {
//         se = colorScheme.getElement(item.getConfigureName());
//      }
//      
//      if (se == null) {
//        se = new SchemeElement(item.getConfigureName(), ColorSwatch.getBlack(), ColorSwatch.getClear());
//      }
      
      item.draw(g, defn);
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
  
  public KeyCommand[] getKeyCommands(GamePiece target) {
    int count = 0;
    Iterator i = items.iterator();
    while (i.hasNext()) {
      count += ((Item) i.next()).getKeyCommandCount();
    }
    
    KeyCommand commands[] = new KeyCommand[count];
    count = 0;
    
    i = items.iterator();
    while (i.hasNext()) {
      KeyCommand[] c = ((Item) i.next()).getKeyCommands(target);
      System.arraycopy(c, 0, commands, count, c.length);
      count += c.length;
    }
    
    return commands;
  }
}
