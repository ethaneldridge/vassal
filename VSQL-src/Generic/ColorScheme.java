/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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

package Generic;

import java.awt.Color;
import java.awt.Image;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.tools.SequenceEncoder;

/**
 *  
 */
public class ColorScheme extends AbstractConfigurable implements Visualizable {

  protected static final String NAME = "name";
  protected static final String BG_COLOR = "bgColor";
  protected static final String SCHEME = "scheme";

  protected ColorSwatch bgColor = ColorSwatch.getWhite();
  protected ColorSchemeConfigurer schemeConfig = null;
  protected ArrayList elements = new ArrayList();
  protected CounterLayout layout;

  public ColorScheme() {
    super();
    setConfigureName("");
  }

  public ColorScheme(SchemeElement se) {
    this();
    elements.add(se);
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Name", "Background Color", "" };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, BgColorSwatchConfig.class, SchemeConfig.class };
  }

  public static class BgColorSwatchConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((ColorScheme) c).getBgColor());
    }
  }

  public static class SchemeConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      ColorScheme cs = (ColorScheme) c;
      cs.schemeConfig = new ColorSchemeConfigurer(key, name, cs);
      return cs.schemeConfig;
    }
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, BG_COLOR, SCHEME };
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (BG_COLOR.equals(key)) {
      if (value instanceof String) {
        value = new ColorSwatch((String) value);
      }
      bgColor = (ColorSwatch) value;
      if (schemeConfig != null) {
        rebuildElements();
        schemeConfig.visualizer.rebuild();
      }
    }
    else if (SCHEME.equals(key)) {
      decodeScheme((String) value);
    }
  }

  /**
   * @param string
   */
  private void decodeScheme(String string) {
    elements.clear();
    if (string.length() > 0) {
      SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(string, ',');
      while (sd.hasMoreTokens()) {
        SchemeElement element = new SchemeElement(sd.nextToken());
        elements.add(element);
      }
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (BG_COLOR.equals(key)) {
      return bgColor.encode();
    }
    else if (SCHEME.equals(key)) {
      return encodeScheme();
    }
    else
      return null;
  }

  private String encodeScheme() {

    String[] items = new String[elements.size()];
    Iterator it = elements.iterator();
    int i = 0;
    while (it.hasNext()) {
      items[i++] = ((SchemeElement) it.next()).encode();
    }

    return (new SequenceEncoder('#')).append(items).getValue();
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
    layout = (CounterLayout) parent;
    rebuildElements();
  }
  
  public Color getBgColor() {
    return bgColor.getColor();
  }

  public int getVisualizerHeight() {
    return layout.getVisualizerHeight();
  }

  public int getVisualizerWidth() {
    return layout.getVisualizerWidth();
  }

  public Image getVisualizerImage() {
    return layout.getVisualizerImage(this, null);
  }

  public void rebuildVisualizerImage() {
    layout.rebuildVisualizerImage(this, null);
  }

  public int getElementCount() {
    return elements.size();
  }

  public SchemeElement getElement(String name) {
    Iterator i = elements.iterator();
    while (i.hasNext()) {
      SchemeElement element = (SchemeElement) i.next();
      if (element.getName().equals(name)) {
        return element;
      }
    }
    return null;
  }

  public Iterator getElements() {
    return elements.iterator();
  }

  public SchemeElement getElement(int row) {
    return (SchemeElement) elements.get(row);
  }

  public void setElementFg(int row, ColorSwatch color) {
    getElement(row).setFgColor(color);
  }

  public void setElementBg(int row, ColorSwatch color) {
    getElement(row).setBgColor(color);
  }

  public CounterLayout getLayout() {
    return layout;
  }

  /*
   * Reconcile our current elements with the elements in the owning layout.
   */
  protected void rebuildElements() {

    ArrayList newElements = new ArrayList();

    Iterator i = elements.iterator();
    while (i.hasNext()) {
      SchemeElement se = (SchemeElement) i.next();
      Item item = layout.getItem(se.getName());
      if (item != null) {
        newElements.add(se);
      }
    }

    i = layout.getItems().iterator();
    while (i.hasNext()) {
      Item item = (Item) i.next();
      SchemeElement se = this.getElement(item.getConfigureName());
      if (se == null) {
        se = new SchemeElement(item.getConfigureName(), ColorSwatch.getBlack(), ColorSwatch.getClear(), item.getType(),
            item.getLocation());
        newElements.add(se);
      }
    }

    elements = newElements;
  }

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
