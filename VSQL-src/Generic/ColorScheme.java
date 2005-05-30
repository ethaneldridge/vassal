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

  protected ColorSwatchConfigurer csConfig = new ColorSwatchConfigurer("", "", "WHITE");
  protected SchemeConfigurer schemeConfig = new SchemeConfigurer("", "", this);
  protected ArrayList elements = new ArrayList();
  protected CounterLayout layout;

  public ColorScheme() {
    super();
    setConfigureName("");
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Name", "Background Color", "" };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class,  BgColorSwatchConfig.class, SchemeConfig.class };
  }

  public static class BgColorSwatchConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      ColorScheme cs = (ColorScheme) c;
      cs.csConfig = new ColorSwatchConfigurer(key, name); 
      return cs.csConfig;
    }
  }

  public static class SchemeConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      ColorScheme cs = (ColorScheme) c;
      cs.schemeConfig = new SchemeConfigurer(key, name, cs);
      return cs.schemeConfig;
    }
//
//    public static void refresh() {
//      if (cs.schemeConfig != null) {
//        cs.schemeConfig.refresh();
//      }
//    }
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, BG_COLOR, SCHEME};
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (BG_COLOR.equals(key)) {
      if (value instanceof String) {
        value = new ColorSwatch((String) value);
      }
      csConfig.setValue((ColorSwatch) value);
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
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(string, ',');
    while (sd.hasMoreTokens()) {
     SchemeElement element = new SchemeElement(sd.nextToken()); 
     elements.add(element);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (BG_COLOR.equals(key)) {
      return csConfig.getValueColorSwatch().encode();
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
    return new Class[0];
  }

  public void addTo(Buildable parent) {
    layout = (CounterLayout) parent;
  }

  public Configurer getConfigurer() {
    return super.getConfigurer();
  }
  
  public int getVisualizerHeight() {
    return layout == null ? 1 : layout.getVisualizerHeight();
  }

  public int getVisualizerWidth() {
    return layout == null ? 1 : layout.getVisualizerWidth();
  }

  public Image getVisualizerImage() {
    return layout == null ? null : layout.getVisualizerImage();
  }

  protected class SchemeElement {

    private String name;
    private Color fgColor;
    private Color bgColor;

    public SchemeElement(String s) {
      decode(s);
    }
    
    public String encode() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(name);
      se.append(fgColor);
      se.append(bgColor);
      return se.getValue();
    }

    public void decode(String s) {
      SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ';');
      name = sd.nextToken("");
      fgColor = sd.nextColor(Color.BLACK);
      bgColor = sd.nextColor(null);
    }
    
    protected void setName(String name) {
      this.name = name;
    }

    protected String getName() {
      return name;
    }

    protected void setFgColor(Color fgColor) {
      this.fgColor = fgColor;
    }

    protected Color getFgColor() {
      return fgColor;
    }

    protected void setBgColor(Color bgColor) {
      this.bgColor = bgColor;
    }

    protected Color getBgColor() {
      return bgColor;
    }

  }

  public int getItemCount() {
    return elements.size();
  }
}
