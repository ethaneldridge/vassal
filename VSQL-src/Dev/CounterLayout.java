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
import java.awt.Window;
import java.util.ArrayList;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
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

  protected int width = 54;
  private int height = 54;
  protected ColorSwatch bgColor = new ColorSwatch();

  protected ArrayList items = new ArrayList();

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
      return getWidth() + "";
    }
    else if (HEIGHT.equals(key)) {
      return getHeight() + "";
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
    return new Class[0];
  }

  public void addTo(Buildable parent) {

  }

  protected void setWidth(int width) {
    this.width = width;
  }

  protected int getWidth() {
    return width;
  }

  protected void setHeight(int height) {
    this.height = height;
  }

  protected int getHeight() {
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
    return new GenericConfigurer(this);
  }

  protected class GenericConfigurer extends Configurer {

    protected JPanel panel;
    protected JPanel itemListPanel;
    protected JPanel itemDispPanel;
    protected Component currentDispControls;
    protected JPanel visualiserPanel;
 
    
    CounterLayout defn;
    StringConfigurer defName;
    NewIntConfigurer height, width;
    ColorSwatchConfigurer bgColor;

    protected GenericConfigurer() {
      super(null, null);
    }

    protected GenericConfigurer(CounterLayout def) {
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
        height = new NewIntConfigurer(null, "  Height:  ", new Integer(defn.width));
        box.add(width.getControls());
        box.add(height.getControls());
        width.setColumns(3);
        height.setColumns(3);
        panel.add(box);

        bgColor = new ColorSwatchConfigurer(null, "Background Color:  ", defn.bgColor);
        panel.add(bgColor.getControls());

        itemListPanel = new JPanel();
        panel.add(itemListPanel);

        itemDispPanel = new JPanel();
        itemDispPanel.setBorder(BorderFactory.createLineBorder(Color.black));
        
        itemDispPanel.add(((Item) defn.items.get(0)).getConfigurer().getControls());
        panel.add(itemDispPanel);
        
        visualiserPanel = new JPanel();
        panel.add(visualiserPanel);
        
        
        Window w = SwingUtilities.getWindowAncestor(box);
        if (w != null) {
          w.pack();
        }
      }

      return panel;
    }

    /*
     * (non-Javadoc)
     * 
     * @see VASSAL.configure.Configurer#getValueString()
     */
    public String getValueString() {
      // TODO Auto-generated method stub
      return null;
    }
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