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

package AutoImage;

import java.awt.Image;
import java.util.ArrayList;
import java.util.Iterator;

import javax.swing.KeyStroke;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyCommand;
import VASSAL.tools.SequenceEncoder;

/**
 *  
 */
public class ImageDefn extends AbstractConfigurable implements Visualizable {

  protected static final String NAME = "name";
  protected static final String PROPS = "props";

  public static final String PART_SIZE = "Size";
  public static final String PART_SYMBOL1 = "Symbol1";
  public static final String PART_SYMBOL2 = "Symbol2";

  public static final String BG_COLOR = "bgColor";
  public static final String BORDER_COLOR = "borderColor";
  
  protected ArrayList instances = new ArrayList();
  protected InstanceConfigurer defnConfig = null;
  protected CounterLayout layout;
  protected ColorSwatch bgColor = ColorSwatch.getWhite();
  protected ColorSwatch borderColor = ColorSwatch.getBlack();

  public ImageDefn() {
    super();
    setConfigureName("");
  }

  public ImageDefn(String s) {
    instances = InstanceConfigurer.StringToProperties(s, this);
  }

  public ImageDefn(CounterLayout l) {
    this();
    layout = l;
    rebuildInstances();
  }
  
  public ArrayList getInstances() {
    return instances;
  }
  
  public String[] getAttributeDescriptions() {
    return new String[] { "Name", "Background Color", "Border Color", "" };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, BgColorSwatchConfig.class, BorderColorSwatchConfig.class, DefnConfig.class };
  }

  public static class BgColorSwatchConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((ImageDefn) c).getBgColor());
    }
  }
  
  public static class BorderColorSwatchConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((ImageDefn) c).getBorderColor());
    }
  }
  
  public static class DefnConfig implements ConfigurerFactory {
    static ImageDefn id;
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      id = (ImageDefn) c;
      id.defnConfig = new InstanceConfigurer(key, name, id);
      return id.defnConfig;
    }
    public static void refresh() {
      if (id.defnConfig != null) {
        id.defnConfig.repack();
      }
    }
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, BG_COLOR, BORDER_COLOR, PROPS };
  }


  public ColorSwatch getBgColor() {
    return bgColor;
  }

  public ColorSwatch getBorderColor() {
    return borderColor;
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
      if (defnConfig != null) {
//        rebuildElements();
        defnConfig.visualizer.rebuild();
      }
    }
    else if (BORDER_COLOR.equals(key)) {
      if (value instanceof String) {
        value = new ColorSwatch((String) value);
      }
      borderColor = (ColorSwatch) value;
      if (defnConfig != null) {
//      rebuildElements();
      defnConfig.visualizer.rebuild();
    }
    }
    else if (PROPS.equals(key)) {
      if (value instanceof String) {
        value = InstanceConfigurer.StringToProperties((String) value, this);
      }
      instances = (ArrayList) value;
      if (defnConfig != null) {
        rebuildInstances();
        defnConfig.visualizer.rebuild();
        defnConfig.repack();
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
    else if (BORDER_COLOR.equals(key)) {
      return borderColor.encode();
    }
    else if (PROPS.equals(key)) {
      return InstanceConfigurer.PropertiesToString(instances);
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
    layout = (CounterLayout) parent;
    rebuildInstances();
  }

  public void refreshConfig() {
//    if (defnConfig != null) {
//      defnConfig.refresh();
//    }
    rebuildVisualizerImage();
  }
  
//  public ColorScheme getColorScheme() {
//    return scheme;
//  }

  public CounterLayout getLayout() {
    return layout;
  }

  public int getVisualizerHeight() {
    return getLayout().getVisualizerHeight();
  }

  public int getVisualizerWidth() {
    return getLayout().getVisualizerWidth();
  }

  public Image getVisualizerImage() {
    rebuildVisualizerImage();
    return getLayout().getVisualizerImage(this);
  }

  public void rebuildVisualizerImage() {
    getLayout().rebuildVisualizerImage(this);
  }

  public ItemInstance getInstance(String name) {
    Iterator i = instances.iterator();
    while (i.hasNext()) {
      ItemInstance instance = (ItemInstance) i.next();
      if (name.equals(instance.getName())) {
        return instance;
      }
    }
    return null;
  }
  
  public TextItemInstance getTextInstance(String name) {
    Iterator i = instances.iterator();
    while (i.hasNext()) {
      ItemInstance instance = (ItemInstance) i.next();
      if (instance instanceof TextItemInstance) {
        if (name.equals(instance.getName())) {
          return (TextItemInstance) instance;
        }
      }
    }
    return null;
  }

  public SymbolItemInstance getSymbolInstance(String name) {
    Iterator i = instances.iterator();
    while (i.hasNext()) {
      ItemInstance instance = (ItemInstance) i.next();
      if (instance instanceof SymbolItemInstance) {
        if (name.equals(instance.getName())) {
          return (SymbolItemInstance) instance;
        }
      }
    }
    return null;
  }
  
  /*
   * Reconcile our current elements with the elements in the owning scheme.
   */
  protected void rebuildInstances() {

    ArrayList newInstances = new ArrayList();

    Iterator e = instances.iterator();
    while (e.hasNext()) {
      ItemInstance prop = (ItemInstance) e.next();
      Item item = layout.getItem(prop.getName());
      if (item != null && item.getType().equals(prop.getType())) {
        prop.setLocation(item.getLocation());
        newInstances.add(prop);
      }
    }
    
    Iterator i = layout.getItems().iterator();
      while (i.hasNext()) {
        Item item = (Item) i.next();
        String name = item.getConfigureName();
        String type = item.getType();
        String location = item.getLocation();

        if (type.equals(SymbolItem.TYPE) || type.equals(TextItem.TYPE)) {
          boolean found = false;
          e = instances.iterator();
          while (e.hasNext() && !found) {
            ItemInstance prop = (ItemInstance) e.next();
            found = name.equals(prop.getName());
          }
  
          if (!found) {
            ItemInstance instance = ItemInstance.newDefaultInstance(name, type, location);
            instance.addTo(this);
            newInstances.add(instance);
          }
        }
      }
  
      instances = newInstances;
      if (defnConfig != null) {
        defnConfig.setValue(instances);
      }
  }
  
  public KeyCommand[] getKeyCommands(GamePiece target) {
    int count = 0;
    Iterator i = instances.iterator();
    while (i.hasNext()) {
      count += ((ItemInstance) i.next()).getKeyCommandCount();
    }
    
    KeyCommand commands[] = new KeyCommand[count];
    count = 0;
    
    i = instances.iterator();
    while (i.hasNext()) {
      KeyCommand[] c = ((ItemInstance) i.next()).getKeyCommands(target);
      System.arraycopy(c, 0, commands, count, c.length);
      count += c.length;
    }
    
    return commands;
  }

  /**
   * @param newState
   */
  public void setState(String newState) {
    String[] s = StringArrayConfigurer.stringToArray(newState);
    for (int i = 0; i < s.length; i++) {
      SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s[i], ',');
      String name = sd.nextToken();
      ItemInstance instance = getInstance(name);
      if (instance != null) {
        instance.setState(newState);
      }
    }
  }

  /**
   * @return
   */
  public String getState() {
    String state = "";
    SequenceEncoder se = new SequenceEncoder(',');
    Iterator i = instances.iterator();
    while (i.hasNext()) {
      ItemInstance instance = (ItemInstance) i.next();
      String s = instance.getState();
      if (s != null &&  s.length() > 0 )
        se.append(s);
    }
    return se.getValue();
  }

  /**
   * Process Key events from a Generic trait. The Generic piece
   * is handling command generation via a ChangeTracker
   */
  public void keyEvent(KeyStroke stroke) {
    Iterator i = instances.iterator();
    while (i.hasNext()) {
      ItemInstance instance = (ItemInstance) i.next();
      instance.keyEvent(stroke);
    }
    return;
  }
}
