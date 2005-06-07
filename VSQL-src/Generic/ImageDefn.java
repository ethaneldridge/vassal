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

import java.awt.Image;
import java.util.Iterator;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;

/**
 *  
 */
public class ImageDefn extends AbstractConfigurable implements Visualizable {

  protected static final String NAME = "name";
  protected static final String PROPS = "props";

  public static final String PART_SIZE = "Size";
  public static final String PART_SYMBOL1 = "Symbol1";
  public static final String PART_SYMBOL2 = "Symbol2";

  protected ColorScheme scheme;
  protected InstanceList instances = new InstanceList();
  protected InstanceConfigurer defnConfig = null;

  public ImageDefn() {
    super();
    setConfigureName("");
  }

  public ImageDefn(String s) {
    instances = InstanceConfigurer.StringToProperties(s, this);
  }

  public InstanceList getInstances() {
    return instances;
  }
  
  public String[] getAttributeDescriptions() {
    return new String[] { "Name", "" };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, DefnConfig.class };
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
    return new String[] { NAME, PROPS };
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (PROPS.equals(key)) {
      if (value instanceof String) {
        value = InstanceConfigurer.StringToProperties((String) value, this);
      }
      instances = (InstanceList) value;
      if (defnConfig != null) {
       // rebuildInstances();
        //defnConfig.visualizer.rebuild();
        defnConfig.repack();
      }
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
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
    scheme = (ColorScheme) parent;
    rebuildInstances();
  }

  public void refreshConfig() {
//    if (defnConfig != null) {
//      defnConfig.refresh();
//    }
    rebuildVisualizerImage();
  }
  
  public ColorScheme getColorScheme() {
    return scheme;
  }

  public CounterLayout getLayout() {
    return scheme.getLayout();
  }

  public int getVisualizerHeight() {
    return getLayout().getVisualizerHeight();
  }

  public int getVisualizerWidth() {
    return getLayout().getVisualizerWidth();
  }

  public Image getVisualizerImage() {
    rebuildVisualizerImage();
    return getLayout().getVisualizerImage(scheme, this);
  }

  public void rebuildVisualizerImage() {
    getLayout().rebuildVisualizerImage(scheme, this);
  }
  
  public TextInstance getTextInstance(String name) {
    Iterator i = instances.iterator();
    while (i.hasNext()) {
      Instance instance = (Instance) i.next();
      if (instance instanceof TextInstance) {
        if (name.equals(instance.getName())) {
          return (TextInstance) instance;
        }
      }
    }
    return null;
  }

  public SymbolInstance getSymbolInstance(String name) {
    Iterator i = instances.iterator();
    while (i.hasNext()) {
      Instance instance = (Instance) i.next();
      if (instance instanceof SymbolInstance) {
        if (name.equals(instance.getName())) {
          return (SymbolInstance) instance;
        }
      }
    }
    return null;
  }
  
  /*
   * Reconcile our current elements with the elements in the owning scheme.
   */
  protected void rebuildInstances() {

    InstanceList newInstances = new InstanceList();

    Iterator e = instances.iterator();
    while (e.hasNext()) {
      Instance prop = (Instance) e.next();

      SchemeElement element = scheme.getElement(prop.getName());
      if (element != null && element.getType().equals(prop.getType())) {
        prop.setLocation(element.getLocation());
        newInstances.add(prop);
      }
    }

    Iterator i = scheme.getElements();
    while (i.hasNext()) {
      SchemeElement element = (SchemeElement) i.next();
      String name = element.getName();
      String type = element.getType();
      String location = element.getLocation();

      if (type.equals(SymbolItem.TYPE) || type.equals(TextItem.TYPE)) {
        boolean found = false;
        e = instances.iterator();
        while (e.hasNext() && !found) {
          Instance prop = (Instance) e.next();
          found = name.equals(prop.getName());
        }

        if (!found) {
          Instance instance = Instance.newDefaultInstance(name, type, location);
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
}
