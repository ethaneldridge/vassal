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
package VASSAL.configure;

import VASSAL.build.AutoConfigurable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;

import javax.swing.*;
import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

/**
 * A Configurer for configuring Configurable components
 * (Is that as redundant as it sounds?)
 * Automatically builds a property editor with controls for setting all
 * of the attributes of the target Configurable component
 */
public class AutoConfigurer extends Configurer
  implements PropertyChangeListener {
  private JPanel p;
  private AutoConfigurable target;
  private Vector configurers = new Vector();;
  private Hashtable conditions;

  public AutoConfigurer(AutoConfigurable c) {
    super(null, c.getConfigureName());

    target = c;
    setValue(target);
    target.addPropertyChangeListener(new PropertyChangeListener() {
      public void propertyChange(final PropertyChangeEvent evt) {
        if (Configurable.NAME_PROPERTY.equals(evt.getPropertyName())) {
          setName((String) evt.getNewValue());
        }
      }
    });

    p = new JPanel();
    p.setLayout(new javax.swing.BoxLayout(p, javax.swing.BoxLayout.Y_AXIS));

    String[] name = c.getAttributeNames();
    String[] prompt = c.getAttributeDescriptions();
    Class[] type = c.getAttributeTypes();

    int n = Math.min(name.length, Math.min(prompt.length, type.length));
    for (int i = 0; i < n; ++i) {
      if (type[i] == null) {
        continue;
      }
      Configurer config;
      if (String.class.isAssignableFrom(type[i])) {
        config = new StringConfigurer(name[i], prompt[i]);
      }
      else if (Integer.class.isAssignableFrom(type[i])) {
        config = new IntConfigurer(name[i], prompt[i]);
      }
      else if (Double.class.isAssignableFrom(type[i])) {
        config = new DoubleConfigurer(name[i], prompt[i]);
      }
      else if (Boolean.class.isAssignableFrom(type[i])) {
        config = new BooleanConfigurer(name[i], prompt[i]);
      }
      else if (Image.class.isAssignableFrom(type[i])) {
        config = new ImageConfigurer(name[i], prompt[i], GameModule.getGameModule().getArchiveWriter());
      }
      else if (Color.class.isAssignableFrom(type[i])) {
        config = new ColorConfigurer(name[i], prompt[i]);
      }
      else if (KeyStroke.class.isAssignableFrom(type[i])) {
        config = new HotKeyConfigurer(name[i], prompt[i]);
      }
      else if (java.io.File.class.isAssignableFrom(type[i])) {
        config = new FileConfigurer(name[i], prompt[i], GameModule.getGameModule().getArchiveWriter());
      }
      else if (String[].class.isAssignableFrom(type[i])) {
        config = new StringArrayConfigurer(name[i], prompt[i]);
      }
      else if (StringEnum.class.isAssignableFrom(type[i])) {
        try {
          String[] validValues = ((StringEnum) type[i].newInstance()).getValidValues(target);
          config = new StringEnumConfigurer(name[i], prompt[i], validValues);
        }
        catch (Exception e) {
          e.printStackTrace();
          config = new StringConfigurer(name[i], prompt[i]);
        }
      }
      else if (ConfigurerFactory.class.isAssignableFrom(type[i])) {
        try {
          ConfigurerFactory f = (ConfigurerFactory) type[i].newInstance();
          config = f.getConfigurer(target, name[i],prompt[i]);
        }
        catch (InstantiationException e) {
          throw new IllegalArgumentException("Invalid class " + type[i].getName());
        }
        catch (IllegalAccessException e) {
          throw new IllegalArgumentException("Invalid class " + type[i].getName());
        }
      }
      else {
        throw new IllegalArgumentException("Invalid class " + type[i].getName());
      }
      if (config != null) {
        config.addPropertyChangeListener(this);
        config.setValue(target.getAttributeValueString(name[i]));
        Box box = Box.createHorizontalBox();
        box.add(config.getControls());
        box.add(Box.createHorizontalGlue());
        p.add(box);
        configurers.addElement(config);
      }
      setVisibility(name[i],c.getAttributeVisibility(name[i]));
    }
  }

  public void reset() {
    String[] s = target.getAttributeNames();
    for (int i=0;i<s.length;++i) {
      Configurer config = getConfigurer(s[i]);
      if (config != null) {
        config.setValue(target.getAttributeValueString(s[i]));
      }
    }
  }

  public String getValueString() {
    return target.getConfigureName();
  }

  public void setValue(String s) {
    throw new RuntimeException
      ("Can't set Configurable from String");
  }

  public java.awt.Component getControls() {
    return p;
  }

  public void propertyChange(final java.beans.PropertyChangeEvent evt) {
    target.setAttribute(evt.getPropertyName(), evt.getNewValue());
    checkVisibility();
  }

  public void setVisibility(String attribute, VisibilityCondition c) {
    if (c != null) {
    if (conditions == null) {
      conditions = new Hashtable();
    }
    conditions.put(attribute, c);
    checkVisibility();
    }
  }

  protected void checkVisibility() {
    boolean visChanged = false;
    if (conditions != null) {
      for (Enumeration e = configurers.elements();
           e.hasMoreElements();) {
        Configurer c = (Configurer) e.nextElement();
        VisibilityCondition cond = (VisibilityCondition) conditions.get(c.getKey());
        if (cond != null) {
          if (c.getControls().isVisible() != cond.shouldBeVisible()) {
            visChanged = true;
            c.getControls().setVisible(cond.shouldBeVisible());
          }
        }
      }
      // Only repack the configurer if an item visiblity has changed.
      if (visChanged && p.getTopLevelAncestor() instanceof Window) {
        ((Window) p.getTopLevelAncestor()).pack();
      }
    }
  }

  public Configurer getConfigurer(String attribute) {
    for (Enumeration e = configurers.elements();
         e.hasMoreElements();) {
      Configurer c = (Configurer) e.nextElement();
      if (c.getKey().equals(attribute)) {
        return c;
      }
    }
    return null;
  }
}
