package VASSAL.build.module;

import VASSAL.build.*;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;

import java.util.Enumeration;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.Iterator;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;

/*
 * $Id$
 *
 * Copyright (c) 2004 by Rodney Kinney
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

/**
 * Container for definitions of Game Piece prototypes.
 * Actual definition is in inner class {@link VASSAL.build.module.PrototypeDefinition}
 */
public class PrototypesContainer extends AbstractConfigurable {
  private static PrototypesContainer instance;
  private HashMap definitions = new HashMap();
  private ArrayList listeners = new ArrayList();


  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  public Class[] getAttributeTypes() {
    return new Class[0];
  }

  public String[] getAttributeNames() {
    return new String[0];
  }

  public String getAttributeValueString(String key) {
    return null;
  }

  public void setAttribute(String key, Object value) {
  }

  public Configurer getConfigurer() {
    return null;
  }

  public void addTo(Buildable parent) {
    if (parent instanceof AbstractBuildable) {
      if (((AbstractBuildable) parent).getComponents(getClass()).hasMoreElements()) {
        throw new IllegalBuildException("Only one instance allowed");
      }
    }
  }



  public Class[] getAllowableConfigureComponents() {
    return new Class[]{PrototypeDefinition.class};
  }

  public static String getConfigureTypeName() {
    return "Game Piece Prototype Definitions";
  }

  public void add(Buildable b) {
    super.add(b);
    if (b instanceof PrototypeDefinition) {
      PrototypeDefinition def = (PrototypeDefinition) b;
      definitions.put(def.getConfigureName(),def);
      def.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          if (Configurable.NAME_PROPERTY.equals(evt.getPropertyName())) {
            definitions.remove(evt.getOldValue());
            definitions.put(evt.getNewValue(),evt.getSource());
          }
        }
      });
      fireDefinitionAdded(def);
    }
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public void removeFrom(Buildable parent) {
  }

  public static PrototypeDefinition getPrototype(String name) {
    if (instance == null) {
      Enumeration e = GameModule.getGameModule().getComponents(PrototypesContainer.class);
      if (e.hasMoreElements()) {
        instance = (PrototypesContainer) e.nextElement();
      }
      else {
        return null;
      }
    }
    return (PrototypeDefinition) instance.definitions.get(name);
  }

  void fireDefinitionChanged(PrototypeDefinition def) {
    fireEvent(new PrototypesContainer.Event(PrototypesContainer.Event.DEFINITION_CHANGED,def));
  }

  void fireDefinitionAdded(PrototypeDefinition def) {
    fireEvent(new PrototypesContainer.Event(PrototypesContainer.Event.DEFINITION_ADDED,def));
  }

  void fireDefinitionRemoved(PrototypeDefinition def) {
    fireEvent(new PrototypesContainer.Event(PrototypesContainer.Event.DEFINITION_REMOVED,def));
  }

  void fireEvent(Event evt) {
    for (Iterator iterator = listeners.iterator(); iterator.hasNext();) {
      Listener listener = (Listener) iterator.next();
      listener.definitionsChanged(evt);
    }
  }

  public void addListener(Listener l) {
    listeners.add(l);
  }

  public static interface Listener {
    public void definitionsChanged(Event evt);
  }

  public static class Event {
    public static final int DEFINITION_REMOVED=0;
    public static final int DEFINITION_ADDED=1;
    public static final int DEFINITION_CHANGED=2;
    private PrototypeDefinition target;
    private int type;

    public Event(int type, PrototypeDefinition target) {
      this.target = target;
      this.type = type;
    }

    public PrototypeDefinition getTarget() {
      return target;
    }

    public int getType() {
      return type;
    }
  }
}
