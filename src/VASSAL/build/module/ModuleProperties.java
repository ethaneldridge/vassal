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
 
package VASSAL.build.module;

import java.util.Enumeration;

import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.map.MapProperties;

/**
 * A container for Global Properties attached to the Game Module.
 */
public class ModuleProperties extends MapProperties {
  
  public void addTo(Buildable parent) {
    GameModule module = (GameModule) parent;
    module.getToolBar().add(getLaunchButton());
  }

  public void removeFrom(Buildable parent) {
    ((GameModule)parent).getToolBar().remove(getLaunchButton());
  }

  /**
   * One of our global Properties has been changed, so repaint all maps
   */
  protected void childWasUpdated() {
    Enumeration maps = GameModule.getGameModule().getComponents(Map.class);
    while (maps.hasMoreElements()) {
      Map map = (Map) maps.nextElement();
      map.repaint();
    }
  }
}
