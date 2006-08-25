/*
 * $Id$
 *
 * Copyright (c) 2005 by Brent Easton
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
package plc;

import java.beans.PropertyChangeSupport;

import VASSAL.build.module.BasicCommandEncoder;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;

/**
 * @author Brent Easton
 */
public class PlcCommandEncoder extends BasicCommandEncoder  {

  protected PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(this);
  
  public PlcCommandEncoder() {
    
    super();

  }

  public Decorator createDecorator(String type, GamePiece inner) {
    if (type.startsWith(PlcLabeler.ID)) {
      return new PlcLabeler(type, inner);
    }
    else {
      return super.createDecorator(type, inner);
    }
  }
  
}
