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

import java.util.ArrayList;
import java.util.Iterator;

public class InstanceList extends ArrayList {
  
  public InstanceList() {
    super();
  }
  
  public int getSymbolCount() {
    return getCount(SymbolItem.TYPE);
  }

  public int getTextCount() {
    return getCount(TextItem.TYPE);
  }
  
  public int getCount(String type) {
    Iterator i = iterator(); 
    int count = 0;
    while (i.hasNext()) {
      Instance ins = (Instance) i.next();
      if (ins.getType().equals(type)) {
        count++;
      }
    }
    return count;
  }
  
  public SymbolInstance getSymbol(int n) {
    return (SymbolInstance) getInstance(SymbolItem.TYPE, n);
  }
  
  public TextInstance getText(int n) {
    return (TextInstance) getInstance(TextItem.TYPE, n);
  }
  
  public Instance getInstance(String type, int n) {
    Iterator i = iterator(); 
    int count = 0;
    while (i.hasNext()) {
      Instance ins = (Instance) i.next();
      if (ins.getType().equals(type)) {
        if (count == n) {
          return ins;
        }
        count++;
      }
    }
    return null;
  }
}