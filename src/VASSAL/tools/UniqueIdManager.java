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
package VASSAL.tools;

import java.util.List;
import java.util.ArrayList;

/**
 * A class for assigning unique identifiers to objects.  Identifiers will be of
 * the form prefix#, where prefix is specified at initialization and the #
 * is an increasing digit.  When an object is removed, the other objects are reassigned
 * identifiers so that they end up with the same identifier they would have if the removed object
 * had never existed
 */
public class UniqueIdManager {
  private List instances=new ArrayList();
  private String prefix;

  public UniqueIdManager(String prefix) {
    this.prefix = prefix;
  }

  public void add(Identifyable i) {
    i.setId(prefix+instances.size());
    instances.add(i);
  }

  public void remove(Identifyable i) {
    int index = instances.indexOf(i);
    if (index >= 0) {
      for (int j=index+1,n=instances.size();j<n;++j) {
        ((Identifyable)instances.get(j)).setId(prefix+(j-1));
      }
      instances.remove(index);
    }
  }
  /**
   * An object with an identifier that can be manipulated by a {@link UniqueIdManager}
   */
  public static interface Identifyable {
    void setId(String id);
    String getId();
  }
}
