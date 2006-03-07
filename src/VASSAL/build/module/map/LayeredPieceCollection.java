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
package VASSAL.build.module.map;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.SingleChildInstance;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.counters.*;

import java.io.File;
import java.net.MalformedURLException;
import java.util.Enumeration;

/**
 * Defines PieceCollection in which pieces are assigned to an arbitrary number of layers
 * according to a property setting
 */
public class LayeredPieceCollection extends AbstractConfigurable {
  public static final String PROPERTY_NAME="property";
  public static final String LAYER_ORDER="layerOrder";
  protected Collection collection = new Collection("Layer", new String[0]);
  protected Map map;

  public String[] getAttributeDescriptions() {
    return new String[]{"Property name for layer","Layer Order"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class,String[].class};
  }

  public String[] getAttributeNames() {
    return new String[]{PROPERTY_NAME,LAYER_ORDER};
  }

  public String getAttributeValueString(String key) {
    if (PROPERTY_NAME.equals(key)) {
      return collection.propertyName;
    }
    else if (LAYER_ORDER.equals(key)) {
      return StringArrayConfigurer.arrayToString(collection.layerOrder);
    }
    else {
      return null;
    }
  }

  public void setAttribute(String key, Object value) {
    if (PROPERTY_NAME.equals(key)) {
      collection.propertyName = (String) value;
    }
    else if (LAYER_ORDER.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      collection.layerOrder = (String[]) value;
      collection.initLayers(collection.layerOrder.length+1);
    }
  }

  public void add(Buildable b) {
    super.add(b);
    if (map != null && b instanceof LayerControl) {
      addControl((LayerControl) b);
    }
  }
  
  public void addTo(Buildable parent) {
    map = (Map)parent;
    validator = new SingleChildInstance(map,getClass());
    map.setPieceCollection(collection);
    Enumeration e = this.getComponents(LayerControl.class); 
    while (e.hasMoreElements()) {
      addControl((LayerControl) e.nextElement());
    }
  }

  protected void addControl(LayerControl lc) {
    map.getToolBar().add(lc.getLaunchButton());
    lc.setMap(map);
  }
  
  public Class[] getAllowableConfigureComponents() {
    return new Class[] { LayerControl.class };
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "Map.htm"), "#GamePieceLayers");
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public static String getConfigureTypeName() {
    return "Game Piece Layers";
  }

  public void removeFrom(Buildable parent) {
    map.setPieceCollection(new DefaultPieceCollection());
    Enumeration e = this.getComponents(LayerControl.class); 
    while (e.hasMoreElements()) {
     ((LayerControl) e.nextElement()).removeFrom(this);
    }
  }
  
  public Map getMap() {
    return map;
  }

  /** The PieceCollection class used by the map to which a LayeredPieceCollection has been added */
  public static class Collection extends CompoundPieceCollection implements DeckVisitor {
    private String propertyName;
    private String[] layerOrder;
    private DeckVisitorDispatcher dispatcher = new DeckVisitorDispatcher(this);

    public Collection(String propertyName, String[] layerOrder) {
      super(0);
      setPropertyName(propertyName);
      setLayerOrder(layerOrder);
    }

    public String[] getLayerOrder() {
      return layerOrder;
    }

    public void setLayerOrder(String[] layerOrder) {
      this.layerOrder = layerOrder;
      initLayers(layerOrder.length+1);
    }

    public String getPropertyName() {
      return propertyName;
    }

    public void setPropertyName(String propertyName) {
      this.propertyName = propertyName;
    }

    public int getLayerForPiece(GamePiece p) {
      return ((Integer)dispatcher.accept(p)).intValue();
    }
    
    public int getLayerForName(String layer) {
      for (int i=0; i < layerOrder.length; i++) {
        if (layer.equals(layerOrder[i])) {
          return i;
        }
      }
      return -1;
    }
    
    public String getLayerNameForPiece(GamePiece p) {
      int layer = getLayerForPiece(p);
      return layer >= layerOrder.length ? "" : layerOrder[layer];
    }

    protected boolean canPiecesMerge(GamePiece p1, GamePiece p2) {
      return super.canPiecesMerge(p1, p2)
          && getLayerForPiece(p1) == getLayerForPiece(p2);
    }

    public Object visitDeck(Deck d) {
      return new Integer(layerOrder.length);
    }

    public Object visitDefault(GamePiece p) {
      String property = (String) p.getProperty(propertyName);
      int layer = layerOrder.length;
      for (int i=0;i<layerOrder.length;++i) {
        if (layerOrder[i].equals(property)) {
          layer = i;
          break;
        }
      }
      return new Integer(layer);
    }

    public Object visitStack(Stack s) {
      GamePiece top = s.topPiece();
      if (top == null) {
        return new Integer(layerOrder.length);
      }
      return visitDefault(top);
    }
  }
}
