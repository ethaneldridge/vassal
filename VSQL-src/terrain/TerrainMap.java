package terrain;

import java.awt.Point;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

public class TerrainMap {
  
  protected String boardName;
  protected HashMap hexMap;
  protected HashMap edgeMap;
  
  public TerrainMap() {
    hexMap = new HashMap();
    edgeMap = new HashMap();
  }
  
  public void setHexTerrainType(Point hexPos, String name) {
    hexMap.put(hexPos, name);
  }

  public void setHexTerrainType(ArrayList hexes, String name) {
    Iterator i = hexes.iterator();
    while (i.hasNext()) {
      setHexTerrainType((Point) i.next(), name);
    }
  }
  
//  public void addEdge(Point hex, String name) {
//    hexMap.put(hex, name);
//  }
  
  public String getHexTerrainType(Point hexPos) {
    return (String) hexMap.get(hexPos);
  }
}