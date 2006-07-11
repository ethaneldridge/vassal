package terrain;

import java.awt.Point;
import java.awt.geom.Area;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

import VASSAL.build.module.map.boardPicker.Board;

public class TerrainMap {
  
  protected Board board;
  protected TerrainHexGrid grid;
  protected HashMap hexMap;
  protected HashMap hexArea;
  protected HashMap edgeMap;
  
  public TerrainMap() {
    hexMap = new HashMap();
    hexArea = new HashMap();
    edgeMap = new HashMap();
  }
  
  public TerrainMap(TerrainHexGrid g) {
    this();
    grid = g;
    board = grid.getBoard();
  }
  
  public void setHexTerrainType(TerrainHex hex) {
    hexMap.put(hex.getLocation(), hex);
  }

  public void setHexTerrainType(ArrayList hexes, HexTerrain terrain) {
    Iterator i = hexes.iterator();
    while (i.hasNext()) {
      setHexTerrainType(new TerrainHex((Point) i.next(), terrain));
    }
  }
  
  public HexTerrain getHexTerrain(Point hexPos) {
    return (HexTerrain) hexMap.get(hexPos);
  }
  
  public Iterator getAllHexTerrain() {
    return hexMap.values().iterator();
  }
  
  public Area getHexArea(String terrainType) {
    Area area = (Area) hexArea.get(terrainType);
    if (area == null) {
      rebuildHexAreas();
      area = (Area) hexArea.get(terrainType);
    }
    return area;
  }
  
  protected void rebuildHexAreas() {
    hexArea.clear();
    Iterator i = getAllHexTerrain();
    while (i.hasNext()) {
      TerrainHex hex = (TerrainHex) i.next();
      String type = hex.getTerrain().getConfigureName();
      Area area = (Area) hexArea.get(type);
      if (area == null) area = new Area();
      area.add(grid.getSingleHex(hex));
      hexArea.put(type, area);
    }
    
  }
}