package terrain;

import java.awt.Point;
import java.awt.Rectangle;
import java.awt.geom.Area;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

import VASSAL.build.GameModule;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.DataArchive;

public class TerrainMap {
  
  public static final String CHAR_SET = "UTF-8";
  public static final String MAP_DIR = "terrainMaps";
  public static final String FILE_SUFFIX = "txt";

  protected static final String NO_TERRAIN = "No Terrain";
  
  protected Board board;
  protected TerrainHexGrid grid;
  protected HashMap hexMap;   // Keyed by Point = Hex Grid Position
  protected HashMap hexArea;
  protected HashMap edgeMap;  // Keyed by Rect = 2 Hex Grid Positions
  protected HashMap edgeArea; 
  
  public TerrainMap() {
    hexMap = new HashMap();
    hexArea = new HashMap();
    edgeMap = new HashMap();
    edgeArea = new HashMap();
  }
  
  public TerrainMap(TerrainHexGrid g) {
    this();
    grid = g;
    board = grid.getBoard();
    load();
  }
  
  public void setHexTerrainType(TerrainHex hex) {
    if (hex.getTerrain() == null) {
      hexMap.remove(hex.getLocation());
    }
    else {
      hexMap.put(hex.getLocation(), hex);
    }
    clearHexAreas();
  }

  /*
   * Add edge to map twice, so that we can look it up from
   * either hex
   */
  public void setEdgeTerrainType(TerrainEdge edge) {
    if (edge.getTerrain() == null) {
      edgeMap.remove(edge.getLocation());
      edgeMap.remove(edge.getReverseLocation());
    }
    else {
      edgeMap.put(edge.getLocation(), edge);
      edgeMap.put(edge.getReverseLocation(), edge.reversed());
    }
    clearEdgeAreas();
  }
  
  public void setHexTerrainType(ArrayList hexes, HexTerrain terrain) {
    Iterator i = hexes.iterator();
    while (i.hasNext()) {
      setHexTerrainType(new TerrainHex((HexRef) i.next(), terrain));
    }
  }

  public void setEdgeTerrainType(ArrayList edges, TerrainHexGrid grid, EdgeTerrain terrain) {
    Iterator i = edges.iterator();
    while (i.hasNext()) {
      setEdgeTerrainType(new TerrainEdge((Point) i.next(), grid, terrain));
    }
  }
  
  public TerrainHex getHexTerrain(HexRef hexPos) {
    return (TerrainHex) hexMap.get(hexPos);
  }
  
  public Iterator getAllHexTerrain() {
    return hexMap.values().iterator();
  }

  public TerrainEdge getEdgeTerrain(HexRef hexPos1, HexRef hexPos2) {
    Rectangle key = new Rectangle(hexPos1.x, hexPos1.y, hexPos2.x, hexPos2.y);
    return (TerrainEdge) edgeMap.get(key);
  }
  
  public Iterator getAllEdgeTerrain() {
    return edgeMap.values().iterator();
  }
  
  public Area getHexArea(String terrainType) {
    Area area = (Area) hexArea.get(terrainType);
    if (area == null) {
      rebuildHexAreas();
      area = (Area) hexArea.get(terrainType);
    }
    return area;
  }

  public Area getEdgeArea(String terrainType) {
    Area area = (Area) edgeArea.get(terrainType);
    if (area == null) {
      rebuildEdgeAreas();
      area = (Area) edgeArea.get(terrainType);
    }
    return area;
  }
  
  public Iterator getHexAreaTypes() {
    return hexArea.keySet().iterator();
  }

  public Iterator getEdgeAreaTypes() {
    return edgeArea.keySet().iterator();
  }
  
  protected void clearHexAreas() {
    hexArea.clear();
  }

  protected void clearEdgeAreas() {
    edgeArea.clear();
  }
  
  protected void rebuildHexAreas() {
    clearHexAreas();
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
  
  protected void rebuildEdgeAreas() {
    clearEdgeAreas();
    Iterator i = getAllEdgeTerrain();
    while (i.hasNext()) {
      TerrainEdge edge = (TerrainEdge) i.next();
      String type = edge.getTerrain().getConfigureName();
      Area area = (Area) edgeArea.get(type);
      if (area == null) area = new Area();
      area.add(grid.getEdge(edge.getHexPos1(), edge.getHexPos2()));
      edgeArea.put(type, area);
    }  
  }
  
  public void save() {
    StringBuffer buffer = new StringBuffer(2000);
    
    Iterator i = hexMap.values().iterator();
    while (i.hasNext()) {
      TerrainHex hex = (TerrainHex) i.next();
      buffer.append(hex.encode());
      buffer.append(System.getProperty("line.separator"));
    }

    /*
     * Edges appear in the TerrainMap twice, once for each hex they seperate.
     * Only write them out once.
     */
    i = edgeMap.values().iterator();
    while (i.hasNext()) {
      TerrainEdge edge = (TerrainEdge) i.next();
      if (edge.getHexPos1().getColumn() < edge.getHexPos2().getColumn() || 
          (edge.getHexPos1().getColumn() == edge.getHexPos2().getColumn() && 
              edge.getHexPos1().getRow() <= edge.getHexPos2().getRow())) {
        buffer.append(edge.encode());
        buffer.append(System.getProperty("line.separator"));
      }
    }
    
    ArchiveWriter writer = GameModule.getGameModule().getArchiveWriter();
    byte[] bytes = new byte[0];
    try {
      bytes = buffer.toString().getBytes(CHAR_SET);
    }
    catch (Exception e) {
      
    }
    writer.addFile(getMapFileName(board), bytes);
    
  }
  
  public void load() {
    try {
      DataArchive archive = GameModule.getGameModule().getDataArchive();
      InputStream stream = archive.getFileStream(getMapFileName(board));
      InputStreamReader reader = new InputStreamReader(stream, CHAR_SET);
      BufferedReader buffer = new BufferedReader(reader);
      for (String line = buffer.readLine(); line != null; line = buffer.readLine()) {
        if (line.startsWith(TerrainHex.TYPE)) {
          addHexTerrain(line);
        }
        else if (line.startsWith(TerrainEdge.TYPE)) {
          addEdgeTerrain(line);
        }
      }
    }
    catch (Exception e) {
      
    }
  }
  
  public String getMapFileName(Board board) {
    return MAP_DIR + "/" + board.getName() + "." + FILE_SUFFIX;
  }

  protected void addHexTerrain(String line) {
    TerrainHex hex = new TerrainHex(line);
    setHexTerrainType(hex);    
  }
  
  protected void addEdgeTerrain(String line) {
    TerrainEdge edge = new TerrainEdge(line);
    setEdgeTerrainType(edge);    
  }
  
}