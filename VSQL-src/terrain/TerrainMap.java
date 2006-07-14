package terrain;

import java.awt.Point;
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

  public void setHexTerrainType(ArrayList hexes, HexTerrain terrain) {
    Iterator i = hexes.iterator();
    while (i.hasNext()) {
      setHexTerrainType(new TerrainHex((Point) i.next(), terrain));
    }
  }
  
  public TerrainHex getHexTerrain(Point hexPos) {
    return (TerrainHex) hexMap.get(hexPos);
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
  
  public Iterator getHexAreaTypes() {
    return hexArea.keySet().iterator();
  }
  
  protected void clearHexAreas() {
    hexArea.clear();
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
  
  public void save() {
    StringBuffer buffer = new StringBuffer(2000);
    
    Iterator i = hexMap.values().iterator();
    while (i.hasNext()) {
      TerrainHex hex = (TerrainHex) i.next();
      buffer.append(hex.encode());
      buffer.append(System.getProperty("line.separator"));
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
}