package terrain;

import java.awt.Point;

import VASSAL.tools.SequenceEncoder;


public class TerrainHex  {

  private static final long serialVersionUID = 1L;
  protected static final String TYPE = "h";
  protected int row;
  protected int column;
  protected HexTerrain terrain;
  
  public TerrainHex (int c, int r, HexTerrain t) {
    row = r;
    column = c;
    terrain = t;    
  }
  
  public TerrainHex (int c, int r) {
    this(c, r, null);
  }

  public TerrainHex (Point p, HexTerrain t) {
    this(p.x, p.y, t);
  }
  
  public TerrainHex(String code) {
    this(0, 0);
    decode(code);
  }
  
  public int getRow() {
    return row;
  }
  
  public int getColumn() {
    return column;
  }
  
  public Point getLocation() {
    return new Point(column, row);
  }
  
  public void setTerrain(HexTerrain t) {
    terrain = t; 
  }
  
  public HexTerrain getTerrain() {
    return terrain;
  }
  
  public String encode() {
    SequenceEncoder se = new SequenceEncoder(TYPE, ',');
    se.append(column);
    se.append(row);
    se.append(terrain == null ? "" : terrain.getTerrainName());
    return se.getValue();
  }
  
  public void decode(String code) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(code, ',');
    sd.nextToken();
    column = sd.nextInt(0);
    row = sd.nextInt(0);
    terrain = (HexTerrain) TerrainDefinitions.getInstance().getHexTerrainDefinitions().getTerrain(sd.nextToken(TerrainMap.NO_TERRAIN)); 
  }
}
