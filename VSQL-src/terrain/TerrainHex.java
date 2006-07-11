package terrain;

import java.awt.Point;


public class TerrainHex  {

  private static final long serialVersionUID = 1L;
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
  
}
