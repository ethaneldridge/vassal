package terrain;

import java.awt.Point;
import java.awt.Rectangle;

import VASSAL.tools.SequenceEncoder;


public class TerrainEdge  {

  private static final long serialVersionUID = 1L;
  protected static final String TYPE = "e";
  protected int fromRow, toRow;
  protected int fromColumn, toColumn;
  protected Point center;
  protected EdgeTerrain terrain;
  
  public TerrainEdge (int c1, int r1, int c2, int r2, EdgeTerrain t, Point c) {
    fromRow = r1;
    fromColumn = c1;
    toRow = r2;
    toColumn = c2;
    terrain = t;    
    center = c;
 }
  
 public TerrainEdge (int c1, int r1, int c2, int r2) {
    this(c1, r1, c2, r2, null, null);
  }

  public TerrainEdge (Point centerPos, TerrainHexGrid grid, EdgeTerrain t) {

    // Work out the Grid positions of the two hexes on each side of this edge
    Point p1 = grid.snapTo(new Point(centerPos.x+4, centerPos.y+4));
    Point p2 = grid.snapTo(new Point(centerPos.x-4, centerPos.y-4));
    
    Point pos1 = grid.getGridPosition(p1);
    grid.rotateIfSideways(pos1);
    Point pos2 = grid.getGridPosition(p2);
    grid.rotateIfSideways(pos2);
    
    fromColumn = pos1.x;
    fromRow = pos1.y;
    toColumn = pos2.x;
    toRow = pos2.y;
    terrain = t;  
    center = centerPos;
  }
  
  public TerrainEdge(String code) {
    this(0, 0, 0, 0);
    decode(code);
  }
  
  public int getRow() {
    return fromRow;
  }
  
  public int getColumn() {
    return fromColumn;
  }
  
  public Point getHexPos1() {
    return new Point(fromColumn, fromRow);
  }

  public Point getHexPos2() {
    return new Point(toColumn, toRow);
  }
  
  public Rectangle getLocation() {
    return new Rectangle(fromColumn, fromRow, toColumn, toRow);
  }
  
  public Point getCenter() {
    return center;
  }
  
  public void setTerrain(EdgeTerrain t) {
    terrain = t; 
  }
  
  public EdgeTerrain getTerrain() {
    return terrain;
  }
  
  public String encode() {
    SequenceEncoder se = new SequenceEncoder(TYPE, ',');
    se.append(fromColumn);
    se.append(fromRow);
    se.append(toColumn);
    se.append(toRow);
    se.append(terrain == null ? "" : terrain.getTerrainName());
    return se.getValue();
  }
  
  public void decode(String code) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(code, ',');
    sd.nextToken();
    fromColumn = sd.nextInt(0);
    fromRow = sd.nextInt(0);
    toColumn = sd.nextInt(0);
    toRow = sd.nextInt(0);
    terrain = (EdgeTerrain) TerrainDefinitions.getInstance().getEdgeTerrainDefinitions().getTerrain(sd.nextToken(TerrainMap.NO_TERRAIN)); 
  }
}
