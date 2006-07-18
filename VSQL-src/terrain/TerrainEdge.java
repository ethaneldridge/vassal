package terrain;

import java.awt.Point;
import VASSAL.tools.SequenceEncoder;

public class TerrainEdge  {

  private static final long serialVersionUID = 1L;
  protected static final String TYPE = "e";
  protected EdgeRef reference;
  protected Point center;
  protected EdgeTerrain terrain;
  
  public TerrainEdge (int c1, int r1, int c2, int r2, EdgeTerrain t, Point c) {
    reference = new EdgeRef(c1, r1, c2, r2);  
    center = c;
 }
  
 public TerrainEdge (EdgeRef ref, EdgeTerrain t, Point c) {
   reference = ref;
   center = c;
   terrain = t;
 }
 
 public TerrainEdge (int c1, int r1, int c2, int r2) {
    this(c1, r1, c2, r2, null, null);
  }

  public TerrainEdge (Point centerPos, TerrainHexGrid grid, EdgeTerrain t) {

    // Work out the Grid positions of the two hexes on each side of this edge
    // centerPos is the centre position of the Edge
    Point p1 = grid.snapTo(new Point(centerPos.x+4, centerPos.y+4));
    Point p2 = grid.snapTo(new Point(centerPos.x-4, centerPos.y-4));
    
    HexRef hex1 = new HexRef(grid.getGridPosition(p1));
    grid.rotateIfSideways(hex1);
    HexRef hex2 = new HexRef(grid.getGridPosition(p2));
    grid.rotateIfSideways(hex2);
    
    reference = new EdgeRef(hex1, hex2);
    terrain = t;  
    center = centerPos;
  }
  
  public TerrainEdge(String code) {
    this(0, 0, 0, 0);
    decode(code);
  }
  
  public HexRef getHexPos1() {
    return reference.getHex1();
  }

  public HexRef getHexPos2() {
    return reference.getHex2();
  }
  
  public EdgeRef getLocation() {
    return reference;
  }

  public EdgeRef getReverseLocation() {
    return reference.reversed();
  }
  
  public TerrainEdge reversed() {
    return new TerrainEdge(reference.reversed(), terrain, center);
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
    se.append(reference.getHex1().getColumn());
    se.append(reference.getHex1().getRow());
    se.append(reference.getHex2().getColumn());
    se.append(reference.getHex2().getRow());
    se.append(terrain == null ? "" : terrain.getTerrainName());
    return se.getValue();
  }
  
  public void decode(String code) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(code, ',');
    sd.nextToken();
    reference = new EdgeRef(sd.nextInt(0), sd.nextInt(0), sd.nextInt(0), sd.nextInt(0));
    terrain = (EdgeTerrain) TerrainDefinitions.getInstance().getEdgeTerrainDefinitions().getTerrain(sd.nextToken(TerrainMap.NO_TERRAIN)); 
  }
}
