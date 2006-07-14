package terrain;

import java.awt.Point;
import java.awt.geom.Area;

import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.GridEditor;
import VASSAL.build.module.map.boardPicker.board.HexGrid;
import VASSAL.configure.AutoConfigurer;

public class TerrainHexGrid extends HexGrid {
  
  protected TerrainHexGridEditor gridEditor;
  
  public TerrainHexGrid() {
    super();
  }
  
  public Board getBoard() {  
    return container == null ? null : container.getBoard();
  }
  
  public Area getSingleHex(int x, int y) {
    return getSingleHexShape(x, y, false);
  }
  
  public Area getSingleHex(Point p) {
    Point snap = snapTo(p);
    return getSingleHex(snap.x, snap.y);
  }
  
  public Area getSingleHex(TerrainHex t) {
    Point p = getHexCenter(t.getColumn(), t.getRow());
    return getSingleHex(p.x, p.y);
  }
  /*
   * Return the row,column co-ords for a hex at the given point
   */
  
  public Point getHexPos(Point p) {
    Point snap = snapTo(p);
    Point pos = getGridPosition(snap);
    rotateIfSideways(pos);
    Point orig = getHexCenter(pos.x, pos.y);
    return pos;
  }
  

  /* 
   * getRawRow & getRowColumn extracted from HexGridNumbering where they
   * do not belong! Should have been in HexGrid in the first place.
   */
  public int getRawColumn(Point p) {
    p = new Point(p);
    rotateIfSideways(p);
    int x = p.x - getOrigin().x;
    x = (int) Math.floor(x / getHexWidth() + 0.5);
    return x;
  }
  
  protected int getRawRow(Point p) {
    p = new Point(p);
    rotateIfSideways(p);
    Point origin = getOrigin();
    double dx = getHexWidth();
    double dy = getHexSize();
    int nx = (int) Math.round((p.x - origin.x) / dx);
    int ny;
    if (nx % 2 == 0) {
      ny = (int) Math.round((p.y - origin.y) / dy);
    }
    else {
      ny = (int) Math.round((p.y - origin.y - dy / 2) / dy);
    }
    return ny;
  }
  
  /*
   * Return the center of the specified hex
   */
  protected Point getHexCenter(int column, int row) {
    int x, y;
    
    if (sideways) {
      x = origin.y + (int) (dy * column) + ((row % 2 == 0) ? 0 : (int) (dy/2));
      y = origin.x + (int) (dx * row);
    }
    else {
      x = origin.x + (int) (dx * column);
      y = origin.y - (int) (dy * 0.5) + (int) (dy * row) + ((column % 2 == 0) ? (int) (dy/2) : (int) dy);
    }
    return new Point(x, y);
  }
  
  protected Point getGridPosition(Point p) {
    return new Point(getRawColumn(p), getRawRow(p));
  }
  
  /*
   * Override editGrid() to use the new Terrain GridEditor
   */
  public void editGrid() {
    gridEditor = new TerrainHexGridEditor((GridEditor.EditableGrid) this);
    gridEditor.setVisible(true);
    // Local variables may have been updated by GridEditor so refresh
    // configurers. Setting the Dy configurer will auto-recalculate dx
  double origDx = dx;
    AutoConfigurer cfg = (AutoConfigurer) getConfigurer();
    cfg.getConfigurer(DY).setValue(String.valueOf(dy));
    dx = origDx;
    cfg.getConfigurer(DX).setValue(String.valueOf(dx));
    cfg.getConfigurer(X0).setValue(String.valueOf(origin.x));
    cfg.getConfigurer(Y0).setValue(String.valueOf(origin.y));
    cfg.getConfigurer(SIDEWAYS).setValue(String.valueOf(sideways));
  }
 
  
}