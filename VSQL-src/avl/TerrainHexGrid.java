package avl;

import java.awt.Point;
import java.awt.Rectangle;
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
   * Return the display shape of an Edge between two hexes
   */
  public Area getEdge(HexRef hexPos1, HexRef hexPos2) {
    Point c1 = getHexCenter(hexPos1.getColumn(), hexPos1.getRow());
    Point c2 = getHexCenter(hexPos2.getColumn(), hexPos2.getRow());
    Point p = new Point((c1.x+c2.x)/2, (c1.y+c2.y)/2);
    Point snap = snapToHexSide(p);
    return new Area(new Rectangle(snap.x-4, snap.y-4, 9, 9));
  }
  
  /*
   * Return the row,column co-ords for a hex at the given point
   */
  
  public HexRef getHexPos(Point p) {
    Point snap = snapTo(p);
    HexRef pos = new HexRef(getGridPosition(snap));
    rotateIfSideways(pos);
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
  
  protected int getMaxRows() {
    if (sideways) {
      return (int) Math.floor(getContainer().getSize().height / getHexSize() + 0.5);
    }
    else {
      return (int) Math.floor(getContainer().getSize().height / getHexWidth() + 0.5);
    }
  }

  protected int getMaxColumns() {
    if (sideways) {
      return (int) Math.floor(getContainer().getSize().width / getHexWidth() + 0.5);
    }
    else {
      return (int) Math.floor(getContainer().getSize().width / getHexSize() + 0.5);
    }
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
  
  /*
   * Return the raw Grid Reference of the hex contining the given point
   */
  protected HexRef getGridPosition(Point p) {
    return new HexRef(getRawColumn(p), getRawRow(p));
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
 
  /*
   * Return a list of the Grid positions of adjacent hexes. Do not include
   * hexes where no part of the hex appears on the board.
   * Argument is a GridPosition (c, r), not a map position(x, y)
   */
  
  
  protected HexRef[] getAdjacentHexes(HexRef pos) {
  
    HexRef[] adjacent = new HexRef[6];
    int c = pos.x;
    int r = pos.y;
    int next = 0;
    
    if (sideways) {
      if (r % 2 == 0) {
        next = addHex(adjacent, next, c-1, r-1);
        next = addHex(adjacent, next, c, r-1);
        next = addHex(adjacent, next, c-1, r);
        next = addHex(adjacent, next, c+1, r);
        next = addHex(adjacent, next, c-1, r+1);
        next = addHex(adjacent, next, c, r+1);
      }
      else {
        next = addHex(adjacent, next, c, r-1);
        next = addHex(adjacent, next, c+1, r-1);
        next = addHex(adjacent, next, c-1, r);
        next = addHex(adjacent, next, c+1, r);
        next = addHex(adjacent, next, c, r+1);
        next = addHex(adjacent, next, c+1, r+1);
      }
    }
    else {
      if (c % 2 == 0) {
        next = addHex(adjacent, next, c-1, r-1);
        next = addHex(adjacent, next, c, r-1);
        next = addHex(adjacent, next, c+1, r-1);
        next = addHex(adjacent, next, c-1, r);
        next = addHex(adjacent, next, c, r+1);
        next = addHex(adjacent, next, c+1, r);
      }
      else {
        next = addHex(adjacent, next, c-1, r);
        next = addHex(adjacent, next, c, r-1);
        next = addHex(adjacent, next, c+1, r);
        next = addHex(adjacent, next, c-1, r+1);
        next = addHex(adjacent, next, c, r+1);
        next = addHex(adjacent, next, c+1, r+1);
      }
    }
    
    return adjacent;
  }
  
  /*
   * Add Hex to array if it is the map
   */
  protected int addHex(HexRef[] points, int next, int column, int row) {
    if (column >= -1 && column <= getMaxColumns()+1 && row >= -1 && row <= getMaxRows()+1) {
      points[next++] = new HexRef(column, row);
    }
    return next;
  }
}