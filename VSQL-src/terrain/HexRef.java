/* HexRef.java
 * Wrapper around a Point to represent a Hex Grid reference.
 */
package terrain;

import java.awt.Point;

public class HexRef extends Point {

  private static final long serialVersionUID = 1L;
    
  public HexRef(int c, int r) {
    super(c, r);
  }
  
  public HexRef(Point p) {
    super(p);
  }
  
  public int getColumn() {
    return x;
  }
  
  public int getRow() {
    return y;
  }
  
  public void setColumn(int c) {
    x = c;
  }
  
  public void setRow(int r) {
    y = r;
  }
  
}
