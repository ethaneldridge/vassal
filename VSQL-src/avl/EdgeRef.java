/* EdgeRef.java
 * Wrapper around a Rectangle to represent a reference to an Edge made up of
 * two hex grid references.
 * 
 */
package avl;

import java.awt.Rectangle;

public class EdgeRef extends Rectangle {
  
  private static final long serialVersionUID = 1L;

  public EdgeRef() {
    this(0, 0, 0, 0);
  }
  
  public EdgeRef(HexRef h1, HexRef h2) {
    this(h1.getColumn(), h1.getRow(), h2.getColumn(), h2.getRow());
  }
  
  public EdgeRef(int c1, int r1, int c2, int r2) {
    x = c1;
    y = r1;
    width = c2;
    height = r2;
  }
  
  public HexRef getHex1() {
    return new HexRef(x, y);
  }
  
  public HexRef getHex2() {
    return new HexRef(width, height);
  }
  
  public void setHex1(int c, int r) {
    x = c;
    y = r;
  }

  public void setHex1(HexRef h) {
    setHex1(h.getColumn(), h.getRow());
  }
  
  public void setHex2(int c, int r) {
    width = c;
    height = r;
  }
  
  public void setHex2(HexRef h) {
    setHex2(h.getColumn(), h.getRow());
  }
  
  public EdgeRef reversed() {
    return new EdgeRef(width, height, x, y);
  }
  
  
}
