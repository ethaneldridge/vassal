package avl;

import java.awt.Point;
import java.awt.geom.Area;

import terrain.TerrainDefinitions;
import terrain.TerrainHex;
import terrain.TerrainHexGrid;
import terrain.TerrainMap;

public class AvlTerrainHexGrid extends TerrainHexGrid {

  /*
   * Take terrain into account when calculating the range shape
   */
  public Area getGridShape(Point center, int range) {
    //Area shape = (Area) shapeCache.get(new Integer(range));
    Area shape = null;
    TerrainMap terrainMap = TerrainDefinitions.getInstance().getTerrainMap(this);
    
    if (shape == null) {
      // Choose a starting point
      // Point origin = new Point(0, 0);
      Point origin = new Point(center);
      shape = getSingleHexShape(origin.x, origin.y, false);
      rotateIfSideways(origin);

      for (int i = -range; i <= range; i++) {
        int x = origin.x + (int) (i * dx);

        int length = range * 2 + 1 - Math.abs(i);

        int startY = 0;
        if (length % 2 == 1) {
          startY = origin.y - (int) (dy * (length - 1) / 2);
        }
        else {
          startY = origin.y - (int) (dy * (0.5 + (length - 2) / 2));
        }

        int y = startY;
        for (int j = 0; j < length; j++) {
          Point p = new Point(x, y);
          rotateIfSideways(p);
          Point hexPos = this.getGridPosition(p);
          rotateIfSideways(hexPos);
          
          TerrainHex hex = terrainMap.getHexTerrain(hexPos);
          if (hex == null || ! "Sea".equals(hex.getTerrain().getTerrainName())) {
            shape.add(getSingleHexShape(p.x, p.y, false));
          }
          y = startY + (int) (j * dy);
        }
      }

      rotateIfSideways(origin);
      // shape.transform(AffineTransform.getTranslateInstance(0 - origin.x, 0 - origin.y));
      // shapeCache.put(new Integer(range), shape);
    }
   // shape = new Area(AffineTransform.getTranslateInstance(center.x, center.y).createTransformedShape(shape));
    return shape;
  }
}
