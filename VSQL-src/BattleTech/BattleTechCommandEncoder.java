package BattleTech;

import VASSAL.build.module.BasicCommandEncoder;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;

public class BattleTechCommandEncoder extends BasicCommandEncoder {
  public Decorator createDecorator(String type, GamePiece inner) {
    Decorator piece = null;
    if (type.startsWith(ViewInfo.ID)) {
      piece = new ViewInfo(type, inner);
    }
    else {
      piece = super.createDecorator(type, inner);
    }
    return piece;
  }
}