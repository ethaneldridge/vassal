/*
 * Created on 1/03/2005
 */
package VSQL;

import VASL.build.module.ASLCommandEncoder;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;

/**
 * @author Brent Easton
 */
public class VSQLCommandEncoder extends ASLCommandEncoder {

  protected Decorator createDecorator(String type, GamePiece inner) {
    if (type.startsWith(VSQLFootprint.ID)) {
      return new VSQLFootprint(type, inner);
    }
    else if (type.startsWith(VSQLMarkMoved.ID)) {
      return new VSQLMarkMoved(type, inner);
    }
    else {
      return super.createDecorator(type, inner);
    }
  }
}
