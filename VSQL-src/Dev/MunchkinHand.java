 
package Dev;

import VASSAL.build.module.PlayerHand;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Stack;

public class MunchkinHand extends PlayerHand {

  protected int maxCount = 8;
  
  /**
   * Need this for VASSAL to load properly
   */
  public MunchkinHand() {
    super();
  }
  
  /**
   * Check the count of items and do something if there is
   * to many
   */
  public void checkCount() {
    int count = 0;
    GamePiece[] pieces = getPieces();
    for (int i = 0; i < pieces.length; i++) {
      GamePiece p = pieces[i];
      if (p instanceof Stack) {
        for (int j = 0; j < ((Stack) p).getPieceCount(); j++) {
          count++;
        }
      }
      else {
        count++;
      }
    }
    if (count > maxCount) {
      /*
       * Run in circles, scream and shout!
       */
    }
  }
  
  
  public void addPiece(GamePiece p) {
    super.addPiece(p);
    checkCount();
  }
  
  public void removePiece(GamePiece p) {
    super.removePiece(p);
    checkCount();
  }
  
}
