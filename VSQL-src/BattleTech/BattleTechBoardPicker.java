package BattleTech;

import java.util.Enumeration;

import org.w3c.dom.Element;

import VASSAL.build.Builder;
import VASSAL.build.module.map.BoardPicker;
import VASSAL.build.module.map.boardPicker.Board;

public class BattleTechBoardPicker extends BoardPicker {
  
  public BattleTechBoardPicker() {
    super();
  }
  
  public void build(Element e) {
    if (e == null) {
      BattleTechBoard b = new BattleTechBoard();
      b.build(null);
      b.addTo(this);
    }
    else {
      super.build(e);      
    }
  }
  
  public Board getBoard(String boardName) {
    for (Enumeration e = possibleBoards.elements();
         e.hasMoreElements();) {
      BattleTechBoard b = (BattleTechBoard) e.nextElement();
      if (b.getName().equals(boardName)) {
        BattleTechBoard clone = new BattleTechBoard();
        clone.build(b.getBuildElement(Builder.createNewDocument()));
        return clone;
      }
    }
    warn("Board " + boardName + " not found");
    return null;
  }
  
}