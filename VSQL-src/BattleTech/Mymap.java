package BattleTech;

import java.util.Enumeration;

import VASSAL.build.module.Map;
import VASSAL.build.module.map.boardPicker.Board;

public class Mymap extends Map
{
  
  public synchronized void setBoards(Enumeration boardList) {
    boards.removeAllElements();
    System.gc();
    while (boardList.hasMoreElements()) {
      BattleTechBoard board = (BattleTechBoard) boardList.nextElement();
      board.setMap(this);
      boards.addElement(board);
    }
    setBoardBoundaries();
  }
  
	  public BattleTechBoard getTechBoardByName(String name) 
	  {
		    BattleTechBoard board = null;
		    if (name != null) 
		    {
		    	for (Enumeration e = getAllBoards(); e.hasMoreElements();) 
		    	{
		    		BattleTechBoard b = (BattleTechBoard) e.nextElement();
		    		if (name.equals(b.getName())) 
		    		{
		    			board = b;
		    			break;
		    		}
		    	}
		    }
		    return board;
	  }
}