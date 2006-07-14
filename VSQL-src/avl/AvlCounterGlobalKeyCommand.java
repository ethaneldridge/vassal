package avl;

import java.awt.Point;
import java.awt.geom.Area;
import java.util.Enumeration;

import terrain.TerrainHexGrid;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.ZonedGrid;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.counters.BooleanAndPieceFilter;
import VASSAL.counters.CounterGlobalKeyCommand;
import VASSAL.counters.Decorator;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceFilter;
import VASSAL.counters.PropertiesPieceFilter;
import VASSAL.tools.FormattedString;

public class AvlCounterGlobalKeyCommand extends CounterGlobalKeyCommand implements EditablePiece {
  
  public AvlCounterGlobalKeyCommand() {
    super();
  }

  public AvlCounterGlobalKeyCommand(String type, GamePiece inner) {
    super(type, inner);
  }

  public void apply() {
    PieceFilter filter = PropertiesPieceFilter.parse(new FormattedString(propertiesFilter).getText(Decorator.getOutermost(this)));
    Command c = new NullCommand();
    if (restrictRange) {
      int r = range;
      if (!fixedRange) {
        try {
          r = Integer.parseInt((String) Decorator.getOutermost(this).getProperty(rangeProperty));
        }
        catch (Exception e) {
          
        }
      }
      filter = new BooleanAndPieceFilter(filter,new AvlRangeFilter(getMap(), getPosition(), r));
    }
    for (Enumeration e = GameModule.getGameModule().getComponents(Map.class); e.hasMoreElements();) {
      Map m = (Map) e.nextElement();
      c = c.append(globalCommand.apply(m, filter));
    }
    GameModule.getGameModule().sendAndLog(c);
  }
  
  public class AvlRangeFilter implements PieceFilter {

    protected TerrainHexGrid grid;
    protected Area area;
    protected Point position;
    
    public AvlRangeFilter(Map map, Point p, int range) {

      position = p;
      Board b = map.findBoard(position);
      if (b != null) {
        if (b.getGrid() instanceof TerrainHexGrid) {
          grid = (TerrainHexGrid) b.getGrid();
        }
        else if (b.getGrid() instanceof ZonedGrid) {
          ZonedGrid g = (ZonedGrid) b.getGrid();
          Enumeration e = g.getComponents(TerrainHexGrid.class);
          if (e != null && e.hasMoreElements()) {
            grid = (TerrainHexGrid) e.nextElement();
          }
        } 
        if (grid != null) {
          area = grid.getGridShape(b.snapTo(position), range);
        }
      }
    }
    
    public boolean accept(GamePiece piece) {
      boolean accept = false;
      if (area != null) {
        accept = area.contains(piece.getPosition());
      }
      return accept;
    }
    
  }

}
