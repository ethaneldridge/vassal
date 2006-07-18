package avl;

import java.util.ArrayList;

public class HexTerrainDefinitions extends BasicTerrainDefinitions {
  
  
  public HexTerrainDefinitions() {
    super();
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[] {HexTerrain.class};
  }

  public static String getConfigureTypeName() {
    return "Full Hex Terrain Definitions";
  }

  public void setTerrain(ArrayList selectedHexList, String terrainName) {
    // TODO Auto-generated method stub
    
  }

  
}
