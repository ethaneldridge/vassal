package avl;

public class EdgeTerrainDefinitions extends BasicTerrainDefinitions {
  
  
  public EdgeTerrainDefinitions() {
    super();
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[] {EdgeTerrain.class};
  }

  public static String getConfigureTypeName() {
    return "Hex Edge Terrain Definitions";
  }
  
}
