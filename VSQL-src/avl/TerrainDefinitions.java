package avl;

import java.util.HashMap;

import org.w3c.dom.Element;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.SingleChildInstance;

public class TerrainDefinitions extends AbstractConfigurable {
  
  protected static TerrainDefinitions instance;
  protected HexTerrainDefinitions hexDefinitions;
  protected EdgeTerrainDefinitions edgeDefinitions;
  protected HashMap terrainMaps;
  
  public TerrainDefinitions() {
    instance = this;
    terrainMaps = new HashMap();
  }
  
  public HexTerrainDefinitions getHexTerrainDefinitions() {
    return hexDefinitions;    
  }
  
  public EdgeTerrainDefinitions getEdgeTerrainDefinitions() {
    return edgeDefinitions;
  }
    
  public TerrainMap getTerrainMap(TerrainHexGrid grid) {
    TerrainMap tm = (TerrainMap) terrainMaps.get(grid.getBoard().getConfigureName());
    if (tm == null) {
      tm = new TerrainMap(grid);
      terrainMaps.put(grid.getBoard().getConfigureName(), tm);
    }
    return tm;
  }
  
  public static TerrainDefinitions getInstance() {
    return instance;
  }
  
  public void build(Element e) {
    super.build(e);

    if (hexDefinitions == null) { 
      addChild(new HexTerrainDefinitions());
      hexDefinitions.build(null);
    }
    if (edgeDefinitions == null) { 
      addChild(new EdgeTerrainDefinitions());
      edgeDefinitions.build(null);
    }
  }
  
  private void addChild(Buildable b) {
    add(b);
    b.addTo(this);
  }
  
  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  public Class[] getAttributeTypes() {
    return new Class[0];
  }

  public String[] getAttributeNames() {
    return new String[0];
  }

  public String getAttributeValueString(String key) {
    return null;
  }

  public void setAttribute(String key, Object value) {
  }

  public Configurer getConfigurer() {
    return null;
  }

  public void addTo(Buildable parent) {
    validator = new SingleChildInstance(GameModule.getGameModule(),getClass());
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[] {
        HexTerrainDefinitions.class,
        EdgeTerrainDefinitions.class};
  }

  public static String getConfigureTypeName() {
    return "Terrain Definitions";
  }

  public void add(Buildable b) {
    super.add(b);
    if (b instanceof HexTerrainDefinitions) {
      hexDefinitions = (HexTerrainDefinitions) b;
    }
    else if (b instanceof EdgeTerrainDefinitions) {
      edgeDefinitions = (EdgeTerrainDefinitions) b;
    }  
  }

  public void remove(Buildable b) {
    super.remove(b);
    if (b instanceof HexTerrainDefinitions) {
      hexDefinitions = null;
    }
    else if (b instanceof EdgeTerrainDefinitions) {
      edgeDefinitions = null;
    }
  }
  
  public HelpFile getHelpFile() {
      return null;
  }

  public void removeFrom(Buildable parent) {
  }
  
}
