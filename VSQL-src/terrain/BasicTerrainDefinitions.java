package terrain;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Composite;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.util.Enumeration;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;

public class BasicTerrainDefinitions extends AbstractConfigurable {
  
  
  protected static final int ICON_SIZE = 16;


  public BasicTerrainDefinitions() {
    super();
  }
  
  public MapTerrain getTerrain(String terrainName) {
    
    Enumeration e = getBuildComponents();
    while (e.hasMoreElements()) {
      MapTerrain terrain = (MapTerrain) e.nextElement();
      if (terrain.getConfigureName().equals(terrainName)) {
        return terrain;
      }
    }
    return null;
  }
  
  public String[] getTerrainNames() {
    String[] names = new String[buildComponents.size()+1];
    Enumeration e = getBuildComponents();
    int i = 0;
    while(e.hasMoreElements()) {
      names[i++] = ((MapTerrain) e.nextElement()).getConfigureName();
    }
    names[i] = TerrainMap.NO_TERRAIN;
    return names;
  }

  public Color[] getTerrainColors() {
    Color[] colors = new Color[buildComponents.size()+1];
    Enumeration e = getBuildComponents();
    int i = 0;
    while(e.hasMoreElements()) {
      colors[i++] = ((MapTerrain) e.nextElement()).getColor();
    }
    colors[i] = null;
    return colors;
  }

  public Icon[] getTerrainIcons() {
    Color[] colors = getTerrainColors();
    Icon[] icons = new Icon[colors.length];
    BufferedImage image;
    int i;
    for (i=0; i < colors.length-1; i++) {
      image = new BufferedImage(ICON_SIZE, ICON_SIZE, BufferedImage.TYPE_4BYTE_ABGR);
      Graphics2D g2 = (Graphics2D) image.getGraphics();
      Composite oldComposite = g2.getComposite();
      g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5f));
      g2.setColor(colors[i]);
      g2.fillRect(0, 0, ICON_SIZE, ICON_SIZE);
      g2.setComposite(oldComposite);
      g2.setColor(Color.black);
      g2.drawRect(0, 0, ICON_SIZE-1, ICON_SIZE-1);
      icons[i] = new ImageIcon(image);
    }
    image = new BufferedImage(ICON_SIZE, ICON_SIZE, BufferedImage.TYPE_4BYTE_ABGR);
    Graphics g = image.getGraphics();
    g.setColor(Color.black);
    g.drawRect(0, 0, ICON_SIZE-1, ICON_SIZE-1);
    icons[i] = new ImageIcon(image);
    
    return icons;
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
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public HelpFile getHelpFile() {
      return null;
  }

  public void removeFrom(Buildable parent) {
  }

  
}
