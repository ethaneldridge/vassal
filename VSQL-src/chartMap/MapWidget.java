package chartMap;

import java.awt.BorderLayout;
import java.awt.Component;

import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.w3c.dom.Element;

import VASSAL.build.Buildable;
import VASSAL.build.Widget;

public class MapWidget extends Widget {

  protected JPanel panel;
  protected JScrollPane mapScroll;
  protected WidgetMap map;
  protected Buildable parent;

  public MapWidget() {
    panel = new JPanel();
    panel.setLayout(new BorderLayout());
  }

  public static String getConfigureTypeName() {
    return "Map Widget";
  }

  public void build(Element e) {
    super.build(e);
    rebuild();        // Force immediate build of the Map
  }
  
  public Component getComponent() {
    return panel;
  }
  
  public void AddTo(Buildable b) {
    parent = b;
  }

  public void add(Buildable b) {
    if (b instanceof WidgetMap) {
      if (mapScroll != null) {
        panel.remove(mapScroll);
        mapScroll = null;
      }
      map = (WidgetMap) b;
      mapScroll = map.getScroll();
      panel.add(mapScroll, BorderLayout.CENTER);
      panel.add(map.getToolBar(), BorderLayout.NORTH);
      panel.revalidate();
    }
    super.add(b);
  }

  public void remove(Buildable b) {
    if (b instanceof WidgetMap) {
      panel.remove(mapScroll);
      panel.remove(map.getToolBar());
      mapScroll = null;
    }
    super.remove(b);
  }

  public String[] getAttributeDescriptions() {
    return new String[] {"Name:  "};
  }

  public Class[] getAttributeTypes() {
    return new Class[] {String.class};
  }

  public String[] getAttributeNames() {
    return new String[] {NAME};
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[] {WidgetMap.class};
  }

}
