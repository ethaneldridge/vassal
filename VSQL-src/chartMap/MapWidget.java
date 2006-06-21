package chartMap;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Point;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;

import org.w3c.dom.Element;

import VASSAL.build.Buildable;
import VASSAL.build.Widget;
import VASSAL.build.module.map.PieceMover;
import VASSAL.build.widget.TabWidget;

public class MapWidget extends Widget implements DropTargetListener, Runnable {

  protected JPanel panel;
  protected JScrollPane mapScroll;
  protected WidgetMap map;
  protected Buildable parent;
  protected JTabbedPane tab;

  public MapWidget() {
    panel = new JPanel();
    panel.setLayout(new BorderLayout());
  }

  public static String getConfigureTypeName() {
    return "Map Widget";
  }

  /*
   * Maps must be built prior to game start, so force a rebuild
   * immediately. Default for widgets is to defer build until
   * first call to getComponent()
   */
  public void build(Element e) {
    super.build(e);
    rebuild();        
  }
  
  /*
   * Parent Widget has now completed building, so set up Drag Target
   * handling if our parent is a TabWidget
   */
  public Component getComponent() {
    if (tab == null && parent instanceof TabWidget) {
      tab = (JTabbedPane) ((TabWidget) parent).getComponent();
      tab.setDropTarget(PieceMover.DragHandler.makeDropTarget(tab, DnDConstants.ACTION_MOVE, this));
    }
    return panel;
  }

  public void addTo(Buildable b) {
    super.addTo(b);
    parent = (Widget) b;
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

  /*
   * A counter has been dragged over the tab of a parent TabWidget
   * containing another map, so bring it to the front after a 500ms delay
   */

  protected Thread delayThread;
  protected int delay = 500;
  protected long expirationTime;
  protected int showTab = -1;
  
  public void dragOver(DropTargetDragEvent e) {
    if (tab != null) {
      Point p = e.getLocation();
      int tabNumber = tab.getUI().tabForCoordinate(tab, p.x, p.y);
      if (tabNumber >= 0 && tabNumber != tab.getSelectedIndex()) {
        if (tabNumber != showTab) {
          restartDelay();
          showTab = tabNumber;
        }
        if (delayThread == null || !delayThread.isAlive()) {
          delayThread = new Thread(this);
          delayThread.start();
        }
      }
      else {
        showTab = -1;
      }
    }
  }
  
  public void run() {
    while (System.currentTimeMillis() < expirationTime) {
      try {
        Thread.sleep(Math.max(0, expirationTime - System.currentTimeMillis()));
      }
      catch (InterruptedException e) {
      }
    }
    if (showTab >= 0 && showTab < tab.getTabCount()) {
      tab.setSelectedIndex(showTab);
      tab.repaint();
    }
  }
  
  protected void restartDelay() {
    expirationTime = System.currentTimeMillis() + delay;
  }
  
  public void dragEnter(DropTargetDragEvent e) {
    restartDelay();
  }

  public void dropActionChanged(DropTargetDragEvent e) {
  }

  public void drop(DropTargetDropEvent e) {
  }

  public void dragExit(DropTargetEvent e) {
    showTab = -1;
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
