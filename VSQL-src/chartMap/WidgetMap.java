package chartMap;

import java.awt.Point;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import VASSAL.build.module.Map;
import VASSAL.configure.VisibilityCondition;

public class WidgetMap extends Map implements Runnable {

  public WidgetMap() {
    super();
  }

  /*
   * Minimal setup - remove all docking and toolbar setup
   */
  public void setup(boolean show) {
    if (show) {
      toolBar.setVisible(true);
      theMap.revalidate();
    }
    else {
      pieces.clear();
      boards.removeAllElements();
      toolBar.setVisible(false);
      System.gc();
    }
  }

  /*
   * Hide options relating to toolbar buttons
   */
  public VisibilityCondition getAttributeVisibility(String name) {
    if (USE_LAUNCH_BUTTON.equals(name) || BUTTON_NAME.equals(name) || ICON.equals(name) || HOTKEY.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return false;
        }
      };
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  public JComponent getView() {
    return super.getView();
  }

  /*
   * Make the scroll pane accessible to the widget
   */
  public JScrollPane getScroll() {
    return scroll;
  }

  /*
   * Delay 500ms before starting scroll
   */
  public void scrollAtEdge(Point evtPt, int dist) {
    Point p = new Point(evtPt.x - scroll.getViewport().getViewPosition().x, evtPt.y - scroll.getViewport().getViewPosition().y);
    int dx = 0, dy = 0;
    if (p.x < dist && p.x >= 0)
      dx = -1;
    if (p.x >= scroll.getViewport().getSize().width - dist && p.x < scroll.getViewport().getSize().width)
      dx = 1;
    if (p.y < dist && p.y >= 0)
      dy = -1;
    if (p.y >= scroll.getViewport().getSize().height - dist && p.y < scroll.getViewport().getSize().height)
      dy = 1;

    if (dx != 0 || dy != 0) {
      scroll(2 * dist * dx, 2 * dist * dy);
    }
  }

  public void run() {
    // TODO Auto-generated method stub

  }

}
