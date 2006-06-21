package chartMap;

import java.awt.Point;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.event.MouseEvent;

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
  protected Thread delayThread;
  protected int delay = 500;
  protected long expirationTime;
  protected int scroll_dist;
  protected int scroll_dx;
  protected int scroll_dy;

  public void scrollAtEdge(Point evtPt, int dist) {
    scroll_dist = dist;
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

    scroll_dx = dx;
    scroll_dy = dy;

    if (dx != 0 || dy != 0) {
      if (delayThread == null || !delayThread.isAlive()) {

        delayThread = new Thread(this);
        delayThread.start();
      }
    }
    else {
      restartDelay();
    }
  }

  protected void restartDelay() {
    expirationTime = System.currentTimeMillis() + delay;
  }

  public void run() {
    while (System.currentTimeMillis() < expirationTime) {
      try {
        Thread.sleep(Math.max(0, expirationTime - System.currentTimeMillis()));
      }
      catch (InterruptedException e) {
      }
    }
    if (scroll_dx != 0 || scroll_dy != 0) {
      scroll(2 * scroll_dist * scroll_dx, 2 * scroll_dist * scroll_dy);
    }
  }

  public void mouseDragged(MouseEvent e) {
    if (!e.isMetaDown()) {
      scrollAtEdge(e.getPoint(), 15);
    }
    else {
      restartDelay();
    }
  }

  /*
   * Start drag delay on entry to map
   */
  public void dragEnter(DropTargetDragEvent e) {
    restartDelay();
  }
  
  /*
   * Cancel final scroll when dragging out of map 
   */
  public void dragExit(DropTargetEvent e) {
    super.dragExit(e);
    scroll_dx = 0;
    scroll_dy = 0;
    repaint();
  }

}
