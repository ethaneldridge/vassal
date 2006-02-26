package VASSAL.build.module;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ContainerEvent;
import java.awt.event.ContainerListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import javax.swing.AbstractButton;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.tools.LaunchButton;

/**
 * Takes buttons from the toolbar of a Map or the main module and places them
 * into a popup menu
 * 
 * @author rkinney
 * 
 */
public class ToolbarMenu extends AbstractConfigurable implements ContainerListener, PropertyChangeListener {
  public static final String BUTTON_TEXT = "text";
  public static final String BUTTON_ICON = "icon";
  public static final String BUTTON_HOTKEY = "hotkey";
  public static final String MENU_ITEMS = "menuItems";
  protected List menuItems = new ArrayList();
  protected java.util.Map buttonsToMenuMap = new HashMap();
  protected LaunchButton launch;
  protected JToolBar toolbar;
  protected JPopupMenu menu;

  public ToolbarMenu() {
    launch = new LaunchButton("Menu", BUTTON_TEXT, BUTTON_HOTKEY, BUTTON_ICON, new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        launch();
      }
    });
  }

  public void launch() {
    menu.show(launch, 0, 0);
  }

  public String[] getAttributeDescriptions() {
    return new String[] {"Button text", "Button Icon", "Hotkey", "Menu Entries"};
  }

  public Class[] getAttributeTypes() {
    return new Class[] {String.class, Icon.class, KeyStroke.class, String[].class};
  }

  public String[] getAttributeNames() {
    return new String[] {BUTTON_TEXT, BUTTON_ICON, BUTTON_HOTKEY, MENU_ITEMS};
  }

  public String getAttributeValueString(String key) {
    if (MENU_ITEMS.equals(key)) {
      return StringArrayConfigurer.arrayToString((String[]) menuItems.toArray(new String[menuItems.size()]));
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }

  public void setAttribute(String key, Object value) {
    if (MENU_ITEMS.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      menuItems = Arrays.asList((String[]) value);
      if (toolbar != null) {
        buildMenu();
      }
    }
    else {
      launch.setAttribute(key, value);
    }
  }

  public void addTo(Buildable parent) {
    if (parent instanceof Map) {
      toolbar = ((Map) parent).getToolBar();
    }
    else if (parent instanceof GameModule) {
      toolbar = ((GameModule) parent).getToolBar();
    }
    toolbar.add(launch);
    toolbar.addContainerListener(this);
    buildMenu();
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public static String getConfigureTypeName() {
    return "Toolbar Menu";
  }

  public void removeFrom(Buildable parent) {
  }

  protected void buildMenu() {
    for (Iterator iter = buttonsToMenuMap.keySet().iterator(); iter.hasNext();) {
      JButton b = (JButton) iter.next();
      b.setVisible(true);
      b.removePropertyChangeListener(this);
    }
    buttonsToMenuMap.clear();
    menu = new JPopupMenu();
    java.util.Map m = new HashMap();
    if (toolbar != null) {
      for (int i = 0, n = toolbar.getComponentCount(); i < n; ++i) {
        if (toolbar.getComponentAtIndex(i) instanceof JButton) {
          JButton b = ((JButton) toolbar.getComponentAtIndex(i));
          m.put(b.getText(), b);
        }
      }
    }
    for (Iterator it = menuItems.iterator(); it.hasNext();) {
      String item = (String) it.next();
      final JButton b = (JButton) m.get(item);
      if (b != null) {
        b.addPropertyChangeListener(this);
        b.setVisible(false);
        JMenuItem mi = new JMenuItem(b.getText(), b.getIcon());
        mi.setEnabled(b.isEnabled());
        mi.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            b.doClick();
          }
        });
        buttonsToMenuMap.put(b, mi);
        menu.add(mi);
      }
    }
  }

  public void componentAdded(ContainerEvent e) {
    buildMenu();
  }

  public void componentRemoved(ContainerEvent e) {
    buildMenu();
  }

  public void propertyChange(PropertyChangeEvent evt) {
    JButton b = (JButton) evt.getSource();
    JMenuItem mi = (JMenuItem) buttonsToMenuMap.get(b);
    if (mi != null) {
      if (AbstractButton.TEXT_CHANGED_PROPERTY.equals(evt.getPropertyName())) {
        mi.setText(b.getText());
      }
      else if ("enabled".equals(evt.getPropertyName())) {
        mi.setEnabled(b.isEnabled());
      }
      else if (AbstractButton.ICON_CHANGED_PROPERTY.equals(evt.getPropertyName())) {
        mi.setIcon(b.getIcon());
      }
    }
  }
}
