/*
 * $Id$
 *
 * Copyright (c) 2000-2005 by Rodney Kinney, Brent Easton, Torsten Spindler
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available 
 * at http://www.opensource.org.
 */
package VASSAL.build.module;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTree;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.DiceButton.IconConfig;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.MenuDisplayer;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.ConfigurerWindow;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.BoundsTracker;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceCloner;
import VASSAL.counters.PieceFilter;
import VASSAL.counters.PieceIterator;
import VASSAL.counters.Properties;
import VASSAL.counters.PropertiesPieceFilter;
import VASSAL.counters.Stack;
import VASSAL.tools.FormattedString;
import VASSAL.tools.LaunchButton;

public class Inventory extends AbstractConfigurable implements GameComponent {
  protected LaunchButton launch;
  // protected ArrayList counters;
  protected CounterInventory results;

  public static final String VERSION = "2.0";
  public static final String HOTKEY = "hotkey";
  public static final String BUTTON_TEXT = "text";
  public static final String NAME = "name";
  public static final String ICON = "icon";
  public static final String DEST = "destination";

  /*
   * For use in formatted text output.
   */
  // public static final String MAP_SEPARATOR = "mapSeparator";
  final protected String mapSeparator = "\n";
  // public static final String GROUP_SEPARATOR = "groupSeparator";
  final protected String groupSeparator = "   ";

  /*
   * Options Destination - Chat, Dialog, File.
   */
  public static final String DEST_CHAT = "Chat Window";
  public static final String DEST_DIALOG = "Dialog Window";
  public static final String DEST_TREE = "Tree Window";
  protected String destination = DEST_CHAT;

  /*
   * Include pieces on named maps Include Offmap pieces? Seperate output by map?
   * Include pieces with marker Sort and break by Marker Total and report
   * integer values in marker. Include location of counter?
   */

  public static final String INCLUDE_ALL_MAPS = "incAllMaps";
  public boolean incAllMaps = true;

  public static final String MAP_LIST = "mapList";
  public String[] mapList = new String[0];

  public static final String ZONE_LIST = "zoneList";
  public String[] zoneList = new String[0];

  public static final String SEPARATE_BY_LOCATION = "separateByLocation";
  protected boolean separateByLocation = false;

  public static final String INCLUDE_OFFMAP = "incOff";

  public static final String FILTER = "include";
  protected String piecePropertiesFilter = "";

  public static final String GROUP_BY = "groupBy";
  protected String[] groupBy = {""};

  public static final String TOTAL = "total";
  protected String nonLeafFormat = "$PropertyName";

  public static final String CENTERONPIECE = "centerOnPiece";
  protected boolean centerOnPiece = false;

  public static final String SIDES = "sides";
  protected String[] sides = null;

  public static final String KEYSTROKE = "keystroke";
  protected KeyStroke keyStroke = null;

  public static final String CUTBELOWROOT = "cutRoot";
  protected int cutBelowRoot = 0;

  public static final String CUTABOVELEAVES = "cutLeaves";
  protected int cutAboveLeaves = 0;

  protected static final boolean sendHotkeyEvents = true;
  protected String pieceFormat = "$PieceName$";

  public Inventory() {
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        generateInventory();
      }
    };
    launch = new LaunchButton(null, BUTTON_TEXT, HOTKEY, ICON, al);
    setAttribute(NAME, "Inventory");
    setAttribute(BUTTON_TEXT, "Inventory");
    launch.setToolTipText(getConfigureName());
    launch.setEnabled(false);
  }

  public static String getConfigureTypeName() {
    return "Inventory v" + VERSION;
  }

  public void addTo(Buildable b) {
    launch.setAlignmentY(0.0F);
    GameModule.getGameModule().getToolBar().add(getComponent());
    GameModule.getGameModule().getGameState().addGameComponent(this);
  }

  protected Component getComponent() {
    return launch;
  }

  public void removeFrom(Buildable b) {
    GameModule.getGameModule().getToolBar().remove(getComponent());
    GameModule.getGameModule().getGameState().removeGameComponent(this);
  }

  public void add(Buildable b) {
  }

  public void remove(Buildable b) {
  }

  protected void generateInventory() {
    buildTree();

    Command c = new DisplayResults(results, destination);
    c.execute();
    GameModule.getGameModule().sendAndLog(c);
  }

  private void buildTree() {
    ArrayList path = new ArrayList();
    for (int i = 0; i < groupBy.length; i++)
      path.add(groupBy[i]);
    results = new CounterInventory(new Counter(this.getConfigureName()), path);

    PieceIterator pi = new PieceIterator(GameModule.getGameModule().getGameState().getPieces(), new Selector(incAllMaps, mapList, piecePropertiesFilter));

    while (pi.hasMoreElements()) {
      ArrayList groups = new ArrayList();
      GamePiece p = pi.nextPiece();

      for (int i = 0; i < groupBy.length; i++) {
        if (groupBy[i].length() > 0) {
          String prop = (String) p.getProperty(groupBy[i]);
          if (prop != null)
            groups.add(p.getProperty(groupBy[i]));
        }
      }

      int count = 1;
      if (nonLeafFormat.length() > 0)
        count = getTotalValue(p);

      Counter c;
      c = new Counter(p, groups, count, pieceFormat);
      // Store
      results.insert(c);
    }
  }

  protected int getTotalValue(GamePiece p) {
    String s = (String) p.getProperty(nonLeafFormat);
    int count = 1;
    try {
      count = Integer.parseInt(s);
    }
    catch (Exception e) {
      count = 1;
    }

    return count;
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public String[] getAttributeDescriptions() {
    return new String[] {"Name", "Button text", "Button icon", "Hotkey", "Report Destination", "Include Counters from All Maps?",
        "Include Counters from these Maps only", // "Separate Output by Map?",
        "Show only pieces matching these properties", "Sort and Group By Marker", "Label for folders", 
        "Center on piece?", "Sides", "Key Command", "Cut tree at depth", "Cut tree above leaves"};
  }

  public Class[] getAttributeTypes() {
    return new Class[] {String.class, String.class, IconConfig.class, KeyStroke.class, Dest.class, Boolean.class, String[].class, // Boolean.class,
        String.class, String[].class, String.class, Boolean.class, String[].class, KeyStroke.class, Integer.class, Integer.class};
  }

  public String[] getAttributeNames() {
    return new String[] {NAME, BUTTON_TEXT, ICON, HOTKEY, DEST, INCLUDE_ALL_MAPS, MAP_LIST, // SEPARATE_BY_MAP,
        FILTER, GROUP_BY, TOTAL, CENTERONPIECE, SIDES, KEYSTROKE, CUTBELOWROOT, CUTABOVELEAVES};
  }

  public void setAttribute(String key, Object o) {

    if (NAME.equals(key)) {
      setConfigureName((String) o);
      launch.setToolTipText((String) o);
    }
    else if (DEST.equals(key)) {
      destination = (String) o;
    }
    else if (INCLUDE_ALL_MAPS.equals(key)) {
      incAllMaps = getBooleanValue(o);
    }
    else if (MAP_LIST.equals(key)) {
      if (o instanceof String) {
        o = StringArrayConfigurer.stringToArray((String) o);
      }
      mapList = (String[]) o;
    }
    else if (FILTER.equals(key)) {
      piecePropertiesFilter = (String) o;
    }
    else if (GROUP_BY.equals(key)) {
      if (o instanceof String) {
        o = StringArrayConfigurer.stringToArray((String) o);
      }
      groupBy = (String[]) o;
    }
    else if (TOTAL.equals(key)) {
      nonLeafFormat = (String) o;
    }
    else if (CENTERONPIECE.equals(key)) {
      centerOnPiece = getBooleanValue(o);
    }
    else if (SIDES.equals(key)) {
      if (o instanceof String) {
        o = StringArrayConfigurer.stringToArray((String) o);
      }
      sides = (String[]) o;
    }
    else if (KEYSTROKE.equals(key)) {
      if (o instanceof String) {
        o = HotKeyConfigurer.decode((String) o);
      }
      keyStroke = (KeyStroke) o;
    }
    else if (CUTBELOWROOT.equals(key)) {
      if (o instanceof String)
        cutBelowRoot = Integer.parseInt((String) o);
      else {
        cutBelowRoot = ((Integer) o).intValue();
      }
    }
    else if (CUTABOVELEAVES.equals(key)) {
      if (o instanceof String)
        cutAboveLeaves = Integer.parseInt((String) o);
      else {
        cutAboveLeaves = ((Integer) o).intValue();
      }
    }

    else {
      launch.setAttribute(key, o);
    }
  }

  /**
   * @param o
   */
  protected boolean getBooleanValue(Object o) {
    if (o instanceof String) {
      o = new Boolean((String) o);
    }
    return ((Boolean) o).booleanValue();
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (DEST.equals(key)) {
      return destination;
    }
    else if (INCLUDE_ALL_MAPS.equals(key)) {
      return incAllMaps + "";
    }
    else if (MAP_LIST.equals(key)) {
      return StringArrayConfigurer.arrayToString(mapList);
    }
    else if (FILTER.equals(key)) {
      return piecePropertiesFilter;
    }
    else if (GROUP_BY.equals(key)) {
      return StringArrayConfigurer.arrayToString(groupBy);
    }
    else if (TOTAL.equals(key)) {
      return nonLeafFormat;
    }
    else if (CENTERONPIECE.equals(key)) {
      return centerOnPiece + "";
    }
    else if (SIDES.equals(key)) {
      return StringArrayConfigurer.arrayToString(sides);
    }
    else if (KEYSTROKE.equals(key)) {
      return HotKeyConfigurer.encode(keyStroke);
    }
    else if (CUTBELOWROOT.equals(key)) {
      return cutBelowRoot + "";
    }
    else if (CUTABOVELEAVES.equals(key)) {
      return cutAboveLeaves + "";
    }

    else {
      return launch.getAttributeValueString(key);
    }
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (MAP_LIST.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return !incAllMaps;
        }
      };
    }
    else {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return true;
        }
      };
    }
  }

  public Command getRestoreCommand() {
    return null;
  }

  public void setup(boolean gameStarting) {
    launch.setEnabled(gameStarting && enabledForPlayersSide());
  }

  protected boolean enabledForPlayersSide() {
    if (sides == null || sides.length == 0)
      return true;
    for (int i = 0; i < sides.length; i++) {
      if (sides[i].equalsIgnoreCase(PlayerRoster.getMySide()))
        return true;
    }
    return false;
  }

  public class DisplayResults extends Command {

    CounterInventory results;
    String destination;
    JTree tree;

    public DisplayResults(CounterInventory results, String destination) {
      this.results = results;
      this.destination = destination;
    }

    protected void executeCommand() {
      if (destination.equals(DEST_CHAT)) {
        Command c = new NullCommand();
        String res[] = results.getResultStringArray();

        for (int i = 0; i < res.length; i++) {
          c.append(new Chatter.DisplayText(GameModule.getGameModule().getChatter(), res[i]));
        }
        c.execute();
        GameModule.getGameModule().sendAndLog(c);
      }
      else if (destination.equals(DEST_DIALOG)) {
        String res[] = results.getResultStringArray();
        String text = "";
        for (int i = 0; i < res.length; i++) {
          text += res[i] + "\n";
        }
        JTextArea textArea = new JTextArea(text);
        textArea.setEditable(false);

        JScrollPane scrollPane = new JScrollPane(textArea, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        JOptionPane.showMessageDialog(GameModule.getGameModule().getFrame(), scrollPane, getConfigureName(), JOptionPane.PLAIN_MESSAGE);
      }
      else if (destination.equals(DEST_TREE)) {
        showTree();
      }
    }

    /**
     * Construct an explorer like interface for the selected counters
     */
    protected void showTree() {
      // Initialize the tree to be displayed from the results tree
      tree = new JTree(results); // .getRoot());
      tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
      // tree.getSelectionModel().setSelectionMode(TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);
      // If wanted center on a selected counter
      if (centerOnPiece) {
        tree.addTreeSelectionListener(new TreeSelectionListener() {
          public void valueChanged(TreeSelectionEvent e) {
            GamePiece piece = getSelectedCounter();
            if (piece != null)
              piece.getMap().centerAt(piece.getPosition());
          }
        });
      }

      tree.addMouseListener(new MouseAdapter() {

        public void mouseReleased(MouseEvent e) {
          if (e.isMetaDown()) {
            final TreePath path = tree.getPathForLocation(e.getX(), e.getY());
            if (path != null) {
              if (path.getLastPathComponent() instanceof CounterNode) {
                final CounterNode node = (CounterNode) path.getLastPathComponent();
                final GamePiece p = node.getCounter().getPiece();
                JPopupMenu menu = MenuDisplayer.createPopup(p);
                menu.addPropertyChangeListener("visible",new PropertyChangeListener() {
                  public void propertyChange(PropertyChangeEvent evt) {
                    if (Boolean.FALSE.equals(evt.getNewValue())) {
                      p.setProperty("oldMap",p.getMap());
                      SwingUtilities.invokeLater(new Runnable() {
                        public void run() {
                          updateTree(p);
                        }
                      });
                    }
                  }
                });
                menu.show(tree, e.getX(), e.getY());
              }
            }
          }
        }
      });
      // Not working at the moment
      // How to get from keycode to keystrokes?
      // disabled by sendHotkeyEvents
      if (sendHotkeyEvents) {
        tree.addKeyListener(new HotKeySender());
      }

      JScrollPane scrollPane = new JScrollPane(tree, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

      int choice = getOptionsDialog(scrollPane);

      Command c = new NullCommand();
      if (choice == JOptionPane.YES_OPTION) {
        // Send the hotKey to the involved pieces
        c.append(sendHotKeyToPieces(keyStroke));
      }
      else if (choice == JOptionPane.NO_OPTION) {
        // c.append(new
        // Chatter.DisplayText(GameModule.getGameModule().getChatter(),
        // "Cancelled."));
      }
      else if (choice == JOptionPane.CANCEL_OPTION) {
        // This is not working at all :(
        if (groupBy == null || groupBy.length == 0) {
          JOptionPane.showMessageDialog(GameModule.getGameModule().getFrame(),
              "Sorry, no tree layout can be defined, as there are neither markers nor map or zones defined.", "Custom Tree Layout Warning",
              JOptionPane.WARNING_MESSAGE);
        }
        else {
          // c.append(new
          // Chatter.DisplayText(GameModule.getGameModule().getChatter(),
          // "Customize tree layout"));
          // SortedListConfigurer configurer = new
          // SortedListConfigurer(groupBy);
          // groupBy = configurer.showList();
          StringArrayConfigurer config = new StringArrayConfigurer(null, "Order", groupBy);
          ConfigurerWindow w = new ConfigurerWindow(config);
          w.setLocationRelativeTo(GameModule.getGameModule().getFrame());
          w.setVisible(true);
          groupBy = config.getStringArray();
        }
      }
      else {
        // Probably the window has been destroyed
        // do nothing.
      }
      c.execute();
      GameModule.getGameModule().sendAndLog(c);

    }

    /**
     * Chose the right option dialog. If there is no hotkey defined, only allow
     * ok. If there is no groupBy (path) defined, present Apply and Cancel. Else
     * add show complete Options.
     * 
     * @param pane
     *          contains the tree output
     * @return the choice made
     */
    protected int getOptionsDialog(JScrollPane pane) {
      final String[] options;
      final int choice;
      if ((groupBy == null || groupBy.length == 0) && (keyStroke == null)) {
        options = new String[] {"Ok"};
        choice = JOptionPane.showOptionDialog(GameModule.getGameModule().getFrame(), pane, getConfigureName(), JOptionPane.YES_OPTION,
            JOptionPane.PLAIN_MESSAGE, null, options, options[0]);
      }
      else if (groupBy == null || groupBy.length == 0) {
        // TODO: Doesn't work?
        options = new String[] {"Apply", "Cancel"};
        choice = JOptionPane.showOptionDialog(GameModule.getGameModule().getFrame(), pane, getConfigureName(), JOptionPane.YES_NO_CANCEL_OPTION,
            JOptionPane.PLAIN_MESSAGE, null, options, options[0]);
      }
      else {
        options = new String[] {"Apply", "Cancel", "Customize"};
        choice = JOptionPane.showOptionDialog(GameModule.getGameModule().getFrame(), pane, getConfigureName(), JOptionPane.YES_NO_CANCEL_OPTION,
            JOptionPane.PLAIN_MESSAGE, null, options, options[0]);
      }
      return choice;
    }

    /**
     * @return Command which only has some text in. The actual processing is
     *         done within the pieces.
     */
    protected Command sendHotKeyToPieces(final KeyStroke keyStroke) {
      Command c = new NullCommand();
      final TreePath[] tp = tree.getSelectionPaths();
      // set to not get duplicates
      HashSet pieces = new HashSet();
      for (int i = 0; i < tp.length; i++) {
        CounterNode node = (CounterNode) tp[i].getLastPathComponent();
        if (node.isLeaf()) {
          pieces.add(node.getCounter().getPiece());
        }
        else {
          for (Iterator j = node.iterator(); j.hasNext();) {
            CounterNode childNode = (CounterNode) j.next();
            if (childNode.isLeaf())
              pieces.add(childNode.getCounter().getPiece());
          }
        }
      }
      for (Iterator i = pieces.iterator(); i.hasNext();) {
        GamePiece piece = (GamePiece) i.next();
        GameModule.getGameModule().sendAndLog(piece.keyEvent(keyStroke));
      }
      return c;
    }

    protected Command myUndoCommand() {
      return null;
    }

    public GamePiece getSelectedCounter() {
      GamePiece piece = null;
      CounterNode node = (CounterNode) tree.getLastSelectedPathComponent();
      if (node != null && node.isLeaf()) {
        piece = node.getCounter().getPiece();
      }
      return piece;
    }

    protected void updateTree(final GamePiece newPiece) {
      boolean structureChanged = true;
      GamePiece oldPiece = (GamePiece) newPiece.getProperty(Properties.SNAPSHOT);
      VASSAL.build.module.Map oldMap = (VASSAL.build.module.Map)newPiece.getProperty("oldMap"); 
      if (oldPiece != null) {
        if (oldMap == null ?  newPiece.getMap() == null : oldMap.equals(newPiece.getMap())) {
          int i=0;
          for (i = 0; i < groupBy.length; i++) {
            Object oldValue = oldPiece.getProperty(groupBy[i]);
            Object newValue = newPiece.getProperty(groupBy[i]);
            if (oldValue == null ? newValue != null : !oldValue.equals(newValue)) {
              break; 
            }
          }
          if (i == groupBy.length) {
            structureChanged = false;
          }
        }
      }
      if (structureChanged) {
        buildTree();
        results = Inventory.this.results;
        tree.setModel(results);
      }
    }

    public class HotKeySender implements KeyListener {

      public void keyCommand(KeyStroke stroke) {
        GamePiece p = getSelectedCounter();
        if (p != null) {
          // Save state first
          p.setProperty(Properties.SNAPSHOT, PieceCloner.getInstance().clonePiece(p));
          p.setProperty("oldMap",p.getMap());
          BoundsTracker t = new BoundsTracker();
          t.addPiece(p);
          Command comm = p.keyEvent(stroke);
          if (comm != null) {
            t.repaint();
            GameModule.getGameModule().sendAndLog(comm);
            updateTree(p);
          }
        }
      }

      public void keyPressed(KeyEvent e) {
        keyCommand(KeyStroke.getKeyStrokeForEvent(e));
      }

      public void keyReleased(KeyEvent e) {
        keyCommand(KeyStroke.getKeyStrokeForEvent(e));
      }

      public void keyTyped(KeyEvent e) {
        keyCommand(KeyStroke.getKeyStrokeForEvent(e));
      }

    }

  }

  public static class Dest extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] {DEST_CHAT, DEST_DIALOG, DEST_TREE};
    }
  }

  /**
   * Holds static information of and a reference to a gamepiece. Pay attention
   * to the equals method. It checks if two pieces can be found under the same
   * path!
   * 
   * @author Brent Easton and Torsten Spindler
   * 
   */
  public class Counter implements PropertySource {
    // The gamepiece is stored here to allow dynamic changes of name, location
    // and so forth
    protected GamePiece piece;
    protected ArrayList groups;
    protected int value;
    // Only used when no piece is defined
    protected String localName;
    protected FormattedString format;
    protected CounterNode node;

    public Counter(String name) {
      this(null, null, 0, nonLeafFormat);
      this.localName = name;
    }

    public Counter(GamePiece piece, ArrayList groups, int value, String format) {
      this.piece = piece;
      this.value = value;
      this.groups = groups;
      this.format = new FormattedString(format);
    }

    // piece can be null, so provide a alternate name
    public String getName() {
      if (piece != null)
        return piece.getName();
      return localName;
    }

    public int hashCode() {
      return getName().hashCode();
    }

    public String toString() {
      return format.getText(this);
    }

    public String[] getPath() {
      String[] retString = new String[groups.size()];
      for (int i = 0; i < groups.size(); i++)
        retString[i] = (String) groups.get(i);
      return retString;
    }

    public int getValue() {
      return value;
    }

    public void setValue(int value) {
      this.value = value;
    }

    public GamePiece getPiece() {
      return piece;
    }

    public void setPiece(GamePiece piece) {
      this.piece = piece;
    }

    public boolean equals(Object o) {
      if (!(o instanceof Counter))
        return false;
      Counter c = (Counter) o;
      return getPath().equals(c.getPath());
    }

    public Object getProperty(Object key) {
      Object value = null;
      String s = (String) key;
      if (s.startsWith("sum_")) {
        if (piece != null) {
          value = piece.getProperty(s.substring(4));
        }
        else {
          int sum = 0;
          int n = results.getChildCount(node);
          for (int i=0;i<n;++i) {
            try {
              CounterNode childNode = (CounterNode) results.getChild(node, i);
              sum += Integer.parseInt((String) (childNode.getCounter()).getProperty(key));
            }
            catch (NumberFormatException e) {
              sum++;
            }
          }
          value = String.valueOf(sum);
        }
      }
      else if ("PropertyName".equals(s)) {
        return localName;
      }
      else if (piece != null) {
        value = piece.getProperty(key);
      }
      return value;
    }
    public void setNode(CounterNode node) {
      this.node = node;
    }

  }

  /**
   * Filter to select pieces required
   * 
   * @author Brent Easton
   */
  protected class Selector implements PieceFilter {

    protected String mapList[];
    protected boolean allMaps;
    protected PieceFilter filter;

    public Selector(boolean all, String[] maps, String include) {
      allMaps = all;
      mapList = maps;
      if (include != null && include.length() > 0) {
        filter = PropertiesPieceFilter.parse(include);
      }
    }

    public boolean accept(GamePiece piece) {
      // Honor visibility

      if (Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME)))
        return false;

      // Ignore Stacks, pieces are reported individually from GameState
      if (piece instanceof Stack)
        return false;

      // Don't report pieces with no map
      if (piece.getMap() == null)
        return false;

      // If a list of Maps supplied, check map names match
      if (!allMaps && mapList != null && mapList.length > 0) {
        String mapName = piece.getMap().getMapName();
        boolean found = false;
        for (int i = 0; i < mapList.length && !found; i++) {
          found = mapName.equals(mapList[i]);
        }
        if (!found)
          return false;
      }

      // Check for marker
      if (filter != null) {
        return filter.accept(piece);
      }

      // Default Accept piece
      return true;
    }

  }

  /**
   * CounterNode for the result tree.
   * 
   * @author spindler
   * 
   */
  public class CounterNode {
    protected final String entry;
    protected final Counter counter;
    protected List children;
    protected int level;
    // protected int depth;

    public CounterNode(final String entry, final Counter counter, final int level) {
      this(entry, counter);
      this.level = level;
    }

    protected CounterNode(String entry, Counter counter) {
      this.level = 0;
      // this.depth = 0;
      this.entry = entry;
      this.counter = counter;
      counter.setNode(this);
      children = new ArrayList();
    }

    public String toString() {
      if (counter != null)
        return counter.toString();
      return getEntry();
    }

    protected String separator() {
      StringBuffer sep = new StringBuffer();
      if (children.isEmpty()) {
        sep.append(groupSeparator);
      }
      else {
        sep.append(mapSeparator);
        for (int i = 0; i < getLevel(); i++)
          sep.append(groupSeparator);
      }
      return sep.toString();
    }

    public String toResultString() {
      StringBuffer name = new StringBuffer();

      name.append(separator());

      if (counter != null)
        name.append(counter.toString());
      else
        name.append(getEntry());

      // if (includeChildrenInResult() == true) {
      for (Iterator i = children.iterator(); i.hasNext();) {
        CounterNode child = (CounterNode) i.next();
        name.append(child.toResultString());
      }
      // }
      return name.toString();
    }

    // private boolean includeChildrenInResult() {
    // if (cutAboveLeaves > 0 && getDepth() <= cutAboveLeaves)
    // return false;
    // if (cutBelowRoot > 0 && getLevel() >= cutBelowRoot)
    // return false;
    // return true;
    // }

    // protected int getDepth() {
    // return calcDepth();
    // }
    // protected int calcDepth() {
    // int newDepth = 0;
    // for (Iterator i = children.iterator(); i.hasNext();) {
    // CounterNode child = (CounterNode) i.next();
    // if (newDepth < child.getDepth()+1)
    // newDepth = child.getDepth()+1;
    // }
    // // System.out.println(entry + " is now at depth " + newDepth);
    // return newDepth;
    // }
    public String getEntry() {
      return entry;
    }

    public Counter getCounter() {
      return counter;
    }

    public void addChild(final CounterNode counterNode) {
      children.add(counterNode);
    }

    public void addChild(final int i, final CounterNode counterNode) {
      children.add(i, counterNode);
    }

    public void removeChild(CounterNode child) {
      children.remove(child);
    }

    public int getChildCount() {
      return children.size();
    }

    public boolean isLeaf() {
      return children.isEmpty();
    }

    public Object getChild(final int index) {
      return children.get(index);
    }

    public int getIndexOfChild(final Object child) {
      return children.indexOf(child);
    }

    public int getLevel() {
      return level;
    }

    public void setLevel(final int level) {
      this.level = level;
    }

    public int updateValues() {
      int value = 0;
      // TODO if counter has a value, should it be added to the total or
      // replaced
      // by then children values?
      // get old value, not sure if this useful?
      if (counter != null)
        value = counter.getValue();

      // inform children about update
      for (Iterator i = children.iterator(); i.hasNext();) {
        CounterNode child = (CounterNode) i.next();
        // System.out.println("In " + getEntry() + " calling " +
        // child.getEntry());
        value += child.updateValues();
      }

      // save new value in counter
      counter.setValue(value);
      return counter.getValue();
    }

    public Iterator iterator() {
      return children.iterator();
    }

    public void cutLevel(int cut) {
      if (cut == 0) {
        children.clear();
        return;
      }
      for (Iterator i = children.iterator(); i.hasNext();) {
        CounterNode child = (CounterNode) i.next();
        child.cutLevel(cut - 1);
      }
    }

    public void cutLeaves() {
      List toBeRemoved = new ArrayList();
      for (Iterator i = children.iterator(); i.hasNext();) {
        CounterNode child = (CounterNode) i.next();
        if (child.isLeaf()) {
          toBeRemoved.add(child);
          // System.out.println("Removing " + child.getEntry());
        }
        else
          child.cutLeaves();
      }
      for (Iterator i = toBeRemoved.iterator(); i.hasNext();) {
        CounterNode removeMe = (CounterNode) i.next();
        // System.out.println("Removing " + removeMe.getEntry());
        children.remove(removeMe);
      }
    }

  }

  public class CounterInventory implements TreeModel {
    // Needed for TreeModel
    private Vector treeModelListeners = new Vector();
    // This contains shortcuts to the nodes of the tree
    protected Map inventory;
    // The start of the tree
    protected CounterNode root;
    // Text view of the tree
    protected String resultString;
    // The path determines where a counter is found in the tree
    protected ArrayList path;
    // Small speed up, only update values in tree when something has changed
    // TODO is this a good idea? What happens when a command on the piece
    // changes the value?
    // this only works single threaded !! Test for online playing when one
    // player alters the game.
    protected boolean changed;

    public CounterInventory(Counter c, ArrayList path) {
      this.root = new CounterNode(c.getName(), c);
      this.path = path;
      // TODO preload hash with the number of counters in the game?
      this.inventory = new HashMap();
      changed = true;
    }

    /**
     * insert counter into the tree. It is not sorted in any way.
     * 
     * @param counter
     */
    public void insert(Counter counter) {
      String[] path = counter.getPath();
      StringBuffer hash = new StringBuffer();

      CounterNode insertNode = root;
      CounterNode newNode = null;
      for (int j = 0; path != null && j < path.length; j++) {
        hash.append(path[j]);
        if (inventory.get(hash.toString()) == null) {
          newNode = new CounterNode(path[j], new Counter(path[j]), insertNode.getLevel() + 1);
          inventory.put(hash.toString(), newNode);
          insertNode.addChild(newNode);
        }
        insertNode = (CounterNode) inventory.get(hash.toString());
      }
      newNode = new CounterNode(counter.toString(), counter, insertNode.getLevel() + 1);
      insertNode.addChild(newNode);
      changed = true;
    }

    private void updateEntries() {
      root.updateValues();
    }

    /**
     * Helping method to obtain a text based representation of the data stored.
     * There is no total calculated for all counters on all maps, as there is no
     * entry in inventory for this.
     */
    // protected void generateTextResult() {
    // StringBuffer resultBuffer = new StringBuffer(); // name + mapSeparator);
    //			
    // if (showTotal)
    // updateEntries();
    // resultBuffer.append(root.toResultString());
    // resultString = resultBuffer.toString();
    // }
    /**
     * Deliver information of the tree as text.
     * 
     * @return String
     */
    public String getResultString() {
      if (changed)
        updateTree();
      // generateTextResult();
      changed = false;
      return root.toResultString();
    }

    /**
     * Compatibility for DisplayResults class.
     * 
     * @return String[]
     */
    public String[] getResultStringArray() {
      return new String[] {getResultString()};
    }

    public Object getRoot() {
      if (changed)
        updateTree();
      return root;
    }

    /**
     * 
     */
    private void updateTree() {
      updateEntries();
      if (cutBelowRoot > 0)
        root.cutLevel(cutBelowRoot);
      for (int i = cutAboveLeaves; i > 0; i--)
        root.cutLeaves();
      changed = false;
    }

    public int getChildCount(Object parent) {
      CounterNode counter = (CounterNode) parent;
      return counter.getChildCount();
    }

    public boolean isLeaf(Object node) {
      CounterNode counter = (CounterNode) node;
      return counter.isLeaf();
    }

    public void addTreeModelListener(TreeModelListener l) {
      treeModelListeners.add(l);
    }

    public void removeTreeModelListener(TreeModelListener l) {
      treeModelListeners.remove(l);
    }

    public void fireNodesRemoved(Object[] path, int[] childIndices, Object[] children) {
      TreeModelEvent e = new TreeModelEvent(this, path, childIndices, children);
      for (Enumeration en = treeModelListeners.elements(); en.hasMoreElements();) {
        ((TreeModelListener) en.nextElement()).treeNodesRemoved(e);
      }

    }

    public Object getChild(Object parent, int index) {
      CounterNode counter = (CounterNode) parent;
      return counter.getChild(index);
    }

    public int getIndexOfChild(Object parent, Object child) {
      CounterNode counter = (CounterNode) parent;
      return counter.getIndexOfChild(child);
    }

    public void valueForPathChanged(TreePath path, Object newValue) {
      throw new RuntimeException("No idea what to do!");

    }
  }
}
