package PB;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Enumeration;

import javax.swing.JButton;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;

import org.w3c.dom.Element;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.command.RemovePiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Stack;

/*
 * Created on Oct 17, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */

/**
 * @author Brent Easton
 *
 * Create a menu of MassKeyRemover commands
 */
public class PBMassKeyMenu extends AbstractConfigurable implements ActionListener {

  protected JButton launch;
  protected KeyStroke stroke = KeyStroke.getKeyStroke(0, 0);
  protected Map map;
  protected JPopupMenu popup;
  protected String buttonText = "Remove";

  public static final String NAME = "name";
  public static final String BUTTON_TEXT = "buttonText";

  public static final String ALL_MARKERS = "All";
  public static final String FIRED = "Fired";
  public static final String DISPERSED = "Dispersed";
  public static final String CAT = "CAT";
  public static final String OPP_FIRE = "Opportunity Fire";

  public static final String ALL = "All";

  protected static final String TRUE = "true";
  protected static final boolean DO_REPORT = true;
  protected static final boolean SUPPRESS_REPORT = false;

  protected static final String COUNTRY_PROPERTY = "country";
  protected static final String DISPERSED_PROPERTY = "CanDisperse";
  protected static final String FIRED_PROPERTY = "CanFire";

  protected static final char DISPERSED_KEY = 'D';
  protected static final char FIRED_KEY = 'F';
  public PBMassKeyMenu() {

    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        doMenu();
      }
    };

    launch = new JButton(buttonText);
    launch.setToolTipText("Remove all markers of a given type");
    launch.setAlignmentY(0.0F);
    launch.addActionListener(al);
  }

  public void build(Element e) {
    super.build(e);

    popup = new JPopupMenu();

    popup.addPopupMenuListener(new javax.swing.event.PopupMenuListener() {
      public void popupMenuCanceled(javax.swing.event.PopupMenuEvent evt) {
        map.getView().repaint();
      }

      public void popupMenuWillBecomeInvisible(javax.swing.event.PopupMenuEvent evt) {
        map.getView().repaint();
      }

      public void popupMenuWillBecomeVisible(javax.swing.event.PopupMenuEvent evt) {
      }
    });

    JMenu allMenu = new JMenu(ALL_MARKERS + " Markers (Fired, Dispersed, CAT, Opp Fire)");
    addItem(allMenu, ALL_MARKERS, PB.ALLIED);
    addItem(allMenu, ALL_MARKERS, PB.GERMAN);
    addItem(allMenu, ALL_MARKERS, PB.RUSSIAN);
    addItem(allMenu, ALL_MARKERS, PB.ARAB);
    addItem(allMenu, ALL_MARKERS, PB.ISRAELI);
    addItem(allMenu, ALL_MARKERS, ALL);
    popup.add(allMenu);

    JMenuItem catItem = new JMenuItem(CAT);
    catItem.addActionListener(this);
    popup.add(catItem);
    
    JMenu fireMenu = new JMenu(FIRED + " Markers");
    addItem(fireMenu, FIRED, PB.ALLIED);
    addItem(fireMenu, FIRED, PB.GERMAN);
    addItem(fireMenu, FIRED, PB.RUSSIAN);
    addItem(fireMenu, FIRED, PB.ARAB);
    addItem(fireMenu, FIRED, PB.ISRAELI);
    addItem(fireMenu, FIRED, ALL);
    popup.add(fireMenu);

    JMenu disperseMenu = new JMenu(DISPERSED + " Markers");
    addItem(disperseMenu, DISPERSED, PB.ALLIED);
    addItem(disperseMenu, DISPERSED, PB.GERMAN);
    addItem(disperseMenu, DISPERSED, PB.RUSSIAN);
    addItem(disperseMenu, DISPERSED, PB.ARAB);
    addItem(disperseMenu, DISPERSED, PB.ISRAELI);
    addItem(disperseMenu, DISPERSED, ALL);
    popup.add(disperseMenu);

    JMenu oppMenu = new JMenu(OPP_FIRE + " Markers");
    addItem(oppMenu, OPP_FIRE, PB.ALLIED);
    addItem(oppMenu, OPP_FIRE, PB.GERMAN);
    addItem(oppMenu, OPP_FIRE, PB.RUSSIAN);
    addItem(oppMenu, OPP_FIRE, PB.ARAB);
    addItem(oppMenu, OPP_FIRE, PB.ISRAELI);
    addItem(oppMenu, OPP_FIRE, ALL);
    popup.add(oppMenu);

  }

  private void addItem(JMenu menu, String name, String action) {
    JMenuItem item = new JMenuItem(action);
    item.addActionListener(this);
    item.setName(name);
    menu.add(item);
  }

  protected void doMenu() {
    popup.show(launch, 0, 0);

  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Description", "Button text" };
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, BUTTON_TEXT };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, String.class };
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (BUTTON_TEXT.equals(key)) {
      return buttonText;
    }
    else
      return null;
  }

  public static String getConfigureTypeName() {
    return "Global Key Menu";
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public void removeFrom(Buildable parent) {
    map.getToolBar().remove(launch);
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (BUTTON_TEXT.equals(key)) {
      buttonText = (String) value;
    }

  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void addTo(Buildable parent) {
    map = (Map) parent;
    map.getToolBar().add(launch);

  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed(ActionEvent a) {

    String action = a.getActionCommand();
    Command comm = new NullCommand();

    if (action.equals(CAT)) {
      comm.append(doMassRemove("", CAT, null, null, true));
    }
    else {
      String subAction = ((JMenuItem) a.getSource()).getName();
      if (subAction.equals(FIRED)) {
        if (action.equals(ALL)) {
          comm.append(doMassKey(action, subAction, FIRED_PROPERTY, TRUE, FIRED_KEY, DO_REPORT));
        }
        else {
          comm.append(doMassKey(action, subAction, COUNTRY_PROPERTY, PB.getPrefix(action), FIRED_KEY, DO_REPORT));
        }
      }
      else if (subAction.equals(DISPERSED)) {
        if (action.equals(ALL)) {
          comm.append(doMassKey(action, subAction, DISPERSED_PROPERTY, TRUE, DISPERSED_KEY, DO_REPORT));
        }
        else {
          comm.append(doMassKey(action, subAction, COUNTRY_PROPERTY, PB.getPrefix(action), DISPERSED_KEY, DO_REPORT));
        }
      }
      else if (subAction.equals(OPP_FIRE)) {
        if (action.equals(ALL)) {
          comm.append(doMassRemove(action, OPP_FIRE, null, null, DO_REPORT));
        }
        else {
          comm.append(doMassRemove(action, OPP_FIRE, COUNTRY_PROPERTY, PB.getPrefix(action), DO_REPORT));
        }
  
      }
      else if (subAction.equals(ALL_MARKERS)) {
        
        String text = "* Remove All " + (action.equals(ALL) ? "" : action + " ") + "Markers";
        comm.append(new Chatter.DisplayText(GameModule.getGameModule().getChatter(), text));
        
        if (action.equals(ALL)) {
          comm.append(doMassRemove(action, CAT, null, null, SUPPRESS_REPORT));
          comm.append(doMassRemove(action, OPP_FIRE, null, null, SUPPRESS_REPORT));
          comm.append(doMassKey(action, subAction, FIRED_PROPERTY, TRUE, FIRED_KEY, SUPPRESS_REPORT));
          comm.append(doMassKey(action, subAction, DISPERSED_PROPERTY, TRUE, DISPERSED_KEY, SUPPRESS_REPORT));          
        }
        else {
          comm.append(doAllCountry(action, subAction, PB.getPrefix(action)));
        }
      }
    }
    comm.execute();
    GameModule.getGameModule().sendAndLog(comm);
  }

  private Command doAllCountry(String action, String subAction, String val) {
    Command comm = doMassRemove(action, CAT, null, null, SUPPRESS_REPORT);
    comm.append(doMassRemove(action, OPP_FIRE, COUNTRY_PROPERTY, val, SUPPRESS_REPORT));
    comm.append(doMassKey(action, subAction, COUNTRY_PROPERTY, val, DISPERSED_KEY, SUPPRESS_REPORT));
    comm.append(doMassKey(action, subAction, COUNTRY_PROPERTY, val, FIRED_KEY, SUPPRESS_REPORT));
    return comm;
  }
  
  private Command doMassKey(String action, String type, String property, String value, char key, boolean isReportable) {

    String description = " Remove " + action + " " + type + " Markers";
    PBMassKey massKey = new PBMassKey(property, value, key, description, map, isReportable);
    return massKey.apply();

  }

  private Command doMassRemove(String action, String counterName, String matchProperty, String matchName, boolean doMessage) {

    Command comm;
    
    if (doMessage) {
      comm =
        new Chatter.DisplayText(GameModule.getGameModule().getChatter(), "* Remove " + action + " " + counterName + " Markers");
    }
    else {
      comm = new NullCommand();
    }
    comm = comm.append(removePiecesWithName(counterName, matchProperty, matchName));
    
    return comm;
  }

  public Command removePiecesWithName(String name, String matchProperty, String matchValue) {

    Command comm = new NullCommand();
    GamePiece piece[] = map.getPieces();

    for (int i = 0; i < piece.length; ++i) {
      if (matchProperty == null || isPropertyMatch(piece[i], matchProperty, matchValue)) {
        if (piece[i] instanceof Stack) {
          for (Enumeration e = ((Stack) piece[i]).getPieces(); e.hasMoreElements();) {
            GamePiece child = (GamePiece) e.nextElement();
            if (isMatch(child, name)) {
              comm = comm.append(new RemovePiece(child));
            }
          }
        }
        else if (isMatch(piece[i], name)) {
          comm = comm.append(new RemovePiece(piece[i]));
        }
      }
    }
    return comm;
  }

  /*
   * Return true if any piece in this stack has a property/value combination
   * we are interested in
   */
  protected boolean isPropertyMatch(GamePiece p, String matchProperty, String matchValue) {
    if (p instanceof Stack) {
      for (Enumeration e = ((Stack) p).getPieces(); e.hasMoreElements();) {
        if (isPropertyMatch((GamePiece) e.nextElement(), matchProperty, matchValue)) {
          return true;
        }
      }
    }
    else {
      return (matchValue.equals(p.getProperty(matchProperty)));
    }
    return false;
  }

  protected boolean isMatch(GamePiece p, String name) {
    return p.getName().equals(name);
  }

}
