/*
 * $Id$
 *
 * Copyright (c) 2004 by Michael Blumohr
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

import VASSAL.build.*;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.*;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.SequenceEncoder;
import VASSAL.command.*;

import javax.swing.*;
import java.awt.*;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;
import java.net.MalformedURLException;
import java.util.*;


/**
 * ...
 */
public class SpecialDiceButton extends AbstractConfigurable implements CommandEncoder {
  protected java.util.Random ran;
  protected boolean bNumeric = false;
  protected boolean bReportTotal = false;
  protected boolean bResultToChatter = true;
  protected boolean bResultInWindow = false;
  protected boolean bResultInButton = false;
  protected int nWinX = 100;
  protected int nWinY = 100;
  private LaunchButton launch;
  protected String id;
  protected String sMapName;

  private Dice[] oaDice;  // DiceSet as objects
  private SpecialDiceDialog oDialog = null; // Dialog to show results graphical

  public static final String BUTTON_TEXT = "text";
  public static final String NAME = "name";
  public static final String ICON = "icon";
  public static final String NUMERIC = "numeric";
  public static final String REPORT_TOTAL = "reportTotal";
  public static final String RESULT_CHATTER = "resultChatter";
  public static final String RESULT_WINDOW = "resultWindow";
  public static final String RESULT_BUTTON = "resultButton";
  public static final String WINDOW_X = "windowX";
  public static final String WINDOW_Y = "windowY";
  public static final String DICE_SET = "diceSet";
  public static final String HOTKEY = "hotkey";
  public static final String NONE = "<none>";

  private static final int[] EMPTY = new int[0];

  public SpecialDiceButton() {
    ActionListener rollAction = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        DR();
      }
    };
    launch = new LaunchButton(null, BUTTON_TEXT, HOTKEY, ICON, rollAction);
    setAttribute(NAME, "SpecialDiceButton");
    setAttribute(BUTTON_TEXT, "SD6");
  }

  public static String getConfigureTypeName() {
    return "Symbolic Dice Button";
  }

  /**
   * The text reported before the results of the roll
   */
  protected String getReportPrefix() {
    return " *** " + getConfigureName() + " = ";
  }

  /**
   * The text reported after the results of the roll;
   */
  protected String getReportSuffix() {
    return " ***  <"
        + GameModule.getGameModule().getChatter().getHandle() + ">";
  }

  /**
   * Forwards the result of the roll to the {@link Chatter#send}
   * method of the {@link Chatter} of the {@link GameModule}.  Format is
   * prefix+[comma-separated roll list]+suffix
   * additionally a command for every die is generated
   */
  protected void DR() {

    GameModule theModule = GameModule.getGameModule();
    String strVal = getReportPrefix();
    int nTotal = 0;
    int nVal = 0;

    Command c = new NullCommand();

    for (int ii = 0; ii < oaDice.length; ++ii) {
      int[] rolls = new int[oaDice[ii].number];
      if (!bReportTotal)
        strVal += "[";

      // get the results of all rolls of this die
      for (int jj = 0; jj < oaDice[ii].number; ++jj) {
        rolls[jj] = (int) (ran.nextFloat() * oaDice[ii].sides + 1);
      }

      oaDice[ii].iaLastRolls = rolls;

      for (int jj = 0; jj < oaDice[ii].number; ++jj) {
        nVal = oaDice[ii].getIntVal(rolls[jj]);

        // report single rolls to chatter
        if (bResultToChatter && !bReportTotal) {
          strVal += oaDice[ii].getStrVal(rolls[jj]);
          if (jj < oaDice[ii].number - 1) {
            strVal += ",";
          }
        }
        if (bNumeric && bReportTotal) {
          nTotal += nVal;
        }
      }
      // generate command to show dice for other players
      c.append(new RollSpecialDice(this, id, ii,
                                   oaDice[ii].number,
                                   oaDice[ii].name,
                                   oaDice[ii].mod,
                                   rolls));
      if (bNumeric && bReportTotal)
        nTotal += oaDice[ii].mod;
      if (!bReportTotal)
        strVal += "] ";
    }

    // if numeric total can be reported
    if (bNumeric && bReportTotal) {
      strVal += nTotal;
      c.append(new RollSpecialDice(this, id, -1,
                                   0, "#total", nTotal, null));
    }

    if (bResultToChatter) {
      strVal += getReportSuffix();
      c.append(new Chatter.DisplayText(theModule.getChatter(), strVal));
    }

    c.execute();
    theModule.sendAndLog(c);
  }

  /**
   * The Attributes of a DiceButton are:
   *
   * <code>BUTTON_TEXT</code> the label of the button in the toolbar
   * <code>ICON</code> the icon of the button in the toolbar
   * <code>HOTKEY</code> the hotkey equivalent of the button
   * <code>DICE_SET</code> list of dice sets, an entry can be:
   *                       [number]name of die[+|-modifier]
   *                       "name of die" must be SpecialDie
   *                       "modifier" is added/subtracted to/from total of dice
   *                       [number]Dnumber of sides (e.g. 2D6)
   * <code>NUMERIC</code> result of all dice is numeric
   * <code>REPORT_TOTAL</code> If numeric and true, add the results of the dice together and report the total.  Otherwise, report the individual results
   * <code>SORT</code> if true sort results per die by numeric value
   * <code>RESULT_CHATTER</code> if true report results in chatter
   * <code>RESULT_WINDOW</code> if true show result graphical in extra window
   * <code>WINDOW_X</code> width of window or button
   * <code>WINDOW_Y</code> height of window or button
   * <code>RESULT_MAP</code> :TODO: if true show result in special area in map
   * <code>MAP_NAME</code> :TODO: name of map
   * <code>RESULT_BUTTON</code> if true show result graphical in button
   */
  public String[] getAttributeNames() {
    String s[] = {NAME, BUTTON_TEXT, ICON, HOTKEY,
                  NUMERIC, REPORT_TOTAL, RESULT_CHATTER, RESULT_BUTTON,
                  RESULT_WINDOW, WINDOW_X, WINDOW_Y};
    return s;
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Name",
                        "Button text",
                        "Button icon",
                        "Hotkey",
                        "Numeric values",
                        "Report total",
                        "Report results as text",
                        "Show result in button",
                        "Show result in window",
                        "Width",
                        "Heidght"};
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "/images/die.gif");
    }
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class,
                       String.class,
                       IconConfig.class,
                       KeyStroke.class,
                       Boolean.class,
                       Boolean.class,
                       Boolean.class,
                       Boolean.class,
                       Boolean.class,
                       Integer.class,
                       Integer.class};
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    // report total only if numeric
    if (REPORT_TOTAL.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return bNumeric;
        }
      };
    }
// get size only when output in window or on button
    else if (WINDOW_X.equals(name) || WINDOW_Y.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return bResultInWindow || bResultInButton;
        }
      };
    }
    else
      return null;
  }

  public void remove(Buildable b) {
    super.remove(b);
    init();
  }

  public void add(Buildable b) {
    super.add(b);
    init();
  }

  /**
   * Expects to be added to a SymbolDice.  Adds the button to the
   * control window's toolbar and registers itself as a {@link
   * KeyStrokeListener} */
  public void addTo(Buildable parent) {
    int count = 0;

    GameModule mod = GameModule.getGameModule();
    ran = mod.getRNG();

    mod.getToolBar().add(launch);

    // Our button doesn't yet appear in the GameModule
    for (Enumeration e = mod.getComponents(SpecialDiceButton.class); e.hasMoreElements();) {
      e.nextElement();
      count++;
    }
    setId("SpecialDiceButton" + count);

    mod.addCommandEncoder(this);
  }


  public void removeFrom(Buildable b) {
    GameModule mod = GameModule.getGameModule();
    mod.removeCommandEncoder(this);

    mod.getToolBar().remove(launch);
    mod.getToolBar().revalidate();
  }

  public void setId(String id) {
    this.id = id;
  }

  public String getId() {
    return id;
  }

  /**
   * create a dialog for the graphic output of dice
   */
  private void createDialog() {
    if (oDialog == null) {
      oDialog = new SpecialDiceDialog();
    }
  }

  /**
   *
   * @param id
   * @return the {@link SpecialDiceButton} with the given id
   */
  public static SpecialDiceButton findSpecialDiceButton(String id) {
    for (Enumeration e = GameModule.getGameModule().getComponents(SpecialDiceButton.class); e.hasMoreElements();) {
      SpecialDiceButton s = (SpecialDiceButton) e.nextElement();
      if (s.getId().equals(id)) {
        return s;
      }
    }
    return null;
  }

  /**
   * The component to be added to the control window toolbar
   */
  protected java.awt.Component getComponent() {
    return launch;
  }

  /**
   * get boolean value of object
   * @param o object as input for setAttribute()
   * @return  boolean value of object
   */
  private boolean getBoolVal(Object o) {
    if (o instanceof Boolean) {
      boolean bool = ((Boolean) o).booleanValue();
      return bool;
    }
    else if (o instanceof String) {
      return "true".equals(o);
    }
    else
      return false;
  }

  public void setAttribute(String key, Object o) {
    if (NAME.equals(key)) {
      setConfigureName((String) o);
      launch.setToolTipText((String) o);
    }
    else if (NUMERIC.equals(key)) {
      bNumeric = getBoolVal(o);
    }
    else if (REPORT_TOTAL.equals(key)) {
      bReportTotal = getBoolVal(o);
    }
    else if (RESULT_CHATTER.equals(key)) {
      bResultToChatter = getBoolVal(o);
    }
    else if (RESULT_BUTTON.equals(key)) {
      bResultInButton = getBoolVal(o);
    }
    else if (RESULT_WINDOW.equals(key)) {
      bResultInWindow = getBoolVal(o);
    }
    else if (WINDOW_X.equals(key)) {
      if (o instanceof Integer) {
        nWinX = ((Integer) o).intValue();
      }
      else if (o instanceof String) {
        nWinX = Integer.parseInt((String) o);
      }
    }
    else if (WINDOW_Y.equals(key)) {
      if (o instanceof Integer) {
        nWinY = ((Integer) o).intValue();
      }
      else if (o instanceof String) {
        nWinY = Integer.parseInt((String) o);
      }
    }
    else {
      launch.setAttribute(key, o);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (NUMERIC.equals(key)) {
      return "" + bNumeric;
    }
    else if (REPORT_TOTAL.equals(key)) {
      return "" + bReportTotal;
    }
    else if (RESULT_CHATTER.equals(key)) {
      return "" + bResultToChatter;
    }
    else if (RESULT_BUTTON.equals(key)) {
      return "" + bResultInButton;
    }
    else if (RESULT_WINDOW.equals(key)) {
      return "" + bResultInWindow;
    }
    else if (WINDOW_X.equals(key)) {
      return "" + nWinX;
    }
    else if (WINDOW_Y.equals(key)) {
      return "" + nWinY;
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }

  public Class[] getAllowableConfigureComponents() {
    Class[] c = {SpecialDie.class};
    return c;
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "GameModule.htm"), "#SpecialDiceButton");
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  /**
   * initialize dice roller
   */
  private void init() {
    ArrayList l = new ArrayList();
    for (Enumeration e = getComponents(SpecialDie.class); e.hasMoreElements();) {
      l.add(new Dice((SpecialDie) e.nextElement()));
    }
    oaDice = (Dice[]) l.toArray(new Dice[l.size()]);
  }

  /**
   * create String from int array
   * @param ia int-array
   * @return encoded String
   */
  public static String intArrayToString(int[] ia) {
    if (ia == null || ia.length == 0) {
      return "";
    }
    SequenceEncoder se = new SequenceEncoder(',');
    for (int i = 0; i < ia.length; ++i) {
      se.append(String.valueOf(ia[i]));
    }
    return se.getValue();
  }

  /**
   * get int array from string
   * @param s string with encoded int array
   * @return int array
   */
  public static int[] stringToIntArray(String s) {
    if (s == null
        || s.length() == 0) {
      return EMPTY;
    }
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ',');
    Vector v = new Vector();
    while (st.hasMoreTokens()) {
      v.addElement(st.nextToken());
    }
    int[] val = new int[v.size()];
    for (int i = 0; i < val.length; ++i) {
      val[i] = Integer.parseInt((String) v.elementAt(i));
    }
    return val;
  }


  public String encode(Command c) {
    if (c instanceof RollSpecialDice) {
      RollSpecialDice c2 = (RollSpecialDice) c;
      SequenceEncoder se = new SequenceEncoder(c2.sButtonId, '\t');
      se.append(id);
      se.append(String.valueOf(c2.nRollNo));
      se.append(name);
      se.append(String.valueOf(c2.nNumber));
      se.append(String.valueOf(c2.nMod));
      se.append(intArrayToString(c2.getRolls()));
      return se.getValue();
    }
    else {
      return null;
    }
  }

  public Command decode(String s) {
    if (s.startsWith(getId() + '\t')) {
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, '\t');
      st.nextToken();
      String contentsId = st.nextToken();
      int nRollNo = Integer.parseInt(st.nextToken());
      String name = st.nextToken();
      int nNumber = Integer.parseInt(st.nextToken());
      int nMod = Integer.parseInt(st.nextToken());
      return new RollSpecialDice(this, contentsId, nRollNo, nNumber,
                                 name, nMod,
                                 stringToIntArray(st.nextToken()));
    }
    else {
      return null;
    }
  }

  /**
   * Command for rolling dice from a dice set
   */
  public static class RollSpecialDice extends Command {
    private SpecialDiceButton target;
    private String sButtonId;  // id of button
    private int nRollNo;       // counts rolls beginning with 0
    private int[] rolls;       // random numbers (not int values!)
    private String name;       // name of die
    private int nNumber;       // number of dice rolled
    private int nMod;          // modifier

//	+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    private class SpecialDiceIcon implements Icon {
      private int nWidth, nHeight;
      private Dice[] oDice;
      private Component oObserver;

      public SpecialDiceIcon(Dice[] oDice, int nWidth, int nHeight) {
        this.nHeight = nHeight;
        this.nWidth = nWidth;
        this.oDice = oDice;
      }

      public void paintIcon(Component c, Graphics g, int x, int y) {
        GameModule theModule = GameModule.getGameModule();
        g.setColor(c.getBackground());
        g.clearRect(0, 0, nWidth, nHeight);
        int offset = 2;
        for (int n = 0; n < oDice.length; ++n) {
          for (int ii = 0; ii < oDice[n].iaLastRolls.length; ii++) {
            String strImage = oDice[n].theDie.getImageName(oDice[n].iaLastRolls[ii]);
            try {
              ImageIcon aImageIcon = new ImageIcon(theModule.getDataArchive().getCachedImage(strImage));
              Image aImage = aImageIcon.getImage();
              g.drawImage(aImage, offset, 2, oObserver);
              offset += aImage.getWidth(oObserver) + 5;
            }
            catch (IOException e) {
              System.err.println("Unable to locate image " + strImage);
            }
          }
        }
      }

      public void setImageObserver(Component c) {
        oObserver = c;
      }

      public int getIconWidth() {
        return nWidth;
      }

      public int getIconHeight() {
        return nHeight;
      }

    }  // end class SpecialDiceIcon


    public RollSpecialDice(SpecialDiceButton oTarget, String pId, int pRollNo, int pNumber, String pName, int pMod, int[] pRolls) {
      target = oTarget;
      sButtonId = pId;
      nRollNo = pRollNo;
      nNumber = pNumber;
      name = pName;
      nMod = pMod;
      rolls = pRolls;
    }

    protected void executeCommand() {
      int ii;

      if (target == null) {
        target = SpecialDiceButton.findSpecialDiceButton(sButtonId);
      }
      target.createDialog();

      // remove old dice symbols
      if (target.bResultInWindow && nRollNo == 0) {
        target.oDialog.removeAll();
      }

      // special commands: show total sum in title
      if (target.bResultInWindow && nRollNo == -1) {
        String s = target.getConfigureName() + ": " + String.valueOf(nMod);
        target.oDialog.setTitle(s);
      }

      // output graphical
      else if ((target.bResultInWindow || target.bResultInButton) &&
          rolls != null && rolls.length > 0) {

        GameModule theModule = GameModule.getGameModule();

        SpecialDie theDie = target.oaDice[nRollNo].theDie;
        target.oaDice[nRollNo].iaLastRolls = rolls;

        if (theDie != null && theDie.hasImages() &&
            target.bResultInButton) {
          target.launch.removeAll();
          SpecialDiceIcon anIcon = new SpecialDiceIcon(target.oaDice, target.nWinX, target.nWinY);
          anIcon.setImageObserver(target.launch);
          JLabel label = new JLabel(anIcon);
          target.launch.add(label);
          target.launch.repaint();
          target.launch.revalidate();
        }

        if (target.bResultInWindow) {
          for (ii = 0; ii < rolls.length; ii++) {
            // get symbol from resource
            if (theDie != null && theDie.hasImages()) {
              String strImage = theDie.getImageName(rolls[ii]);
              try {
                ImageIcon aImage = new ImageIcon(theModule.getDataArchive().getCachedImage(strImage));
                JLabel label = new JLabel(aImage);
                target.oDialog.addL(label);
              }
              catch (IOException e) {
                System.err.println("Unable to locate image " + strImage);
              }
            }
            else {
              JLabel label = new JLabel();
              if (theDie != null) {
                label.setText(theDie.getStrVal(rolls[ii]));
              }
              else {
                label.setText(String.valueOf(rolls[ii]));
              }
              target.oDialog.addL(label);
            }
          }
          target.oDialog.showDialog();
        }
      }
    }

    protected Command myUndoCommand() {
      return null;
    }

    public int[] getRolls() {
      return rolls;
    }
  }


//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  private class Dice {
    public int number;  // number of rolls
    public String name; // Name of SpecialDie
    public int mod;     // Modifier added after all rolls
    public int[] iaLastRolls; // last rolls of die

    private int sides;   // number of sides
    private SpecialDie theDie; // special die used

    public Dice(int pNo, String pName, int pMod) {
      sides = 0;
      number = pNo;
      if (number == 0)
        number = 1;

      mod = pMod;
      name = pName;

      // get the number of sides of SpecialDie
      theDie = SpecialDie.findSpecialDie(name);
      if (theDie != null)
        sides = theDie.getSides();
      iaLastRolls = null;
    }

    public Dice(SpecialDie die) {
      theDie = die;
      sides = theDie.getSides();
      name = theDie.getConfigureName();
      number = 1;
    }

    public String getStrVal(int pRoll) {
      if (pRoll > 0) {
        return theDie.getStrVal(pRoll);
      }
      else
        return null;
    }


    public int getIntVal(int pRoll) {
      if (pRoll > 0) {
        return theDie.getIntVal(pRoll);
      }
      else
        return 0;
    }

    public boolean isNumeric() {
      return theDie.bNumeric;
    }

  }  // end class Dice


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  private class SpecialDiceDialog extends JDialog {
    private JPanel m_oPanel;
    private ScrollPane m_oScrollPane;

    /**
     * create a dialog for the graphic output of dice
     */
    public SpecialDiceDialog() {
      super(GameModule.getGameModule().getFrame());
      setTitle(getConfigureName());
      m_oScrollPane = new ScrollPane(ScrollPane.SCROLLBARS_AS_NEEDED);
      setContentPane(m_oScrollPane);
      m_oPanel = new JPanel();
      Dimension oSize = new Dimension(nWinX, nWinY);
      m_oPanel.setPreferredSize(oSize);
      m_oScrollPane.setSize(nWinX + 20, nWinY + 20);
      m_oScrollPane.add(m_oPanel);
    }

    public void centerDialog() {
      if (getX() < 2) {
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        Dimension windowSize = getSize();
        setLocation(Math.max(0, (screenSize.width - windowSize.width) / 2),
                    Math.max(0, (screenSize.height - windowSize.height) / 2));
      }
    }

    public void showDialog() {
      validate();
      pack();
      centerDialog();
      setVisible(true);
    }

    public void removeAll() {
      m_oPanel.removeAll();
    }

    public Component addL(Component arg0) {
      return m_oPanel.add(arg0);
    }

  }  // end class SpecialDiceDialog

}
