/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Brent Easton
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

import java.io.*;
import java.net.*;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.StringTokenizer;
import java.util.Vector;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.preferences.Prefs;

/**
 * @author Brent Easton
 *
 * Internet Die Roller Manager. Includes all the smarts to interface to web-based
 * Die Servers
 */

public class DieManager extends AbstractConfigurable {

  private Hashtable servers;
  private Vector dieButtons = new Vector();
  private String desc = "Die Manager";
  private boolean useMultiRoll;
  private int defaultNDice = 2;
  private int defaultNSides = 6;

  private DieServer server;
  private String lastServerName = "";
  private MultiRoll myMultiRoll;

  public static final String USE_INTERNET_DICE = "useinternetdice";
  public static final String DICE_SERVER = "diceserver";
  public static final String SERVER_PW = "serverpw";
  public static final String USE_EMAIL = "useemail";
  public static final String PRIMARY_EMAIL = "primaryemail";
  public static final String SECONDARY_EMAIL = "secondaryemail";
  public static final String MULTI_ROLL = "multiroll";
  public static final String DIE_MANAGER = "Die Manager";

  public static final String ROLL_MARKER = "VASSAL Internet Dice Roll";

  public static final String DESC = "description";
  public static final String DFLT_NSIDES = "dfltnsides";
  public static final String DFLT_NDICE = "dfltndice";

  public DieManager() {

    DieServer d;
    servers = new Hashtable();

    /*
     * Create the Internet Dice Servers we know about
     */
    d = new InbuiltDieServer();
    servers.put(d.getName(), d);
    server = d; // Set the default Internet Server

//        d = new IronyDieServer();
//        servers.put(d.getName(), d);
//
//        d = new InternetGamesDieServer();
//        servers.put(d.getName(), d);

    d = new ShadowDiceDieServer();
    servers.put(d.getName(), d);

    /*
     * The Dice Manager needs some preferences
     */

    final StringEnumConfigurer dieserver = new StringEnumConfigurer(DICE_SERVER, "Internet Dice Server", getDescriptions());
    final StringConfigurer serverpw = new StringConfigurer(SERVER_PW, "Dice Server Password");
    final BooleanConfigurer useemail = new BooleanConfigurer(USE_EMAIL, "Email results?");
    final StringConfigurer pemail = new StringConfigurer(PRIMARY_EMAIL, "Primary Email");
    final StringArrayConfigurer semail = new StringArrayConfigurer(SECONDARY_EMAIL, "Secondary Emails");
    final BooleanConfigurer multiroll = new BooleanConfigurer(MULTI_ROLL, "Use Multi-Roll?");

    GameModule.getGameModule().getPrefs().addOption(DIE_MANAGER, dieserver);
    GameModule.getGameModule().getPrefs().addOption(DIE_MANAGER, serverpw);
    GameModule.getGameModule().getPrefs().addOption(DIE_MANAGER, useemail);
    GameModule.getGameModule().getPrefs().addOption(DIE_MANAGER, pemail);
    GameModule.getGameModule().getPrefs().addOption(DIE_MANAGER, semail);
    GameModule.getGameModule().getPrefs().addOption(DIE_MANAGER, multiroll);
  }

  // Return names of all known Dice Servers
  public String[] getNames() {
    if (servers == null) {
      return null;
    }
    else {
      String s[] = new String[servers.size()];
      Enumeration e = servers.keys();

      for (int i = 0; e.hasMoreElements(); i++) {
        s[i] = (String) e.nextElement();
      }
      return s;
    }
  }

  // Return descriptions of all known dice servers
  public String[] getDescriptions() {
    if (servers == null) {
      return null;
    }
    else {
      String s[] = new String[servers.size()];
      Enumeration e = servers.elements();

      for (int i = 0; e.hasMoreElements(); i++) {
        s[i] = ((DieServer) e.nextElement()).getDescription();
      }
      return s;
    }
  }

  // Return server matching Name
  public DieServer getServerForName(String name) {
    return (DieServer) servers.get(name);
  }

  // Return server matching Description
  public DieServer getServerFromDescription(String de) {
    DieServer d = null;

    Enumeration e = servers.elements();
    while (e.hasMoreElements()) {
      d = (DieServer) e.nextElement();
      if (de.equals(d.getDescription())) {
        return d;
      }
    }
    return null;
  }

  public DieServer getServer() {
    getPrefs();
    return server;
  }

  public String getServerDescription() {
    return getServer().getDescription();
  }

  public String getServerName() {
    return getServer().getName();
  }

  public int getDfltNDice() {
    return defaultNDice;
  }

  public int getDfltNSides() {
    return defaultNSides;
  }

  public MultiRoll getMultiRoll() {

    String serverName = getServer().getName();
    if (myMultiRoll == null || !serverName.equals(lastServerName)) {
      myMultiRoll = new MultiRoll(this, defaultNDice, defaultNSides);
    }
    lastServerName = serverName;

    return myMultiRoll;
  }

  public void roll(int nDice, int nSides, int plus, boolean reportTotal, String buttonName) {

    MultiRoll mroll = getMultiRoll();

    getPrefs();

    // Do we want full multi-roll capabilities? If required, pop-up the multi-roll
    // cofigurer to get the details
    if (useMultiRoll) {

      mroll.setSingleRoll(false);
      mroll.setVisible(true);
      mroll.refresh();

      if (mroll.wasCancelled()) {
        return;
      }
    }

    // Multi Roll preference not selected, so build a dummy MultiRoll object
    else {
      mroll.setSingleRoll(server.getDescription() + ": " + buttonName, nDice, nSides, plus, reportTotal);
    }

    // Let the Die Server do it's stuff.
    server.roll(mroll);

  }

  /*
   * Retrieve the Dice Manager preferences and update the current Server
   * Preferences may change at ANY time!
   */
  private void getPrefs() {

    Prefs prefs = GameModule.getGameModule().getPrefs();

    // Get the correct server
    String serverName = ((String) prefs.getValue(DICE_SERVER));
    server = getServerFromDescription(serverName);

    // And tell it the prefs it will need
    server.setPasswd((String) prefs.getValue(SERVER_PW));
    server.setUseEmail(((Boolean) prefs.getValue(USE_EMAIL)).booleanValue());
    server.setPrimaryEmail((String) prefs.getValue(PRIMARY_EMAIL));
    server.setSecondaryEmail((String[]) prefs.getValue(SECONDARY_EMAIL));

    useMultiRoll = ((Boolean) prefs.getValue(MULTI_ROLL)).booleanValue();

  }

  /*
      public void addDie(SpecialDie d) {
          specialDice.add(d);
      }

      public void removeDie(SpecialDie d) {
          specialDice.remove(d);
      }
  */

  public void addDieButton(InternetDiceButton d) {
    dieButtons.add(d);
  }

  public void removeDieButton(InternetDiceButton d) {
    dieButtons.remove(d);
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Description", "Multi-roll Default Ndice", "Multi-roll Default Nsides"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class, Integer.class, Integer.class};
  }

  public String[] getAttributeNames() {
    return new String[]{DESC, DFLT_NDICE, DFLT_NSIDES};
  }

  public void setAttribute(String key, Object value) {
    if (DESC.equals(key)) {
      desc = (String) value;
    }
    else if (DFLT_NDICE.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      defaultNDice = ((Integer) value).intValue();
    }
    else if (DFLT_NSIDES.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      defaultNSides = ((Integer) value).intValue();
    }
  }

  public String getAttributeValueString(String key) {
    if (DESC.equals(key)) {
      return desc;
    }
    else if (DFLT_NDICE.equals(key)) {
      return defaultNDice + "";
    }
    else if (DFLT_NSIDES.equals(key)) {
      return defaultNSides + "";
    }
    else
      return null;
  }

  public void removeFrom(Buildable parent) {
    //((GameModule) parent).setDieManager(null);
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[]{InternetDiceButton.class};
  }

  public void addTo(Buildable parent) {
    //((GameModule) parent).setDieManager(this);
  }

  public static String getConfigureTypeName() {
    return "Die Manager";
  }

  /*
   * Base DieServer Class
   * Does most of the work. Individual Die Servers just need to implement
   * buildInternetRollString and parseInternetRollString methods.
   */

  public abstract class DieServer {

    protected java.util.Random ran;
    protected String name;
    protected String description;
    protected boolean emailOnly;
    protected int maxRolls;
    protected int maxEmails;
    protected String serverURL;
    protected boolean passwdRequired = false;
    protected String password = "";
    protected boolean useEmail;
    protected String primaryEmail;
    protected String[] secondaryEmail;
    protected boolean canDoSeperateDice = false;

    /*
     * Each implemented die server must provide this routine to build a
     * string that will be sent to the internet site to drive the web-based
     * die server. This will usually be a control string passed toa cgi script
     * on the site.
     */
    public abstract String[] buildInternetRollString(MultiRoll mr);

    /*
     * Each implemented die server must provide this routine to interpret the
     * html output generated by the site in response to the buildInternetRollString
     * call.
     */
    public abstract void parseInternetRollString(MultiRoll mr, Vector results);

    /*
     * Internet Die Servers should always implement roll by calling back to
     * doInternetRoll in the base DieServer class
     */
    public abstract void roll(MultiRoll mr);

    public DieServer() {
      ran = GameModule.getGameModule().getRNG();
    }

    /*
     * Some Internet servers can only roll specific numbers of dice or
     * dice with specific sides. These are the default settings.
     */
    public int[] getnDiceList() {
      return new int[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20};
    }

    public int[] getnSideList() {
      return new int[]{2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 20, 30, 50, 100, 1000};
    }

    public String getName() {
      return name;
    }

    public String getDescription() {
      return description;
    }

    public boolean isPasswdRequired() {
      return passwdRequired;
    }

    public String getPasswd() {
      return password;
    }

    public void setPasswd(String s) {
      password = s;
    }

    public void setPrimaryEmail(String e) {
      primaryEmail = e;
    }

    public String getPrimaryEmail() {
      return primaryEmail;
    }

    public void setSecondaryEmail(String[] e) {
      secondaryEmail = e;
    }

    public String[] getSecondaryEmail() {
      return secondaryEmail;
    }

    public void setUseEmail(boolean use) {
      useEmail = use;
    }

    public boolean getUseEmail() {
      return useEmail;
    }

    public int getMaxEmails() {
      return maxEmails;
    }

    /**
     * The text reported before the results of the roll
     */
    protected String getReportPrefix(String d) {
      return " *** " + d + " = ";
    }

    /**
     * The text reported after the results of the roll;
     */
    protected String getReportSuffix() {
      return " ***  <" + GameModule.getGameModule().getChatter().getHandle() + ">";
    }

    /*
     * Called by the Inbuilt server - Basically the same as the code
     * in the original DiceButton
     */
    public void doInbuiltRoll(MultiRoll mroll) {

      if (useMultiRoll) {
        String s = server.getDescription() + ": " + mroll.getDescription();
        GameModule.getGameModule().getChatter().send(s);
      }

      for (int i = 0; i < mroll.rolls.length; i++) {
        if (mroll.useDie[i]) {
          DieRoll roll = mroll.rolls[i];
          String desc = roll.description;
          int nSides = roll.numSides;
          int nDice = roll.numDice;
          int plus = roll.plus;
          boolean reportTotal = roll.reportTotal;

          String val = getReportPrefix(desc);
          int total = 0;
          for (int j = 0; j < nDice; ++j) {
            int result = (int) (ran.nextFloat() * nSides + 1) + plus;
            if (reportTotal) {
              total += result;
            }
            else {
              val += result;
              if (j < nDice - 1)
                val += ",";
            }
          }

          if (reportTotal)
            val += total;

          val += getReportSuffix();
          GameModule.getGameModule().getChatter().send(val);
        }
      }
    }

    /*
     * Internet Servers will call this routine to do their dirty work.
     */
    public void doInternetRoll(MultiRoll mroll) {

      if (!mroll.getSingleRoll()) {
        String s = server.getDescription() + ": " + mroll.getDescription();
        GameModule.getGameModule().getChatter().send(s);
      }

      try {
        doIRoll(mroll);
      }
      catch (Exception e) {
        String s = "Internet Dice Roll Attempt to " + description + " failed.";
        GameModule.getGameModule().getChatter().send(s);
      }

      for (int i = 0; i < mroll.rolls.length; i++) {
        if (mroll.useDie[i]) {
          DieRoll roll = mroll.rolls[i];
          int nDice = roll.numDice;
          boolean reportTotal = roll.reportTotal;

          String val = getReportPrefix(roll.description);
          int total = 0;

          for (int j = 0; j < nDice; j++) {
            int result = roll.result[j];
            if (reportTotal) {
              total += result;
            }
            else {
              val += result;
              if (j < nDice - 1)
                val += ",";
            }
          }

          if (reportTotal)
            val += total;

          val += getReportSuffix();
          GameModule.getGameModule().getChatter().send(val);
        }
      }

    }

    public void doIRoll(MultiRoll toss) throws Exception {

      String[] rollString = buildInternetRollString(toss);
      Vector returnString = new Vector();
      //            rollString[0] =
      //                "number1=2&type1=6&number2=2&type2=30&number3=2&type3=30"
      //                    + "&number4=0&type4=2&number5=0&type5=2&number6=0&type6=2&number7=0&type7=2"
      //                    + "&number8=0&type8=2&number9=0&type9=2&number10=0&type10=2"
      //                    + "&emails=&email=b.easton@uws.edu.au&password=IG42506&Submit=Throw+Dice";
      URL url = new URL(serverURL);

      URLConnection connection = url.openConnection();
      connection.setDoOutput(true);

      PrintWriter out = new PrintWriter(connection.getOutputStream());
      for (int i = 0; i < rollString.length; i++) {
        out.println(rollString[i]);
      }

      out.close();

      BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));

      String inputLine;
      int i = 0;

      while ((inputLine = in.readLine()) != null)
        returnString.add(i++, inputLine);

      in.close();

      parseInternetRollString(toss, returnString);

    }

  }

  /*
    *
    * Vassal In-built random number generator
    *
    */
  private class InbuiltDieServer extends DieServer {

    public InbuiltDieServer() {

      name = "inbuilt";
      description = "Inbuilt Random Number Generator";
    }

    public String[] buildInternetRollString(MultiRoll mr) {
      return null;
    }

    public void parseInternetRollString(MultiRoll mr, Vector results) {
    }

    public void roll(MultiRoll mr) {
      super.doInbuiltRoll(mr);
    }

  }

  /*
   *
   * Internet Games Dice Server
   * www.internetgames.org
   *
   */
  private class InternetGamesDieServer extends DieServer {

    public InternetGamesDieServer() {

      name = "igames";
      description = "Internet Games Dice Server";
      emailOnly = true;
      maxRolls = 0;
      maxEmails = 0;
      serverURL = "http://www.internetgames.org/diceserver/dice1.asp";
      passwdRequired = true;
      // password = "IG42506";
    }

    public String[] buildInternetRollString(MultiRoll toss) {
      String s = "";
      for (int i = 0; i < MultiRoll.MAX_ROLLS; i++) {

        if (toss.useDie[i]) {
          if (!s.equals("")) {
            s += "&";
          }
          s += "number" + (i + 1) + "=" + toss.rolls[i].numDice;
          s += "&type" + (i + 1) + "=" + toss.rolls[i].numSides;
        }
        else {
          s += "&number" + (i + 1) + "=0&type" + (i + 1) + "=2";
        }
      }
      s += "&emails=";
      s += "&email=" + getPrimaryEmail();
      s += "&password=" + getPasswd();
      s += "&Submit=Throw+Dice";

      return new String[]{s};
    }

    public void parseInternetRollString(MultiRoll mr, Vector results) {

      Enumeration e = results.elements();

      // Initialise and search for start line
      String line = (String) e.nextElement();
      while (e.hasMoreElements() && !line.startsWith("<table bgcolor='F0F0FF'><tr>"))
        line = (String) e.nextElement();

      // And process the results, 1 per roll in the multiroll
      for (int i = 0; i < mr.rolls.length; i++) {

        if (mr.useDie[i]) {

          //					Find next roll
          int pos = line.indexOf("throws:&nbsp;");
          line = line.substring(pos);
          StringTokenizer st = new StringTokenizer(line, "&;");

          for (int j = 0; j < mr.rolls[i].numDice; j++) {
            st.nextToken();
            st.nextToken();
            st.nextToken();
          }

        }
      }
    }

    public void roll(MultiRoll mr) {
      super.doInternetRoll(mr);
    }

    /*
     * Internet Games only supports specific nSides
     */
    public int[] getnSideList() {
      return new int[]{2, 4, 6, 8, 10, 12, 20, 30, 100};
    }
  }

  /**
   *
   * Irony Games Dice Server
   *
   */
  private class IronyDieServer extends DieServer {

    public IronyDieServer() {

      name = "Irony";
      description = "Irony Games Dice Server";
      emailOnly = true;
      maxRolls = 0;
      maxEmails = 2;
      serverURL = "http://www.irony.com/cgi-bin/mroll-query";
      passwdRequired = false;

    }

    public String[] buildInternetRollString(MultiRoll mr) {
      String s;

      //numdice=2&numsides=6&modroll=No&numroll=3&subject=test+Roll&roller=b.easton@uws.edu.au&gm=

      s = "numdice=" + mr.getRollCount();
      //s += "&numsides=" + mr.getFirstNSides();
      return new String[]{s};
    }

    public void parseInternetRollString(MultiRoll toss, Vector results) {
    }

    public void roll(MultiRoll mr) {
      super.doInternetRoll(mr);
    }

    /*
     * Irony only supports specific dice combinations
     */
    public int[] getnDiceList() {
      return new int[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 20};
    }

    public int[] getnSideList() {
      return new int[]{2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 18, 20, 25, 30, 100, 1000};
    }
  }

  /**
   *
   * ShadowDice Dice Server
   *
   */
  private class ShadowDiceDieServer extends DieServer {

    public ShadowDiceDieServer() {

      name = "ShadowDice";
      description = "ShadowDice Dice Server";
      emailOnly = false;
      maxRolls = 0;
      maxEmails = 0;
      serverURL = "http://www.gamerz.net/shadowdice/shadowdice.cgi";
      passwdRequired = false;
      canDoSeperateDice = true;
    }

    public String[] buildInternetRollString(MultiRoll toss) {

      final String CRLF = "%0D%0A"; // CRLF
      final String LSQUARE = "%5B"; // '['
      final String RSQUARE = "%5D"; // ']'
      final String HASH = "%23";
      final String PLUS = "%2B";

      String desc, s, pEmail = "", sEmail = "";

      if (getUseEmail()) {
        pEmail = getPrimaryEmail();
        String[] se = getSecondaryEmail();
        if (se.length > 0)
          sEmail = se[0];
      }

      desc = hexify(toss.description);

      s = "mto=" + pEmail + "&mcc=" + sEmail + "&yem=" + pEmail;
      s += "&sbj=" + desc;
      s += "&msg=" + ROLL_MARKER + CRLF + desc + CRLF;

      int mLen = toss.getMaxDescLength();

      for (int i = 0; i < toss.rolls.length; i++) {
        if (toss.useDie[i]) {
          s += hexify(toss.rolls[i].description);
          for (int j = 0; j < mLen - toss.rolls[i].description.length(); j++) {
            s += ' ';
          }
          s += ' ' + HASH;
          int nd = toss.rolls[i].numDice;
          int ns = toss.rolls[i].numSides;
          int p = toss.rolls[i].plus;
          for (int j = 0; j < nd; j++) {
            s += LSQUARE + "1d" + ns + RSQUARE;
          }
          s += CRLF;
        }
      }

      s += "&todo=Action%21&hid=1";
      s = s.replace(' ', '+'); // No spaces allowed, use '+' instead.

      return new String[]{s};
    }

    /*
     * The Irony server requires most of the non-alphanumerics to be
     * converted to a hex escape code %nn. '*-_.' excepted.
     * '#' characters interfere with the output parsing and are stripped out.
     */
    public String hexify(String s) {

      final String hexyChars = "~!$%^&()+`={}[]|:;'<>,?/\\\"";
      StringBuffer b = new StringBuffer();

      for (int i = 0; i < s.length(); i++) {
        char c = s.charAt(i);

        if (c == '#') {
          b.append('.');
        }
        else if (hexyChars.indexOf(c) >= 0) {
          b.append("%" + Integer.toHexString(c).toUpperCase());
        }
        else {
          b.append(c);
        }
      }
      return b.toString();
    }

    public void parseInternetRollString(MultiRoll mr, Vector results) {

      Enumeration e = results.elements();

      // Initialise and search for start line
      String line = (String) e.nextElement();
      while (e.hasMoreElements() && !line.startsWith("! " + ROLL_MARKER))
        line = (String) e.nextElement();

      // Skip description line
      line = (String) e.nextElement();

      // And process the results, 1 per roll in the multiroll
      for (int i = 0; i < mr.rolls.length; i++) {

        if (mr.useDie[i]) {
          line = (String) e.nextElement();

          int firsthash = line.indexOf("#") - 1;
          StringTokenizer st = new StringTokenizer(line.substring(firsthash), " ");

          for (int j = 0; j < mr.rolls[i].numDice; j++) {
            st.nextToken();
            String result = st.nextToken();
            int res = Integer.parseInt(result);
            mr.rolls[i].setResult(j, res);
          }
        }
      }
    }

    public void roll(MultiRoll mr) {
      super.doInternetRoll(mr);
    }

  }

}