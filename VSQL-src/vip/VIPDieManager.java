/*
 * $Id$
 *
 * Copyright (c) 2006 by Rodney Kinney, Brent Easton
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

package vip;

import java.io.IOException;
import java.io.PrintStream;
import java.util.Vector;

import sun.net.smtp.SmtpClient;
import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.DieManager;
import VASSAL.build.module.DieRoll;
import VASSAL.command.Command;
import VASSAL.tools.FormattedString;

/**
 * @author Brent Easton
 * 
 * TODO To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
public class VIPDieManager extends DieManager {

  public static final String MAIL_SERVER = "mailServer";
  protected VIPDieServer server;

  public VIPDieManager() {
    server = new VIPDieServer();
  }

  public void roll(int nDice, int nSides, int plus, boolean reportTotal, String description, FormattedString format) {

    VIPMultiRoll mroll = getMyMultiRoll(1, 0);

    RollSet rollSet;

    String desc = GameModule.getGameModule().getChatter().getInputField().getText();
    if (desc != null && desc.length() > 0) {
      mroll.setDescription(desc);
    }

    // Do we want full multi-roll capabilities? If required, pop-up the
    // multi-roll
    // cofigurer to get the details
    mroll.setVisible(true);

    if (mroll.wasCancelled()) {
      return;
    }
    rollSet = mroll.getRollSet();
    desc = rollSet.getDescription();

    // Multi Roll preference not selected, so build a dummy MultiRoll object

    Command chatCommand = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), " - Roll sent to VIP Server");

    if (desc == null || desc.length() == 0) {
      desc = GameModule.getGameModule().getChatter().getInputField().getText();
    }

    String secondaryEmail = GameModule.getGameModule().getPrefs().getStoredValue(SECONDARY_EMAIL);

    if (server.getUseEmail()) {
      if (desc == null || desc.length() == 0) {
        chatCommand.append(new Chatter.DisplayText(GameModule.getGameModule().getChatter(), " - Emailing " + secondaryEmail + " (no subject line)"));
        chatCommand.append(new Chatter.DisplayText(GameModule.getGameModule().getChatter(), " - Leave text in the chat input area to provide a subject line"));
      }
      else {
        chatCommand.append(new Chatter.DisplayText(GameModule.getGameModule().getChatter(), " - Emailing " + secondaryEmail + " (Subject:  " + desc + ")"));
      }
    }
    chatCommand.execute();
    GameModule.getGameModule().sendAndLog(chatCommand);

    GameModule.getGameModule().getChatter().getInputField().setText("");
    rollSet.setDescription(desc);

    String[] rollString = server.buildInternetRollString(rollSet);

    //server.roll(rollSet, format);

    String from = (String) GameModule.getGameModule().getPrefs().getValue(PRIMARY_EMAIL);
    String to = (String) GameModule.getGameModule().getPrefs().getValue(SECONDARY_EMAIL);
    String mailServer = (String) GameModule.getGameModule().getPrefs().getValue(MAIL_SERVER);

    try {
      SmtpClient smtp = new SmtpClient(mailServer); // assume localhost
      smtp.from(from);
      smtp.to(to);
      PrintStream msg = smtp.startMessage();

      msg.println("To: " + server.getServerURL()); // so mailers will display
                                                   // the To: address
      msg.println("Subject: " + desc);
      msg.println("From: " + from);
      msg.println("Cc: " + to);
      msg.println();

      for (int i = 0; i < rollString.length; i++) {
        msg.println(rollString[i]);
      }

      smtp.closeServer();

      String s = "Roll Request  " + desc + " emailed to " + to;
      GameModule.getGameModule().getChatter().send(s);
      smtp.closeServer();

    }
    catch (IOException e) {
      String s = "- Internet dice roll attempt " + desc + " failed: " + e.getMessage();
      GameModule.getGameModule().getChatter().send(s);
    }

  }

  protected VIPMultiRoll myMultiRoll;

  public VIPMultiRoll getMyMultiRoll(int nDice, int nSides) {
    String serverName = getServer().getName();
    if (myMultiRoll == null) {
      myMultiRoll = new VIPMultiRoll(server, nDice, nSides);
    }

    return myMultiRoll;
  }

  protected class VIPDieServer extends DieServer {

    public VIPDieServer() {

      name = "VIP";
      description = "VIP Dice Server";
      emailOnly = true;
      maxRolls = 0;
      maxEmails = 0;
      serverURL = "newdice@dartmouth.east.sun.com";
      passwdRequired = false;
      canDoSeperateDice = true;
    }

    public String[] buildInternetRollString(RollSet mr) {
      String[] lines = new String[mr.getDieRolls().length * 3 + 1];
      int idx = 0;
      lines[idx++] = mr.getDescription();

      for (int i = 0; i < mr.getDieRolls().length; i++) {
        DieRoll roll = mr.getDieRolls()[i];
        lines[idx++] = "";
        lines[idx++] = roll.getDescription() + "";
        String s = "#" + roll.getNumDice();
        if (roll.getNumSides() > 0) {
          s += "-" + roll.getNumSides();
        }
        lines[idx++] = s;
      }
      return lines;
    }

    public void parseInternetRollString(RollSet rollSet, Vector results) {
      // Not required by an email only roller
    }

    public void roll(RollSet mr, FormattedString format) {
      super.doInternetRoll(mr, format);
    }

    public String getServerURL() {
      return serverURL;
    }
  }

}
