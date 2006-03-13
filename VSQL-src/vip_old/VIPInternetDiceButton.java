/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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
 
package vip_old;

import java.util.Vector;

import VASSAL.build.GameModule;
import VASSAL.build.module.DieManager;
import VASSAL.build.module.InternetDiceButton;
import VASSAL.build.module.DieManager.DieServer;
import VASSAL.build.module.DieManager.RollSet;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.FormattedString;

/**
 * @author Brent Easton
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class VIPInternetDiceButton extends InternetDiceButton {
  
  public VIPInternetDiceButton() {
    super();
  }
  
  public static String getConfigureTypeName() {
    return "VIP Internet Dice Button";
  }
  
  protected void initDieManager() {
    super.initDieManager();
 //   dieManager.addDieServer(new VIPDieServer());
    
 //   final StringConfigurer mailServer = new StringConfigurer(DieManager.MAIL_SERVER, "Outgoing Mail Server");
 //   GameModule.getGameModule().getPrefs().addOption(DieManager.DIE_MANAGER, mailServer);
  }
  
  protected class VIPDieServer extends DieServer {

    public VIPDieServer() {

      name = "VIP";
      description = "VIP Dice Server (Email Only)";
      emailOnly = true;
      maxRolls = 0;
      maxEmails = 0;
      serverURL = "newdice@dartmouth.east.sun.com";
      passwdRequired = false;
      canDoSeperateDice = true;
      

    }

    
    public String[] buildInternetRollString(RollSet mr) {
      /*
       * Translate the Rollset into the format required
       */
      return new String[] {
          "Testing",
          "23rd AF vs Lexington",
          "#3-6"
      };
    }

    public void parseInternetRollString(RollSet rollSet, Vector results) {
      // Not required by an email only roller
    }

    public void roll(RollSet mr, FormattedString format) {
      super.doInternetRoll(mr, format);     
    }
  }
}
