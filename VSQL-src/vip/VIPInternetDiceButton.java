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
 
package vip;

import VASSAL.build.GameModule;
import VASSAL.build.module.InternetDiceButton;
import VASSAL.configure.StringConfigurer;

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
    if (dieManager == null) {
      dieManager = new VIPDieManager();
      dieManager.build(null);
    }
    
    final StringConfigurer mailServer = new StringConfigurer(VIPDieManager.MAIL_SERVER, "Outgoing Mail Server");
    GameModule.getGameModule().getPrefs().addOption(VIPDieManager.DIE_MANAGER, mailServer);
  }
  

}
