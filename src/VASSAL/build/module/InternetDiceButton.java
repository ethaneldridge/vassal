/*
 * $Id$
 *
 * Copyright (c) 2003 by Brent Easton and Rodney Kinney
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

/*
 *
 * @author Brent Easton
 *
 * Enhanced Dice Button includes access to Internet Die Servers via the DieManager.
 *
 */

import java.io.File;
import java.net.MalformedURLException;

import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;

/**
 * This component places a button into the controls window toolbar.
 * Pressing the button generates random numbers and displays the
 * result in the Chatter */

public class InternetDiceButton extends DiceButton {

  protected static DieManager dieManager;

  public InternetDiceButton() {
    super();
  }

  public static String getConfigureTypeName() {
    return "Internet Dice Button";
  }

  /**
   * Ask the die manager to do our roll!
   */
  protected void DR() {
    dieManager.roll(nDice, nSides, plus, reportTotal, getConfigureName());
  }

  /**
   * Expects to be added to the DieManager.
   */
  public void addTo(Buildable parent) {
    initDieManager();
    dieManager.addDieButton(this);
    super.addTo(parent);
  }

  protected void initDieManager() {
    if (dieManager == null) {
      dieManager = new DieManager();
      dieManager.build(null);
    }
  }


  public void removeFrom(Buildable b) {
    dieManager.removeDieButton(this);
    super.removeFrom(b);
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(
          null,
          new File(dir, "GameModule.htm"),
          "#InternetDiceButton");
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }
}

