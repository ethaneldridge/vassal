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
 
package turn;

import java.util.ArrayList;
import java.util.Enumeration;

import javax.swing.JComponent;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;


public abstract class TurnLevel extends AbstractConfigurable {

  protected static final String NAME = "name";
  
//  protected static final String TURN_NAME = "turnName";
//  protected static final String TURN_TEXT = "turnText";
    
  protected int start = 0;		// Counter Start value
  
  protected int current = 0;	// Current counter pointer 
  
  protected int currentSubLevel = 0;		// sub-level pointer
  
  protected boolean subLevelRolledOver = false;
  protected boolean rolledOver = false;
  
  protected abstract String getState();
  protected abstract void setState(String code);
  protected abstract String getValueString();
  protected abstract String getLongestValueName();
  protected abstract void retreat();
  protected abstract JComponent getSetControls();
  protected abstract void toggleConfigVisibility();
  protected abstract void setConfigVisibility(boolean b);

  public TurnLevel() {
    super();
  }
  
  
  protected boolean hasSubLevelRolledOver() {
    return subLevelRolledOver;
  }
  
  protected void reset() {
    for (int i = 0; i < getTurnLevelCount(); i++) {
      getTurnLevel(i).reset();
    }
    currentSubLevel = getTurnLevelCount()-1;
  }
  
  protected  void advance() {
    rolledOver = false;
    subLevelRolledOver = false;
    if (getTurnLevelCount() > 0) {
      TurnLevel subLevel = getTurnLevel(currentSubLevel);
      subLevel.advance();    
      if (subLevel.hasRolledOver()) {
        currentSubLevel++;
        if (currentSubLevel >= getTurnLevelCount()) {
          currentSubLevel = 0;
          subLevelRolledOver = true;
        }
      }
    }
  }
  
  // Return the description of this turn 
//  public String getTurnString() {
//    turnFormat.setProperty(TURN_NAME, getConfigureName());
//    turnFormat.setProperty(TURN_TEXT, getValueString());
//    return turnFormat.getText();
//  }
  
  public void getTurnStrings(ArrayList desc) {
    desc.add(getValueString());
    if (getTurnLevelCount() > 0) {
      getTurnLevel(currentSubLevel).getTurnStrings(desc);
    }
  }
  
  protected void setRolledOver(boolean b) {
    rolledOver = b;
  }
  
  protected boolean hasRolledOver() {
    return rolledOver;
  }

  protected TurnLevel getTurnLevel(int i) {
    return (TurnLevel) buildComponents.get(i);
  }

  protected int getTurnLevelCount() {
    return buildComponents.size();
  }
  
  protected Enumeration getTurnLevels() {
    return getBuildComponents();
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }

  }
  
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else {
      return "";
    }
  }
  
  public void removeFrom(Buildable parent) {
    
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[] {CounterTurnLevel.class, ListTurnLevel.class};
  }

  public void addTo(Buildable parent) {
    
  }

}
