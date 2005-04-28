/*
 * $Id$
 * 
 * Copyright (c) 2000-2005 by Brent Easton, Rodney Kinney
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License (LGPL) as published by
 * the Free Software Foundation.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, copies are available at
 * http://www.opensource.org.
 */
package VSQL;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;

import VASL.counters.Concealable;
import VASSAL.build.GameModule;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyCommand;

public class PLConcealable extends Concealable implements ActionListener {

  protected JPopupMenu popup;

  public static final String ID = "plconceal;";

  public static final String[] NATIONS = new String[] { "PLC", 
      													"German", 
      													"Russian", 
      													"American",
      													"British",
      													"French",
      													"Axis Minor",
      													"Allied Minor"};

  public static final String[] PREFIXES = new String[] { "pl", 
      													 "ge", 
      													 "ru", 
      													 "am",
      													 "br",
      													 "fr",
      													 "ax",
      													 "al"};

  public static final String NATIONALITY_COMMAND = "Nationality";

  public static final KeyStroke NATIONALITY_KEY = KeyStroke.getKeyStroke('N', InputEvent.CTRL_MASK);

  public PLConcealable() {
    super();
  }

  public PLConcealable(String type, GamePiece inner) {
    super(type, inner);
  }

  public String getDescription() {
    return "PLC can be concealed";
  }
  
  public String myGetType() {
    String s = ID +
        obscureKey + ";" +
        imageName + ";" +
        nation;
    if (nation2 != null) {
      s += ";" + nation2;
    }
    return s;
  }
  
  /**
   * Add the Nationality command if counter is not already concealed
   *  
   */
  public KeyCommand[] myGetKeyCommands() {

    KeyCommand[] oldCommands = super.myGetKeyCommands();
    KeyCommand[] newCommands;

    if (obscuredToMe() || obscuredToOthers()) {
      newCommands = oldCommands;
    }
    else {
      newCommands = new KeyCommand[oldCommands.length + 1];
      System.arraycopy(oldCommands, 0, newCommands, 0, oldCommands.length);
      newCommands[oldCommands.length] = new KeyCommand(NATIONALITY_COMMAND, NATIONALITY_KEY, Decorator
          .getOutermost(this));
    }

    return newCommands;
  }

  public Command myKeyEvent(KeyStroke stroke) {

    if (stroke.equals(NATIONALITY_KEY)) {
      return doPopup();
    }
    else {
      return super.myKeyEvent(stroke);
    }

  }

  protected Command doPopup() {

    if (popup == null) {
      buildPopup();
    }

    popup.show(getMap().getView(), getPosition().x, getPosition().y);
    return null;

  }

  protected void buildPopup() {
    popup = new JPopupMenu();

    popup.addPopupMenuListener(new javax.swing.event.PopupMenuListener() {
      public void popupMenuCanceled(javax.swing.event.PopupMenuEvent evt) {
        getMap().getView().repaint();
      }

      public void popupMenuWillBecomeInvisible(javax.swing.event.PopupMenuEvent evt) {
        getMap().getView().repaint();
      }

      public void popupMenuWillBecomeVisible(javax.swing.event.PopupMenuEvent evt) {
      }
    });

    for (int i = 0; i < NATIONS.length; i++) {
      addItem(NATIONS[i]);
    }
  }

  protected void addItem(String item) {
    JMenuItem catItem = new JMenuItem(item);
    catItem.addActionListener(this);
    popup.add(catItem);
  }

  public void actionPerformed(ActionEvent e) {

    String nationality = e.getActionCommand();
    for (int i = 0; i < NATIONS.length; i++) {
      if (NATIONS[i].equals(nationality)) {
        ChangeTracker tracker = new ChangeTracker(this);
        setNationality(PREFIXES[i]);
        Command c = tracker.getChangeCommand();
        GameModule.getGameModule().sendAndLog(c);
      }
    }

  }

  protected void setNationality(String nationality) {
    nation = nationality;
    imageName = nationality + "-conceal";
  }
}