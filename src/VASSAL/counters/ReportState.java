/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
/*
 * Created by IntelliJ IDEA.
 * User: rkinney
 * Date: Oct 2, 2002
 * Time: 6:30:35 AM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.counters;

import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.MassKeyCommand;
import VASSAL.command.Command;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;

import javax.swing.*;
import java.awt.*;
import java.awt.event.InputEvent;
import java.io.File;
import java.net.MalformedURLException;

/**
 * A GamePiece with this trait will echo the piece's current name when any of a given key commands are pressed
 * (and after they take effect)
 */
public class ReportState extends Decorator implements EditablePiece {
  public static final String ID = "report;";
  private String keys = "";
  private String format1 = "$mapRef$: $newUnitName$";
  private String format2 = "$mapRef$: $newUnitName$";
  
  public ReportState() {
    this(ID, null);
  }

  public ReportState(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  public String getName() {
    return piece.getName();
  }

  protected KeyCommand[] myGetKeyCommands() {
    return new KeyCommand[0];
  }

  public String myGetState() {
    return "";
  }

  public String myGetType() {
    return ID + keys + ";" + format1 + ";" + format2;
  }

  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  public Command keyEvent(KeyStroke stroke) {
  	
  	FormattedString format = new FormattedString();
  	
  	String playerId = GlobalOptions.getPlayerId();
  	
  	// Retrieve the name, location and visibilty of the unit prior to the 
  	// trait being executed if it is outside this one.
  	
	String oldUnitName = (String) getProperty(GlobalOptions.INITIAL_NAME);
	String initialLoc = (String) getProperty(GlobalOptions.INITIAL_LOCATION);
	boolean isInitiallyInvisible = ((Boolean) getProperty(GlobalOptions.INITIAL_INVISIBILITY)).booleanValue();

    // The following line will execute the trait if it is inside this one
	Command c = super.keyEvent(stroke);
	
	boolean isFinallyInvisible = ((Boolean) getOutermost(this).getProperty(Properties.INVISIBLE_TO_OTHERS)).booleanValue();
	
	// Only make a report if:
	//  1. It's not part of a global command with Single Reporting on
	//  2. The piece is visible either before or after the trait was executed.
	
	if (!MassKeyCommand.suppressTraitReporting() && (!isInitiallyInvisible || !isFinallyInvisible)) {
	    GamePiece outer = getOutermost(this);
	  
	    //if (!Boolean.TRUE.equals(outer.getProperty(Properties.INVISIBLE_TO_OTHERS))) {
	    String location = "Offmap";
	    if (getMap() != null) {
		    location = getMap().locationName(getPosition());
	    }
		if (location != null) {
		  for (int i = 0; i < keys.length(); ++i) {
			if (stroke.equals(KeyStroke.getKeyStroke(keys.charAt(i), InputEvent.CTRL_MASK))) {
				
			  String newUnitName = getPieceName();
			  if (oldUnitName.equals(newUnitName)) {
			  	  format.setFormat(format1);
			  }
			  else {
			  	  format.setFormat(format2);
			  }
			  
			  //
			  // Find the Command Name
			  //
			  String commandName = "";
			  KeyCommand[] k = ((Decorator) outer).getKeyCommands();
			  for (int j = 0; j < k.length; j++) {
			  	KeyStroke commandKey = k[j].getKeyStroke();
			  	if (stroke.equals(commandKey)) {
			  		commandName = k[j].getName();
			  	}
			  }
			  
			  format.setProperty(GlobalOptions.PLAYER_ID, GlobalOptions.getPlayerId());
			  format.setProperty(GlobalOptions.OLD_UNIT_NAME, oldUnitName);
			  format.setProperty(GlobalOptions.UNIT_NAME, oldUnitName);
			  format.setProperty(GlobalOptions.NEW_UNIT_NAME, newUnitName);
			  format.setProperty(GlobalOptions.MAP_REF, initialLoc);
			  format.setProperty(GlobalOptions.FROM_MAP_REF, initialLoc);
			  format.setProperty(GlobalOptions.TO_MAP_REF, location);
			  format.setProperty(GlobalOptions.COMMAND_NAME, commandName);
			  
			  String reportText = format.getText();
			  	
			  if (reportText.length() > 0) {
			      Command display = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), "* " + reportText);
			      display.execute();
			      c = c == null ? display : c.append(display);
			  }
			  break;
			}
		  }
		}
	  //}
	}

//    Original Code  	
//    Command c = super.keyEvent(stroke);
//    if (getMap() != null) {
//      GamePiece outer = getOutermost(this);
//      if (!Boolean.TRUE.equals(outer.getProperty(Properties.OBSCURED_TO_OTHERS))
//        && !Boolean.TRUE.equals(outer.getProperty(Properties.OBSCURED_TO_ME))
//        && !Boolean.TRUE.equals(outer.getProperty(Properties.INVISIBLE_TO_OTHERS))) {
//        String location = getMap().locationName(getPosition());
//        if (location != null) {
//          for (int i = 0; i < keys.length(); ++i) {
//            if (stroke.equals(KeyStroke.getKeyStroke(keys.charAt(i), InputEvent.CTRL_MASK))) {
//              Command display = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), " * " + location + ":  " + outer.getName() + " * ");
//              display.execute();
//              c = c == null ? display : c.append(display);
//              break;
//            }
//          }
//        }
//      }
//    }

    return c;
  }
  
  protected String getPieceName() {
  	
  	String name = "";
  	
	Hideable.setAllHidden(true);
	Obscurable.setAllHidden(true);
	
	name = getOutermost(this).getName();
	
	Hideable.setAllHidden(false);
	Obscurable.setAllHidden(false);
	
	return name;
  }

  public void mySetState(String newState) {
  }

  public Shape getShape() {
    return piece.getShape();
  }

  public String getDescription() {
    return "Report Changes";
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir,"ReferenceManual");
    try {
      return new HelpFile(null,new File(dir,"ReportChanges.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public void mySetType(String type) {
    // keys = type.length() <= ID.length() ? "" : type.substring(ID.length());
	SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
	st.nextToken();
	if (st.hasMoreTokens()) {
		keys = st.nextToken();
	}
	if (st.hasMoreTokens()) {
		format1 = st.nextToken();
	}
	if (st.hasMoreTokens()) {
		format2 = st.nextToken();
	}	
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public static class Ed implements PieceEditor {

	StringConfigurer tf;
	StringConfigurer fmt, fmt2;  	
	private JPanel box;

    public Ed(ReportState piece) {

	  box = new JPanel();
	  box.setLayout(new BoxLayout(box,BoxLayout.Y_AXIS));    	
	  tf = new StringConfigurer(null,"Report when player presses CTRL-", piece.keys);
	  fmt = new FormattedStringConfigurer(null, "Report format, piece name unchanged", GlobalOptions.getTraitOptions());
	  fmt.setValue(piece.format1);
	  fmt2 = new FormattedStringConfigurer(null, "Report format, piece name changes", GlobalOptions.getTraitOptions());
	  fmt2.setValue(piece.format2);
      box.add(tf.getControls());
 	  box.add(fmt.getControls());
	  box.add(fmt2.getControls());     
    }

    public Component getControls() {
      return box;
    }

    public String getState() {
      return "";
    }

    public String getType() {
      return ID + tf.getValueString() + ";" + fmt.getValueString() + ";" + fmt2.getValueString();
    }
  }
}
