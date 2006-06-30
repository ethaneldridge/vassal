/*
 * $Id$
 *
 * Copyright (c) 2004 by Rodney Kinney
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
package VASSAL.counters;

import VASSAL.build.GameModule;
import VASSAL.build.module.GameState;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.DrawPile;
import VASSAL.build.module.map.CCGPile;
import VASSAL.command.AddPiece;
import VASSAL.command.ChangePiece;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.ColorConfigurer;
import VASSAL.tools.FormattedString;
import VASSAL.tools.PlayerIdFormattedString;
import VASSAL.tools.SequenceEncoder;

import javax.swing.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;

/**
 * A collection of pieces that behaves like a deck, i.e.:
 * Doesn't move.
 * Can't be expanded.
 * Can be shuffled.
 * Can be turned face-up and face-down
 */

public class CCGDeck extends Deck {
	public CCGDeck() {
	    this(ID);
	  }

	  public CCGDeck(String type) {
	    mySetType(type);
	  }

	 private boolean shouldConfirmOverwrite() {
		    return System.getProperty("os.name").trim().equalsIgnoreCase("linux");
		  }
	
	private File getSaveFileName() {
	    File outputFile = null;
	    FileDialog fd = GameModule.getGameModule().getFileDialog();
	    String name = fd.getFile();
	    if (name != null) {
	      int index = name.lastIndexOf('.');
	      if (index > 0) {
	        name = name.substring(0, index) + ".sav";
	        fd.setFile(name);
	      }
	    }
	    fd.setMode(FileDialog.SAVE);
	    fd.setVisible(true);
	    if (fd.getFile() != null) {
	      if (fd.getDirectory() != null) {
	        outputFile = new File(new File(fd.getDirectory()), fd.getFile());
	      }
	      else {
	        outputFile = new File(fd.getFile());
	      }
	      if (outputFile.exists()
	          && shouldConfirmOverwrite()
	          && JOptionPane.NO_OPTION
	          == JOptionPane.showConfirmDialog(GameModule.getGameModule().getFrame(),
	                                           "Overwrite " + outputFile.getName() + "?", "File Exists", JOptionPane.YES_NO_OPTION)) {
	        outputFile = null;
	      }
	    }
	    return outputFile;
	  }

	private Command saveDeck() {
		Command c = new NullCommand();
			GameModule.getGameModule().warn("Saving deck ...");
			try {
		    File saveFile = getSaveFileName();
		      if (saveFile != null) {
		        saveDeck(saveFile);
		        GameModule.getGameModule().warn("deck Saved");
		      }
		      else {
		        GameModule.getGameModule().warn("Save Canceled");
		      }
			}
		    catch (IOException err) {
		      GameModule.getGameModule().warn("Save Failed.  Try again.");
		    }
			return c;
	}
	public  void saveDeck(File f) throws IOException{
		Command comm = new NullCommand();
		
		FileWriter dest = new FileWriter(f);
		ChangeTracker t = new ChangeTracker(this);
		
		
		
		for (Enumeration e = getPieces(); e.hasMoreElements();) {
		      GamePiece p = (GamePiece) e.nextElement();
		      comm = comm.append(new AddPiece(p));
		      
		      }
		comm.append(new ChangePiece(this.getId(),null,getState()));
		dest.write(GameModule.getGameModule().encode(comm));
	    dest.close();
		}
	private File getLoadFileName() {
	    File inputFile = null;
	    FileDialog fd = GameModule.getGameModule().getFileDialog();
	    String name = fd.getFile();
	    if (name != null) {
	      int index = name.lastIndexOf('.');
	      if (index > 0) {
	        name = name.substring(0, index) + ".sav";
	        fd.setFile(name);
	      }
	    }
	    fd.setMode(FileDialog.LOAD);
	    fd.setVisible(true);
	    if (fd.getFile() != null) {
	      if (fd.getDirectory() != null) {
	        inputFile = new File(new File(fd.getDirectory()), fd.getFile());
	      }
	      else {
	        inputFile = new File(fd.getFile());
	      }
	    }  
	    return inputFile;
	  }    
	private Command loadDeck() {
		Command c = new NullCommand();
		GameModule.getGameModule().warn("Loading deck ...");
		try {
	    File saveFile = getLoadFileName();
	      if (saveFile != null) {
	        LoadDeck(saveFile);
	        GameModule.getGameModule().warn("deck Loaded");
	      }
	      else {
	        GameModule.getGameModule().warn("Load Canceled");
	      }
		}
	    catch (IOException err) {
	      GameModule.getGameModule().warn("Load Failed.  Try again.");
	    }
		return c;
		
	}
	public void LoadDeck(File f) throws IOException{
		int size = (int) f.length();
		FileInputStream src = new FileInputStream(f);
		byte[]data = new byte[size];
		String myString; 
		src.read(data,0,size);
		myString = new String(data,0);
			    
		GameModule.getGameModule().decode(myString).execute();
	
		src.close();
	
		
	}
	 public Object getProperty(Object key) {
		    Object value = null;
		    if (Properties.NO_STACK.equals(key)) {
		      value = Boolean.TRUE;
		    }
		    else if (Properties.KEY_COMMANDS.equals(key)) {
		      value = getKeyCommands();
		    }
		    return value;
		  }
	  protected KeyCommand[] getKeyCommands() {
		  if (commands == null) {
			  ArrayList l = new ArrayList();
			  KeyCommand c = null;
			  
		      if (USE_MENU.equals(shuffleOption)) {
		          c = new KeyCommand("Shuffle", null, this) {
		            public void actionPerformed(ActionEvent e) {
		              GameModule.getGameModule().sendAndLog(shuffle());
		              map.repaint();
		            }
		          };
		          l.add(c);
		        }
		        if (reshuffleCommand.length() > 0) {
		          c = new KeyCommand(reshuffleCommand, null, this) {
		            public void actionPerformed(ActionEvent evt) {
		              GameModule.getGameModule().sendAndLog(sendToDeck());
		              map.repaint();
		            }
		          };
		          l.add(c);
		        }
		        if (USE_MENU.equals(faceDownOption)) {
		          KeyCommand faceDownAction = new KeyCommand(faceDown ? "Face up" : "Face down", null, this) {
		            public void actionPerformed(ActionEvent e) {
		              Command c = setContentsFaceDown(!faceDown);
		              GameModule.getGameModule().sendAndLog(c);
		              map.repaint();
		            }
		          };
		          l.add(faceDownAction);
		        }
		        if (reversible) {
		          c = new KeyCommand("Reverse order", null, this) {
		            public void actionPerformed(ActionEvent e) {
		              Command c = reverse();
		              GameModule.getGameModule().sendAndLog(c);
		              map.repaint();
		            }
		          };
		          l.add(c);
		        }
		        if (allowMultipleDraw) {
		          c = new KeyCommand("Draw multiple cards", null, this) {
		            public void actionPerformed(ActionEvent e) {
		              promptForDragCount();
		            }
		          };
		          l.add(c);
		        }
		        if (allowSelectDraw) {
		          c = new KeyCommand("Draw specific cards", null, this) {
		            public void actionPerformed(ActionEvent e) {
		              promptForNextDraw();
		              map.repaint();
		            }
		          };
		          l.add(c);
		        }
		        c = new KeyCommand("Save", null, this) {
			          public void actionPerformed(ActionEvent e) {
			            GameModule.getGameModule().sendAndLog(saveDeck());
			            map.repaint();
			          }

			    };
			    l.add(c);
			    c = new KeyCommand("Load", null, this) {
			          public void actionPerformed(ActionEvent e) {
			            GameModule.getGameModule().sendAndLog(loadDeck());
			            map.repaint();
			          }
			    };
			    l.add(c);
		        commands = (KeyCommand[]) l.toArray(new KeyCommand[l.size()]);
		      }
		      for (int i = 0; i < commands.length; ++i) {
		        if ("Face up".equals(commands[i].getValue(Action.NAME)) && !faceDown) {
		          commands[i].putValue(Action.NAME, "Face down");
		        }
		        else if ("Face down".equals(commands[i].getValue(Action.NAME)) && faceDown) {
		          commands[i].putValue(Action.NAME, "Face up");
		        }
		      
		    }           
		    return commands;
		  }
  
  
}

