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
package VASSAL.build.module;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Configurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.tools.*;
import VASSAL.configure.*;
import VASSAL.command.*;

import java.util.*;
import java.awt.event.*;
import java.net.URL;
import java.net.MalformedURLException;
import java.io.File;
import javax.swing.*;

import org.w3c.dom.*;

/**
 * This is a {@link GameComponent} that allows players to type and
 * save text notes during a game.  There is one set of shared public
 * notes, and each player has a set of private notes visible only to
 * him
 */
public class NotesWindow extends AbstractConfigurable
  implements GameComponent, CommandEncoder {

  private JFrame frame;
  private Hashtable notesTable = new Hashtable();
  private LaunchButton launch;

  public NotesWindow() {
    frame = new NotesFrame();
    frame.setTitle("Notes");
    ActionListener al = new ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent e) {
        frame.setVisible(!frame.isShowing());
      }
    };
    launch = new LaunchButton("Notes", null, null, al);
    frame.pack();
    setup(false);
  }

  private void save() {
    notesTable.put(GameModule.getGameModule().getUserId(),
                   privateNotes.getValueString());
    GameModule.getGameModule().sendAndLog(getRestoreCommand());
  }

  private class NotesFrame extends JFrame {
    private NotesFrame() {
      initComponents();
    }

    private void initComponents() {
      setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
      jLabel1 = new javax.swing.JLabel();
      jScrollPane1 = new javax.swing.JScrollPane();
      notes = new TextConfigurer(null, "Notes");
      jLabel3 = new javax.swing.JLabel();
      jScrollPane2 = new javax.swing.JScrollPane();
      privateNotes = new TextConfigurer(null, "Private Notes");
      getContentPane().setLayout(new javax.swing.BoxLayout(getContentPane(), 1));

      getContentPane().add(notes.getControls());

      getContentPane().add(privateNotes.getControls());

      JPanel p = new JPanel();
      JButton saveButton = new JButton("Save");
      p.add(saveButton);
      saveButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          save();
          setVisible(false);
        }
      });
      getContentPane().add(p);
    }
  }


  public HelpFile getHelpFile() {
    File dir = new File("docs");
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "GameModule.htm"), "#NotesWindow");
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public String[] getAttributeNames() {
    return new String[0];
  }

  public void setAttribute(String name, Object value) {
  }

  public String getAttributeValueString(String name) {
    return null;
  }

  private static final String NOTES = "NOTES\t";
  private static final String PRIVATE_NOTE = "PNOTE\t";

  public String encode(Command c) {
    String s = null;
    if (c instanceof SetNote) {
      s = NOTES + ((SetNote) c).msg;
    }
    else if (c instanceof SetPrivate) {
      SequenceEncoder se = new SequenceEncoder('\t');
      s = PRIVATE_NOTE +
        se.append(((SetPrivate) c).player)
        .append(((SetPrivate) c).msg)
        .getValue();
    }
    return s;
  }

  public Command decode(String command) {
    if (command.startsWith(NOTES)) {
      return new SetNote(command.substring(NOTES.length()));
    }
    else if (command.startsWith(PRIVATE_NOTE)) {
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder
        (command.substring(PRIVATE_NOTE.length()), '\t');
      String player = st.nextToken();
      String msg = st.hasMoreTokens() ? st.nextToken() : "";
      return new SetPrivate(player, msg);
    }
    else {
      return null;
    }
  }

  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  public Class[] getAttributeTypes() {
    return new Class[0];
  }

  public Configurer getConfigurer() {
    return null;
  }

  public Configurable[] getConfigureComponents() {
    return new Configurable[0];
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public static String getConfigureTypeName() {
    return "Notes Window";
  }

  /**
   * Expects to be added to a {@link VASSAL.build.GameModule}.  Adds a button to
   * the controls window toolbar to show the window containing the
   * notes */
  public void addTo(Buildable b) {
    GameModule.getGameModule().getToolBar().add(launch);
    launch.setAlignmentY(0.0F);
    GameModule.getGameModule().addCommandEncoder(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
  }

  public void removeFrom(Buildable b) {
    GameModule.getGameModule().getToolBar().remove(launch);
    GameModule.getGameModule().removeCommandEncoder(this);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
  }

  public void setup(boolean show) {
    launch.setEnabled(show);
    if (!show) {
      notesTable.clear();
      notes.setValue("");
      privateNotes.setValue("");
    }
  }

  public Command getRestoreCommand() {
    Command c = new SetNote(notes.getValueString());
    for (Enumeration e = notesTable.keys();
         e.hasMoreElements();) {
      String player = (String) e.nextElement();
      c.append(new SetPrivate(player, (String) notesTable.get(player)));
    }
    return c;
  }

  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JLabel jLabel1;
  private javax.swing.JScrollPane jScrollPane1;
  //    private javax.swing.JTextArea notes;
  private TextConfigurer notes;
  private javax.swing.JLabel jLabel3;
  private javax.swing.JScrollPane jScrollPane2;
  //    private javax.swing.JTextArea privateNotes;
  private TextConfigurer privateNotes;
  // End of variables declaration//GEN-END:variables

  private class SetNote extends Command {
    private String msg;

    private SetNote(String s) {
      msg = s;
    }

    protected void executeCommand() {
      notes.setValue(msg);
    }

    protected Command myUndoCommand() {
      return null;
    }
  }

  private class SetPrivate extends Command {
    private String msg;
    private String player;

    private SetPrivate(String player, String msg) {
      this.player = player;
      this.msg = msg;
    }

    protected void executeCommand() {
      notesTable.put(player, msg);
      if (player.equals(GameModule.getUserId())) {
        privateNotes.setValue(msg);
      }
    }

    protected Command myUndoCommand() {
      return null;
    }
  }
}
