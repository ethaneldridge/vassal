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

import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.Configurer;
import VASSAL.tools.SequenceEncoder;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.net.MalformedURLException;
import java.util.Enumeration;
import java.util.Vector;

/**
 * Maintains a list of players involved in the current game
 */
public class PlayerRoster implements Configurable, CommandEncoder, GameComponent {
  public static final String COMMAND_PREFIX = "PLAYER\t";
  protected Vector players = new Vector();
  protected DefaultListModel sides = new DefaultListModel();
  protected JButton retireButton = new JButton("Retire");

  public void removeFrom(Buildable parent) {
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    GameModule.getGameModule().removeCommandEncoder(this);
  }

  public void remove(Buildable child) {
  }

  public void build(Element e) {
    if (e != null) {
      NodeList n = e.getElementsByTagName("*");
      for (int i = 0; i < n.getLength(); ++i) {
        Element el = (Element) n.item(i);
        sides.addElement(Builder.getText(el));
      }
    }
  }

  public String getConfigureName() {
    return null;
  }

  public static String getConfigureTypeName() {
    return "Definition of Player Sides";
  }

  public void add(Buildable child) {
  }

  public Configurable[] getConfigureComponents() {
    return new Configurable[0];
  }

  public Element getBuildElement(Document doc) {
    Element el = doc.createElement(getClass().getName());
    for (Enumeration e = sides.elements(); e.hasMoreElements();) {
      Element sub = doc.createElement("entry");
      sub.appendChild(doc.createTextNode((String) e.nextElement()));
      el.appendChild(sub);
    }
    return el;
  }

  public Configurer getConfigurer() {
    return new Con();
  }

  public void addPropertyChangeListener(PropertyChangeListener l) {
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "GameModule.htm"), "#Definition_of_Player_Sides");
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void addTo(Buildable b) {
    retireButton.setToolTipText("Allow another player to take your side in this game");
    retireButton.setVisible(false);
    retireButton.setAlignmentY(0.0F);
    retireButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        String mySide = getMySide();
        if (mySide != null
            && JOptionPane.YES_OPTION
            == JOptionPane.showConfirmDialog(null, "Give up your position as '" + mySide + "'?",
                                             "Retire", JOptionPane.YES_NO_OPTION)) {
          remove(GameModule.getUserId());
        }
      }
    });
    GameModule.getGameModule().getGameState().addGameComponent(this);
    GameModule.getGameModule().addCommandEncoder(this);
    GameModule.getGameModule().getToolBar().add(retireButton);
  }

  public static boolean isActive() {
    return GameModule.getGameModule().getComponents(PlayerRoster.class).hasMoreElements();
  }

  public static String getMySide() {
    Enumeration e = GameModule.getGameModule().getComponents(PlayerRoster.class);
    if (e.hasMoreElements()) {
      PlayerRoster r = (PlayerRoster) e.nextElement();
      Entry[] players = r.getPlayers();
      for (int i = 0; i < players.length; ++i) {
        if (players[i].playerId.equals(GameModule.getUserId())) {
          return players[i].side;
        }
      }
    }
    return null;
  }

  public Entry[] getPlayers() {
    Entry[] p = new Entry[players.size()];
    for (int i = 0; i < p.length; ++i) {
      p[i] = (Entry) players.elementAt(i);
    }
    return p;
  }

  public void add(String playerId, String playerName, String side) {
    Entry e = new Entry(playerId, playerName, side);
    if (players.contains(e)) {
      players.setElementAt(e, players.indexOf(e));
    }
    else {
      players.addElement(e);
    }
  }

  public void remove(String playerId) {
    Entry e = new Entry(playerId, null, null);
    players.removeElement(e);
  }

  public Command decode(String command) {
    if (command.startsWith(COMMAND_PREFIX)) {
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(command, '\t');
      st.nextToken();
      return new Add(this, st.nextToken(), st.nextToken(), st.nextToken());
    }
    else {
      return null;
    }
  }

  public String encode(Command c) {
    if (c instanceof Add) {
      SequenceEncoder se = new SequenceEncoder('\t');
      se.append(((Add) c).id);
      se.append(((Add) c).name);
      se.append(((Add) c).side);
      return COMMAND_PREFIX + se.getValue();
    }
    else {
      return null;
    }
  }

  public Command getRestoreCommand() {
    Command c = null;
    for (Enumeration e = players.elements();
         e.hasMoreElements();) {
      Entry entry = (Entry) e.nextElement();
      Command sub = new Add(this, entry.playerId, entry.playerName, entry.side);
      c = c == null ? sub : c.append(sub);
    }
    return c;
  }

  public void setup(boolean gameStarting) {
    if (gameStarting) {
      GameModule gm = GameModule.getGameModule();
      Entry me = new Entry(gm.getUserId(), GlobalOptions.getInstance().getPlayerId(), null);
      if (players.contains(me)) {
        Entry saved = (Entry) players.elementAt(players.indexOf(me));
        saved.playerName = me.playerName;
      }
      else {
        me.side = new Pick().getValue();
        if (me.side != null) {
          Add a = new Add(this, me.playerId, me.playerName, me.side);
          a.execute();
          gm.getServer().sendToOthers(a);
        }
      }
    }
    else {
      players.removeAllElements();
    }
    retireButton.setVisible(gameStarting && getMySide() != null);
  }

  public static class Entry {
    public String playerId;
    public String playerName;
    public String side;

    public Entry(String id, String name, String side) {
      playerId = id;
      playerName = name;
      this.side = side;
    }

    public boolean equals(Object o) {
      if (o instanceof Entry && playerId != null) {
        return playerId.equals(((Entry) o).playerId);
      }
      else {
        return false;
      }
    }
  }

  public static class Add extends Command {
    private PlayerRoster roster;
    private String id, name, side;

    public Add(PlayerRoster r, String playerId, String playerName, String side) {
      roster = r;
      id = playerId;
      name = playerName;
      this.side = side;
    }

    protected void executeCommand() {
      roster.add(id, name, side);
    }

    protected Command myUndoCommand() {
      return null;
    }
  }

  private class Con extends Configurer {
    private Con() {
      super(null, null);
    }

    public String getValueString() {
      return null;
    }

    public void setValue(String s) {
    }

    public Component getControls() {
      JPanel panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
      panel.add(new JLabel("List of sides available to players:"));
      final JList l = new JList(sides);
      panel.add(new JScrollPane(l));
      Box b = Box.createHorizontalBox();
      final JTextField tf = new JTextField(8);
      ActionListener al = new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          String s = tf.getText();
          if (s.length() > 0
              && !sides.contains(s)
              && !sides.equals(OBSERVER)) {
            sides.addElement(s);
          }
          tf.setText("");
        }
      };
      JButton addButton = new JButton("Add");
      addButton.addActionListener(al);
      tf.addActionListener(al);
      b.add(tf);
      b.add(addButton);
      JButton removeButton = new JButton("Remove");
      removeButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          Object[] o = l.getSelectedValues();
          for (int i = 0; i < o.length; ++i) {
            sides.removeElement(o[i]);
          }
        }
      });
      b.add(removeButton);
      panel.add(b);
      return panel;
    }
  }

  private static String OBSERVER = "<observer>";

  private class Pick extends JDialog {
    private JComboBox box;

    private Pick() {
      super((java.awt.Frame) GameModule.getGameModule().getFrame(), "Choose Side", true);
      getContentPane().setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
      getContentPane().add(new JLabel("Join game as which side?"));
      Vector alreadyTaken = new Vector();
      for (int i = 0; i < players.size(); ++i) {
        alreadyTaken.addElement(((Entry) players.elementAt(i)).side);
      }
      Vector v = new Vector();
      v.addElement(OBSERVER);
      for (Enumeration e = sides.elements(); e.hasMoreElements();) {
        String side = (String) e.nextElement();
        if (!alreadyTaken.contains(side)) {
          v.addElement(side);
        }
      }
      box = new JComboBox(v);
      getContentPane().add(box);
      JButton b = new JButton("Ok");
      b.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          dispose();
        }
      });
      getContentPane().add(b);
      setLocationRelativeTo(GameModule.getGameModule().getFrame());
      pack();
      if (v.size() > 1) {
        setVisible(true);
      }
    }

    public String getValue() {
      String val = (String) box.getSelectedItem();
      return OBSERVER.equals(val) ? null : val;
    }
  }
}
