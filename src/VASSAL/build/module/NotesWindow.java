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

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.Configurer;
import VASSAL.configure.TextConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.SequenceEncoder;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import java.awt.event.*;
import java.awt.*;
import java.io.File;
import java.net.MalformedURLException;
import java.util.Enumeration;
import java.util.Hashtable;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;

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
  private MutableComboBoxModel secretNotes;
  private SecretNotesControls secretControls;
  private LaunchButton launch;
  private TextConfigurer notes;
  private TextConfigurer privateNotes;
  private static final String NOTES = "NOTES\t";
  private static final String PRIVATE_NOTE = "PNOTE\t";
  private static final String SECRET_NOTE = "SNOTE\t";
  private static final String REVEAL_NOTE = "RNOTE\t";


  public NotesWindow() {
    frame = new NotesFrame();
    frame.setTitle("Notes");
    ActionListener al = new ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent e) {
        frame.setVisible(!frame.isShowing());
      }
    };
    launch = new LaunchButton(null, null, null, "icon", al);
    launch.setAttribute("icon", "/images/notes.gif");
    launch.setToolTipText("Notes");
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
      getContentPane().setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
      setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
      addWindowListener(new WindowAdapter() {
        public void windowClosing(WindowEvent e) {
          save();
          setVisible(false);
        }
      });

      notes = new TextConfigurer(null, null);
      privateNotes = new TextConfigurer(null, null);
      JTabbedPane tab = new JTabbedPane();
      getContentPane().add(tab);

      Box b = Box.createVerticalBox();
      b.add(new JLabel("Visible to all"));
      b.add(notes.getControls());
      tab.addTab("Public", b);

      b = Box.createVerticalBox();
      b.add(new JLabel("Invisible to others"));
      b.add(privateNotes.getControls());
      tab.addTab("Private", b);

      secretNotes = new DefaultComboBoxModel();
      secretControls = new SecretNotesControls(secretNotes);

      b = Box.createVerticalBox();
      JLabel l = new JLabel("Visible once revealed");
      l.setAlignmentX(0.0F);
      b.add(l);
      b.add(secretControls);
      tab.addTab("Delayed", b);

      JPanel p = new JPanel();
      JButton saveButton = new JButton("Ok");
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
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
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
    else if (c instanceof AddSecretNote) {
      SecretNote note = ((AddSecretNote) c).note;
      SequenceEncoder se = new SequenceEncoder('\t');
      s = SECRET_NOTE
          + se.append(note.getName())
          .append(note.getOwner())
          .append(note.isHidden() + "")
          .append(TextConfigurer.escapeNewlines(note.getText()))
          .getValue();
    }
    else if (c instanceof RevealNote) {
      s = REVEAL_NOTE + ((RevealNote) c).index;
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
    else if (command.startsWith(SECRET_NOTE)) {
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder
          (command.substring(SECRET_NOTE.length()), '\t');
      String name = st.nextToken();
      String owner = st.nextToken();
      boolean hidden = "true".equals(st.nextToken());
      String text = TextConfigurer.restoreNewlines(st.nextToken());
      return new AddSecretNote(name, owner, hidden, text);
    }
    else if (command.startsWith(REVEAL_NOTE)) {
      int index = Integer.parseInt(command.substring(REVEAL_NOTE.length()));
      return new RevealNote(index);
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
      while (secretNotes.getSize() > 0) {
        secretNotes.removeElementAt(0);
      }
    }
  }

  public Command getRestoreCommand() {
    Command c = new SetNote(notes.getValueString());
    for (Enumeration e = notesTable.keys();
         e.hasMoreElements();) {
      String player = (String) e.nextElement();
      c.append(new SetPrivate(player, (String) notesTable.get(player)));
    }
    for (int i = 0,n = secretNotes.getSize(); i < n; ++i) {
      SecretNote note = (SecretNote) secretNotes.getElementAt(i);
      c.append(new AddSecretNote(note.name, note.owner, note.hidden, note.text));
    }
    return c;
  }

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

  public static class SecretNote {
    private String owner;
    private String name;
    private String text;
    private boolean hidden = true;

    public SecretNote(String name, String owner, String text) {
      this.name = name;
      this.owner = owner;
      this.text = text;
    }

    public void reveal() {
      hidden = false;
    }

    public boolean isHidden() {
      return hidden;
    }

    public String getName() {
      return name;
    }

    public String getOwner() {
      return owner;
    }

    public String getText() {
      return text;
    }
  }

  private class SecretNotesControls extends JPanel implements ItemListener {
    private MutableComboBoxModel model;
    private JComboBox box;
    private JCheckBox status;
    private JTextArea text;

    public SecretNotesControls(MutableComboBoxModel model) {
      this.model = model;
      setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
      JButton newButton = new JButton("New");
      newButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          createNewNote();
        }
      });
      box = new JComboBox(model);
      box.addItemListener(this);
      box.setRenderer(new DefaultListCellRenderer() {
        public Component getListCellRendererComponent(
            JList list,
            Object value,
            int index,
            boolean isSelected,
            boolean cellHasFocus) {
          if (value instanceof SecretNote) {
            value = ((SecretNote) value).getName();
          }
          return super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
        }
      });
      Box b = Box.createHorizontalBox();
      b.setAlignmentX(0.0F);
      b.add(newButton);
      b.add(box);
      status = new JCheckBox("Revealed");
      status.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          revealSelectedNote();
        }
      });
      b.add(status);
      add(b);
      text = new JTextArea(6, 20);
      text.setEditable(false);
      JScrollPane scroll = new JScrollPane(text);
      scroll.setBorder(new TitledBorder("Text"));
      add(scroll);
    }

    private void revealSelectedNote() {
      int index = box.getSelectedIndex();
      if (index >= 0) {
        SecretNote note = (SecretNote) model.getElementAt(index);
        String msg = " - " + GameModule.getGameModule().getChatter().getHandle() + " has revealed message \'" + note.getName() + "\'";
        Command c = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), msg);
        c.append(new RevealNote(index));
        c.execute();
        GameModule.getGameModule().sendAndLog(c);
      }
    }

    public void createNewNote() {
      Frame parent = (Frame) SwingUtilities.getAncestorOfClass(Frame.class, this);
      final JDialog d = new JDialog(parent, true);
      d.setTitle("Delayed Note");
      final StringConfigurer name = new StringConfigurer(null, "Name");
      final TextConfigurer text = new TextConfigurer(null, "Text");
      d.getContentPane().setLayout(new BoxLayout(d.getContentPane(), BoxLayout.Y_AXIS));
      d.getContentPane().add(name.getControls());
      d.getContentPane().add(text.getControls());
      Box buttonPanel = Box.createHorizontalBox();
      final JButton okButton = new JButton("Ok");
      okButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          String msg = " - " + GameModule.getGameModule().getChatter().getHandle() + " has created message \'" + name.getValueString() + "\'";
          Command c = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), msg);
          c.append(new AddSecretNote(name.getValueString(), GameModule.getUserId(), true, (String) text.getValue()));
          c.execute();
          GameModule.getGameModule().sendAndLog(c);
          d.dispose();
        }
      });
      PropertyChangeListener l = new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          okButton.setEnabled(name.getValueString() != null
                              && name.getValueString().length() > 0
                              && text.getValueString() != null
                              && text.getValueString().length() > 0);
        }
      };
      name.addPropertyChangeListener(l);
      text.addPropertyChangeListener(l);
      okButton.setEnabled(false);
      buttonPanel.add(okButton);
      JButton cancelButton = new JButton("Cancel");
      cancelButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          d.dispose();
        }
      });
      d.getContentPane().add(buttonPanel);
      d.pack();
      d.setLocationRelativeTo(d.getOwner());
      d.setVisible(true);
    }

    public void itemStateChanged(ItemEvent e) {
      displaySelected();
    }

    private void displaySelected() {
      int index = box.getSelectedIndex();
      if (index >= 0) {
        SecretNote note = (SecretNote) model.getElementAt(index);
        status.setSelected(!note.isHidden());
        if (note.getOwner().equals(GameModule.getUserId())) {
          text.setText(note.getText());
          status.setEnabled(note.isHidden());
        }
        else {
          text.setText(note.isHidden() ? "<message not revealed>" : note.getText());
          status.setEnabled(false);
        }
      }
      else {
        text.setText("");
        status.setEnabled(false);
      }
    }
  }

  private class AddSecretNote extends Command {
    private SecretNote note;

    public AddSecretNote(String name, String owner, boolean hidden, String text) {
      note = new SecretNote(name,owner,text);
      if (!hidden) {
        note.reveal();
      }
    }

    protected void executeCommand() {
      secretNotes.addElement(note);
    }

    protected Command myUndoCommand() {
      return null;
    }
  }

  private class RevealNote extends Command {
    int index;

    public RevealNote(int index) {
      this.index = index;
    }

    protected void executeCommand() {
      if (index >= 0 && index < secretNotes.getSize()) {
        SecretNote note = (SecretNote) secretNotes.getElementAt(index);
        note.reveal();
        secretControls.displaySelected();
      }
    }

    protected Command myUndoCommand() {
      return null;
    }
  }
}
