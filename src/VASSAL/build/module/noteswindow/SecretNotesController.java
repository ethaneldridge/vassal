package VASSAL.build.module.noteswindow;

import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Chatter;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.TextConfigurer;
import VASSAL.tools.SequenceEncoder;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

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

public class SecretNotesController implements GameComponent, CommandEncoder, AddSecretNoteCommand.Interface {
  public static final String COMMAND_PREFIX = "SNOTE\t";

  private Controls controls;
  private JPanel panel;
  private ArrayList notes;
  private List lastSavedNotes;

  public SecretNotesController() {
    notes = new ArrayList();
    controls = new Controls();
  }

  public Command getRestoreCommand() {
    Command comm = null;
    for (Iterator iterator = notes.iterator(); iterator.hasNext();) {
      SecretNote note = (SecretNote) iterator.next();
      Command c = new AddSecretNoteCommand(this, note);
      if (comm == null) {
        comm = c;
      }
      else {
        comm.append(c);
      }
    }
    return comm;
  }

  public void setup(boolean gameStarting) {
    if (!gameStarting) {
      notes.clear();
      controls.refresh();
    }
  }

  public Command decode(String command) {
    Command comm = null;
    if (command.startsWith(COMMAND_PREFIX)) {
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder
          (command.substring(COMMAND_PREFIX.length()), '\t');
      String name = st.nextToken();
      String owner = st.nextToken();
      boolean hidden = "true".equals(st.nextToken());
      String text = TextConfigurer.restoreNewlines(st.nextToken());
      SecretNote note = new SecretNote(name, owner, text, hidden);
      comm = new AddSecretNoteCommand(this, note);
    }
    return comm;
  }

  public String encode(Command c) {
    String s = null;
    if (c instanceof AddSecretNoteCommand) {
      SecretNote note = ((AddSecretNoteCommand) c).getNote();
      SequenceEncoder se = new SequenceEncoder('\t');
      s = COMMAND_PREFIX +
          se.append(note.getName())
          .append(note.getOwner())
          .append("" + note.isHidden())
          .append(note.getText()).getValue();
    }
    return s;
  }

  public void addSecretNote(SecretNote note) {
    int index = notes.indexOf(note);
    if (index >= 0) {
      notes.set(index, note);
    }
    else {
      notes.add(note);
    }
    controls.refresh();
  }


  public JComponent getControls() {
    if (panel == null) {
      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
      JLabel l = new JLabel("Visible once revealed");
      l.setAlignmentX(0.0F);
      panel.add(l);
      panel.add(controls);
    }
    return panel;
  }

  public Command save() {
    Command comm = null;
    for (Iterator iterator = notes.iterator(); iterator.hasNext();) {
      SecretNote secretNote = (SecretNote) iterator.next();
      int index = lastSavedNotes.indexOf(secretNote);
      if (index < 0
          || ((SecretNote) lastSavedNotes.get(index)).isHidden() != secretNote.isHidden()) {
        Command c = new AddSecretNoteCommand(this, secretNote);
        if (comm == null) {
          comm = c;
        }
        else {
          comm.append(c);
        }
        String msg;
        if (index < 0) {
          msg = "- " + GameModule.getGameModule().getChatter().getHandle() + " has created message \'" + secretNote.getName() + "\'";
        }
        else {
          msg = "- " + GameModule.getGameModule().getChatter().getHandle() + " has revealed message \'" + secretNote.getName() + "\'";
        }
        c = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), msg);
        c.execute();
        comm.append(c);
      }
    }
    return comm;
  }

  public void captureState() {
    lastSavedNotes = (List) notes.clone();
  }

  public void restoreState() {
    notes.clear();
    notes.addAll(lastSavedNotes);
    controls.refresh();
  }

  private class Controls extends JPanel implements ItemListener {
    private JComboBox box;
    private JCheckBox status;
    private JTextArea text;

    public Controls() {
      setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
      JButton newButton = new JButton("New");
      newButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          createNewNote();
        }
      });
      box = new JComboBox();
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

    public void refresh() {
      Object selected = box.getSelectedItem();
      box.setModel(new DefaultComboBoxModel(notes.toArray()));
      box.setSelectedItem(notes.contains(selected) ? selected : null);
      displaySelected();
    }

    private void revealSelectedNote() {
      SecretNote note = (SecretNote) box.getSelectedItem();
      note = new SecretNote(note.getName(), note.getOwner(), note.getText(), false);
      if (note != null) {
        notes.set(notes.indexOf(note), note);
        refresh();
      }
    }

    public void createNewNote() {
      Dialog parent = (Dialog) SwingUtilities.getAncestorOfClass(Dialog.class, this);
      JDialog tmp = null;
      if (parent != null) {
        tmp = new JDialog(parent, true);
      }
      else {
        tmp = new JDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class, this), true);
      }
      final JDialog d = tmp;
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
          SecretNote note = new SecretNote(name.getValueString(), GameModule.getUserId(), (String) text.getValue(), true);
          if (notes.contains(note)) {
            JOptionPane.showMessageDialog(Controls.this, "A note of this name already exists");
          }
          else {
            notes.add(note);
            refresh();
            d.dispose();
          }
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
      SecretNote note = (SecretNote) box.getSelectedItem();
      if (note != null) {
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


}
