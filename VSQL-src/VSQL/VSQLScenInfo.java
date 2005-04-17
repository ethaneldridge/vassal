/*
 * $Id$
 *
 * Copyright (c) 2000-2005 by Rodney Kinney and Brent Easton
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
package VSQL;

import VASSAL.build.AbstractBuildable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.Configurer;
import VASSAL.configure.TextConfigurer;
import VASSAL.tools.KeyStrokeListener;
import VASSAL.tools.SequenceEncoder;

import javax.swing.*;
import javax.swing.border.TitledBorder;

import java.awt.*;
import java.awt.event.*;
import java.util.Hashtable;
import java.util.Enumeration;
import java.util.StringTokenizer;

public class VSQLScenInfo extends AbstractBuildable implements GameComponent, CommandEncoder {

 
  private JTextField AxisELR, AxisSAN, AlliedELR, AlliedSAN;
  private FixedTextConfigurer notes;
  private Hashtable privateNotes = new Hashtable();
  private JComboBox movesFirst;
  private JButton launch;
  private JButton nextTurn;
  private JFrame frame;

  private TurnMarker turn;

  private KeyStrokeListener keyListener;
  private AbstractAction launchAction;

  private int axisSAN, alliedSAN;
  private TextConfigurer myPrivate;

  public VSQLScenInfo() {
    frame = new JFrame("Scenario Information");
    frame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);

    launch = new JButton("Info");
    launch.setAlignmentY(0.0F);
    launch.setToolTipText("Scenario Info Window [F7]");
    launchAction = new AbstractAction() {
      public void actionPerformed(ActionEvent e) {
        frame.setVisible(!frame.isShowing());
      }
    };
    launch.addActionListener(launchAction);
    launchAction.setEnabled(false);
    launch.setEnabled(false);

    keyListener = new KeyStrokeListener(launchAction);
    keyListener.setKeyStroke(KeyStroke.getKeyStroke(KeyEvent.VK_F7, 0, false));

    turn = new TurnMarker("Axis", "Axis", 1);
    AxisELR = new JTextField(" ? ");
    AxisELR.setMaximumSize(AxisELR.getPreferredSize());
    AlliedELR = new JTextField(" ? ");
    AlliedELR.setMaximumSize(AlliedELR.getPreferredSize());
    AxisSAN = new JTextField(" ? ");
    AxisSAN.setMaximumSize(AxisSAN.getPreferredSize());
    AlliedSAN = new JTextField(" ? ");
    AlliedSAN.setMaximumSize(AlliedSAN.getPreferredSize());
    movesFirst = new JComboBox();
    movesFirst.addItemListener(new ItemListener() {
      public void itemStateChanged(ItemEvent evt) {
        turn.current = 1;
        String s = (String) movesFirst.getSelectedItem();
        int len = (s.startsWith("Ax") ? 4 : 6);
        turn.movesFirst = s.substring(0, len);
        turn.player = s.substring(0, len);
        turn.repaint();
      }
    });
    movesFirst.addItem("Axis moves first");
    movesFirst.addItem("Allied moves first");
    movesFirst.setSelectedIndex(0);

    frame.getContentPane().setLayout(new BoxLayout(frame.getContentPane(), BoxLayout.Y_AXIS));

    Box b = Box.createHorizontalBox();
    b.add(movesFirst);
    b.add(turn);

    nextTurn = new JButton("Next Turn");
    nextTurn.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        turn.advance();
      }
    });
    b.add(nextTurn);
    
    Dimension base = new Dimension(300, 50);
    b.setMaximumSize(base);
    frame.getContentPane().add(b);

    b = Box.createHorizontalBox();
    b.add(new JLabel("Axis ELR: "));
    b.add(AxisELR);
    b.add(new JLabel("Axis SAN: "));
    b.add(AxisSAN);
    // frame.getContentPane().add(b); Leave out for VSQL

    b = Box.createHorizontalBox();
    b.add(new JLabel("Allied ELR: "));
    b.add(AlliedELR);
    b.add(new JLabel("Allied SAN: "));
    b.add(AlliedSAN);
    // frame.getContentPane().add(b); Leave out for VSQL

   
    notes = new FixedTextConfigurer(null, "Notes: ");
    myPrivate = new TextConfigurer(null, "Private notes: ");
    
    JTabbedPane tab = new JTabbedPane();
    tab.setPreferredSize(new Dimension(650, 650));
    tab.addTab("Scenario Notes", null, notes.getControls(), "Scenario Notes");
    tab.addTab("Private Notes", null, myPrivate.getControls(), "Private Notes");

    frame.getContentPane().add(tab);
    
    JPanel p = new JPanel();
    JButton saveButton = new JButton("Save");
    saveButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        save();
        frame.setVisible(false);
      }
    });
    p.add(saveButton);
    p.setMaximumSize(base);
    
    frame.getContentPane().add(p);
    
    frame.addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
        save();
        frame.setVisible(false);
      }
    });

    frame.pack();
  }

  private void save() {
    privateNotes.put(GameModule.getUserId(), myPrivate.getValue());
    GameModule.getGameModule().sendAndLog(new SetInfo(getState(), this));
  }

  public void addTo(Buildable b) {
    GameModule.getGameModule().getToolBar().add(launch);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    GameModule.getGameModule().addKeyStrokeListener(keyListener);
    GameModule.getGameModule().addCommandEncoder(this);
  }

  public void setState(String in) {
    reset();

    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(in, '\t');
    String mf = "Axis",pl = "Axis",c = "1",xELR = "?",lELR = "?",xSAN = "?",lSAN = "?";
    try {
      mf = st.nextToken();
      pl = st.nextToken();
      c = st.nextToken();
      xELR = st.nextToken();
      xSAN = st.nextToken();
      lELR = st.nextToken();
      lSAN = st.nextToken();
    }
    catch (Exception e) {
    }

    turn.movesFirst = mf;
    movesFirst.setSelectedItem(mf + " moves first");
    turn.player = pl;
    turn.current = Integer.parseInt(c);
    AxisELR.setText(xELR);
    AlliedELR.setText(lELR);
    AxisSAN.setText(xSAN);
    AlliedSAN.setText(lSAN);

    turn.repaint();

    axisSAN = getSAN(xSAN);
    alliedSAN = getSAN(lSAN);

    if (st.hasMoreTokens()) {
      notes.setValue(st.nextToken());
    }
    while (st.hasMoreTokens()) {
      String id = st.nextToken();
      String encodedNotes = st.nextToken();
      StringBuffer buffer = new StringBuffer();
      SequenceEncoder.Decoder st2 = new SequenceEncoder.Decoder(encodedNotes,'|');
      while (st2.hasMoreTokens()) {
        buffer.append(st2.nextToken());
        if (st2.hasMoreTokens()) {
          buffer.append('\n');
        }
      }
      if (id.equals(GameModule.getUserId())) {
        myPrivate.setValue(buffer.toString());
      }
      privateNotes.put(id,buffer.toString());
    }
  }

  public static final String COMMAND_PREFIX = "INFO\t";

  public Command decode(String command) {
    if (command.startsWith(COMMAND_PREFIX)) {
      return new SetInfo(command.substring(COMMAND_PREFIX.length()), this);
    }
    else if (command.startsWith("NOTES")) { // VASL 3.0 format
      return new AppendNotes(command.substring("NOTES".length()));
    }
    else {
      return null;
    }
  }

  public String encode(Command c) {
    if (c instanceof SetInfo) {
      return COMMAND_PREFIX + ((SetInfo) c).getState();
    }
    else {
      return null;
    }
  }

  public int getAxisSAN() {
    return axisSAN;
  }

  public int getAlliedSAN() {
    return alliedSAN;
  }

  private int getSAN(String s) {
    int n = 0;
    try {
      n = Integer.parseInt(s.trim());
    }
    catch (Exception e) {
    }
    return n;
  }

  public void setup(boolean show) {
    launch.setEnabled(show);
    launchAction.setEnabled(show);
    if (!show) {
      reset();
      frame.setVisible(false);
    }
  }

  public Command getRestoreCommand() {
    return new SetInfo(getState(), this);
  }

  public void reset() {
    notes.setValue("");
    myPrivate.setValue("");
    privateNotes.clear();
  }

  public String[] getAttributeNames() {
    return new String[0];
  }

  public void setAttribute(String name, Object value) {
  }

  public String getAttributeValueString(String name) {
    return null;
  }

  public String getState() {
    axisSAN = getSAN(AxisSAN.getText());
    alliedSAN = getSAN(AlliedSAN.getText());

    SequenceEncoder se = new SequenceEncoder('\t');
    se.append(turn.movesFirst).append(turn.player).append("" + turn.current)
        .append(AxisELR.getText()).append(AxisSAN.getText())
        .append(AlliedELR.getText()).append(AlliedSAN.getText());
    se.append(notes.getValueString());
    for (Enumeration e = privateNotes.keys(); e.hasMoreElements();) {
      String id = (String) e.nextElement();
      String notes = (String) privateNotes.get(id);
      if (notes != null && notes.length() > 0) {
        SequenceEncoder se2 = new SequenceEncoder('|');
        StringTokenizer st = new StringTokenizer(notes,"\r\n");
        while (st.hasMoreTokens()) {
          se2.append(st.nextToken());
        }
        se.append(id);
        se.append(se2.getValue());
      }
    }
    return se.getValue();
  }

  public static class SetInfo extends Command {
    private VSQLScenInfo info;
    private String oldState;
    private String newState;

    public SetInfo(String value, VSQLScenInfo info) {
      newState = value;
      oldState = info.getState();
      this.info = info;
    }

    public String getState() {
      return newState;
    }

    protected void executeCommand() {
      info.setState(newState);
    }

    protected Command myUndoCommand() {
      return new SetInfo(oldState, info);
    }
  }

  class AppendNotes extends Command {
    private String newLine;

    AppendNotes(String s) {
      newLine = s;
    }

    protected void executeCommand() {
      notes.setValue((String) notes.getValue() + '\n' + newLine);
    }

    protected Command myUndoCommand() {
      return null;
    }
  }
  
  public class FixedTextConfigurer extends Configurer {
    private JTextArea textArea;
    private JPanel p;

    public FixedTextConfigurer(String key, String name) {
      this(key, name, "");
    }

    public FixedTextConfigurer(String key, String name, String val) {
      super(key, name, val);
    }

    public String getValueString() {
      return TextConfigurer.escapeNewlines((String) getValue());
    }

//    public String escapeNewlines(String s) {
//      SequenceEncoder se = new SequenceEncoder('|');
//      StringTokenizer st = new StringTokenizer(s, "\n\r");
//      while (st.hasMoreTokens()) {
//        se.append(st.nextToken());
//      }
//      return se.getValue() == null ? "" : se.getValue();
//    }

    public void setValue(String s) {
      String text = TextConfigurer.restoreNewlines(s);
      setValue((Object) text);
    }

    public void setValue(Object o) {
      super.setValue(o);
      if (!noUpdate && textArea != null) {
        textArea.setText((String) o);
      }
    }

//    public String restoreNewlines(String s) {
//      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, '|');
//      String text = "";
//      while (st.hasMoreTokens()) {
//        text += st.nextToken();
//        if (st.hasMoreTokens()) {
//          text += "\n";
//        }
//      }
//      return text;
//    }

    public java.awt.Component getControls() {
      if (p == null) {
        p = new JPanel();
        p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));
        textArea = new JTextArea(6, 20);
        textArea.setFont(new Font("monospaced", Font.PLAIN, 12));
        textArea.addKeyListener(new java.awt.event.KeyAdapter() {
          public void keyReleased(java.awt.event.KeyEvent evt) {
            noUpdate = true;
            setValue((Object) textArea.getText());
            noUpdate = false;
          }
        });
        textArea.setText((String) getValue());
        JScrollPane scroll = new JScrollPane(textArea);
        if (name != null) {
          scroll.setBorder(new TitledBorder(name));
        }
        p.add(scroll);
      }
      return p;
    }
  }

}

class TurnMarker extends Canvas {
  int current;
  String player, movesFirst;

  TurnMarker(String f, String p, int c) {
    movesFirst = f;
    player = p;
    current = c;
  }

  public void paint(Graphics g) {
    FontMetrics fm;
    g.setColor(Color.white);
    g.fillRect(0, 0, getSize().width - 1, getSize().height - 1);
    g.setColor(Color.black);
    g.drawRect(0, 0, getSize().width - 1, getSize().height - 1);
    fm = g.getFontMetrics();
    g.drawString(player, getSize().width / 2 - fm.stringWidth(player) / 2,
                 getSize().height - 2);
    g.setFont(new Font("TimesRoman", Font.BOLD, 18));
    fm = g.getFontMetrics();
    g.drawString("" + current,
                 getSize().width / 2 - fm.stringWidth("" + current) / 2,
                 getSize().height / 2 + fm.getAscent() / 2);
  }

  public void advance() {
    current += (movesFirst.equals(player) ? 0 : 1);
    player = (player.equals("Axis") ? "Allied" : "Axis");
    repaint();
  }

  public Dimension getPreferredSize() {
    return (new Dimension(48, 48));
  }
  
  
}
