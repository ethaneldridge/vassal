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
package PB;

import java.awt.BorderLayout;
import java.awt.Canvas;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.net.MalformedURLException;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.border.TitledBorder;
import javax.swing.text.JTextComponent;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.noteswindow.PrivateNotesController;
import VASSAL.build.module.noteswindow.SecretNotesController;
import VASSAL.build.widget.HtmlChart.HtmlChartHyperlinkListener;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.command.NullCommand;
import VASSAL.configure.Configurer;
import VASSAL.configure.TextConfigurer;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.SequenceEncoder;

/**
 * This is a {@link GameComponent}that allows players to type and save text
 * notes during a game. There is one set of shared public notes, and each player
 * has a set of private notes visible only to him
 */
public class PBNotesWindow extends AbstractConfigurable implements GameComponent, CommandEncoder {

  private JDialog frame;
  private LaunchButton launch;
  private TextConfigurer notes;
  private PrivateNotesController privateNotes;
  private SecretNotesController secretNotes;
  private static final String COMMAND_PREFIX = "NOTES\t";
  public static final String HOT_KEY = "hotkey";
  private String lastSavedNotes;


  private static final String SCENARIO_COMMAND_PREFIX = "SCENARIO\t";
  private ScenarioNotesController scenarioNotes;

  private static final String TURN_COMMAND_PREFIX = "TURN\t";

  private static int DFLT_SIDE_1 = 0; // Allies
  private static int DFLT_SIDE_2 = 2; // Germans

  protected TurnMarker turn;

  public PBNotesWindow() {
    privateNotes = new PrivateNotesController();
    secretNotes = new SecretNotesController();
    scenarioNotes = new ScenarioNotesController();
    turn = new TurnMarker();

    frame = new NotesDialog();
    frame.setTitle("Notes");
    ActionListener al = new ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent e) {
        captureState();
        frame.setVisible(!frame.isShowing());
      }
    };
    launch = new LaunchButton(null, null, null, "icon", al);
    launch.setAttribute("icon", "/images/notes.gif");
    launch.setToolTipText("Notes");
    frame.pack();
    setup(false);
  }

  /**
   * Capture this object's state, to be restored if the user hits "Cancel"
   */
  protected void captureState() {
    lastSavedNotes = (String) notes.getValue();
    scenarioNotes.captureState();
    privateNotes.captureState();
    secretNotes.captureState();
    turn.captureState();
  }

  public void cancel() {
    restoreState();
  }

  protected void restoreState() {
    privateNotes.restoreState();
    secretNotes.restoreState();
    notes.setValue(lastSavedNotes);
    scenarioNotes.restoreState();
    turn.restoreState();
  }

  protected void save() {

    Command c = new NullCommand();
    if (!lastSavedNotes.equals(notes.getValue())) {
      c.append(new SetNote((String) notes.getValue()));
    }

    c.append(scenarioNotes.save());
    c.append(privateNotes.save());
    c.append(secretNotes.save());
    c.append(turn.save());

    GameModule.getGameModule().sendAndLog(c.append(c));

  }

  protected class NotesDialog extends JDialog {
    protected NotesDialog() {
      super(GameModule.getGameModule().getFrame());
      initComponents();
      setLocationRelativeTo(getOwner());
    }

    protected void initComponents() {

      getContentPane().setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
      setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
      addWindowListener(new WindowAdapter() {
        public void windowClosing(WindowEvent e) {
          cancel();
          setVisible(false);
        }
      });

      // Create the Panel for the Turn Marker

      JPanel turnPanel = new JPanel();

      JPanel contentPanel = new JPanel();
      contentPanel.setBorder(BorderFactory.createRaisedBevelBorder());

      JPanel sidePanel = new JPanel();
      sidePanel.setLayout(new BoxLayout(sidePanel, BoxLayout.Y_AXIS));
      JPanel side1Panel = new JPanel();
      side1Panel.setAlignmentY(Component.RIGHT_ALIGNMENT);
      side1Panel.add(new JLabel("First Side"), BorderLayout.CENTER);

      final JComboBox side1Combo = new JComboBox(PB.getSides());
      side1Combo.setSelectedItem(turn.getSide(0));
      side1Combo.addItemListener(new ItemListener() {

        public void itemStateChanged(ItemEvent arg0) {
          turn.setSide(0, (String) side1Combo.getSelectedItem());
        }

      });
      side1Panel.add(side1Combo, BorderLayout.LINE_END);

      JPanel side2Panel = new JPanel();
      side2Panel.add(new JLabel("Second Side"), BorderLayout.CENTER);

      final JComboBox side2Combo = new JComboBox(PB.getSides());
      side2Combo.setSelectedItem(turn.getSide(1));
      side2Combo.addItemListener(new ItemListener() {

        public void itemStateChanged(ItemEvent arg0) {
          turn.setSide(1, (String) side2Combo.getSelectedItem());
        }

      });
      side2Panel.add(side2Combo, BorderLayout.LINE_END);

      sidePanel.add(side1Panel);
      sidePanel.add(side2Panel);

      JPanel buttonPanel = new JPanel();
      JButton nextButton = new JButton("Next");
      nextButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          turn.advance();
        }
      });
      buttonPanel.add(nextButton);

      contentPanel.add(sidePanel, BorderLayout.LINE_START);
      contentPanel.add(turn, BorderLayout.CENTER);
      contentPanel.add(buttonPanel, BorderLayout.LINE_END);

      turnPanel.add(new JPanel(), BorderLayout.LINE_START);
      turnPanel.add(contentPanel, BorderLayout.CENTER);
      turnPanel.add(new JPanel(), BorderLayout.LINE_END);

      getContentPane().add(turnPanel);

      // Notes Section

      notes = new TextConfigurer(null, null);
      JTabbedPane tab = new JTabbedPane();
      getContentPane().add(tab);

      tab.addTab("Scenario", scenarioNotes.getControls());

      Box b = Box.createVerticalBox();
      b.add(new JLabel("Visible to all"));
      b.add(notes.getControls());
      tab.addTab("Public", b);

      tab.addTab("Private", privateNotes.getControls());

      tab.addTab("Delayed", secretNotes.getControls());

      JPanel p = new JPanel();
      JButton saveButton = new JButton("Save");
      p.add(saveButton);
      saveButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          save();
          setVisible(false);
        }
      });
      JButton cancelButton = new JButton("Cancel");
      cancelButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          cancel();
          setVisible(false);
        }
      });
      p.add(cancelButton);
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
      s = COMMAND_PREFIX + ((SetNote) c).msg;
    }
    else if (c instanceof SetScenarioNote) {
      s = SCENARIO_COMMAND_PREFIX + ((SetScenarioNote) c).msg;
    }
    else if (c instanceof SetTurn) {
      s = TURN_COMMAND_PREFIX + ((SetTurn) c).getState();
    }
    else {
      s = privateNotes.encode(c);
      if (s == null) {
        s = secretNotes.encode(c);
      }
    }
    return s;
  }

  public Command decode(String command) {
    Command comm;
    if (command.startsWith(COMMAND_PREFIX)) {
      comm = new SetNote(command.substring(COMMAND_PREFIX.length()));
    }
    else if (command.startsWith(SCENARIO_COMMAND_PREFIX)) {
      comm = new SetScenarioNote(command.substring(SCENARIO_COMMAND_PREFIX.length()));
    }
    else if (command.startsWith(TURN_COMMAND_PREFIX)) {
      return new SetTurn(command.substring(TURN_COMMAND_PREFIX.length()));
    }
    else {
      comm = privateNotes.decode(command);
      if (comm == null) {
        comm = secretNotes.decode(command);
      }
    }
    return comm;
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
   * Expects to be added to a {@link VASSAL.build.GameModule}. Adds a button to
   * the controls window toolbar to show the window containing the notes
   */
  public void addTo(Buildable b) {
    GameModule.getGameModule().getToolBar().add(launch);
    launch.setAlignmentY(0.0F);
    GameModule.getGameModule().addCommandEncoder(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    GameModule.getGameModule().addCommandEncoder(privateNotes);
    GameModule.getGameModule().getGameState().addGameComponent(privateNotes);
    GameModule.getGameModule().addCommandEncoder(secretNotes);
    GameModule.getGameModule().getGameState().addGameComponent(secretNotes);
  }

  public void removeFrom(Buildable b) {
    GameModule.getGameModule().getToolBar().remove(launch);
    GameModule.getGameModule().removeCommandEncoder(this);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    GameModule.getGameModule().removeCommandEncoder(privateNotes);
    GameModule.getGameModule().getGameState().removeGameComponent(privateNotes);
    GameModule.getGameModule().removeCommandEncoder(secretNotes);
    GameModule.getGameModule().getGameState().removeGameComponent(secretNotes);
  }

  public void setup(boolean show) {
    launch.setEnabled(show);
    if (!show) {
      notes.setValue("");
      turn.init();
    }
  }

  public Command getRestoreCommand() {
    Command c = new SetNote(notes.getValueString());
    c.append(new SetTurn(turn.getState()));
    c.append(new SetScenarioNote(scenarioNotes.getText()));
    c.append(privateNotes.getRestoreCommand());
    c.append(secretNotes.getRestoreCommand());
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

  private class SetScenarioNote extends Command {
    private String msg;

    private SetScenarioNote(String s) {
      msg = s;
    }

    protected void executeCommand() {
      scenarioNotes.setText(msg);
    }

    protected Command myUndoCommand() {
      return null;
    }
  }

  protected class SetTurn extends Command {

    private String oldState;
    private String newState;

    public SetTurn(String state) {
      newState = state;
      oldState = turn.getState();
    }

    public SetTurn(String state, String origState) {
      newState = state;
      oldState = origState;
    }

    public String getState() {
      return newState;
    }

    protected void executeCommand() {
      turn.setState(newState);
    }

    protected Command myUndoCommand() {
      return new SetTurn(oldState);
    }
  }

  protected class TurnMarker extends Canvas {

    protected String sides[];
    protected int currentSide;
    protected int currentTurn;
    protected String savedState;

    public TurnMarker() {
      init();
    }

    public void init() {
      sides = new String[] { PB.getSides()[DFLT_SIDE_1], PB.getSides()[DFLT_SIDE_2] };
      currentSide = 0;
      currentTurn = 1;
    }

    public String getState() {
      return sides[0] + ";" + sides[1] + ";" + currentSide + ";" + currentTurn;
    }

    public void setState(String s) {
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ';');
      sides[0] = st.nextToken();
      sides[1] = st.nextToken();
      String x = st.nextToken();
      String y = st.nextToken();
      currentSide = Integer.parseInt(x);
      currentTurn = Integer.parseInt(y);
      repaint();
    }

    protected void captureState() {
      savedState = getState();
    }

    protected void restoreState() {
      setState(savedState);
    }

    protected String getSavedState() {
      return savedState;
    }

    protected Command save() {

      if (getState().equals(getSavedState())) {
        return new NullCommand();
      }
      else {
        Command c = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), "* Turn Advanced - "
            + turn.getCurrentTurn() + " " + turn.getCurrentSideName());
        c.execute();
        c.append(new SetTurn(getState(), getSavedState()));
        return c;
      }
    }

    public void setSide(int i, String side) {
      sides[i] = side;
      repaint();
    }

    public String getSide(int i) {
      return sides[i];
    }

    public int getCurrentTurn() {
      return currentTurn;
    }

    public String getCurrentSideName() {
      return sides[currentSide];
    }

    public int getNextTurn() {
      return currentSide == 0 ? currentTurn : currentTurn + 1;
    }

    public String getNextSide() {
      return sides[getNextSideNo()];
    }

    public int getNextSideNo() {
      return 1 - currentSide;
    }

    public void advance() {
      currentTurn = getNextTurn();
      currentSide = getNextSideNo();
      repaint();
    }

    public void paint(Graphics g) {
      FontMetrics fm;
      g.setColor(Color.white);
      g.fillRect(0, 0, getSize().width - 1, getSize().height - 1);
      g.setColor(Color.black);
      g.drawRect(0, 0, getSize().width - 1, getSize().height - 1);
      fm = g.getFontMetrics();
      g.drawString(sides[currentSide], getSize().width / 2 - fm.stringWidth(sides[currentSide]) / 2,
          getSize().height - 2);
      g.setFont(new Font("TimesRoman", Font.BOLD, 18));
      fm = g.getFontMetrics();
      g.drawString("Turn", getSize().width / 2 - fm.stringWidth("Turn") / 2, fm.getAscent() - 4);
      g.drawString("" + currentTurn, getSize().width / 2 - fm.stringWidth("" + currentTurn) / 2, getSize().height / 2
          + fm.getAscent() / 2 - 2);
    }

    public Dimension getPreferredSize() {
      return (new Dimension(60, 60));
    }
  }

  public class ScenarioNotesController {

    private String savedNotes;
    private Component controls;
    private JEditorPane htmlWin;
    private JPanel p;
    private EditDialog editDialog;

    private void setText(String text) {
      htmlWin.setContentType("text/html");
      htmlWin.setText(text);
      // ensure hyperlink engine knows we no longer at the last URL
      htmlWin.getDocument().putProperty("stream", null);
      htmlWin.revalidate();
    }

    public String getText() {
      return htmlWin.getText();
    }

    public void captureState() {
      savedNotes = htmlWin.getText();
    }

    public void restoreState() {
      htmlWin.setText(savedNotes);
    }

    public Command save() {
      if (savedNotes.equals(getText())) {
        return new NullCommand();
      }
      else {
        return new SetScenarioNote(getText());
      }
    }

    public Component getControls() {
      if (controls == null) {
        Box b = Box.createVerticalBox();
        b.add(new JLabel("Scenario Notes"));
        p = new JPanel();
        p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));
        htmlWin = new JEditorPane();
        htmlWin.setPreferredSize(new Dimension(20, 100));
        htmlWin.addMouseListener(new MouseAdapter() {
          public void mouseClicked(MouseEvent evt) {
            if (evt.getClickCount() > 1) {
              doEdit();
            }
          }
        });

        JScrollPane scroll = new JScrollPane(htmlWin);
        if (name != null) {
          scroll.setBorder(new TitledBorder("Scenario Notes"));
        }
        p.add(scroll);
        b.add(p);

        htmlWin.setFocusable(true);
        htmlWin.setEditable(false);

        controls = b;
      }
      return controls;
    }

    protected void doEdit() {
      if (editDialog == null) {
        editDialog = new EditDialog(htmlWin);
      }
      editDialog.setVisible(true);
    }
    
    public class EditDialog extends JDialog {

      private String savedText;
      private JTextComponent myText;
      private JTextArea editText;

      public EditDialog(JTextComponent text) {
        getContentPane().setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        addWindowListener(new WindowAdapter() {
          public void windowClosing(WindowEvent e) {
            cancel();
            setVisible(false);
          }
        });

        setModal(true);

        Box b = Box.createVerticalBox();
        b.add(new JLabel("HTML Notes Editor"));
        
        JPanel editPanel = new JPanel();
        editPanel.setLayout(new BoxLayout(editPanel, BoxLayout.Y_AXIS));

        editText = new JTextArea();
        editText.setPreferredSize(new Dimension(400,400));
        editText.addKeyListener(new java.awt.event.KeyAdapter() {
          public void keyReleased(java.awt.event.KeyEvent evt) {
            myText.setText(editText.getText());
          }
        });
        editText.setText(text.getText());
        JScrollPane scroll = new JScrollPane(editText);
        scroll.setBorder(new TitledBorder("Edit Scenario Notes"));
        editPanel.add(scroll);

        b.add(editPanel);

        JPanel buttonPanel = new JPanel();

        JButton saveButton = new JButton("Save");
        saveButton.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            doSave();
          }
        });
        buttonPanel.add(saveButton);

        JButton canButton = new JButton("Cancel");
        canButton.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            doCancel();
          }
        });
        buttonPanel.add(canButton);

        b.add(buttonPanel);
        getContentPane().add(b);
        pack();

        myText = text;
        savedText = text.getText();
      }

      protected void doSave() {
        myText.setText(editText.getText());
        setVisible(false);
      }

      protected void doCancel() {
        myText.setText(savedText);
        setVisible(false);
      }
    }
  }
}