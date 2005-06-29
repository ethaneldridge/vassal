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

package Dev;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.Iterator;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.tools.FormattedString;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.PlayerIdFormattedString;
import VASSAL.tools.SequenceEncoder;

/**
 * Generic Turn Counter
 */
public class TurnCounter extends AbstractConfigurable implements CommandEncoder, GameComponent {

  protected static final String COMMAND_PREFIX = "TURN\t";

  public static final String HOT_KEY = "hotkey";
  public static final String ICON = "icon";
  public static final String BUTTON_TEXT = "buttonText";
  public static final String TURN_FORMAT = "turnFormat";
  public static final String REPORT_FORMAT = "reportFormat";

  /** Variable name for reporting format */
  public static final String LEVEL_NAME = "name";
  public static final String LEVEL_TURN = "turn";
  public static final String OLD_TURN = "oldTurn";
  public static final String NEW_TURN = "newTurn";

  protected FormattedString turnFormat = new FormattedString("$"+LEVEL_NAME+"1$: $"+LEVEL_TURN+"1$");
  protected FormattedString reportFormat = new PlayerIdFormattedString("* <$" + GlobalOptions.PLAYER_ID
      + "$> Turn Updated from $"+OLD_TURN+"$ to $"+NEW_TURN+"$");

  protected ArrayList levels = new ArrayList(5);
  protected TurnDialog turnDialog;
  protected SetDialog setDialog;
  protected LaunchButton launch;
  protected String savedState = "";
  protected String savedSetState = "";
  protected String savedTurn = "";

  public TurnCounter() {
    turnDialog = new TurnDialog();
    turnDialog.setTitle("Turn Counter");
    ActionListener al = new ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent e) {
        captureState();
        turnDialog.setControls();
        turnDialog.setVisible(!turnDialog.isShowing());
      }
    };
    launch = new LaunchButton("Turn", BUTTON_TEXT, HOT_KEY, ICON, al);
    launch.setToolTipText("Turn Counter");
    launch.setEnabled(false);
    turnDialog.pack();    
    
  }
  
  public String getState() {
    SequenceEncoder se = new SequenceEncoder('|');
    Iterator it = levels.iterator();
    while (it.hasNext()) {
      TurnLevel level = (TurnLevel) it.next();
      se.append(level.getState());
    }
    return se.getValue();
  }

  public void setState(String newState) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(newState, '|');
    for (int i = 0; i < levels.size(); i++) {
      ((TurnLevel) levels.get(i)).setState(sd.nextToken(""));
    }
    setLaunchToolTip();
  }
  
  protected void setLaunchToolTip() {
    launch.setToolTipText(getTurn());
  }
  /*
   * Module level Configuration stuff
   */
  public String[] getAttributeNames() {
    return new String[] { BUTTON_TEXT, ICON, HOT_KEY, TURN_FORMAT, REPORT_FORMAT };
  }

  public void setAttribute(String name, Object value) {
    if (REPORT_FORMAT.equals(name)) {
      reportFormat.setFormat((String) value);
    }
    else if (TURN_FORMAT.equals(name)) {
      turnFormat.setFormat((String) value);
    }
    else {
      launch.setAttribute(name, value);
    }
  }

  public String getAttributeValueString(String name) {
    if (REPORT_FORMAT.equals(name)) {
      return reportFormat.getFormat();
    }
    else if (TURN_FORMAT.equals(name)) {
      return turnFormat.getFormat();
    }
    else {
      return launch.getAttributeValueString(name);
    }
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Button text:  ", "Button Icon:  ", "Hotkey:  ", "Turn Format", "Report Format:  " };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, IconConfig.class, KeyStroke.class, TurnFormatConfig.class, ReportFormatConfig.class };
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, ((TurnCounter) c).launch.getAttributeValueString(ICON));
    }
  }

  public static class ReportFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[] {OLD_TURN, NEW_TURN } );
    }
  }

  public static class TurnFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      TurnCounter tc = (TurnCounter) c;
      String s[] = new String[2 * tc.levels.size()];
      for (int i = 0; i < tc.levels.size(); i++) {
        s[i * 2] = LEVEL_NAME + ((i + 1) + "");
        s[i * 2 + 1] = LEVEL_TURN + ((i + 1) + "");
      }
      return new FormattedStringConfigurer(key, name, s);
    }
  }
  
  public Class[] getAllowableConfigureComponents() {
    return new Class[] { TurnLevel.class };
  }

  public static String getConfigureTypeName() {
    return "Turn Counter";
  }

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

  public HelpFile getHelpFile() {
    return null;
  }

  protected void captureState() {
    savedState = getState();
    savedTurn = getTurn();
  }

  public void cancel() {
    restoreState();
  }

  protected void restoreState() {
    setState(savedState);
  }

  protected void save() {

    if (!savedState.equals(getState())) {
      
      reportFormat.setProperty(OLD_TURN, savedTurn);
      reportFormat.setProperty(NEW_TURN, getTurn());

      Command c = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), reportFormat.getText());
      c.execute();
      c.append(new SetTurn(this, savedState));

      GameModule.getGameModule().sendAndLog(c);
      
      setLaunchToolTip();
    }

  }

  protected String getTurn() {
   
    for (int i = 0; i < levels.size(); i++) {
      TurnLevel level = (TurnLevel) levels.get(i);
      turnFormat.setProperty(LEVEL_NAME + (i + 1), level.getConfigureName());
      turnFormat.setProperty(LEVEL_TURN + (i + 1), level.getValueName());
    }
    return turnFormat.getText();
  }
  /*
   * Advance the lowest level counter, plus propagate any rollovers
   */
  protected void next() {

    if (levels.size() == 0) {
      return;
    }

    int i = levels.size();
    TurnLevel level = ((TurnLevel) levels.get(--i));
    level.advance();
    boolean rollOver = level.hasRolledOver();

    for (i--; i >= 0; i--) {
      if (rollOver) {
        level = (TurnLevel) levels.get(i);
        level.advance();
        rollOver = level.hasRolledOver();
      }
    }
    updateTurnDisplay();
  }

  protected void prev() {

    if (levels.size() == 0) {
      return;
    }

    int i = levels.size();
    TurnLevel level = ((TurnLevel) levels.get(--i));
    level.retreat();
    boolean rollOver = level.hasRolledOver();

    for (i--; i >= 0; i--) {
      if (rollOver) {
        level = (TurnLevel) levels.get(i);
        level.retreat();
        rollOver = level.hasRolledOver();
      }
    }
    updateTurnDisplay();
  }

  protected void set() {
    savedSetState = getState();
    if (setDialog == null) {
      setDialog = new SetDialog();
      setDialog.setTitle("Set Turn");
    }
    setDialog.setControls();
    setSetVisibility(false);
    turnDialog.setVisible(false);
    setDialog.setVisible(true);
  }

  protected void updateTurnDisplay() {
    turnDialog.setControls();
    turnDialog.repaint();
  }

  public Command decode(String command) {
    Command comm = null;
    if (command.startsWith(COMMAND_PREFIX)) {
     comm = new SetTurn(command.substring(COMMAND_PREFIX.length()), this);
    }
    return comm;
  }

  public String encode(Command c) {
    String s = null;
    if (c instanceof SetTurn) {
      s = COMMAND_PREFIX + ((SetTurn) c).newState;
    }
    return s;
  }
  
  public void setup(boolean gameStarting) {
    launch.setEnabled(gameStarting);
    if (gameStarting) {
      reset();
    }
    else {
      turnDialog.setVisible(false);
    }
  }

  protected void reset() {
    for (int i = 0; i < levels.size(); i++) {
      ((TurnLevel) levels.get(i)).reset();
    }
    setLaunchToolTip();
  }
  
  public Command getRestoreCommand() {
    return new SetTurn(getState(), this);
  }

  protected void addLevel(TurnLevel level) {
    levels.add(level);
  }

  protected void removeLevel(TurnLevel level) {
    levels.remove(level);
  }

  protected class TurnDialog extends JDialog {

    protected JPanel controlPanel;
    protected JPanel controls = null;

    protected TurnDialog() {
      super(GameModule.getGameModule().getFrame());
      initComponents();
      setLocation(100, 100);
      //setLocationRelativeTo(getOwner());
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

      controlPanel = new JPanel();
      controlPanel.setLayout(new BoxLayout(controlPanel, BoxLayout.X_AXIS));
      getContentPane().add(controlPanel);

      JPanel p = new JPanel();

      JButton nextButton = new JButton("Next");
      p.add(nextButton);
      nextButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          next();
        }
      });

      JButton prevButton = new JButton("Prev");
      p.add(prevButton);
      prevButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          prev();
        }
      });

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

      JButton configButton = new JButton("Set");
      p.add(configButton);
      configButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          set();
        }
      });
      getContentPane().add(p);
    }

    public void setControls() {
      if (controls != null) {
        controlPanel.remove(controls);
      }

      controls = new JPanel();

      Iterator it = levels.iterator();
      while (it.hasNext()) {
        TurnLevel level = (TurnLevel) it.next();
        controls.add(level.getDisplayControls());
      }

      controlPanel.add(controls);
      pack();
    }
  }


  protected class SetDialog extends JDialog {

    protected JPanel panel;
    protected JPanel controls = null;

    protected SetDialog() {
      super(GameModule.getGameModule().getFrame());
      initComponents();
      setLocation(100, 100);
    }
    
    protected void initComponents() {
      getContentPane().setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
      setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
      addWindowListener(new WindowAdapter() {
        public void windowClosing(WindowEvent e) {
          cancelSet();
          setVisible(false);
        }
      });

      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
      getContentPane().add(panel);

      JPanel p = new JPanel();

      JButton saveButton = new JButton("Save");
      p.add(saveButton);
      saveButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          saveSet();
          setVisible(false);
        }
      });

      JButton cancelButton = new JButton("Cancel");
      cancelButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          cancelSet();
          setVisible(false);
        }
      });
      p.add(cancelButton);

      JButton configButton = new JButton("Configure");
      configButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          toggleSetVisibility();
        }
      });
      p.add(configButton);
      
      getContentPane().add(p);
    }

    public void setControls() {
      if (controls != null) {
        panel.remove(controls);
      }

      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.X_AXIS));

      Iterator it = levels.iterator();
      while (it.hasNext()) {
        TurnLevel level = (TurnLevel) it.next();
        JComponent c = level.getSetControls();
        c.setAlignmentX(Component.TOP_ALIGNMENT);
        controls.add(c);
      }

      panel.add(controls);
      pack();
    }
  }
  
  protected void cancelSet() {
    setState(savedSetState);
    turnDialog.setVisible(true);
  }
  
  protected void saveSet() {
    updateTurnDisplay();
    turnDialog.setVisible(true);
  }
  
  protected void toggleSetVisibility() {
    for (int i = 0; i < levels.size(); i++) {
      ((TurnLevel) levels.get(i)).toggleConfigVisibility();
    }
    setDialog.pack();
  }
  
  protected void setSetVisibility(boolean b) {
    for (int i = 0; i < levels.size(); i++) {
      ((TurnLevel) levels.get(i)).setConfigVisibility(b);
    }
    setDialog.pack();
  }
  
  public static class SetTurn extends Command {
    private String oldState;
    private String newState;
    private TurnCounter turn;

    public SetTurn(String newState, TurnCounter t) {
      this.newState = newState;
      oldState = t.getState();
      turn = t;
    }
    
    public SetTurn(TurnCounter t, String oldState) {
      newState = t.getState();
      this.oldState = oldState;
      turn = t;
    }

    protected void executeCommand() {
      turn.setState(newState);
    }

    protected Command myUndoCommand() {
      return new SetTurn(oldState, turn);
    }
  }
}
