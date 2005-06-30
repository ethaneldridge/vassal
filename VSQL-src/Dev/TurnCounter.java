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

import java.awt.BasicStroke;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.font.FontRenderContext;
import java.awt.font.TextLayout;
import java.awt.geom.AffineTransform;
import java.util.ArrayList;
import java.util.Iterator;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JWindow;
import javax.swing.KeyStroke;
import javax.swing.plaf.basic.BasicArrowButton;

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
import VASSAL.configure.StringEnum;
import VASSAL.tools.FormattedString;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.PlayerIdFormattedString;
import VASSAL.tools.SequenceEncoder;

/**
 * Generic Turn Counter
 */
public class TurnCounter extends AbstractConfigurable implements CommandEncoder, GameComponent, ActionListener {

  protected static final String COMMAND_PREFIX = "TURN\t";
  public static final String VERSION = "1.2";

  public static final String HOT_KEY = "hotkey";
  public static final String ICON = "icon";
  public static final String BUTTON_TEXT = "buttonText";
  public static final String FONT_FAMILY = "fontFamily";
  public static final String FONT_SIZE = "fontSize";
  public static final String FONT_BOLD = "bold";
  public static final String FONT_ITALIC = "italic";
  public static final String TURN_FORMAT = "turnFormat";
  public static final String REPORT_FORMAT = "reportFormat";

  /** Variable name for reporting format */
  public static final String LEVEL_NAME = "name";
  public static final String LEVEL_TURN = "turn";
  public static final String OLD_TURN = "oldTurn";
  public static final String NEW_TURN = "newTurn";
  
  public static final String SET_COMMAND = "Set";

  protected FormattedString turnFormat = new FormattedString("$"+LEVEL_NAME+"1$: $"+LEVEL_TURN+"1$");
  protected FormattedString reportFormat = new PlayerIdFormattedString("* <$" + GlobalOptions.PLAYER_ID
      + "$> Turn Updated from $"+OLD_TURN+"$ to $"+NEW_TURN+"$");

  protected ArrayList levels = new ArrayList(5);
  protected TurnWindow turnWindow;
  protected SetDialog setDialog;
  protected LaunchButton launch;
  protected String fontFamily = "Dialog";
  protected int fontSize = 18;
  protected boolean fontBold = false;
  protected boolean fontItalic = false;
  protected JLabel turnLabel = new JLabel();
  
  protected String savedState = "";
  protected String savedSetState = "";
  protected String savedTurn = "";
  protected JPopupMenu popup;

  public TurnCounter() {
    turnWindow = new TurnWindow();    
    //turnLabel.setToolTipText("Use Right-click to Set or Configure Turn Counter");
    
    ActionListener al = new ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent e) {
        captureState();
        turnWindow.setControls();
        turnWindow.setVisible(!turnWindow.isShowing());
      }
    };
    launch = new LaunchButton("Turn", BUTTON_TEXT, HOT_KEY, ICON, al);
    launch.setToolTipText("Turn Counter");
    launch.setEnabled(false);
    turnWindow.pack();    
    
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
    updateTurnDisplay();
  }
  
  protected void setLaunchToolTip() {
    launch.setToolTipText(getTurn());
  }
  /*
   * Module level Configuration stuff
   */
  public String[] getAttributeNames() {
    return new String[] { BUTTON_TEXT, ICON, HOT_KEY, FONT_FAMILY, FONT_SIZE, FONT_BOLD, FONT_ITALIC, TURN_FORMAT, REPORT_FORMAT };
  }

  public void setAttribute(String name, Object value) {
    if (REPORT_FORMAT.equals(name)) {
      reportFormat.setFormat((String) value);
    }
    else if (TURN_FORMAT.equals(name)) {
      turnFormat.setFormat((String) value);
    }
    else if (FONT_FAMILY.equals(name)) {
      fontFamily = (String) value;
      setDisplayFont();
    }
    else if (FONT_SIZE.equals(name)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      fontSize = ((Integer) value).intValue();
      setDisplayFont();
    }
    else if (FONT_BOLD.equals(name)) {
      if (value instanceof String) {
        value = new Boolean((String) value);
      }
      fontBold = ((Boolean) value).booleanValue();
      setDisplayFont();
    }
    else if (FONT_ITALIC.equals(name)) {
      if (value instanceof String) {
        value = new Boolean((String) value);
      }
      fontItalic = ((Boolean) value).booleanValue();
      setDisplayFont();
    }
    else {
      launch.setAttribute(name, value);
    }
  }

  protected void setDisplayFont() {
    int style = (fontBold ? Font.BOLD : 0) + (fontItalic ? Font.ITALIC : 0);
    turnLabel.setFont(new Font(fontFamily, style, fontSize));
  }
  
  public String getAttributeValueString(String name) {
    if (REPORT_FORMAT.equals(name)) {
      return reportFormat.getFormat();
    }
    else if (TURN_FORMAT.equals(name)) {
      return turnFormat.getFormat();
    }
    else if (FONT_FAMILY.equals(name)) {
      return fontFamily;
    }
    else if (FONT_SIZE.equals(name)) {
      return fontSize + "";
    }
    else if (FONT_BOLD.equals(name)) {
      return fontBold + "";
    }
    else if (FONT_ITALIC.equals(name)) {
      return fontItalic + "";
    }
    else {
      return launch.getAttributeValueString(name);
    }
  }
  
  public String[] getAttributeDescriptions() {
    return new String[] { "Button text:  ", "Button Icon:  ", "Hotkey:  ", 
        "Font Family:  ", "Size:  ", "Bold?", "Italic?", 
        "Turn Format:  ", "Report Format:  " };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, IconConfig.class, KeyStroke.class, 
        FontFamilyConfig.class, Integer.class, Boolean.class, Boolean.class, 
        TurnFormatConfig.class, ReportFormatConfig.class };
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
  
  public static class FontFamilyConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { "Dialog", "DialogInput", "Monospaced", "SanSerif", "Serif"};
    }
 
  }
  
  public Class[] getAllowableConfigureComponents() {
    return new Class[] { TurnLevel.class };
  }

  public static String getConfigureTypeName() {
    return "Turn Counter v"+VERSION;
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

//  public void cancel() {
//    restoreState();
//  }
//
//  protected void restoreState() {
//    setState(savedState);
//  }

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
    
    captureState();

  }

  protected String getTurn() {
   
    for (int i = 0; i < levels.size(); i++) {
      TurnLevel level = (TurnLevel) levels.get(i);
      turnFormat.setProperty(LEVEL_NAME + (i + 1), level.getConfigureName());
      turnFormat.setProperty(LEVEL_TURN + (i + 1), level.getValueName());
    }
    return turnFormat.getText();
  }
  
  // Find longest possible string that can be returned
  protected String getLongestTurn() {
    
    for (int i = 0; i < levels.size(); i++) {
      TurnLevel level = (TurnLevel) levels.get(i);
      turnFormat.setProperty(LEVEL_NAME + (i + 1), level.getConfigureName());
      turnFormat.setProperty(LEVEL_TURN + (i + 1), level.getLongestValueName());
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

  public void actionPerformed(ActionEvent e) {
    if (e.getActionCommand().equals(SET_COMMAND)) {
      set();
    }    
  }
  
  protected void set() {
    savedSetState = getState();
    if (setDialog == null) {
      setDialog = new SetDialog();
      setDialog.setTitle("Set " + getConfigureName());
    }
    setDialog.setControls();
    setSetVisibility(false);
    setDialog.setVisible(true);
  }

  protected void updateTurnDisplay() {
    turnWindow.setControls();
    turnWindow.repaint();
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
      turnWindow.setVisible(false);
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

  protected class TurnWindow extends JWindow implements MouseListener, MouseMotionListener {

    protected JPanel mainPanel;
    protected JPanel controlPanel;
    protected JPanel buttonPanel;
    protected JPanel rightPanel;
    protected Point lastDrag;
    protected boolean dragging = false;

    protected TurnWindow() {
      super(GameModule.getGameModule().getFrame());
      initComponents();
      setLocation(100, 100);
      //setLocationRelativeTo(getOwner());
    }

    protected void initComponents() {

      //setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
      mainPanel = new JPanel();
      //((JFrame) getParent()).getRootPane().setWindowDecorationStyle(JRootPane.NONE);
      mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
      mainPanel.setBorder(BorderFactory.createCompoundBorder(
          BorderFactory.createRaisedBevelBorder(), BorderFactory.createLoweredBevelBorder()));
      getContentPane().add(mainPanel);
//      
//      addWindowListener(new WindowAdapter() {
//        public void windowClosing(WindowEvent e) {
//          cancel();
//          setVisible(false);
//        }
//      });

      controlPanel = new JPanel();
      
      buttonPanel = new JPanel();
      buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.Y_AXIS));
      buttonPanel.setPreferredSize(new Dimension(18, 36));
      
      JButton nextButton = new BasicArrowButton(BasicArrowButton.NORTH);
      nextButton.setToolTipText("Next Turn");
      buttonPanel.add(nextButton);
      nextButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          next();
          save();
        }
      });

      JButton prevButton = new BasicArrowButton(BasicArrowButton.SOUTH);
      prevButton.setToolTipText("Previous Turn");
      buttonPanel.add(prevButton);
      prevButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          prev();
          save();
        }
      });
      
      controlPanel.add(buttonPanel);
      
      //controlPanel.setLayout(new BoxLayout(controlPanel, BoxLayout.X_AXIS));
      //turnLabel = new JLabel();
      setDisplayFont();
      controlPanel.add(turnLabel);      

//      JPanel p = new JPanel();

//      JButton saveButton = new JButton("Save");
//      p.add(saveButton);
//      saveButton.addActionListener(new ActionListener() {
//        public void actionPerformed(ActionEvent e) {
//          save();
//          setVisible(false);
//        }
//      });

      rightPanel = new JPanel();
      rightPanel.setLayout(new BoxLayout(rightPanel, BoxLayout.Y_AXIS));
      rightPanel.setPreferredSize(new Dimension(18, 18));
      
      JButton cancelButton = new CancelButton(); 
      cancelButton.setToolTipText("Hide Turn Window");
      //cancelButton.setSize(15, 15);
      //cancelButton.setPreferredSize(new Dimension(15, 15));
//      cancelButton.setMaximumSize(new Dimension(15, 15));
      cancelButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          //cancel();
          setVisible(false);
        }
      });
      rightPanel.add(cancelButton);
      
//      JPanel fillerPanel = new JPanel();
//      Dimension size = new Dimension(15, 15);
//      fillerPanel.setPreferredSize(size);
//      rightPanel.add(fillerPanel);
      //JButton tempButton = new BasicArrowButton(BasicArrowButton.NORTH); 
      //tempButton.setVisible(false);
      //rightPanel.add(tempButton);
      
      controlPanel.add(rightPanel);
      mainPanel.add(controlPanel);

//      mainPanel.add(p);
      
      addMouseListener(this);
      addMouseMotionListener(this);
      pack();
    }

    public void setControls() {
      turnLabel.setText(getTurn());
      TextLayout layout = new TextLayout(getLongestTurn(), turnLabel.getFont(), new FontRenderContext(new AffineTransform(), true, false));
      turnLabel.setPreferredSize(new Dimension((int) layout.getBounds().getWidth()+10, (int) layout.getBounds().getHeight()+10));
      pack();
      turnLabel.repaint();
      dragging = false;
    }

    public void mouseDragged(MouseEvent e) {
      Point here = e.getPoint();
      int xOffset = here.x - lastDrag.x;
      int yOffset = here.y - lastDrag.y;
      setLocation(getLocation().x+xOffset, getLocation().y+yOffset);
      repaint();
    }

    public void mouseMoved(MouseEvent e) {
      
    }

    public void mouseClicked(MouseEvent e) {
      if (e.isMetaDown()) {
        doPopup(e.getPoint());
      }
    }

    public void mouseEntered(MouseEvent e) {
      
    }

    public void mouseExited(MouseEvent e) {
      
    }

    public void mousePressed(MouseEvent e) {
      dragging = true;
      lastDrag = e.getPoint();
    }

    public void mouseReleased(MouseEvent e) {
      dragging = false;
    }
    
    public void doPopup(Point p) {
      if (popup == null) {
        buildPopup();
      }

      popup.show(this, p.x, p.y);
    }
    
  }
  
  protected void buildPopup() {
    
    popup = new JPopupMenu();
    popup.addPopupMenuListener(new javax.swing.event.PopupMenuListener() {
      public void popupMenuCanceled(javax.swing.event.PopupMenuEvent evt) {
        turnWindow.repaint();
      }

      public void popupMenuWillBecomeInvisible(javax.swing.event.PopupMenuEvent evt) {
        turnWindow.repaint();
      }

      public void popupMenuWillBecomeVisible(javax.swing.event.PopupMenuEvent evt) {
      }
    });
    
    JMenuItem item = new JMenuItem(SET_COMMAND);
    item.addActionListener(this);
    popup.add(item);
  }

  protected class CancelButton extends BasicArrowButton {
    
    public CancelButton() {
      super(BasicArrowButton.CENTER);
    }
    
    public void paint(Graphics g) {
      super.paint(g);
      Graphics2D g2 = (Graphics2D) g;
      g2.setStroke(new BasicStroke(1.8f));
      //g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
      Rectangle r = getBounds();
      g.drawLine(5, 5, r.width-5, r.height-5);
      g.drawLine(5, r.height-5, r.width-5, 5);
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
      saveButton.setToolTipText("Save Changes to Turn Counter");
      p.add(saveButton);
      saveButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          saveSet();
          setVisible(false);
        }
      });

      JButton cancelButton = new JButton("Cancel");
      cancelButton.setToolTipText("Discard Changes to Turn Counter");
      cancelButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          cancelSet();
          setVisible(false);
        }
      });
      p.add(cancelButton);

      JButton configButton = new JButton("Configure");
      configButton.setToolTipText("Show/Hide Configure Options");
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
    turnWindow.setVisible(true);
  }
  
  protected void saveSet() {
    save();
    updateTurnDisplay();
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
