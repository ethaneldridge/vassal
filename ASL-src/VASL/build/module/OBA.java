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
package VASL.build.module;

import VASSAL.build.AbstractBuildable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.GameComponent;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.command.NullCommand;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.SequenceEncoder;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.Random;

/**
 * This components keeps track of OBA draw piles
 */
public class OBA extends AbstractBuildable
    implements GameComponent, CommandEncoder {
  public static final String HOTKEY = "hotkey";

  private JFrame frame;
  private JPanel controls;

  private LaunchButton launch;

  public OBA() {
    frame = new JFrame();
    frame.setTitle("OBA Modules");
    frame.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);

    frame.getContentPane().setLayout(new BoxLayout(frame.getContentPane(), BoxLayout.Y_AXIS));

    controls = new JPanel();
    controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));
    frame.getContentPane().add(controls);

    JButton b = new JButton("Add Module");
    b.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        Module mod = new Module(OBA.this);
        CreateModule c = new CreateModule(OBA.this, OBA.this.getModuleCount(), mod.getState());
        c.execute();
        GameModule.getGameModule().sendAndLog(c);
      }
    });
    frame.getContentPane().add(b);
    frame.pack();

    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        frame.setVisible(!frame.isShowing());
      }
    };
    launch = new LaunchButton("OBA", null, HOTKEY, al);
    launch.setEnabled(false);
  }

  public void addTo(Buildable b) {
    GameModule.getGameModule().getToolBar().add(launch);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    GameModule.getGameModule().addCommandEncoder(this);
  }

  public String[] getAttributeNames() {
    return new String[]{HOTKEY};
  }

  public void setAttribute(String name, Object value) {
    if (HOTKEY.equals(name)) {
      launch.setAttribute(name, value);
    }
  }

  public String getAttributeValueString(String name) {
    if (HOTKEY.equals(name)) {
      return launch.getAttributeValueString(name);
    }
    else {
      return null;
    }
  }

  public String encode(Command c) {
    if (c instanceof UpdateModule) {
      SequenceEncoder se = new SequenceEncoder("OBA", '/');
      se.append("D")
          .append(((UpdateModule) c).getIndex() + "")
          .append(((UpdateModule) c).getNewState());
      return se.getValue();
    }
    else if (c instanceof CreateModule) {
      SequenceEncoder se = new SequenceEncoder("OBA", '/');
      se.append("+")
          .append(((CreateModule) c).getIndex() + "")
          .append(((CreateModule) c).getState());
      return se.getValue();
    }
    else if (c instanceof RemoveModule) {
      SequenceEncoder se = new SequenceEncoder("OBA", '/');
      se.append("-")
          .append(((RemoveModule) c).getIndex() + "");
      return se.getValue();
    }
    else {
      return null;
    }
  }

  public Command decode(String s) {
    if (s.startsWith("OBA")) {
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, '/');
      st.nextToken();
      int index = -1;
      String state;
      switch (st.nextToken().charAt(0)) {
        case '+':
          index = Integer.parseInt(st.nextToken());
          state = st.nextToken();
          return new CreateModule(this, index, state);
        case '-':
          index = Integer.parseInt(st.nextToken());
          return new RemoveModule(this, index);
        case 'D':
          index = Integer.parseInt(st.nextToken());
          state = st.nextToken();
          return new UpdateModule(this, index, state);
        default:
          return null;
      }
    }
    else {
      return null;
    }
  }

  public void addModule(Module mod, int index) {
    controls.add(mod.getControls(), index);
    frame.pack();
  }

  public int getModuleCount() {
    return controls.getComponentCount();
  }

  public Module getModuleAt(int index) {
    return index < 0 ? null
        : ((ModuleControls) controls.getComponent(index)).getModule();
  }

  public int indexOf(Module mod) {
    for (int i = 0,max = getModuleCount(); i < max; ++i) {
      if (((ModuleControls) controls.getComponent(i)).getModule() == mod) {
        return i;
      }
    }
    return -1;
  }

  public void removeModule(Module mod) {
    controls.remove(mod.getControls());
    frame.pack();
  }

  public void setup(boolean show) {
    launch.setEnabled(show);
    if (!show) {
      controls.removeAll();
      frame.setVisible(false);
    }
  }

  public Command getRestoreCommand() {
    Command c = new NullCommand();
    for (int i = 0; i < getModuleCount(); ++i) {
      c = c.append(new CreateModule(this, i, getModuleAt(i).getState()));
    }
    return c;
  }

  public static class UpdateModule extends Command {
    private Module mod;
    private String oldState;
    private String newState;
    private OBA oba;
    private int index;

    public UpdateModule(OBA oba, int index, String newState) {
      this.oba = oba;
      this.index = index;
      this.newState = newState;
    }

    public UpdateModule(Module mod, String oldState, String newState) {
      oba = mod.getOba();
      index = oba.indexOf(mod);
      this.oldState = oldState;
      this.newState = newState;
    }

    public void executeCommand() {
      Module mod = oba.getModuleAt(index);
      oldState = mod.getState();
      mod.setState(newState);
    }

    protected Command myUndoCommand() {
      return new UpdateModule(oba, index, oldState);
    }

    public String getOldState() {
      return oldState;
    }

    public String getNewState() {
      return newState;
    }

    public int getIndex() {
      return index;
    }
  }

  public static class CreateModule extends Command {
    private OBA oba;
    private Module mod;
    private int index;
    private String newState;

    public CreateModule(OBA oba, int index, String newState) {
      this.oba = oba;
      this.index = index;
      this.newState = newState;
      mod = new Module(oba);
      mod.setState(newState);
    }

    public void executeCommand() {
      oba.addModule(mod, index);
    }

    protected Command myUndoCommand() {
      return new RemoveModule(oba, index);
    }

    public int getIndex() {
      return index;
    }

    public String getState() {
      return newState;
    }
  }

  public static class RemoveModule extends Command {
    private String state;
    private Module mod;
    private OBA oba;
    private int index;

    public RemoveModule(Module mod) {
      this(mod.getOba(), mod.getOba().indexOf(mod));
    }

    public RemoveModule(OBA oba, int index) {
      this.oba = oba;
      this.index = index;
    }

    public void executeCommand() {
      Module mod = oba.getModuleAt(index);
      if (mod != null) {
        state = mod.getState();
        oba.removeModule(mod);
      }
    }

    protected Command myUndoCommand() {
      try {
        return new CreateModule(oba, index, state);
      }
      catch (RuntimeException ex) {
        ex.printStackTrace();
        return null;
      }
    }

    public int getIndex() {
      return index;
    }
  }

  public static class Module {
    private OBA oba;
    private String owner, label = "", showing = "";
    private int nRed = 3, nBlack = 8;

    private ModuleControls controls;

    public Module(OBA oba) {
      this.oba = oba;
      owner = GameModule.getUserId();
      controls = new ModuleControls(this);
    }

    public OBA getOba() {
      return oba;
    }

    public int getRed() {
      return nRed;
    }

    public int getBlack() {
      return nBlack;
    }

    public void setRed(int r) {
      nRed = r;
    }

    public void setBlack(int b) {
      nBlack = b;
    }

    public String getShowing() {
      return showing;
    }

    public void setShowing(String s) {
      showing = s;
    }

    public String getOwner() {
      return owner;
    }

    public void setOwner(String s) {
      owner = s;
    }

    public String getLabel() {
      return label;
    }

    public void setLabel(String s) {
      label = s;
    }

    public ModuleControls getControls() {
      return controls;
    }

    public String getState() {
      SequenceEncoder se = new SequenceEncoder(',');
      se.append(owner).append(label).append("" + nRed).append("" + nBlack).append(showing);
      return se.getValue();
    }

    public void setState(String s) {
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ',');
      owner = st.nextToken();
      label = st.nextToken();
      nRed = Integer.parseInt(st.nextToken());
      nBlack = Integer.parseInt(st.nextToken());
      showing = st.nextToken();

      controls.refresh();
    }

    public void sendUpdate(String oldState) {
      GameModule.getGameModule().sendAndLog(new UpdateModule(this, oldState, getState()));
    }
  }

  /**
   * A Module has a deck of cards and controls to set the
   * number of reds and blacks
   * The controls are shown only if the user is the owner
   */
  public static class ModuleControls extends JPanel {
    private JTextField red, black, label;
    private Module mod;
    private DeckView view;

    public ModuleControls(Module m) {
      mod = m;

      setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
      view = new DeckView(mod);

      add(view);

      FocusAdapter updateOnFocus = new FocusAdapter() {
        public void focusLost(FocusEvent e) {
          valuesUpdated();
        }
      };

      ActionListener updateOnAction = new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          valuesUpdated();
        }
      };

      red = new JTextField(2);
      red.setMaximumSize(red.getPreferredSize());
      red.addActionListener(updateOnAction);
      red.addFocusListener(updateOnFocus);
      red.setEditable(mod.getOwner().equals(GameModule.getUserId()));

      black = new JTextField(2);
      black.setMaximumSize(red.getPreferredSize());
      black.addActionListener(updateOnAction);
      black.addFocusListener(updateOnFocus);
      black.setEditable(mod.getOwner().equals(GameModule.getUserId()));

      label = new JTextField(10);
      label.setMaximumSize(new Dimension(label.getMaximumSize().width, label.getPreferredSize().height));
      label.addActionListener(updateOnAction);
      label.addFocusListener(updateOnFocus);
      label.setEditable(mod.getOwner().equals(GameModule.getUserId()));

      Box vBox = Box.createVerticalBox();
      Box hBox = Box.createHorizontalBox();
      hBox.add(new JLabel("black/red:"));
      hBox.add(black);
      hBox.add(red);
      vBox.add(hBox);
      vBox.add(label);
      add(vBox);

    }

    public Module getModule() {
      return mod;
    }

    public void refresh() {
      red.setText(mod.getRed() + "");
      black.setText(mod.getBlack() + "");
      label.setText(mod.getLabel());
      view.refresh();
    }

    public void valuesUpdated() {
      String oldState = mod.getState();
      mod.setLabel(label.getText());
      try {
        int r = Integer.parseInt(red.getText());
        int b = Integer.parseInt(black.getText());
        if (r != mod.getRed() || b != mod.getBlack()) {
          mod.setShowing("");
        }
        if (r + b == 0 && mod.getShowing().length() == 0) {
          Command c = new RemoveModule(mod);
          c.execute();
          GameModule.getGameModule().sendAndLog(c);
          return;
        }
        mod.setRed(r);
        mod.setBlack(b);
        view.refresh();
      }
      catch (NumberFormatException ex) {
      }
      if (!oldState.equals(mod.getState())) {
        mod.sendUpdate(oldState);
      }
    }

  }

  /**
   * Draws face-up cards (represented by showing string) and
   * deck of face-down cards.
   * Clicking draws the next card or shuffles a face-up card back into the deck
   */
  public static class DeckView extends Canvas implements MouseListener {
    private static Color cardBack = new Color(200, 200, 200);
    private static int boxX = 22, boxY = 33;
    private Random ran = new Random();
    private Module mod;

    DeckView(Module m) {
      mod = m;
      if (m.getOwner().equals(GameModule.getUserId())) {
        addMouseListener(this);
      }
    }

    public void refresh() {
      setSize(getPreferredSize());
      repaint();
    }

    public void paint(Graphics g) {
      for (int i = 0; i < mod.getShowing().length(); ++i) {
        if (mod.getShowing().substring(i, i + 1).equals("r")) {
          g.setColor(Color.red);
          g.fillRect(2 + (boxX + 4) * i, 0, boxX, boxY);
        }
        else if (mod.getShowing().substring(i, i + 1).equals("b")) {
          g.setColor(Color.black);
          g.fillRect(2 + (boxX + 4) * i, 0, boxX, boxY);
        }
      }
      for (int i = 0; i < mod.getRed() + mod.getBlack(); ++i) {
        g.setColor(cardBack);
        int nx = 2 + (boxX + 4) *
            (mod.getRed() + mod.getBlack() + mod.getShowing().length() + 1) + 2 * i;
        int ny = boxY - 2 * i;
        g.fillRect(nx, ny, boxX, boxY);
        g.setColor(Color.black);
        g.drawRect(nx, ny, boxX, boxY);
      }
    }

    public void mousePressed(MouseEvent e) {
    }

    public void mouseEntered(MouseEvent e) {
    }

    public void mouseExited(MouseEvent e) {
    }

    public void mouseClicked(MouseEvent e) {
    }

    public void mouseReleased(MouseEvent e) {
      int x = e.getX();
      int y = e.getY();

      String oldState = mod.getState();
      int pos = x / (boxX + 4);

      Command report = null;
      if (pos > mod.getRed() + mod.getBlack() + mod.getShowing().length()) {        // Draw from the Deck
        if (mod.getBlack() + mod.getRed() == 0 ||
            x < 2 + (boxX + 4) * (mod.getRed() + mod.getBlack() + mod.getShowing().length() + 1))
          return;
        if (ran.nextFloat() <= (float) (mod.getBlack() / ((float) (mod.getRed() + mod.getBlack())))) {
          mod.setBlack(mod.getBlack() - 1);
          mod.setShowing(mod.getShowing() + "b");
          ;
          report = new Chatter.DisplayText
              (GameModule.getGameModule().getChatter(), " *** Battery Access Draw = Black ***");
        }
        else {
          mod.setRed(mod.getRed() - 1);
          mod.setShowing(mod.getShowing() + "r");
          ;
          report = new Chatter.DisplayText
              (GameModule.getGameModule().getChatter(), " *** Battery Access Draw = Red ***");
        }
      }
      else if (pos < mod.getShowing().length()) {    // Shuffle card back into Deck
        String col = mod.getShowing().substring(pos, pos + 1);
        mod.setShowing(mod.getShowing().substring(0, pos) + mod.getShowing().substring(pos + 1));
        if ("r".equals(col)) {
          mod.setRed(mod.getRed() + 1);
        }
        else {
          mod.setBlack(mod.getBlack() + 1);
        }
      }
      else
        return;
      if (report != null) {
        report.execute();
        report.append(new UpdateModule(mod, oldState, mod.getState()));
        GameModule.getGameModule().sendAndLog(report);
      }
      else {
        mod.sendUpdate(oldState);
      }
      mod.getControls().refresh();
    }

    public Dimension getPreferredSize() {
      return (new Dimension((boxX + 4)
                            * (mod.getRed() + mod.getBlack() + mod.getShowing().length() + 3)
                            , 2 * (boxY + 4)));
    }
  }
}
