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
/*
 * Created by IntelliJ IDEA.
 * User: rkinney
 * Date: Jul 14, 2002
 * Time: 4:25:21 PM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.counters;

import VASSAL.build.GameModule;
import VASSAL.build.Configurable;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.widget.CardSlot;
import VASSAL.build.widget.PieceSlot;
import VASSAL.command.AddPiece;
import VASSAL.command.Command;
import VASSAL.configure.ConfigurerWindow;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.ChooseComponentDialog;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.ComponentPathBuilder;
import VASSAL.counters.*;

import javax.swing.*;
import javax.swing.tree.TreePath;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.event.TreeSelectionEvent;
import java.util.List;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.io.File;
import java.net.MalformedURLException;
import java.util.Arrays;

/**
 * This Decorator defines a key command to places another counter on top of this one.
 */
public class PlaceMarker extends Decorator implements EditablePiece {
  public static final String ID = "placemark;";
  protected KeyCommand command;
  protected char key;
  protected String markerSpec;
  protected String markerText = "";

  public PlaceMarker() {
    this(ID + "Place Marker;M;null;null", null);
  }

  public PlaceMarker(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  public String getName() {
    return piece.getName();
  }

  protected KeyCommand[] myGetKeyCommands() {
    command.setEnabled(getMap() != null
                       && markerSpec != null);
    return new KeyCommand[]{command};
  }

  public String myGetState() {
    return "";
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(command.getName());
    se.append(key != 0 ? "" + key : "");
    se.append(markerSpec == null ? "null" : markerSpec);
    se.append(markerText == null ? "null" : markerText);
    return ID + se.getValue();
  }

  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    if (command.matches(stroke)) {
      GamePiece marker = createMarker();
      Command c = getMap().getStackMetrics().placeOrMerge(Decorator.getOutermost(this), marker);
      KeyBuffer.getBuffer().add(marker);
      if (markerText != null && getMap() != null) {
        GamePiece outer = getOutermost(this);
        if (!Boolean.TRUE.equals(outer.getProperty(Properties.OBSCURED_TO_OTHERS))
            && !Boolean.TRUE.equals(outer.getProperty(Properties.OBSCURED_TO_ME))
            && !Boolean.TRUE.equals(outer.getProperty(Properties.INVISIBLE_TO_OTHERS))) {
          String location = getMap().locationName(getPosition());
          if (location != null) {
            Command display = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), " * " + location + ":  " + outer.getName() + " " + markerText + " * ");
            display.execute();
            c = c == null ? display : c.append(display);
          }
        }
      }
      return c;
    }
    else {
      return null;
    }
  }

  private GamePiece createMarker() {
    GamePiece piece = null;
    if (markerSpec.startsWith(GameModule.class.getName())) {
      try {
        Configurable[] c = ComponentPathBuilder.getInstance().getPath(markerSpec);
        if (c[c.length-1] instanceof PieceSlot) {
          piece = PieceCloner.getInstance().clonePiece(((PieceSlot)c[c.length-1]).getPiece());
        }
      }
      catch (ComponentPathBuilder.PathFormatException e) {
      }
    }
    else {
      piece = ((AddPiece) GameModule.getGameModule().decode(markerSpec)).getTarget();
    }
    if (piece == null) {
      piece = new BasicPiece();
    }
    return piece;
  }

  public void mySetState(String newState) {
  }

  public Shape getShape() {
    return piece.getShape();
  }

  public String getDescription() {
    return "Place Marker";
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "Marker.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    String name = st.nextToken();
    key = st.nextChar('\0');
    command = new KeyCommand(name, KeyStroke.getKeyStroke(key, InputEvent.CTRL_MASK), this);
    markerSpec = st.nextToken();
    if ("null".equals(markerSpec)) {
      markerSpec = null;
    }
    markerText = st.nextToken("null");
    if ("null".equals(markerText)) {
      markerText = null;
    }
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  protected static class Ed implements PieceEditor {
    private KeySpecifier keyInput;
    private StringConfigurer commandInput;
    private PieceSlot pieceInput;
    private JPanel p = new JPanel();
    private String markerSlotPath;
    protected JButton defineButton = new JButton("Define Marker");
    protected JButton selectButton = new JButton("Select");

    protected Ed(PlaceMarker piece) {
      keyInput = new KeySpecifier(piece.key);
      commandInput = new StringConfigurer(null, "Command: ", piece.command.getName());
      GamePiece marker = piece.markerSpec == null ? null
          : ((AddPiece) GameModule.getGameModule().decode(piece.markerSpec)).getTarget();
      pieceInput = new PieceSlot(marker);

      markerSlotPath = piece.markerSpec;

      p = new JPanel();
      p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));
      p.add(commandInput.getControls());
      Box b = Box.createHorizontalBox();
      b.add(new JLabel("Key:  "));
      b.add(keyInput);
      p.add(b);
      b = Box.createHorizontalBox();
      b.add(pieceInput.getComponent());
      defineButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          markerSlotPath = null;
          new ConfigurerWindow(pieceInput.getConfigurer()).setVisible(true);
        }
      });
      b.add(defineButton);
      selectButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          ChoosePieceDialog d = new ChoosePieceDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class, p), PieceSlot.class);
          d.setVisible(true);
          if (d.getTarget() instanceof PieceSlot) {
            pieceInput.setPiece(((PieceSlot) d.getTarget()).getPiece());
          }
          if (d.getPath() != null) {
            markerSlotPath = ComponentPathBuilder.getInstance().getId(d.getPath());
          }
          else {
            markerSlotPath = null;
          }
        }
      });
      b.add(selectButton);
      p.add(b);
    }

    public Component getControls() {
      return p;
    }

    public String getState() {
      return "";
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(commandInput.getValueString());
      se.append(keyInput.getKey());
      if (pieceInput.getPiece() == null) {
        se.append("null");
      }
      else if (markerSlotPath != null) {
        se.append(markerSlotPath);
      }
      else {
        String spec = GameModule.getGameModule().encode(new AddPiece(pieceInput.getPiece()));
        se.append(spec);
      }
      se.append("null"); // Older versions specified a text message to echo.  Now performed by the ReportState trait, but we remain backward-compatible.
      return ID + se.getValue();
    }

    private static class ChoosePieceDialog extends ChooseComponentDialog {
      private Configurable[] path;

      public ChoosePieceDialog(Frame owner, Class targetClass) {
        super(owner, targetClass);
      }

      public void valueChanged(TreeSelectionEvent e) {
        super.valueChanged(e);
        TreePath p = e.getPath();
        if (p != null) {
          DefaultMutableTreeNode node = (DefaultMutableTreeNode) p.getLastPathComponent();
          List l = Arrays.asList(node.getUserObjectPath());
          path = (Configurable[]) l.toArray(new Configurable[l.size()]);
        }
        else {
          path = null;
        }
      }

      public Configurable[] getPath() {
        return path;
      }

      protected boolean isValidTarget(Object selected) {
        return super.isValidTarget(selected) || CardSlot.class.isInstance(selected);
      }
    }
  }
}
