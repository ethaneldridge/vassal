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
package VASSAL.counters;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.MenuDisplayer;
import VASSAL.command.AddPiece;
import VASSAL.command.ChangePiece;
import VASSAL.command.Command;
import VASSAL.command.RemovePiece;
import VASSAL.tools.SequenceEncoder;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.io.File;
import java.net.MalformedURLException;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

/**
 * Basic class for representing a physical component of the game
 * Can be a counter, a card, or an overlay
 */
public class BasicPiece implements EditablePiece {

  public static final String ID = "piece;";
  private static Highlighter highlighter;

  public static Font POPUP_MENU_FONT = new Font("Dialog", 0, 11);
  protected Dimension imageSize;// = new Dimension(-1,-1);
  protected JPopupMenu popup;

  private Map map;
  private KeyCommand[] commands;
  private Stack parent;
  private Point pos = new Point(0, 0);
  private String id;
  private Hashtable props;

  private char cloneKey, deleteKey;
  protected String imageName;
  private String commonName;

  public BasicPiece() {
    this(ID + "C;X;;;");
  }

  public BasicPiece(String type) {
    mySetType(type);
  }

  public void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    cloneKey = st.nextChar('\0');
    deleteKey = st.nextChar('\0');
    imageName = st.nextToken();
    commonName = st.nextToken();

    Image im = myImage();
    if (im != null) {
      JLabel l = new JLabel();
      l.setIcon(new ImageIcon(im));
      imageSize = l.getPreferredSize();
    }
    else {
      imageSize = new Dimension(0, 0);
    }

    commands = null;
  }

  public String getType() {
    SequenceEncoder se = new SequenceEncoder(cloneKey > 0 ? "" + cloneKey : "", ';');
    return ID + se.append(deleteKey > 0 ? "" + deleteKey : "")
      .append(imageName).append(commonName).getValue();
  }

  public void setMap(Map map) {
    if (map != this.map) {
      commands = null;
      this.map = map;
    }
  }

  public Map getMap() {
    return getParent() == null ? map : getParent().getMap();
  }

  public Object getProperty(Object key) {
    if (Properties.KEY_COMMANDS.equals(key)) {
      return getKeyCommands();
    }
    return props == null ? null : props.get(key);
  }

  public void setProperty(Object key, Object val) {
    if (props == null) {
      props = new Hashtable();
    }
    if (val == null) {
      props.remove(key);
    }
    else {
      props.put(key, val);
    }
  }

  protected Object prefsValue(String s) {
    return GameModule.getGameModule().getPrefs().getValue(s);
  }

  protected Image myImage() {
    try {
      return GameModule.getGameModule() == null ? null
        : GameModule.getGameModule().getDataArchive().getCachedImage(imageName + ".gif");
    }
    catch (java.io.IOException ex) {
      return null;
    }
  }

  public static void verifySize(Dimension d, Image i, Component obs) {
    if ((d.width < 0 || d.height < 0)
      && i != null) {
      MediaTracker mt = new MediaTracker(obs);
      mt.addImage(i, 0);
      try {
        mt.waitForAll();
      }
      catch (Exception e) {
      }
      d.setSize(i.getWidth(obs),
                i.getHeight(obs));
    }
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    Image im = myImage();
    if (im != null) {
      g.drawImage(im,
                  x - (int) (zoom * imageSize.width / 2),
                  y - (int) (zoom * imageSize.height / 2),
                  (int) (zoom * imageSize.width),
                  (int) (zoom * imageSize.height),
                  obs);
    }
  }

  protected KeyCommand[] getKeyCommands() {
    if (commands == null) {
      Vector v = new Vector();
      GamePiece target = Decorator.getOutermost(this);
      if (cloneKey > 0) {
        v.addElement(new KeyCommand("Clone",
                                    KeyStroke.getKeyStroke(cloneKey, InputEvent.CTRL_MASK), target));
      }
      if (deleteKey > 0) {
        v.addElement(new KeyCommand("Delete",
                                    KeyStroke.getKeyStroke(deleteKey, InputEvent.CTRL_MASK), target));
      }
      commands = new KeyCommand[v.size()];
      for (int i = 0; i < v.size(); ++i) {
        commands[i] = (KeyCommand) v.elementAt(i);
      }
    }
    GamePiece outer = Decorator.getOutermost(this);
    boolean canAdjustPosition = outer.getMap() != null &&
      outer.getParent() != null
      && outer.getParent().topPiece() != getParent().bottomPiece();
    enableCommand("Move up", canAdjustPosition);
    enableCommand("Move down", canAdjustPosition);
    enableCommand("Move to top", canAdjustPosition);
    enableCommand("Move to bottom", canAdjustPosition);
    enableCommand("Clone", outer.getMap() != null);
    enableCommand("Delete", outer.getMap() != null);
    return commands;
  }

  private void enableCommand(String name, boolean enable) {
    for (int i = 0; i < commands.length; ++i) {
      if (name.equals(commands[i].getName())) {
        commands[i].setEnabled(enable);
      }
    }
  }

  private boolean isEnabled(KeyStroke stroke) {
    if (stroke == null) {
      return false;
    }
    for (int i = 0; i < commands.length; ++i) {
      if (stroke.equals(commands[i].getKeyStroke())) {
        return commands[i].isEnabled();
      }
    }
    return true;
  }

  public Point getPosition() {
    return getParent() == null ? new Point(pos) : getParent().getPosition();
  }

  public void setPosition(Point p) {
    if (getMap() != null
      && getParent() == null) {
      getMap().repaint(getMap().boundingBoxOf(Decorator.getOutermost(this)));
    }
    pos = p;
    if (getMap() != null
      && getParent() == null) {
      getMap().repaint(getMap().boundingBoxOf(Decorator.getOutermost(this)));
    }
  }

  public Stack getParent() {
    return parent;
  }

  public void setParent(Stack s) {
    parent = s;
  }

  public Rectangle boundingBox() {
    return getShape().getBounds();
  }

  public Shape getShape() {
    Dimension d = imageSize.width < 0 ? new Dimension(0, 0) : imageSize;
    Rectangle r = new Rectangle(new Point(), d);
    r.translate(-r.width / 2, -r.height / 2);
    return r;
  }

  public boolean equals(GamePiece c) {
    return c == this;
  }

  public String getName() {
    return commonName;
  }

  public Command keyEvent(KeyStroke stroke) {
    getKeyCommands();
    if (!isEnabled(stroke)) {
      return null;
    }

    Command comm = null;

    GamePiece outer = Decorator.getOutermost(this);
    if (KeyStroke.getKeyStroke(cloneKey, InputEvent.CTRL_MASK).equals(stroke)) {
      GamePiece newPiece
        = ((AddPiece) GameModule.getGameModule().decode
        (GameModule.getGameModule().encode
         (new AddPiece(outer)))).getTarget();
      newPiece.setId(null);
      GameModule.getGameModule().getGameState().addPiece(newPiece);
      newPiece.setState(outer.getState());
      comm = new AddPiece(newPiece);
      if (getMap() != null) {
        comm.append(getMap().getStackMetrics().merge(outer, newPiece));
        KeyBuffer.getBuffer().remove(outer);
        KeyBuffer.getBuffer().add(newPiece);
      }
    }
    else if (KeyStroke.getKeyStroke(deleteKey, InputEvent.CTRL_MASK).equals(stroke)) {
      comm = new RemovePiece(outer);
      comm.execute();
    }
    else if (getMap() != null &&
      stroke.equals(getMap().getStackMetrics().getMoveUpKey())) {
      if (parent != null) {
        String oldState = parent.getState();
        int index = parent.indexOf(outer);
        if (index < parent.getPieceCount() - 1) {
          parent.insert(outer, index + 1);
          comm = new ChangePiece(parent.getId(), oldState, parent.getState());
        }
        else {
          getMap().reposition(parent, getMap().getPieces().length - 1);
        }
      }
      else {
        getMap().reposition(outer, getMap().getPieces().length - 1);
      }
    }
    else if (getMap() != null &&
      stroke.equals(getMap().getStackMetrics().getMoveDownKey())) {
      if (parent != null) {
        String oldState = parent.getState();
        int index = parent.indexOf(outer);
        if (index > 0) {
          parent.insert(outer, index - 1);
          comm = new ChangePiece(parent.getId(), oldState, parent.getState());
        }
        else {
          getMap().reposition(parent, 0);
        }
      }
      else {
        getMap().reposition(outer, 0);
      }
    }
    else if (getMap() != null &&
      stroke.equals(getMap().getStackMetrics().getMoveTopKey())) {
      parent = outer.getParent();
      if (parent != null) {
        String oldState = parent.getState();
        if (parent.indexOf(outer) < parent.getPieceCount() - 1) {
          parent.insert(outer, parent.getPieceCount() - 1);
          comm = new ChangePiece(parent.getId(), oldState, parent.getState());
        }
        else {
          getMap().reposition(parent, getMap().getPieces().length - 1);
        }
      }
      else {
        getMap().reposition(outer, getMap().getPieces().length - 1);
      }
    }
    else if (getMap() != null &&
      stroke.equals(getMap().getStackMetrics().getMoveBottomKey())) {
      parent = getParent();
      if (parent != null) {
        String oldState = parent.getState();
        if (parent.indexOf(outer) > 0) {
          parent.insert(outer, 0);
          comm = new ChangePiece(parent.getId(), oldState, parent.getState());
        }
        else {
          getMap().reposition(parent, 0);
        }
      }
      else {
        getMap().reposition(outer, 0);
      }
    }
    return comm;
  }

  public void showPopup(MouseEvent e) {
    DragBuffer.getBuffer().clear();
    KeyBuffer.getBuffer().clear();
    KeyBuffer.getBuffer().add(Decorator.getOutermost(this));
    Point p = e.getPoint();
    if (e.getSource() == getMap()) {
      p = getMap().componentCoordinates(e.getPoint());
    }
    if (popup == null) {
      popup = MenuDisplayer.createPopup(Decorator.getOutermost(this));
    }
    if (e.getSource() instanceof Component) {
      final Component target = (Component) e.getSource();
      popup.addPopupMenuListener(new javax.swing.event.PopupMenuListener() {
        public void popupMenuCanceled
          (javax.swing.event.PopupMenuEvent evt) {
          target.repaint();
        }

        public void popupMenuWillBecomeInvisible
          (javax.swing.event.PopupMenuEvent evt) {
          target.repaint();
        }

        public void popupMenuWillBecomeVisible
          (javax.swing.event.PopupMenuEvent evt) {
        }
      });
      popup.show(target, p.x, p.y);
    }
  }

  public String getState() {
    SequenceEncoder se = new SequenceEncoder(';');
    String mapName = map == null ? "null" : map.getId();
    se.append(mapName);
    se.append("" + pos.x).append("" + pos.y);
    return se.getValue();
  }

  public void setState(String s) {
    GamePiece outer = Decorator.getOutermost(this);
    Map oldMap = getMap();
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ';');
    String mapId = st.nextToken();
    Map newMap = null;
    if (!"null".equals(mapId)) {
      for (Enumeration e = GameModule.getGameModule().getComponents(Map.class);
           e.hasMoreElements();) {
        Map m = (Map) e.nextElement();
        if (mapId.equals(m.getId())) {
          newMap = m;
          break;
        }
      }
      if (newMap == null) {
        System.err.println("Could not find map " + mapId);
        return;
      }
    }
    Point newPos = new Point(st.nextInt(0),st.nextInt(0));
    setPosition(newPos);
    if (newMap != oldMap) {
      if (newMap != null) {
        // This will remove from oldMap
        // and set the map to newMap
        newMap.addPiece(outer);
      }
      else if (oldMap != null) {
        oldMap.removePiece(outer);
        setMap(null);
      }
      else {
        setMap(null);
      }
    }
  }

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  /**
   * Get the Highlighter instance for drawing selected pieces.  Note that
   * since this is a static method, all pieces in a module will
   * always use the same Highlighter
   */
  public static Highlighter getHighlighter() {
    if (highlighter == null) {
      highlighter = new ColoredBorder();
    }
    return highlighter;
  }

  /**
   * Set the Highlighter for all pieces
   */
  public static void setHighlighter(Highlighter h) {
    highlighter = h;
  }

  public String getDescription() {
    return "Basic Piece";
  }

  public HelpFile getHelpFile() {
    File dir = new File("docs");
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "BasicPiece.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  private static class Ed implements PieceEditor {
    private JPanel panel;
    private KeySpecifier cloneKeyInput;
    private KeySpecifier deleteKeyInput;
    private JTextField pieceName;
    private ImagePicker picker;
    private String state;

    private Ed(BasicPiece p) {
      state = p.getState();
      initComponents();
      reset(p);
    }

    private void initComponents() {
      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));

      picker = new ImagePicker();
      panel.add(picker);

      cloneKeyInput = new KeySpecifier('C');
      deleteKeyInput = new KeySpecifier('X');

      pieceName = new JTextField(12);
      pieceName.setMaximumSize(pieceName.getPreferredSize());

      Box col = Box.createVerticalBox();
      Box row = Box.createHorizontalBox();
      row.add(new JLabel("Name:  "));
      row.add(pieceName);
      col.add(row);

      row = Box.createHorizontalBox();
      row.add(new JLabel("To Clone:  "));
      row.add(cloneKeyInput);
      col.add(row);

      row = Box.createHorizontalBox();
      row.add(new JLabel("To Delete:  "));
      row.add(deleteKeyInput);
      col.add(row);

      panel.add(col);
    }

    public void reset(BasicPiece p) {
      cloneKeyInput.setKey(p.cloneKey);
      deleteKeyInput.setKey(p.deleteKey);
      pieceName.setText(p.commonName);
      picker.setImageName(p.imageName);
    }

    public Component getControls() {
      return panel;
    }

    public String getState() {
      return state;
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(cloneKeyInput.getKey(), ';');
      String type = se.append(deleteKeyInput.getKey())
        .append(picker.getImageName())
        .append(pieceName.getText()).getValue();
      return BasicPiece.ID + type;
    }
  }
}
