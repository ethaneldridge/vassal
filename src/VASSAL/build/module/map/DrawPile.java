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
package VASSAL.build.module.map;

import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.IllegalBuildException;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.widget.CardSlot;
import VASSAL.command.ChangePiece;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.*;
import VASSAL.tools.SequenceEncoder;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.net.MalformedURLException;
import java.util.Enumeration;
import java.util.Vector;

public class DrawPile extends SetupStack implements Drawable, MouseListener {
  protected Stack contents;
  protected Dimension size = new Dimension(40, 40);
  protected int dragCount = 0;
  protected boolean shuffle = true;
  protected boolean faceDown = false;
  private String faceDownOption = ALWAYS;
  private String shuffleOption = ALWAYS;
  private boolean allowMultipleDraw = false;
  private boolean allowSelectDraw = false;
  private boolean reversible = false;
  private boolean drawOutline = true;
  private Color outlineColor = Color.black;
  private boolean isActive;
  private Vector nextDraw;

  protected Action faceDownAction;

  protected JPopupMenu buildPopup() {
    JPopupMenu popup = new JPopupMenu();
    if (USE_MENU.equals(shuffleOption)) {
      popup.add(new AbstractAction("Shuffle") {
        public void actionPerformed(ActionEvent e) {
          GameModule.getGameModule().sendAndLog(shuffle());
          map.repaint();
        }
      }).setFont(MenuDisplayer.POPUP_MENU_FONT);
    }
    if (USE_MENU.equals(faceDownOption)) {
      faceDownAction = new AbstractAction(faceDown ? "Face up" : "Face down") {
        public void actionPerformed(ActionEvent e) {
          GameModule.getGameModule().sendAndLog(setFaceDown(!faceDown));
          map.repaint();
        }
      };
      popup.add(faceDownAction).setFont(MenuDisplayer.POPUP_MENU_FONT);
    }
    if (reversible) {
      popup.add(new AbstractAction("Reverse order") {
        public void actionPerformed(ActionEvent e) {
          GameModule.getGameModule().sendAndLog(reverse());
          map.repaint();
        }
      }).setFont(MenuDisplayer.POPUP_MENU_FONT);
    }
    if (allowMultipleDraw) {
      popup.add(new AbstractAction("Draw multiple cards") {
        public void actionPerformed(ActionEvent e) {
          promptForDragCount();
        }
      }).setFont(MenuDisplayer.POPUP_MENU_FONT);
    }
    if (allowSelectDraw) {
      popup.add(new AbstractAction("Draw specific cards") {
        public void actionPerformed(ActionEvent e) {
          promptForNextDraw();
        }
      }).setFont(MenuDisplayer.POPUP_MENU_FONT);
    }
    return popup.getComponentCount() > 0 ? popup : null;
  }

  private void promptForNextDraw() {
    final JDialog d = new JDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class, map.getView()), true);
    d.setTitle("Draw");
    d.getContentPane().setLayout(new BoxLayout(d.getContentPane(), BoxLayout.Y_AXIS));
    final String[] pieces = new String[contents.getPieceCount()];
    for (int i = 0; i < pieces.length; ++i) {
      pieces[pieces.length - i - 1] = Decorator.getInnermost(contents.getPieceAt(i)).getName();
    }
    final JList list = new JList(pieces);
    list.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    d.getContentPane().add(new JScrollPane(list));
    d.getContentPane().add(new JLabel("Select cards to draw"));
    d.getContentPane().add(new JLabel("Then click and drag from the deck."));
    Box box = Box.createHorizontalBox();
    JButton b = new JButton("Ok");
    b.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        nextDraw = new Vector();
        int[] selection = list.getSelectedIndices();
        for (int i = 0; i < selection.length; ++i) {
          nextDraw.addElement(contents.getPieceAt(pieces.length - selection[i] - 1));
        }
        d.dispose();
      }
    });
    box.add(b);
    b = new JButton("Cancel");
    b.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        d.dispose();
      }
    });
    box.add(b);
    d.getContentPane().add(box);
    d.pack();
    d.setLocationRelativeTo(d.getOwner());
    d.setVisible(true);
  }

  public void addTo(Buildable b) {
    map = (Map) b;
    map.addDrawComponent(this);
    map.addLocalMouseListener(this);
    int count = 0;
    for (Enumeration e = GameModule.getGameModule().getComponents(Map.class); e.hasMoreElements();) {
      Map m = (Map) e.nextElement();
      for (Enumeration e2 = m.getComponents(DrawPile.class); e2.hasMoreElements();) {
        e2.nextElement();
        count++;
      }
    }
    // Our map doesn't yet appear in the GameModule
    for (Enumeration e = map.getComponents(DrawPile.class); e.hasMoreElements();) {
      e.nextElement();
      count++;
    }
    setId("Deck" + count);

    GameModule.getGameModule().addCommandEncoder(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
  }

  public void removeFrom(Buildable b) {
    if (map != b) {
      throw new IllegalBuildException("Parent is not " + b);
    }
    map.removeDrawComponent(this);
    map.removeLocalMouseListener(this);
    GameModule.getGameModule().removeCommandEncoder(this);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
  }

  /**
   *
   * @param id
   * @return the {@link DrawPile} with the given id
   */
  public static DrawPile findDrawPile(String id) {
    for (Enumeration e = GameModule.getGameModule().getComponents(Map.class); e.hasMoreElements();) {
      Map m = (Map) e.nextElement();
      for (Enumeration e2 = m.getComponents(DrawPile.class); e2.hasMoreElements();) {
        DrawPile p = (DrawPile) e2.nextElement();
        if (p.getId().equals(id)) {
          return p;
        }
      }
    }
    return null;
  }


  public final static String WIDTH = "width";
  public final static String HEIGHT = "height";
  public static final String ALLOW_MULTIPLE = "allowMultiple";
  public static final String ALLOW_SELECT = "allowSelect";
  public static final String FACE_DOWN = "faceDown";
  public static final String SHUFFLE = "shuffle";
  public static final String REVERSIBLE = "reversible";
  public static final String DRAW = "draw";
  public static final String COLOR = "color";

  public static final String ALWAYS = "Always";
  public static final String NEVER = "Never";
  public static final String USE_MENU = "Via right-click Menu";

  public String[] getAttributeNames() {
    return new String[]{NAME, OWNING_BOARD, X_POSITION, Y_POSITION, WIDTH, HEIGHT, ALLOW_MULTIPLE, ALLOW_SELECT, FACE_DOWN, SHUFFLE, REVERSIBLE, DRAW, COLOR};
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Name",
                        "Belongs to board",
                        "X position",
                        "Y position",
                        "Width",
                        "Height",
                        "Allow Multiple Cards to be Drawn",
                        "Allow Specific Cards to be Drawn",
                        "Contents are Face-down",
                        "Re-shuffle",
                        "Reversible",
                        "Draw Outline when empty",
                        "Color"};
  }

  public static class Prompt extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ALWAYS, NEVER, USE_MENU};
    }
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class,
                       OwningBoardPrompt.class,
                       Integer.class,
                       Integer.class,
                       Integer.class,
                       Integer.class,
                       Boolean.class,
                       Boolean.class,
                       Prompt.class,
                       Prompt.class,
                       Boolean.class,
                       Boolean.class,
                       Color.class};
  }

  public String getAttributeValueString(String key) {
    if (WIDTH.equals(key)) {
      return "" + size.width;
    }
    else if (HEIGHT.equals(key)) {
      return "" + size.height;
    }
    else if (FACE_DOWN.equals(key)) {
      return faceDownOption;
    }
    else if (SHUFFLE.equals(key)) {
      return shuffleOption;
    }
    else if (REVERSIBLE.equals(key)) {
      return "" + reversible;
    }
    else if (ALLOW_MULTIPLE.equals(key)) {
      return "" + allowMultipleDraw;
    }
    else if (ALLOW_SELECT.equals(key)) {
      return "" + allowSelectDraw;
    }
    else if (DRAW.equals(key)) {
      return "" + drawOutline;
    }
    else if (COLOR.equals(key)) {
      return ColorConfigurer.colorToString(outlineColor);
    }
    else {
      return super.getAttributeValueString(key);
    }
  }


  public void setAttribute(String key, Object value) {
    if (value == null) {
      return;
    }
    if (WIDTH.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      size.width = ((Integer) value).intValue();
    }
    else if (HEIGHT.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      size.height = ((Integer) value).intValue();
    }
    else if (FACE_DOWN.equals(key)) {
      faceDownOption = (String) value;
      faceDown = !faceDownOption.equals(NEVER);
    }
    else if (SHUFFLE.equals(key)) {
      shuffleOption = (String) value;
    }
    else if (REVERSIBLE.equals(key)) {
      if (value instanceof Boolean) {
        reversible = Boolean.TRUE.equals(value);
      }
      else {
        reversible = "true".equals(value);
      }
    }
    else if (ALLOW_MULTIPLE.equals(key)) {
      if (value instanceof Boolean) {
        allowMultipleDraw = Boolean.TRUE.equals(value);
      }
      else {
        allowMultipleDraw = "true".equals(value);
      }
    }
    else if (ALLOW_SELECT.equals(key)) {
      if (value instanceof Boolean) {
        allowSelectDraw = Boolean.TRUE.equals(value);
      }
      else {
        allowSelectDraw = "true".equals(value);
      }
    }
    else if (DRAW.equals(key)) {
      if (value instanceof Boolean) {
        drawOutline = Boolean.TRUE.equals(value);
      }
      else {
        drawOutline = "true".equals(value);
      }
    }
    else if (COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      outlineColor = (Color) value;
    }
    else {
      super.setAttribute(key, value);
    }
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (COLOR.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return drawOutline;
        }
      };
    }
    else {
      return null;
    }
  }

  /** Shuffle the contents of the Deck */
  public Command shuffle() {
    Vector indices = new Vector();
    for (int i = 0; i < contents.getPieceCount(); ++i) {
      indices.addElement(new Integer(i));
    }
    Vector newContents = new Vector();
    DragBuffer.getBuffer().clear();
    for (int count = contents.getPieceCount(); count > 0; --count) {
      int i = (int) (GameModule.getGameModule().getRNG().nextFloat()
          * indices.size());
      int index = ((Integer) indices.elementAt(i)).intValue();
      indices.removeElementAt(i);
      newContents.addElement(contents.getPieceAt(index));
    }
    return setContents(newContents.elements());
  }

  /** Set the contents of this Deck to an Enumeration of GamePieces */
  protected Command setContents(Enumeration e) {
    ChangeTracker track = new ChangeTracker(contents);
    contents.removeAll();
    while (e.hasMoreElements()) {
      contents.add((GamePiece) e.nextElement());
    }
    return track.getChangeCommand();
  }

  /** Reverse the order of the contents of the Deck */
  public Command reverse() {
    Vector v = new Vector();
    for (Enumeration e = contents.getPiecesInReverseOrder();
         e.hasMoreElements();) {
      v.addElement(e.nextElement());
    }
    return setContents(v.elements());
  }

  public Command setFaceDown(boolean down) {
    Command c;
    faceDown = !faceDown;
    if (!faceDown) {
      c = new NullCommand();
      for (Enumeration e = contents.getPieces(); e.hasMoreElements();) {
        GamePiece p = (GamePiece) e.nextElement();
        if (p.getProperty(Obscurable.ID) != null) {
          ChangeTracker tracker = new ChangeTracker(p);
          p.setProperty(Obscurable.ID, null);
          c.append(tracker.getChangeCommand());
        }
      }
      return c = c.append(new SetContents(this, contents.getId(), faceDown));
    }
    else {
      c = new SetContents(this, contents.getId(), faceDown);
    }
    return c;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[]{CardSlot.class};
  }

  public void draw(java.awt.Graphics g, Map map) {
    Point p = map.componentCoordinates(getPosition());
    draw(g, p.x, p.y, map.getView(), map.getZoom());
  }

  public void draw(java.awt.Graphics g, int x, int y, Component obs, double zoom) {
    int count = 0;
    if (contents != null
        && (count = contents.getPieceCount()) > 0) {
      GamePiece top = contents.topPiece();
      Rectangle r = top.getShape().getBounds();
      r.setLocation(x + (int) (zoom * (r.x)), y + (int) (zoom * (r.y)));
      r.setSize((int) (zoom * r.width), (int) (zoom * r.height));
      count = count > 10 ? 10 : count;
      for (int i = 0; i < count - 1; ++i) {
        g.setColor(Color.white);
        g.fillRect(r.x + (int) (zoom * 2 * i),
                   r.y - (int) (zoom * 2 * i), r.width, r.height);
        g.setColor(Color.black);
        g.drawRect(r.x + (int) (zoom * 2 * i),
                   r.y - (int) (zoom * 2 * i), r.width, r.height);
      }
      if (faceDown) {
        Obscurable.setAllHidden(true);
        Object oldValue = top.getProperty(Obscurable.ID);
        top.setProperty(Obscurable.ID, "dummy");
        top.draw(g, x + (int) (zoom * 2 * (count - 1)),
                 y - (int) (zoom * 2 * (count - 1)), obs, zoom);
        top.setProperty(Obscurable.ID, oldValue);
        Obscurable.setAllHidden(false);
      }
      else {
        top.draw(g, x + (int) (zoom * 2 * (count - 1)),
                 y - (int) (zoom * 2 * (count - 1)), obs, zoom);
      }
    }
    else {
      if (drawOutline) {
        Rectangle r = boundingBox();
        r.setLocation(x + (int) (zoom * (r.x - getPosition().x)), y + (int) (zoom * (r.y - getPosition().y)));
        r.setSize((int) (zoom * r.width), (int) (zoom * r.height));
        g.setColor(outlineColor);
        g.drawRect(r.x, r.y, r.width, r.height);
      }
    }
  }

  public Point getPosition() {
    Point p = new Point(pos);
    Board b = map.getBoardByName(owningBoardName);
    if (b != null) {
      p.translate(b.bounds().x, b.bounds().y);
    }
    return p;
  }

  /**
   * The bounds of this deck in the {@link Map} window, adjusted for owning board, if any
   * @return
   */
  public Rectangle boundingBox() {
    Rectangle r = null;
    if (contents != null
        && contents.getPieceCount() > 0) {
      GamePiece p = contents.topPiece();
      r = p.getShape().getBounds();
      r.translate(pos.x, pos.y);
      for (int i = 0, n = Math.min(10, contents.getPieceCount()); i < n; ++i) {
        r.setSize(r.width + 2, r.height + 2);
        r.y -= 2;
      }
    }
    else {
      r = new Rectangle(pos.x - size.width / 2, pos.y - size.height / 2, size.width, size.height);
    }
    if (owningBoardName != null) {
      for (Enumeration e = map.getAllBoards(); e.hasMoreElements();) {
        Board b = (Board) e.nextElement();
        if (owningBoardName.equals(b.getName())) {
          r.translate(b.bounds().x, b.bounds().y);
          break;
        }
      }
    }
    return r;
  }

  /**
   * Add the given number of GamePieces to the Drag buffer
   */
  protected Command addToDragBuffer(int count) {
    Command c = new NullCommand();
    DragBuffer.getBuffer().clear();
    if (ALWAYS.equals(shuffleOption)) {
      Vector indices = new Vector();
      for (int i = 0; i < contents.getPieceCount(); ++i) {
        indices.addElement(new Integer(i));
      }
      while (count-- > 0) {
        int i = (int) (GameModule.getGameModule().getRNG().nextFloat()
            * indices.size());
        int index = ((Integer) indices.elementAt(i)).intValue();
        indices.removeElementAt(i);
        GamePiece p = contents.getPieceAt(index);
        c = c.append(addToDragBuffer(p));
      }
    }
    else {
      Enumeration e = contents.getPiecesInReverseOrder();
      while (count-- > 0 && e.hasMoreElements()) {
        GamePiece p = (GamePiece) e.nextElement();
        c = c.append(addToDragBuffer(p));
      }
    }
    return c;
  }

  protected Command addToDragBuffer(GamePiece p) {
    Command c = null;
    if (faceDown) {
      ChangeTracker tracker = new ChangeTracker(p);
      p.setProperty(Obscurable.ID, GameModule.getGameModule().getUserId());
      c = tracker.getChangeCommand();
    }
    DragBuffer.getBuffer().add(p);
    return c;
  }

  public void promptForDragCount() {
    while (true) {
      String s = JOptionPane.showInputDialog("Enter number to grab.\nThen click and drag to draw that number.");
      if (s != null) {
        try {
          dragCount = Integer.parseInt(s);
          dragCount = Math.min(dragCount, contents.getPieceCount());
          if (dragCount >= 0) {
            break;
          }
        }
        catch (NumberFormatException ex) {
        }
      }
    }
  }

  public void mousePressed(MouseEvent evt) {
    if (isActive && boundingBox().contains(evt.getPoint())
        && contents.getPieceCount() > 0) {
      if (!evt.isMetaDown()) {
        if (nextDraw != null) {
          DragBuffer.getBuffer().clear();
          for (Enumeration e = nextDraw.elements(); e.hasMoreElements();) {
            GamePiece p = (GamePiece) e.nextElement();
            if (p.getProperty(Obscurable.ID) != null) {
              p.setProperty(Obscurable.ID, GameModule.getUserId());
            }
            DragBuffer.getBuffer().add(p);
          }
        }
        else {
          if (dragCount == 0) {
            dragCount = 1;
          }
          GameModule.getGameModule().sendAndLog(addToDragBuffer(dragCount));
        }
      }
    }
    else {
      dragCount = 0;
      nextDraw = null;
    }
  }

  public void mouseReleased(MouseEvent evt) {
    if (isActive && boundingBox().contains(evt.getPoint())) {
      if (evt.isMetaDown()) {
        JPopupMenu popup = buildPopup();
        if (popup != null) {
          Point p = map.componentCoordinates(evt.getPoint());
          popup.show(map.getView(), p.x, p.y);
        }
      }
      else {
        GamePiece p = map.findPiece(evt.getPoint(), PieceFinder.MOVABLE);
        if (p != null) {
          String oldContents = contents.getState();
          GameModule.getGameModule().sendAndLog
              (addToContents(p).append
               (new ChangePiece(contents.getId(),
                                oldContents,
                                contents.getState())));
        }
      }
    }
    if (!evt.isMetaDown()) {
      dragCount = 0;
    }
  }

  public Map getMap() {
    return map;
  }

  public Command addToContents(GamePiece p) {
    Command comm;
    ChangeTracker contentsTracker = new ChangeTracker(contents);
    if (p instanceof Stack) {
      Command c = new NullCommand();
      for (Enumeration e = ((Stack) p).getPieces();
           e.hasMoreElements();) {
        GamePiece sub = (GamePiece) e.nextElement();
        c = c.append(addToContents(sub));
      }
      comm = c;
    }
    else {
      contents.add(p);
      comm = contentsTracker.getChangeCommand();
    }
    return comm;
  }

  public void mouseEntered(MouseEvent e) {
  }

  public void mouseExited(MouseEvent e) {
  }

  public void mouseClicked(MouseEvent e) {
  }

  public void setup(boolean gameStarting) {
    isActive = gameStarting && isOwningBoardActive();
    if (isActive) {
      map.addDrawComponent(this);
    }
    else {
      map.removeDrawComponent(this);
    }
    if (!isActive) {
      contents = null;
    }
    else if (contents == null) {
      contents = initializeContents();
    }
  }

  public Command getRestoreCommand() {
    return new SetContents(this, contents == null ? "null" : contents.getId(), faceDown);
  }

  public static class SetContents extends Command {
    private DrawPile deck;
    private String contentsId;
    private boolean faceDown;

    public SetContents(DrawPile deck, String contentsId, boolean faceDown) {
      this.deck = deck;
      this.contentsId = contentsId;
      this.faceDown = faceDown;
    }

    public void executeCommand() {
      deck.contents = (Stack) GameModule.getGameModule().getGameState().getPieceForId(contentsId);
      deck.faceDown = faceDown;
      if (deck.map != null) {
        deck.map.repaint();
      }
    }

    public Command myUndoCommand() {
      return null;
    }
  }

  public Command decode(String s) {
    if (s.startsWith(getId() + '\t')) {
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, '\t');
      st.nextToken();
      String contentsId = st.nextToken();
      boolean faceDown = st.hasMoreTokens() && "true".equals(st.nextToken());
      return new SetContents(this, contentsId, faceDown);
    }
    else {
      return null;
    }
  }

  public String encode(Command c) {
    if (c instanceof SetContents
        && ((SetContents) c).deck == this) {
      SequenceEncoder se = new SequenceEncoder(getId(), '\t');
      se.append(((SetContents) c).contentsId);
      se.append("" + ((SetContents) c).faceDown);
      return se.getValue();
    }
    else {
      return null;
    }
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "Deck.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public static String getConfigureTypeName() {
    return "Deck";
  }

}
