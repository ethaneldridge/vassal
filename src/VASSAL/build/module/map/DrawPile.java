/*
 * $Id$
 *
 * Copyright (c) 2000-2004 by Rodney Kinney and Michael Blumoehr
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
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.widget.CardSlot;
import VASSAL.command.Command;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.Deck;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Stack;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.UniqueIdManager;

import javax.swing.*;
import java.awt.*;
import java.io.File;
import java.net.MalformedURLException;
import java.util.Enumeration;
import java.util.ArrayList;

public class DrawPile extends SetupStack {
  protected Deck dummy = new Deck(); // Used for storing type information
  private Deck myDeck;
  private String reportFormat = "";
  private String mostRecentReshuffleCommand = "Re-shuffle"; // Maintained independent of the Deck because setting the Deck's value to null disables reshuffling
  private VisibilityCondition colorVisibleCondition = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return dummy.isDrawOutline();
    }
  };
  private VisibilityCondition reshuffleVisibleCondition = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return dummy.getReshuffleCommand().length() > 0;
    }
  };
  private static UniqueIdManager idMgr = new UniqueIdManager("Deck");

  protected JPopupMenu buildPopup() {
    JPopupMenu popup = new JPopupMenu();
    return popup.getComponentCount() > 0 ? popup : null;
  }

  public void addTo(Buildable b) {
    map = (Map) b;
    idMgr.add(this);

    GameModule.getGameModule().addCommandEncoder(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
  }

  public void removeFrom(Buildable b) {
    if (map != b) {
      throw new IllegalBuildException("Parent is not " + b);
    }
    GameModule.getGameModule().removeCommandEncoder(this);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    idMgr.remove(this);
  }

  /**
   * Return the DrawPile instance with the matching id or name
   * @param id the Id or ConfigureName of the target DrawPile
   * @return the matching {@link DrawPile}, or null if none found
   * @see DrawPile#getId
   * @see VASSAL.build.AbstractConfigurable#getConfigureName
   */
  public static DrawPile findDrawPile(String id) {
    for (Enumeration e = GameModule.getGameModule().getComponents(Map.class); e.hasMoreElements();) {
      Map m = (Map) e.nextElement();
      for (Enumeration e2 = m.getComponents(DrawPile.class); e2.hasMoreElements();) {
        DrawPile p = (DrawPile) e2.nextElement();
        if (p.getId().equals(id)
            || p.getConfigureName().equals(id)) {
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
  public static final String RESHUFFLABLE = "reshufflable";
  public static final String RESHUFFLE_COMMAND = "reshuffleCommand";
  public static final String RESHUFFLE_TARGET = "reshuffleTarget";
  public static final String RESHUFFLE_MESSAGE = "reshuffleMessage";
  public static final String REPORT_FORMAT = "reportFormat";

  public static final String ALWAYS = "Always";
  public static final String NEVER = "Never";
  public static final String USE_MENU = "Via right-click Menu";

  public String[] getAttributeNames() {
    return new String[]{NAME, OWNING_BOARD, X_POSITION, Y_POSITION, WIDTH, HEIGHT, ALLOW_MULTIPLE,
                        ALLOW_SELECT, FACE_DOWN, SHUFFLE, REVERSIBLE, DRAW, COLOR,
                        RESHUFFLABLE, RESHUFFLE_COMMAND, RESHUFFLE_MESSAGE, RESHUFFLE_TARGET,
                        REPORT_FORMAT};
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
                        "Color",
                        "Include command to send entire deck to another deck",
                        "Menu text",
                        "Message to echo to chat area",
                        "Name of deck to send to",
                        "Report Format"};
  }

  public static class Prompt extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ALWAYS, NEVER, USE_MENU};
    }
  }

  /**
   * generates a prompt with the names of all decks already defined
   */
  public static class AssignedDeckPrompt extends StringEnum {
    public static final String NONE = "<none>";

    public AssignedDeckPrompt() {
      // do nothing
    }

    /**
     * looks for the names of all decks already defined
     */
    public String[] getValidValues(AutoConfigurable target) {
      String[] values;
      DrawPile dp;
      Map m;

      ArrayList l = new ArrayList();
      l.add(NONE);
      for (Enumeration e = GameModule.getGameModule().getComponents(Map.class); e.hasMoreElements();) {
        m = (Map) e.nextElement();
        if (m != null) {
          for (Enumeration e1 = m.getComponents(DrawPile.class); e1.hasMoreElements();) {
            dp = (DrawPile) e1.nextElement();
            if (dp.getConfigureName() != null)
              l.add(dp.getConfigureName());
          }
        }
      }
      values = (String[]) l.toArray(new String[l.size()]);
      return values;
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
                       Color.class,
                       Boolean.class,
                       String.class,
                       FormattedStringConfig.class,
                       AssignedDeckPrompt.class,
                       FormattedStringConfig.class};
  }

  public static class FormattedStringConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, new String[]{GlobalOptions.PLAYER_NAME,
                                  GlobalOptions.PLAYER_SIDE,
                                    GlobalOptions.DECK_NAME,
                                    GlobalOptions.COMMAND_NAME});
    }
  }

  public String getAttributeValueString(String key) {
    if (WIDTH.equals(key)) {
      return "" + dummy.getSize().width;
    }
    else if (HEIGHT.equals(key)) {
      return "" + dummy.getSize().height;
    }
    else if (FACE_DOWN.equals(key)) {
      return dummy.getFaceDownOption();
    }
    else if (SHUFFLE.equals(key)) {
      return dummy.getShuffleOption();
    }
    else if (REVERSIBLE.equals(key)) {
      return "" + dummy.isReversible();
    }
    else if (ALLOW_MULTIPLE.equals(key)) {
      return "" + dummy.isAllowMultipleDraw();
    }
    else if (ALLOW_SELECT.equals(key)) {
      return "" + dummy.isAllowSelectDraw();
    }
    else if (DRAW.equals(key)) {
      return "" + dummy.isDrawOutline();
    }
    else if (COLOR.equals(key)) {
      return ColorConfigurer.colorToString(dummy.getOutlineColor());
    }
    else if (RESHUFFLABLE.equals(key)) {
      return "" + (dummy.getReshuffleCommand().length() > 0);
    }
    else if (RESHUFFLE_COMMAND.equals(key)) {
      return dummy.getReshuffleCommand();
    }
    else if (RESHUFFLE_TARGET.equals(key)) {
      return dummy.getReshuffleTarget();
    }
    else if (RESHUFFLE_MESSAGE.equals(key)) {
      return dummy.getReshuffleMessage();
    }
    else if (REPORT_FORMAT.equals(key)) {
      return reportFormat;
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
      dummy.getSize().width = ((Integer) value).intValue();
    }
    else if (HEIGHT.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      dummy.getSize().height = ((Integer) value).intValue();
    }
    else if (FACE_DOWN.equals(key)) {
      dummy.setFaceDownOption((String) value);
    }
    else if (SHUFFLE.equals(key)) {
      dummy.setShuffleOption((String) value);
    }
    else if (REVERSIBLE.equals(key)) {
      if (value instanceof Boolean) {
        dummy.setReversible(Boolean.TRUE.equals(value));
      }
      else {
        dummy.setReversible("true".equals(value));
      }
    }
    else if (ALLOW_MULTIPLE.equals(key)) {
      if (value instanceof Boolean) {
        dummy.setAllowMultipleDraw(Boolean.TRUE.equals(value));
      }
      else {
        dummy.setAllowMultipleDraw("true".equals(value));
      }
    }
    else if (ALLOW_SELECT.equals(key)) {
      if (value instanceof Boolean) {
        dummy.setAllowSelectDraw(Boolean.TRUE.equals(value));
      }
      else {
        dummy.setAllowSelectDraw("true".equals(value));
      }
    }
    else if (DRAW.equals(key)) {
      if (value instanceof Boolean) {
        dummy.setDrawOutline(Boolean.TRUE.equals(value));
      }
      else {
        dummy.setDrawOutline("true".equals(value));
      }
    }
    else if (COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      dummy.setOutlineColor((Color) value);
    }
    else if (RESHUFFLABLE.equals(key)) {
      boolean b = "true".equals(value) || Boolean.TRUE.equals(value);
      dummy.setReshuffleCommand(b ? mostRecentReshuffleCommand : "");
    }
    else if (RESHUFFLE_COMMAND.equals(key)) {
      String s = (String) value;
      if (s != null
          && s.length() > 0) {
        mostRecentReshuffleCommand = s;
      }
      dummy.setReshuffleCommand(mostRecentReshuffleCommand);
    }
    else if (RESHUFFLE_TARGET.equals(key)) {
      dummy.setReshuffleTarget((String) value);
    }
    else if (RESHUFFLE_MESSAGE.equals(key)) {
      dummy.setReshuffleMessage((String) value);
    }
    else if (REPORT_FORMAT.equals(key)) {
      reportFormat = ((String) value);
    }
    else {
      super.setAttribute(key, value);
    }
  }


  public VisibilityCondition getAttributeVisibility(String name) {
    if (COLOR.equals(name)) {
      return colorVisibleCondition;
    }
    else if (RESHUFFLE_COMMAND.equals(name)
        || RESHUFFLE_MESSAGE.equals(name)
        || RESHUFFLE_TARGET.equals(name)) {
      return reshuffleVisibleCondition;
    }
    else {
      return null;
    }
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[]{CardSlot.class};
  }

  public Point getPosition() {
    Point p = new Point(pos);
    Board b = map.getBoardByName(owningBoardName);
    if (b != null) {
      p.translate(b.bounds().x, b.bounds().y);
    }
    return p;
  }

  public Map getMap() {
    return map;
  }

  public Rectangle boundingBox() {
    return (myDeck == null) ? null : myDeck.boundingBox();
  }

  public Command addToContents(GamePiece p) {
    return map.placeOrMerge(p, getPosition());
  }

  protected Stack initializeContents() {
    Stack s = super.initializeContents();
    myDeck = new Deck(getDeckType());
    myDeck.setDeckName(getConfigureName());
    myDeck.setReportFormat(reportFormat);
    for (Enumeration e = s.getPieces(); e.hasMoreElements();) {
      myDeck.add((GamePiece) e.nextElement());
    }
    myDeck.setFaceDown(!Deck.NEVER.equals(dummy.getFaceDownOption()));
    return myDeck;
  }

  private String getDeckType() {
    return dummy.getType();
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

  public Command decode(String s) {
    if (s.startsWith(getId() + '\t')) {
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, '\t');
      st.nextToken();
      String contentsId = st.nextToken();
      return new PlaceDeck(this, contentsId, st.hasMoreTokens() && "true".equals(st.nextToken()));
    }
    else {
      return super.decode(s);
    }
  }

  /** Only necessary for backward compatibility */
  public static class PlaceDeck extends Command {
    private DrawPile drawPile;
    private String contentsId;
    private boolean faceDown;

    public PlaceDeck(DrawPile drawPile, String contentsId, boolean faceDown) {
      this.drawPile = drawPile;
      this.contentsId = contentsId;
      this.faceDown = faceDown;
    }

    // Replace the identified Stack with a Deck
    public void executeCommand() {
      Stack stack = (Stack) GameModule.getGameModule().getGameState().getPieceForId(contentsId);
      if (stack != null) {
        Deck deck = new Deck(drawPile.getDeckType());
        for (Enumeration e = stack.getPieces(); e.hasMoreElements();) {
          deck.add((GamePiece) e.nextElement());
        }
        deck.setFaceDown(faceDown);
        Point p = new Point(drawPile.pos);
        if (drawPile.owningBoardName != null) {
          Rectangle r = drawPile.map.getBoardByName(drawPile.owningBoardName).bounds();
          p.translate(r.x, r.y);
        }
        drawPile.map.placeAt(deck, p);
        GameModule.getGameModule().getGameState().addPiece(deck);
        GameModule.getGameModule().getGameState().removePiece(contentsId);
        drawPile.setStackInitialized(true);
      }
    }

    public Command myUndoCommand() {
      return null;
    }
  }
}
