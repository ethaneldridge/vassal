package VASSAL.build.module.map;

import VASSAL.build.*;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.widget.PieceSlot;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.Configurer;
import VASSAL.configure.StringEnum;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceCloner;
import VASSAL.counters.Stack;

import java.awt.*;
import java.io.File;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;

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

/**
 * This is the "At-Start Stack" component, which initializes a Map or Board with a specified stack.
 * Because it uses a regular stack, this component is better suited for limited-force-pool collections
 * of counters than a {@link DrawPile}
 *
 */
public class SetupStack extends AbstractConfigurable implements GameComponent, CommandEncoder {
  public static final String COMMAND_PREFIX = "SETUP_STACK\t";
  protected Point pos = new Point();
  public static final String OWNING_BOARD = "owningBoard";
  public final static String X_POSITION = "x";
  public final static String Y_POSITION = "y";
  protected Map map;
  protected String owningBoardName;
  private boolean stackInitialized;
  protected String id;
  public static final String NAME = "name";
  private PieceCloner cloner = new PieceCloner();

  public void setup(boolean gameStarting) {
    if (gameStarting && !stackInitialized && isOwningBoardActive()) {
      Stack s = initializeContents();
      Point p = new Point(pos);
      if (owningBoardName != null) {
        Rectangle r = map.getBoardByName(owningBoardName).bounds();
        p.translate(r.x, r.y);
      }
      map.placeAt(s, p);
    }
    if (!gameStarting) {
      stackInitialized = false;
    }
  }

  public Command getRestoreCommand() {
    return new MarkInitialized(getId());
  }

  /**
   * Return true if the stack that this component places on the map board
   * has already been placed
   * @return
   */
  public boolean isStackInitialized() {
    return stackInitialized;
  }

  public void setStackInitialized(boolean stackInitialized) {
    this.stackInitialized = stackInitialized;
  }

  public Command decode(String command) {
    Command c = null;
    if (command.startsWith(COMMAND_PREFIX)) {
      return new MarkInitialized(command.substring(COMMAND_PREFIX.length()));
    }
    return c;
  }

  public String encode(Command c) {
    String s = null;
    if (c instanceof MarkInitialized) {
      return COMMAND_PREFIX + ((MarkInitialized) c).id;
    }
    return s;
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Name", "Belongs to Board", "X position", "Y position"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class, OwningBoardPrompt.class, Integer.class, Integer.class};
  }

  public String[] getAttributeNames() {
    return new String[]{NAME, OWNING_BOARD, X_POSITION, Y_POSITION};
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (OWNING_BOARD.equals(key)) {
      return owningBoardName;
    }
    else if (X_POSITION.equals(key)) {
      return "" + pos.x;
    }
    else if (Y_POSITION.equals(key)) {
      return "" + pos.y;
    }
    else {
      return null;
    }
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (OWNING_BOARD.equals(key)) {
      if (OwningBoardPrompt.ANY.equals(value)) {
        owningBoardName = null;
      }
      else {
        owningBoardName = (String) value;
      }
    }
    else if (X_POSITION.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      pos.x = ((Integer) value).intValue();
    }
    else if (Y_POSITION.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      pos.y = ((Integer) value).intValue();
    }
  }

  private static int instanceCount = 0;
  public void addTo(Buildable parent) {
    map = (Map)parent;
    setId("SetupStack"+instanceCount++);

    GameModule.getGameModule().addCommandEncoder(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);

  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[]{PieceSlot.class};
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "SetupStack.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public static String getConfigureTypeName() {
    return "At-Start Stack";
  }

  public void removeFrom(Buildable parent) {
    instanceCount--;
  }

  protected boolean isOwningBoardActive() {
    boolean active = false;
    if (owningBoardName == null) {
      active = true;
    }
    else if (map.getBoardByName(owningBoardName) != null) {
      active = true;
    }
    return active;
  }

  protected Stack initializeContents() {
    Stack s = createStack();
    Configurable[] c = getConfigureComponents();
    for (int i = 0; i < c.length; ++i) {
      PieceSlot slot = (PieceSlot) c[i];
      GamePiece p = slot.getPiece();
      p = cloner.clonePiece(p);
      GameModule.getGameModule().getGameState().addPiece(p);
      s.add(p);
    }
    GameModule.getGameModule().getGameState().addPiece(s);
    return s;
  }

  protected Stack createStack() {
    Stack s = new Stack();
    return s;
  }

  public void setId(String id) {
    this.id = id;
  }

  public String getId() {
    return id;
  }

  /**
   *
   * @param id
   * @return the {@link SetupStack} with the given id
   */
  public static SetupStack findSetupStack(String id) {
    for (Enumeration e = GameModule.getGameModule().getComponents(Map.class); e.hasMoreElements();) {
      Map m = (Map) e.nextElement();
      for (Enumeration e2 = m.getComponents(SetupStack.class); e2.hasMoreElements();) {
        SetupStack s = (SetupStack) e2.nextElement();
        if (s.getId().equals(id)) {
          return s;
        }
      }
    }
    return null;
  }

  public Configurer getConfigurer() {
    config = null; // Don't cache the Configurer so that the list of available boards won't go stale
    return super.getConfigurer();
  }

  public static class OwningBoardPrompt extends StringEnum {
    public static final String ANY = "<any>";

    public OwningBoardPrompt() {
    }

    public String[] getValidValues(AutoConfigurable target) {
      String[] values;
      if (target instanceof SetupStack) {
        ArrayList l = new ArrayList();
        l.add(ANY);
        Map m = ((SetupStack) target).map;
        if (m != null) {
          l.addAll(Arrays.asList(m.getBoardPicker().getAllowableBoardNames()));
        }
        else {
          for (Enumeration e = GameModule.getGameModule().getComponents(Map.class); e.hasMoreElements();) {
            m = (Map) e.nextElement();
            l.addAll(Arrays.asList(m.getBoardPicker().getAllowableBoardNames()));
          }
        }
        values = (String[]) l.toArray(new String[l.size()]);
      }
      else {
        values = new String[]{ANY};
      }
      return values;
    }
  }

  /**
   * This command indicates that a Stack has been created already in a saved
   * game, so there is no need to initialized a new one when the game is opened.
   */
  public static class MarkInitialized extends Command {
    private String id;

    public MarkInitialized(String id) {
      this.id = id;
    }

    protected void executeCommand() {
      SetupStack s = SetupStack.findSetupStack(id);
      if (s != null) {
        s.stackInitialized = true;
      }
    }

    protected Command myUndoCommand() {
      return null;
    }
  }
}
