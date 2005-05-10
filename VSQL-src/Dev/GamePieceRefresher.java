/*
 * $Id$
 * 
 * Copyright (c) 2005 by Brent Easton
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License (LGPL) as published by
 * the Free Software Foundation.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, copies are available at
 * http://www.opensource.org.
 */

package Dev;

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;

import VASSAL.build.AbstractBuildable;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.PieceWindow;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.widget.PieceSlot;
import VASSAL.command.AddPiece;
import VASSAL.command.RemovePiece;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Labeler;
import VASSAL.counters.Stack;
import VASSAL.tools.LaunchButton;
import VSQL.VSQLEmbellishment;

public class GamePieceRefresher extends AbstractConfigurable {

  protected LaunchButton launch;
  protected Map map;
  protected boolean matchLayer = true;
  protected boolean keepLabels = true;

  public static final String BUTTON_TEXT = "text";
  public static final String NAME = "name";
  public static final String MATCH_LAYER = "matchLayer";
  public static final String KEEP_LABELS = "keepLabels";

  public GamePieceRefresher() {
    ActionListener refreshAction = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        refresh();
      }
    };
    launch = new LaunchButton(null, BUTTON_TEXT, null, null, refreshAction);
  }

  public static String getConfigureTypeName() {
    return "Gamepiece Refresher";
  }

  public String[] getAttributeNames() {
    String s[] = { NAME, BUTTON_TEXT, MATCH_LAYER, KEEP_LABELS };
    return s;
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Name", "Button text", "Match on first layer image name also?", "Keep Text Labels?" };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, String.class, Boolean.class, Boolean.class };
  }

  public void addTo(Buildable parent) {
    GameModule.getGameModule().getToolBar().add(getComponent());
  }

  /**
   * The component to be added to the control window toolbar
   */
  protected java.awt.Component getComponent() {
    return launch;
  }

  public void setAttribute(String key, Object o) {
    if (NAME.equals(key)) {
      setConfigureName((String) o);
      launch.setToolTipText((String) o);
    }
    else if (MATCH_LAYER.equals(key)) {
      if (o instanceof String) {
        o = new Boolean((String) o);
      }
      matchLayer = ((Boolean) o).booleanValue();
    }
    else if (KEEP_LABELS.equals(key)) {
      if (o instanceof String) {
        o = new Boolean((String) o);
      }
      keepLabels = ((Boolean) o).booleanValue();
    }
    else {
      launch.setAttribute(key, o);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (MATCH_LAYER.equals(key)) {
      return matchLayer + "";
    }
    else if (KEEP_LABELS.equals(key)) {
      return keepLabels + "";
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void removeFrom(Buildable b) {
    GameModule.getGameModule().getToolBar().remove(getComponent());
    GameModule.getGameModule().getToolBar().revalidate();
  }

  public HelpFile getHelpFile() {
    return null;
  }

  protected void refresh() {

    // First, Find all maps with pieces
    HashMap mapList = new HashMap();
    Enumeration e = GameModule.getGameModule().getGameState().getPieces();
    while (e.hasMoreElements()) {
      GamePiece pieceOrStack = (GamePiece) e.nextElement();
      if (pieceOrStack instanceof Stack) {
        Enumeration se = ((Stack) pieceOrStack).getPieces();
        while (se.hasMoreElements()) {
          map = ((GamePiece) se.nextElement()).getMap();
          mapList.put(map, map);
        }
      }
      else {
        map = pieceOrStack.getMap();
        mapList.put(map, map);
      }
    }

    // Now process the pieces on each map
    Iterator maps = mapList.values().iterator();
    while (maps.hasNext()) {
      Map map = (Map) maps.next();
      GamePiece pieces[] = map.getPieces();
      for (int i = 0; i < pieces.length; i++) {
        GamePiece pieceOrStack = pieces[i];
        if (pieceOrStack instanceof Stack) {
          Enumeration se = ((Stack) pieceOrStack).getPieces();
          while (se.hasMoreElements()) {
            processPiece((GamePiece) se.nextElement());
          }
        }
        else {
          processPiece(pieceOrStack);
        }
      }
    }
  }

  protected void processPiece(GamePiece oldPiece) {

    GamePiece newPiece = findNewPiece(oldPiece);

    if (newPiece != null) {
      Map map = oldPiece.getMap();
      Point pos = oldPiece.getPosition();
      map.placeOrMerge(newPiece, pos);
      new RemovePiece(Decorator.getOutermost(oldPiece)).execute();
    }

  }

  // Find a new Piece matching the oldpiece
  protected GamePiece findNewPiece(GamePiece oldPiece) {
    GamePiece newPiece = null;

    Enumeration pwe = GameModule.getGameModule().getComponents(PieceWindow.class);
    while (pwe.hasMoreElements() && newPiece == null) {
      AbstractBuildable b = (AbstractBuildable) pwe.nextElement();
      newPiece = checkBuildable(oldPiece, b);
    }
    return newPiece;
  }

  // Check for piece in a PieceWindow widget
  protected GamePiece checkBuildable(GamePiece oldPiece, AbstractBuildable b) {
    GamePiece newPiece = null;
    Enumeration pwComponents = b.getBuildComponents();
    while (pwComponents.hasMoreElements() && newPiece == null) {
      AbstractBuildable bb = (AbstractBuildable) pwComponents.nextElement();
      if (bb instanceof PieceSlot) {
        GamePiece p = ((PieceSlot) bb).getPiece();
        newPiece = checkNewPiece(oldPiece, p);
      }
      else {
        newPiece = checkBuildable(oldPiece, bb);
      }
    }
    return newPiece;
  }

  //Compare old Piece with a piece on the pallette
  protected GamePiece checkNewPiece(GamePiece oldPiece, GamePiece pallettePiece) {
    GamePiece newPiece = null;

    String oldPieceName = Decorator.getInnermost(oldPiece).getName();
    String newPieceName = Decorator.getInnermost(pallettePiece).getName();

    //Same BasicPiece name?
    if (oldPieceName.equals(newPieceName)) {
      if (embellishmentMatch(oldPiece, pallettePiece)) {
        GamePiece outer = Decorator.getOutermost(pallettePiece);
        newPiece = ((AddPiece) GameModule.getGameModule()
            .decode(GameModule.getGameModule().encode(new AddPiece(outer)))).getTarget();
        updateLabels(newPiece, oldPiece);
      }
    }

    return newPiece;
  }

  /**
   * Compare the name of the 1st image of the innermost Embellishment of 2
   * Decorators.
   */
  protected boolean embellishmentMatch(GamePiece piece1, GamePiece piece2) {

    String imageName1 = innerEmbImageName(piece1);
    String imageName2 = innerEmbImageName(piece2);

    if (imageName1 == null) {
      return imageName2 == null;
    }
    else {
      if (imageName2 == null) {
        return imageName1 == null;
      }
      else {
        return imageName1.equals(imageName2);
      }
    }
  }

  /*
   * Find the name of the first image name of the innermost Embellishment
   */

  protected String innerEmbImageName(GamePiece piece) {

    GamePiece p = piece;
    String name = null;

    while (p != null) {
      p = Decorator.getDecorator(p, VSQLEmbellishment.class);
      if (p != null) {
        name = ((VSQLEmbellishment) p).getImageNames()[0];
        p = ((Decorator) p).getInner();
      }
    }

    return name;

  }

  public void updateLabels(GamePiece newPiece, GamePiece oldPiece) {

    GamePiece p = newPiece;
    String type;

    while (p != null) {
      p = Decorator.getDecorator(p, Labeler.class);
      if (p != null) {
        Labeler label = (Labeler) p;
        type = label.myGetType();
        String newState = findState(oldPiece, type, Labeler.class);
        if (newState != null) {
          label.mySetState(newState);
        }
        p = ((Decorator) p).getInner();
      }
    }
  }

  public String findState(GamePiece piece, String typeToFind, Class findClass) {

    GamePiece p = piece;
    while (p != null) {
      p = Decorator.getDecorator(p, findClass);
      if (p != null) {
        if (p instanceof Labeler) {
          Labeler l = (Labeler) p;
          if (l.myGetType().equals(typeToFind)) {
            return l.myGetState();
          }
        }
      }
    }
    return null;
  }

  //  protected class Embellishment2 extends Embellishment {
  //    
  //    public Embellishment2(Embellishment e) {
  //      mySetType(e.myGetType());
  //      mySetState(e.myGetState());
  //    }
  //    
  //    public String[] getImageNames() {
  //      return imageName;
  //    }
  //    
  //  }
}