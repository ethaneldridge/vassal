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

package RFT;

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;

import AutoImage.AutoImage;
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
import VASSAL.counters.BasicPiece;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Labeler;
import VASSAL.counters.Stack;
import VASSAL.tools.LaunchButton;

public class RFTRefresher extends AbstractConfigurable {

  protected LaunchButton launch;
  protected Map map;

  public static final String BUTTON_TEXT = "text";
  public static final String NAME = "name";

  public RFTRefresher() {
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
    String s[] = { NAME, BUTTON_TEXT };
    return s;
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Name", "Button text" };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, String.class };
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
    else {
      launch.setAttribute(key, o);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
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

        GamePiece outer = Decorator.getOutermost(pallettePiece);
        newPiece = ((AddPiece) GameModule.getGameModule()
            .decode(GameModule.getGameModule().encode(new AddPiece(outer)))).getTarget();
        updateLabels(newPiece, oldPiece);
        updateAutoImages(newPiece, oldPiece);
    }

    return newPiece;
  }
  
  public void updateAutoImages(GamePiece newPiece, GamePiece oldPiece) {

    GamePiece p = newPiece;
    String type;

    while (p != null && !(p instanceof BasicPiece)) {
      p = Decorator.getDecorator(p, AutoImage.class);
      if (p != null) {
        AutoImage label = (AutoImage) p;
        type = label.myGetType();
        String newState = findState(oldPiece, type, AutoImage.class);
        if (newState != null) {
          label.mySetState(newState);
        }
        p = ((Decorator) p).getInner();
      }
    }
  }

  public void updateLabels(GamePiece newPiece, GamePiece oldPiece) {

    GamePiece p = newPiece;
    String type;

    while (p != null && !(p instanceof BasicPiece)) {
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
    while (p != null && !(p instanceof BasicPiece)) {
      Decorator d = (Decorator) Decorator.getDecorator(p, findClass);
      if (d != null) {
        if (d.getClass().equals(findClass)) {
          if (d.myGetType().equals(typeToFind)) {
            return d.myGetState();
          }
        }
        p = d.getInner();
      }
      else
        p = null;
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