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

import VASSAL.build.module.BasicCommandEncoder;
import VASSAL.build.module.GameState;
import VASSAL.build.module.Map;
import VASSAL.command.ChangePiece;
import VASSAL.command.Command;

import javax.swing.*;
import java.awt.*;

/**
 * Basic class for representing a physical component of the game
 */
public interface GamePiece {

  public void setMap(Map map);

  public Map getMap();

  public void draw(Graphics g, int x, int y, Component obs, double zoom);

  public Point getPosition();

  public void setPosition(Point p);

  /**
   * The area which this GamePiece occupies when
   * drawn at the point given by getPosition().
   */
  public Rectangle boundingBox();

  /**
   * The area which is considered part of this GamePiece
   * for purposes of selecting it with the mouse
   */
  public Rectangle selectionBounds();

  public Stack getParent();

  public void setParent(Stack s);

  /**
   * Keyboard events are forward to this method when a piece is selected
   * The GamePiece can respond in any way it likes
   *
   * @return a {@link Command} that, when executed, will invoke
   * the same response.  Usually a {@link ChangePiece} command.
   *
   * @see VASSAL.build.module.map.ForwardToKeyBuffer
   */
  public Command keyEvent(KeyStroke stroke);

  /** The plain English name for this piece */
  public String getName();

  /**
   * Each GamePiece must have a unique String identifier
   * @see GameState#getNewPieceId
   */
  public String getId();

  public void setId(String id);

  /** The type information is information that does not change
   * during the course of a game.  Image file names, popup menu
   * command names, etc., all should be reflected in the type.
   * @see BasicCommandEncoder */
  public String getType();

  /** The state information is information that can change during
   * the course of a game.  State information is saved when the game
   * is saved and is transferred between players on the server.  For
   * example, the relative order of pieces in a stack is state
   * information, but whether the stack is expanded is not */
  public String getState();

  public void setState(String newState);

  /**
   * The GamePiece should store properties for retrieval (e.g in a Hashtable)
   * It is not necessary that properties stored with this method be
   * retained in the piece's {@link #getState} method.  */
  public void setProperty(Object key, Object val);

  public Object getProperty(Object key);


}
