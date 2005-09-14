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
package VASSAL.build.module;

import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.GameModule;
import VASSAL.command.*;
import VASSAL.counters.*;
import VASSAL.tools.SequenceEncoder;

import java.awt.*;

/** A {@link CommandEncoder} that handles the basic commands: {@link
 * AddPiece}, {@link RemovePiece}, {@link ChangePiece}, {@link MovePiece}. If a module
 * defines custom {@link GamePiece} classes, then this class may be
 * overriden and imported into the module. Subclasses should override the
 * {@link #createDecorator} method or, less often, the {@link
 * #createBasic} or {@link #createPiece} methods to allow instantiation
 * of the custom {@link GamePiece} classes.
 */
public class BasicCommandEncoder implements CommandEncoder, Buildable {
  public BasicCommandEncoder() {
  }

  /**
   * Creates a {@link Decorator} instance
   *
   * @param type the type of the Decorator to be created.  Once created, the
   * Decorator should return this value from its {@link Decorator#myGetType} method.
   *
   * @param inner the inner piece of the Decorator @see Decorator
   */
  protected Decorator createDecorator(String type, GamePiece inner) {
    if (type.startsWith(Immobilized.ID)) {
      return new Immobilized(inner, type);
    }
    else if (type.startsWith(Embellishment.ID)
      || type.startsWith(Embellishment.OLD_ID)) {
      return new Embellishment(type, inner);
    }
    else if (type.startsWith(Hideable.ID)) {
      return new Hideable(type, inner);
    }
    else if (type.startsWith(Obscurable.ID)) {
      return new Obscurable(type, inner);
    }
    else if (type.startsWith(Labeler.ID)) {
      return new Labeler(type, inner);
    }
    else if (type.startsWith(TableInfo.ID)) {
      return new TableInfo(type, inner);
    }
    else if (type.startsWith(PropertySheet.ID)) {
      return new PropertySheet(type, inner);
    }
    else if (type.startsWith(FreeRotator.ID)) {
      return new FreeRotator(type, inner);
    }
    else if (type.startsWith(Pivot.ID)) {
      return new Pivot(type, inner);
    }
    else if (type.startsWith(NonRectangular.ID)) {
      return new NonRectangular(type, inner);
    }
    else if (type.startsWith(Marker.ID)) {
      return new Marker(type, inner);
    }
    else if (type.startsWith(Restricted.ID)) {
      return new Restricted(type, inner);
    }
    else if (type.startsWith(PlaceMarker.ID)) {
      return new PlaceMarker(type, inner);
    }
    else if (type.startsWith(Replace.ID)) {
      return new Replace(type, inner);
    }
    else if (type.startsWith(ReportState.ID)) {
      return new ReportState(type, inner);
    }
    else if (type.startsWith(MovementMarkable.ID)) {
      return new MovementMarkable(type, inner);
    }
    else if (type.startsWith(Footprint.ID)) {
      return new Footprint(type, inner);
    }
    else if (type.startsWith(ReturnToDeck.ID)) {
      return new ReturnToDeck(type, inner);
    }
    else if (type.startsWith(SendToLocation.ID)) {
      return new SendToLocation(type, inner);
    }
    else if (type.startsWith(UsePrototype.ID)) {
      return new UsePrototype(type, inner);
    }
    else if (type.startsWith(Clone.ID)) {
      return new Clone(type, inner);
    }
    else if (type.startsWith(Delete.ID)) {
      return new Delete(type, inner);
    }
    else if (type.startsWith(SubMenu.ID)) {
      return new SubMenu(type, inner);
    }
    else if (type.startsWith(Translate.ID)) {
      return new Translate(type, inner);
    }
    else if (type.startsWith(AreaOfEffect.ID)) {
      return new AreaOfEffect(type, inner);
    }
//    else if (type.startsWith(CounterGlobalKeyCommand.ID)) {
//      return new CounterGlobalKeyCommand(type, inner);
//    }

    return null;
  }

  /**     Create a GamePiece instance that is not a Decorator @param
   *     type the type of the GamePiece.  The created piece should
   *     return this value from its {@link GamePiece#getType}
   *     method  */
  protected GamePiece createBasic(String type) {
    if (type.equals(Stack.TYPE)) {
      return new Stack();
    }
    else if (type.startsWith(BasicPiece.ID)) {
      return new BasicPiece(type);
    }
    else if (type.startsWith(Deck.ID)) {
      return new Deck(type);
    }
    return null;
  }

  /**     Creates a GamePiece instance from the given type
   *     information.  Determines from the type whether the
   *     represented piece is a {@link Decorator} or not and
   *     forwards to {@link #createDecorator} or {@link
   *     #createBasic}.  This method should generally not need to be
   *     overridden.  Instead, override createDecorator or
   *     createBasic  */
  protected GamePiece createPiece(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, '\t');
    type = st.nextToken();
    String innerType = st.hasMoreTokens() ? st.nextToken() : null;

    if (innerType != null) {
      GamePiece inner = createPiece(innerType);
      Decorator d = createDecorator(type,inner);
      return d != null ? d : inner;
    }
    else {
      return createBasic(type);
    }
  }

  public void build(org.w3c.dom.Element e) {
    Builder.build(e, this);
  }

  public void addTo(Buildable parent) {
    ((GameModule) parent).addCommandEncoder(this);
  }

  public void add(Buildable b) {
  }

  public org.w3c.dom.Element getBuildElement(org.w3c.dom.Document doc) {
    return doc.createElement(getClass().getName());
  }

  private static final char PARAM_SEPARATOR = '/';
  public static final String ADD = "+" + PARAM_SEPARATOR;
  public static final String REMOVE = "-" + PARAM_SEPARATOR;
  public static final String CHANGE = "D" + PARAM_SEPARATOR;
  public static final String MOVE = "M" + PARAM_SEPARATOR;

  public Command decode(String command) {
    if (command.length() == 0) {
      return new NullCommand();
    }
    SequenceEncoder.Decoder st;
    if (command.startsWith(ADD)) {
      command = command.substring(ADD.length());
      st = new SequenceEncoder.Decoder(command, PARAM_SEPARATOR);
      String id = unwrapNull(st.nextToken());
      String type = st.nextToken();
      String state = st.nextToken();
      GamePiece p = createPiece(type);
      if (p == null) {
        return null;
      }
      else {
        p.setId(id);
        return new AddPiece(p, state);
      }
    }
    else if (command.startsWith(REMOVE)) {
      String id = command.substring(REMOVE.length());
      GamePiece target = GameModule.getGameModule().getGameState().getPieceForId(id);
      if (target == null) {
        return new RemovePiece(id);
      }
      else {
        return new RemovePiece(target);
      }
    }
    else if (command.startsWith(CHANGE)) {
      command = command.substring(CHANGE.length());
      st = new SequenceEncoder.Decoder(command, PARAM_SEPARATOR);
      String id = st.nextToken();
      String newState = st.nextToken();
      String oldState = st.hasMoreTokens() ? st.nextToken() : null;
      return new ChangePiece(id, oldState, newState);
    }
    else if (command.startsWith(MOVE)) {
      command = command.substring(MOVE.length());
      st = new SequenceEncoder.Decoder(command, PARAM_SEPARATOR);
      String id = unwrapNull(st.nextToken());
      String newMapId = unwrapNull(st.nextToken());
      int newX = Integer.parseInt(st.nextToken());
      int newY = Integer.parseInt(st.nextToken());
      String newUnderId = unwrapNull(st.nextToken());
      String oldMapId = unwrapNull(st.nextToken());
      int oldX = Integer.parseInt(st.nextToken());
      int oldY = Integer.parseInt(st.nextToken());
      String oldUnderId = unwrapNull(st.nextToken());
      String playerid = st.nextToken(GameModule.getUserId());
      return new MovePiece(id, newMapId, new Point(newX, newY), newUnderId, oldMapId, new Point(oldX, oldY), oldUnderId, playerid);
    }
    else {
      return null;
    }
  }

  private String wrapNull(String s) {
    return s == null ? "null" : s;
  }

  private String unwrapNull(String s) {
    return "null".equals(s) ? null : s;
  }

  public String encode(Command c) {
    SequenceEncoder se = new SequenceEncoder(PARAM_SEPARATOR);
    if (c instanceof AddPiece) {
      AddPiece a = (AddPiece) c;
      return ADD
          + se.append(wrapNull(a.getTarget().getId()))
          .append(a.getTarget().getType())
          .append(a.getState()).getValue();
    }
    else if (c instanceof RemovePiece) {
      return REMOVE + ((RemovePiece) c).getId();
    }
    else if (c instanceof ChangePiece) {
      ChangePiece cp = (ChangePiece) c;
      se.append(cp.getId()).append(cp.getNewState());
      if (cp.getOldState() != null) {
        se.append(cp.getOldState());
      }
      return CHANGE + se.getValue();
    }
    else if (c instanceof MovePiece) {
      MovePiece mp = (MovePiece) c;
      se.append(mp.getId())
          .append(wrapNull(mp.getNewMapId()))
          .append(mp.getNewPosition().x + "")
          .append(mp.getNewPosition().y + "")
          .append(wrapNull(mp.getNewUnderneathId()))
          .append(wrapNull(mp.getOldMapId()))
          .append(mp.getOldPosition().x + "")
          .append(mp.getOldPosition().y + "")
          .append(wrapNull(mp.getOldUnderneathId()))
          .append(mp.getPlayerId());
      return MOVE + se.getValue();
    }
    else if (c instanceof NullCommand) {
      return "";
    }
    else {
      return null;
    }
  }
}
