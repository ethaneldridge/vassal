/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney
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

import VASSAL.tools.FormattedString;
import VASSAL.tools.PlayerIdFormattedString;
import VASSAL.build.module.Map;
import VASSAL.build.module.Chatter;
import VASSAL.build.GameModule;
import VASSAL.command.NullCommand;
import VASSAL.command.Command;

import javax.swing.*;
import java.util.Enumeration;

/**
 * Applies a given keyboard command to all counters on a map
 */
public class GlobalCommand {
  private KeyStroke keyStroke;
  private boolean reportSingle;
  private FormattedString reportFormat = new PlayerIdFormattedString();

  public void setKeyStroke(KeyStroke keyStroke) {
    this.keyStroke = keyStroke;
  }

  public void setReportFormat(String format) {
    this.reportFormat.setFormat(format);
  }

  public KeyStroke getKeyStroke() {
    return keyStroke;
  }

  public String getReportFormat() {
    return reportFormat.getFormat();
  }

  public boolean isReportSingle() {
    return reportSingle;
  }

  public void setReportSingle(boolean reportSingle) {
    this.reportSingle = reportSingle;
  }

  /**
   * Apply the key command to all pieces on the map that pass the given filter
   * @param m
   * @param filter
   * @return a the corresponding {@link Command}
   */
  public Command apply(Map m, PieceFilter filter) {
    String mapFormat = m.getChangeFormat();
    if (reportSingle) {
      m.setAttribute(Map.CHANGE_FORMAT, "");
    }
    String reportText = reportFormat.getText();
    Command keyCommand;
    if (reportText.length() > 0) {
      keyCommand = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), "*" + reportText);
      keyCommand.execute();
    }
    else {
      keyCommand = new NullCommand();
    }
    Visitor visitor = new Visitor(keyCommand, filter, keyStroke);
    PieceVisitorDispatcher dispatcher = new PieceVisitorDispatcher(visitor);
    GamePiece[] p = m.getPieces();
    for (int i = 0; i < p.length; ++i) {
      dispatcher.accept(p[i]);
    }
    visitor.getTracker().repaint();
    if (reportSingle) {
      m.setAttribute(Map.CHANGE_FORMAT, mapFormat);
    }
    return visitor.getCommand();
  }

  /* We don't treat {@link Deck}s any differently than {@link Stack}s, so
  no need to implement {@link DeckVisitor */
  private static class Visitor implements PieceVisitor {
    private Command command;
    private BoundsTracker tracker;
    private PieceFilter filter;
    private KeyStroke stroke;

    public Visitor(Command command, PieceFilter filter, KeyStroke stroke) {
      this.command = command;
      tracker = new BoundsTracker();
      this.filter = filter;
      this.stroke = stroke;
    }

    public Object visitStack(Stack s) {
      for (Enumeration e = s.getPieces(); e.hasMoreElements();) {
        apply((GamePiece) e.nextElement());
      }
      return null;
    }

    public Object visitDefault(GamePiece p) {
      apply(p);
      return null;
    }

    private void apply(GamePiece p) {
      if (filter == null || filter.accept(p)) {
        tracker.addPiece(p);
        p.setProperty(Properties.SNAPSHOT, PieceCloner.getInstance().clonePiece(p));
        command.append(p.keyEvent(stroke));
        tracker.addPiece(p);
      }
    }

    public Command getCommand() {
      return command;
    }

    public BoundsTracker getTracker() {
      return tracker;
    }

  }
}
