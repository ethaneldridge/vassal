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

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.RemovePiece;

import javax.swing.*;
import java.io.File;
import java.net.MalformedURLException;

/**
 * GamePiece trait that replaces a GamePiece with another one
 */
public class Replace extends PlaceMarker {
  public static final String ID = "replace;";

  public Replace() {
    this(ID + "Replace;R;null", null);
  }

  public Replace(String type, GamePiece inner) {
    super(type, inner);
  }

  public Command myKeyEvent(KeyStroke stroke) {
    Command c = null;
    if (command.matches(stroke)) {
      c = super.myKeyEvent(stroke);
      Command remove = new RemovePiece(Decorator.getOutermost(this));
      remove.execute();
      c.append(remove);
    }
    return c;
  }

  public String getDescription() {
    return "Replace with Other";
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir,"ReferenceManual");
    try {
      return new HelpFile(null,new File(dir,"Replace.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public String myGetType() {
    return ID+super.myGetType().substring(PlaceMarker.ID.length());
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  protected static class Ed extends PlaceMarker.Ed {
    public Ed(Replace piece) {
      super(piece);
      defineButton.setText("Define Replacement");
    }

    public String getType() {
      String s = super.getType();
      s = ID+s.substring(PlaceMarker.ID.length());
      return s;
    }
  }
}
