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
package dynamic;

import java.awt.Component;
import java.awt.Shape;
import java.io.File;
import java.net.MalformedURLException;

import javax.swing.KeyStroke;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.AutoConfigurer;
import VASSAL.counters.Decorator;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyCommand;
import VASSAL.counters.PieceEditor;

/**
 * Decorator Adapter class to allow counters to share in the goodness of Dynamic
 * Properties. All the good stuff is delegated to an AbstractConfigurable
 * DynamicProperty object. This allows the same DynamicProperty object to be used
 * in a counter trait as a Map or Module level component. 
 * */
public class CounterDynamicProperty extends Decorator implements EditablePiece {
  
  public static final String ID = DynamicProperty.COMMAND_PREFIX;

  protected DynamicProperty delegate;
  
  public CounterDynamicProperty() {
    this(ID, null);
  }

  public CounterDynamicProperty(String type, GamePiece p) {
    delegate = new DynamicProperty(this);
    mySetType(type);
    setInner(p);
  }

  public void mySetType(String s) {
    delegate.setType(s);
  }

  public void draw(java.awt.Graphics g, int x, int y, java.awt.Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  public String getName() {
    return piece.getName();
  }

  public java.awt.Rectangle boundingBox() {
    return piece.boundingBox();
  }

  public Shape getShape() {
    return piece.getShape();
  }

  public Object getProperty(Object key) {
    if (key.equals(getKey())) {
      return getValue();
    }
    return super.getProperty(key);
  }

  public void setProperty(Object key, Object value) {
    if (key.equals(getKey())) {
      setValue((String) value);
    }
    super.setProperty(key, value);
  }

  public String myGetState() {
    return getValue();
  }

  public void mySetState(String state) {
    setValue(state);
  }

  public String getKey() {
    return delegate.getAttributeValueString(DynamicProperty.NAME);
  }
  
  public String getValue() {
    return delegate.getAttributeValueString(DynamicProperty.VALUE);
  }
  
  public void setValue(String value) {
    delegate.setAttribute(DynamicProperty.VALUE, value);
  }
  
  public String myGetType() {
    return delegate.getType();
  }

  protected KeyCommand[] myGetKeyCommands() {
    return delegate.getKeyCommands(Decorator.getOutermost(this));
  }

  public Command myKeyEvent(KeyStroke stroke) {
    ChangeTracker tracker = new ChangeTracker(this);
    delegate.keyEvent(stroke);
    return tracker.getChangeCommand();
  }

  public String getDescription() {
    String s = "Dynamic Property";
    if (getKey() != null && getKey().length() > 0) {
      s += " - " + getKey();
    }
    return s;
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "DynamicProperty.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  protected static class Ed implements PieceEditor {

    DynamicProperty edit;
    
    public Ed(CounterDynamicProperty m) {
      edit = new DynamicProperty(m.myGetType());
      edit.setValue(m.getValue());
    }
    
    public Component getControls() {
      return new AutoConfigurer(edit).getControls();
    }

    public String getType() {
      return edit.getType();
    }

    public String getState() {
      return edit.getValue();
    }

  }
}
