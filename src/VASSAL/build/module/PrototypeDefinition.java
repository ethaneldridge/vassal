package VASSAL.build.module;

import VASSAL.build.*;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.AddPiece;
import VASSAL.configure.Configurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceDefiner;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.PieceEditor;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.swing.*;
import java.awt.*;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
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


public class PrototypeDefinition implements Configurable {
  private String name = "Prototype";
  private GamePiece piece;
  private String pieceDefinition;

  private PropertyChangeSupport propSupport = new PropertyChangeSupport(this);

  public void addPropertyChangeListener(PropertyChangeListener l) {
    propSupport.addPropertyChangeListener(l);
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public Configurable[] getConfigureComponents() {
    return new Configurable[0];
  }

  public String getConfigureName() {
    return name;
  }

  public void setConfigureName(String s) {
    String oldName = name;
    this.name = s;
    propSupport.firePropertyChange(NAME_PROPERTY, oldName, name);
  }

  public Configurer getConfigurer() {
    return new Config(this);
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public void remove(Buildable child) {
  }

  public void removeFrom(Buildable parent) {
  }

  public void add(Buildable child) {
  }

  public void addTo(Buildable parent) {
    if (parent instanceof AbstractBuildable) {
      for (Enumeration e = ((AbstractBuildable) parent).getComponents(getClass());
           e.hasMoreElements();) {
        PrototypeDefinition d = (PrototypeDefinition) e.nextElement();
        if (d.name.equals(name)) {
          throw new IllegalBuildException("Must have unique name");
        }
      }
    }
  }

  public GamePiece getPiece() {
    if (piece == null
        && pieceDefinition != null) {
      AddPiece comm = (AddPiece) GameModule.getGameModule().decode(pieceDefinition);
      if (comm == null) {
        System.err.println("Couldn't build piece " + pieceDefinition);
        pieceDefinition = null;
      }
      else {
        piece = comm.getTarget();
        piece.setState(comm.getState());
      }
    }
    return piece;
  }

  private void setPiece(GamePiece p) {
    piece = p;
    pieceDefinition = piece == null ? null
        : GameModule.getGameModule().encode(new AddPiece(piece));
  }

  public void build(Element e) {
    if (e != null) {
      setConfigureName(e.getAttribute(NAME_PROPERTY));
      pieceDefinition = Builder.getText(e);
    }
  }

  public Element getBuildElement(Document doc) {
    Element el = doc.createElement(getClass().getName());
    el.setAttribute(NAME_PROPERTY, name);
    el.appendChild(doc.createTextNode(piece == null ? pieceDefinition : GameModule.getGameModule().encode(new AddPiece(piece))));
    return el;
  }

  public static String getConfigureTypeName() {
    return "Definition";
  }

  public static class Config extends Configurer {
    private Box box;
    private PieceDefiner pieceDefiner;
    private StringConfigurer name;
    private PrototypeDefinition def;

    public Config(PrototypeDefinition def) {
      super(null, null, def);
      box = Box.createVerticalBox();
      name = new StringConfigurer(null, "Name", def.name);
      box.add(name.getControls());
      pieceDefiner = new Definer();
      pieceDefiner.setPiece(def.getPiece());
      box.add(pieceDefiner);
      this.def = def;
    }

    public Object getValue() {
      if (def != null) {
        def.setPiece(pieceDefiner.getPiece());
        def.setConfigureName(name.getValueString());
      }
      return def;
    }

    public Component getControls() {
      return box;
    }

    public String getValueString() {
      return null;
    }

    public void setValue(String s) {
    }

    public static class Definer extends PieceDefiner {
      public Definer() {
        inUseModel.setElementAt(new Plain(), 0);
      }
      public void setPiece(GamePiece piece) {
        super.setPiece(piece);
        inUseModel.setElementAt(new Plain(), 0);
      }
      private static class Plain extends BasicPiece {
        public Plain() {
          super(ID + ";;;;");
        }

        public String getDescription() {
          return "";
        }

        public PieceEditor getEditor() {
          return null;
        }
      }
    }
  }
}
