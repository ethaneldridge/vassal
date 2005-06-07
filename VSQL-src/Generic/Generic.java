/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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
package Generic;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.image.BufferedImage;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.StringConfigurer;
import VASSAL.counters.Decorator;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyCommand;
import VASSAL.counters.PieceEditor;
import VASSAL.tools.SequenceEncoder;

/** A Class that creates a generic counter based on a layout recorded
 * in a GenericDefinition
 */

public class Generic extends Decorator  implements EditablePiece {

  public static final String ID = "generic;";
  
  protected static final String NAME = "name";
  protected static final String DEFN_NAME = "defnName";
  
  protected String definitionName;
  
  protected CounterLayout layout = new CounterLayout();
  protected ColorScheme scheme = new ColorScheme();
  protected ImageDefn defn = new ImageDefn();
  
  protected BufferedImage image = null;
  protected Rectangle box = new Rectangle();
  
  
  public Generic() {
    this(ID, null);
  }

  public Generic(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }
  
  public void mySetState(String newState) {
  }

  public String myGetState() {
    return "";
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(definitionName);
    return ID + se.getValue();
  }

  protected KeyCommand[] myGetKeyCommands() {
    return new KeyCommand[0];
  }

  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    
    if (image == null) {
      getImage();
    }

    if (image != null) {
      int x1 = x - image.getWidth()/2;
      int y1 = y - image.getHeight()/2;
      g.drawImage(image, x1, y1, null);
    }
    
//    int w = layout.getLayoutHeight();
//    int h = layout.getLayoutWidth();
//    Color bgColor = layout.;
//    
//    int x1 = x - w/2;
//    int y1 = y - h/2;
//    
//    g.setColor(bgColor);
//    g.fillRect(x1, y1, w, h);
//    g.setColor(Color.BLACK);
//    g.drawRect(x1, y1, w, h);
    
  }

  protected void getImage() {
  
    defn = GenericsContainer.getInstance().getGenericDefn(definitionName);
    if (defn != null) {
      image = (BufferedImage) defn.getVisualizerImage();
      if (image != null) {
        int h = image.getHeight();
        int w = image.getWidth();
        box = new Rectangle(0, 0, w, h);
      }
    }
  }
  
  public Rectangle boundingBox() {
    //Rectangle box = piece.boundingBox();
    //if (box == null) {
//    int w = layout.getLayoutHeight();
//    int h = layout.getLayoutWidth();
//      Rectangle box = new Rectangle(-w/2, -h/2, layout.getLayoutWidth(), layout.getLayoutHeight());
//    //}
    return box;
  }

  public Shape getShape() {
    return boundingBox();
  }


  public PieceEditor getEditor() {
    return new Ed(this);
  }
  
  public String getName() {
    return piece.getName();
  }

  public String getDescription() {
    return "Generic";
  }

  public void mySetType(String type) {
    type = type.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    definitionName = st.nextToken("");
  }

  public HelpFile getHelpFile() {
    return null;
  }
  
  protected static class Ed implements PieceEditor {
    protected StringConfigurer definition;
    protected  JPanel controls;
    
    protected Ed(Generic g) {
      
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      definition = new StringConfigurer(null, "Generic Definition Name:  ", g.definitionName);
      controls.add(definition.getControls());
     
    }

    public Component getControls() {
      return controls;
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(definition.getValueString());
      return ID + se.getValue();
    }

    public String getState() {
      return "";
    }
  }
}