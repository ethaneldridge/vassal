/*
 * $Id$
 *
 * Copyright (c) 2000-2005 by Rodney Kinney, Brent Easton
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
package Shader;

import java.awt.Component;
import java.awt.Shape;
import java.awt.Window;
import java.awt.geom.Area;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import VASSAL.command.Command;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.counters.Decorator;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyCommand;
import VASSAL.counters.PieceEditor;
import VASSAL.tools.SequenceEncoder;

public class Shading extends Decorator implements EditablePiece {
  
  public static final String ID = "shade;";
  public static final String SHADE_TYPE = "shadeType";
  public static final String SHADE_SHAPE = "shadeShape";
  public static final String SHADE_RANGE = "shadeRange";
  
  public static final String RANGE_FIXED = "Fixed";
  public static final String RANGE_SHADE = "From Shade Type";
  
  protected String shade = "";
  protected Area shape = null;
  protected String rangeType;
  protected int fixedRange = 0;

  public Shading() {
    this(ID, null);
  }

  public Shading(String type, GamePiece p) {
    mySetType(type);
    setInner(p);
  }

  public void mySetType(String s) {
    s = s.substring(ID.length());
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
    shade = sd.nextToken("");
    rangeType = sd.nextToken(RANGE_FIXED);
    fixedRange = sd.nextInt(0);
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
    if (SHADE_TYPE.equals(key)) {
      return shade;
    }
    else if (SHADE_SHAPE.equals(key)) {
      return shape;
    }
    else if (SHADE_RANGE.equals(key)) {
      if (rangeType.equals(RANGE_FIXED)) {
        return new Integer(fixedRange);
      }
      else {
        return new Integer(-1);
      }
    }
    else {
      return super.getProperty(key);
    }
  }

  public void setProperty(Object key, Object value) {
    super.setProperty(key, value);
  }

  public String myGetState() {
    return "";
  }

  public void mySetState(String state) {
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(',');
    se.append(shade);
    se.append(rangeType);
    se.append(fixedRange);
    return ID + se.getValue();
  }

  protected KeyCommand[] myGetKeyCommands() {
    return new KeyCommand[0];
  }

  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  public String getDescription() {
    return "Shading";
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return null;   
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  protected static class Ed implements PieceEditor {
    protected StringConfigurer shadeType;
    protected StringEnumConfigurer rangeType;
    protected IntConfigurer fixedRange;
    protected Component fixedRangeControls;
    
    protected JPanel panel;
    protected Ed(Shading m) {
      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel,BoxLayout.Y_AXIS));
      
      shadeType = new StringConfigurer(null,"Shade Type Name:  ",m.shade);
      
      rangeType = new StringEnumConfigurer(null, "Range:  ", new String[] {RANGE_FIXED, RANGE_SHADE });
      rangeType.setValue(m.rangeType);
      
      fixedRange = new IntConfigurer(null, "Range in Hexes:  ", new Integer(m.fixedRange));
      fixedRangeControls = fixedRange.getControls();
      fixedRangeControls.setVisible(m.rangeType.equals(RANGE_FIXED));   
     
      rangeType.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent e) {
          fixedRangeControls.setVisible(rangeType.getValueString().equals(RANGE_FIXED));
          Window w = SwingUtilities.getWindowAncestor(panel);
          if ( w!= null) {
            w.pack();
          }
        }});
      
      panel.add(shadeType.getControls());
      panel.add(rangeType.getControls());
      panel.add(fixedRangeControls);
      
    }

    public Component getControls() {
      return panel;
    }

    public String getState() {
      return "";
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(',');
      se.append(shadeType.getValueString());
      se.append(rangeType.getValueString());
      se.append(fixedRange.getValueString());
      return ID + se.getValue();
    }
  }
}
