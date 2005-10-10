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

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import VASSAL.command.Command;
import VASSAL.configure.HotKeyConfigurer;
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

  public static final String RANGE_MARKER = "Use Marker Value";
  public static final String RANGE_FIXED = "Fixed";
  public static final String RANGE_GRID = "Grid Elements";
  public static final String RANGE_PIXELS = "Pixels";
  
  public static final String ALWAYS_ON = "Always On";
  public static final String BY_COMMAND = "By Command";
  public static final String BY_FILTER = "By Matching Properties";   
  
  protected String shade = "";
  protected Area shape = null;
  protected String activation;
  protected String enableCommand = "";
  protected String disableCommand = "";
  protected String toggleCommand = "";
  protected KeyStroke enableStroke = null;
  protected KeyStroke disableStroke = null;
  protected KeyStroke toggleStroke = null;
  protected String propertyFilter = "";
  protected String rangeType = "";
  protected String rangeSource = "";
  protected int fixedRange = 0;
  protected String markerName = "";

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
    activation = sd.nextToken(BY_COMMAND);
    propertyFilter = sd.nextToken("");
    enableCommand = sd.nextToken("Enable");
    disableCommand = sd.nextToken("Disable");
    toggleCommand = sd.nextToken("Toggle");
    enableStroke = sd.nextKeyStroke('E');
    disableStroke = sd.nextKeyStroke('D');
    toggleStroke = sd.nextKeyStroke('T');
    rangeType = sd.nextToken(RANGE_GRID);
    rangeSource = sd.nextToken(RANGE_FIXED);
    markerName = sd.nextToken("");
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

//  public Object getProperty(Object key) {
//    if (SHADE_TYPE.equals(key)) {
//      return shade;
//    }
//    else if (SHADE_SHAPE.equals(key)) {
//      return shape;
//    }
//    else if (SHADE_RANGE.equals(key)) {
//      if (rangeType.equals(RANGE_FIXED)) {
//        return new Integer(fixedRange);
//      }
//      else {
//        return new Integer(-1);
//      }
//    }
//    else {
//      return super.getProperty(key);
//    }
//  }

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
    se.append(shade)
      .append(activation)
      .append(propertyFilter)
      .append(enableCommand)
      .append(disableCommand)
      .append(toggleCommand)
      .append(enableStroke)
      .append(disableStroke)
      .append(toggleStroke)
      .append(rangeType)
      .append(rangeSource)
      .append(markerName)
      .append(fixedRange);
    return ID + se.getValue();
  }

  protected KeyCommand[] myGetKeyCommands() {
    return new KeyCommand[0];
  }

  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  public String getDescription() {
    String s = "Shading";
    if (shade.length() > 0) {
      s += " - " + shade;
    }
    return s;
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return null;   
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  protected static class Ed implements PieceEditor, PropertyChangeListener {
    protected StringConfigurer shadeType;
    protected StringEnumConfigurer activationType;
    protected StringConfigurer propertyFilter;
    protected StringConfigurer enableCommand;
    protected HotKeyConfigurer enableStroke;
    protected StringConfigurer disableCommand;
    protected HotKeyConfigurer disableStroke;
    protected StringConfigurer toggleCommand;
    protected HotKeyConfigurer toggleStroke;
    protected StringEnumConfigurer rangeType;
    protected StringEnumConfigurer rangeSource;
    protected IntConfigurer fixedRange;
    protected StringConfigurer markerName;
    protected Component fixedRangeControls;
    
    protected Box commandBox;
    protected Box filterBox;
    protected Box rangeBox;
    protected Box markerBox;
    
    protected JPanel panel;
    protected Ed(Shading m) {
      
      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel,BoxLayout.Y_AXIS));
      
      shadeType = new StringConfigurer(null,"Shade Type Name:  ",m.shade);
      panel.add(shadeType.getControls());

      activationType = new StringEnumConfigurer(null, "Activation Method:  ", new String[] { ALWAYS_ON, BY_COMMAND, BY_FILTER });
      activationType.setValue(m.activation);
      activationType.addPropertyChangeListener(this);
      panel.add(activationType.getControls());
      
      commandBox = Box.createVerticalBox();
      Box box = Box.createHorizontalBox();
      enableCommand = new StringConfigurer(null,"Enable Command:  ",m.enableCommand);
      enableStroke  = new HotKeyConfigurer(null, "  KeyStroke:  ", m.enableStroke);
      box.add(enableCommand.getControls());
      box.add(enableStroke.getControls());
      commandBox.add(box);
      
      box = Box.createHorizontalBox();
      disableCommand = new StringConfigurer(null,"Disable Command:  ",m.disableCommand);
      disableStroke  = new HotKeyConfigurer(null, "  KeyStroke:  ", m.disableStroke);
      box.add(disableCommand.getControls());
      box.add(disableStroke.getControls());
      commandBox.add(box);
      
      box = Box.createHorizontalBox();
      toggleCommand = new StringConfigurer(null,"Toggle Command:  ",m.toggleCommand);
      toggleStroke  = new HotKeyConfigurer(null, "  KeyStroke:  ", m.toggleStroke);
      box.add(toggleCommand.getControls());
      box.add(toggleStroke.getControls());
      commandBox.add(box);
      
      panel.add(commandBox);
      
      filterBox = Box.createHorizontalBox();
      propertyFilter = new StringConfigurer(null,"Matching Properties:  ",m.propertyFilter);
      filterBox.add(propertyFilter.getControls());
      panel.add(filterBox);
        
      rangeType = new StringEnumConfigurer(null, "Range Type:  ", new String[] {RANGE_GRID, RANGE_PIXELS });
      rangeType.setValue(m.rangeType);
      panel.add(rangeType.getControls());

      rangeSource = new StringEnumConfigurer(null, "Range Source:  ", new String[] {RANGE_FIXED, RANGE_MARKER });
      rangeSource.setValue(m.rangeSource);
      rangeSource.addPropertyChangeListener(this);
      panel.add(rangeSource.getControls());
      
      rangeBox = Box.createHorizontalBox();
      fixedRange = new IntConfigurer(null, "Range:  ", new Integer(m.fixedRange));
      fixedRangeControls = fixedRange.getControls();
      rangeBox.add(fixedRange.getControls());
      panel.add(rangeBox);
      
      markerBox = Box.createHorizontalBox();
      markerName = new StringConfigurer(null,"Marker Name:  ",m.markerName);
      markerBox.add(markerName.getControls());
      panel.add(markerBox);

      adjustVisibility();
      
    }

    public void propertyChange(PropertyChangeEvent arg0) {
      adjustVisibility();
    }
    
    public void adjustVisibility() {
      commandBox.setVisible(activationType.getValueString().equals(BY_COMMAND));
      filterBox.setVisible(activationType.getValueString().equals(BY_FILTER));
      rangeBox.setVisible(rangeSource.getValueString().equals(RANGE_FIXED));
      markerBox.setVisible(rangeSource.getValueString().equals(RANGE_MARKER));
      Window w = SwingUtilities.getWindowAncestor(panel);
      if ( w!= null) {
        w.pack();
      }
    }
    
    public Component getControls() {
      return panel;
    }

    public String getState() {
      return "";
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(',');
      se.append(shadeType.getValueString())
      .append(activationType.getValueString())
      .append(propertyFilter.getValueString())
      .append(enableCommand.getValueString())
      .append(disableCommand.getValueString())
      .append(toggleCommand.getValueString())
      .append((KeyStroke) enableStroke.getValue())
      .append((KeyStroke) disableStroke.getValue())
      .append((KeyStroke) toggleStroke.getValue())
      .append(rangeType.getValueString())
      .append(rangeSource.getValueString())
      .append(markerName.getValueString())
      .append(fixedRange.getValueString());
      return ID + se.getValue();
    }

  }
}
