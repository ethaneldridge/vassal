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

package Dev;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.Enumeration;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import org.nfunk.jep.JEP;
import org.nfunk.jep.SymbolTable;
import org.nfunk.jep.Variable;

import UpFront.StringArrayConfigurer;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.StringConfigurer;
import VASSAL.counters.Decorator;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyCommand;
import VASSAL.counters.PieceEditor;
import VASSAL.tools.SequenceEncoder;

/**
 * Conditional Marker
 * A marker with a variable value depending on conditions.
 * */
public class ConditionalMarker extends Decorator implements EditablePiece {
  
  public static final String ID = "cond;";
  
  protected String name = "";
  protected String[] propertyFilters = new String[0];
  protected String[] expressions = new String[0];
  protected String defaultExpression = "";

  public ConditionalMarker() {
    this(ID, null);
  }

  public ConditionalMarker(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  public String getName() {
    return piece.getName();
  }

  protected KeyCommand[] myGetKeyCommands() {
    return new KeyCommand[0];
  }

  public String myGetState() {
    return "";
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(name)
      .append(defaultExpression)
      .append(StringArrayConfigurer.arrayToString(propertyFilters))
      .append(StringArrayConfigurer.arrayToString(expressions));
    return ID + se.getValue();
  }
  
  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  public void mySetState(String newState) {
  }

  public Shape getShape() {
    return piece.getShape();
  }

  public String getDescription() {
    return "Conditional Expression";
  }

  public HelpFile getHelpFile() {
      return null;
  }

  public void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    name = st.nextToken("");
    defaultExpression = st.nextToken("");
    propertyFilters = StringArrayConfigurer.stringToArray(st.nextToken(""));
    expressions = StringArrayConfigurer.stringToArray(st.nextToken(""));
  }

  public Object getProperty(Object key) {
    if (name.length() > 0 && name.equals(key)) {
      return evaluate();
    }
    return super.getProperty(key);
  }
  
  protected String evaluate() {
    return evaluate(defaultExpression);
  }
  
  protected String evaluate(String expression) {
    JEP myParser = new VassalJEP();
    myParser.addStandardFunctions();
    myParser.addStandardConstants();
    myParser.addFunction("toString", new ToJepString());
    myParser.setAllowUndeclared(true);
    myParser.parseExpression(expression);
    SymbolTable table = myParser.getSymbolTable();
    Enumeration e = table.elements();
    
    while (e.hasMoreElements()) {
      Variable var = (Variable) e.nextElement();
      if (!var.hasValidValue()) {
        String varName = var.getName();
        String val = (String) Decorator.getOutermost(this).getProperty(varName);
        if (val != null) {
          myParser.setVarValue(varName, new Double(val));
        }
      }
    }

    Object result = myParser.getValueAsObject();
    if (result instanceof Double) {
      return doubleToString((Double) result);
    }
    else if (result instanceof String) {
      return (String) result;
    }
    return "";
  }

  public static String doubleToString(Double d) {
    return doubleToString(d.doubleValue());
  }
  
  public static String doubleToString(double d) {
    String s = Double.toString(d);
    if (s.endsWith(".0")) {
      s = s.substring(0, s.length()-2);
    }
    return s;
  }
  
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public static class Ed implements PieceEditor {

    private StringConfigurer name;
    private StringConfigurer defaultExpression;
    private JPanel box;
    
    public Ed(ConditionalMarker piece) {

      box = new JPanel();
      box.setLayout(new BoxLayout(box, BoxLayout.Y_AXIS));
      
      name = new StringConfigurer(null, "Marker Name:  ", piece.name);
      box.add(name.getControls());

      defaultExpression = new StringConfigurer(null, "Default Expression:  ", piece.defaultExpression);
      box.add(defaultExpression.getControls());
    }

   
    public Component getControls() {
      return box;
    }

    public String getState() {
      return "";
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(name.getValueString())
      .append(defaultExpression.getValueString())
      .append(StringArrayConfigurer.arrayToString(new String[0]))
      .append(StringArrayConfigurer.arrayToString(new String[0]));
      return ID + se.getValue();
    }
  }
}
