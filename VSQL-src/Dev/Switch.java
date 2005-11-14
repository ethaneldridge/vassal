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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import UpFront.StringArrayConfigurer;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.StringConfigurer;
import VASSAL.counters.Decorator;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.ExpressionEvaluator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyCommand;
import VASSAL.counters.PieceEditor;
import VASSAL.tools.SequenceEncoder;

/**
 * Conditional Marker
 * A marker with a variable value depending on conditions.
 * */
public class Switch extends Decorator implements EditablePiece {
  
  public static final String ID = "switch;";
  
  protected String name = "";
  protected String expression = "";
  protected String[] values = new String[0];
  protected String[] results = new String[0];
  protected String defaultValue = "";

  public Switch() {
    this(ID, null);
  }

  public Switch(String type, GamePiece inner) {
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
      .append(expression)
      .append(defaultValue)
      .append(StringArrayConfigurer.arrayToString(values))
      .append(StringArrayConfigurer.arrayToString(results));
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
    return "Switch Statement";
  }

  public HelpFile getHelpFile() {
      return null;
  }

  public void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    name = st.nextToken("");
    expression = st.nextToken("");
    defaultValue = st.nextToken("");
    values = StringArrayConfigurer.stringToArray(st.nextToken(""));
    results = StringArrayConfigurer.stringToArray(st.nextToken(""));
  }

  public Object getProperty(Object key) {
    if (name.length() > 0 && name.equals(key)) {
      return evaluate();
    }
    return super.getProperty(key);
  }
  
  protected String evaluate() {
    String value = evaluate(expression);
    String result = defaultValue;
    for (int i = 0; i < values.length; i++) {
      if (result.equals(values[i])) {
        // TODO: Are the results expressions? if so, then return evaluate(results[0])
        return results[i];
      }
    }
    return result;
  }
  
  protected String evaluate(String expression) {
    GamePiece outer = Decorator.getOutermost(this);
    String result = ExpressionEvaluator.evaluate(expression, outer);
    return result;
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

  public static class Ed implements PieceEditor, ActionListener {

    protected StringConfigurer nameConfig;
    protected StringConfigurer expressionConfig;
    protected StringConfigurer defaultValueConfig;
    protected JPanel box;
    protected Box switchBox;
    protected Box valuesBox;
    protected String[] values = new String[0];
    protected String[] results = new String[0];
    
    public Ed(Switch piece) {

      box = new JPanel();
      box.setLayout(new BoxLayout(box, BoxLayout.Y_AXIS));
      
      nameConfig = new StringConfigurer(null, "Result Marker Name:  ", piece.name);
      box.add(nameConfig.getControls());

      expressionConfig = new StringConfigurer(null, "Switch on Expression:  ", piece.expression);
      box.add(expressionConfig.getControls());
      
      switchBox = Box.createVerticalBox();
      switchBox.setBorder(BorderFactory.createEtchedBorder());
      Box buttonBox = Box.createHorizontalBox();
      buttonBox.add(new JLabel("Switch Values:  "));
      JButton button = new JButton("Add");
      button.addActionListener(this);
      buttonBox.add(button);
      switchBox.add(buttonBox);
      values = (String[]) piece.values.clone();
      results = (String[]) piece.results.clone();
      addNullValue();
      updateValuesBox();
      box.add(switchBox);
      
      defaultValueConfig = new StringConfigurer(null, "Default Value:  ", piece.defaultValue);
      box.add(defaultValueConfig.getControls());
    }

   
    /**
     * @return
     */
    protected void updateValuesBox() {
      if (valuesBox != null) {
        switchBox.remove(valuesBox);
      }
      valuesBox = Box.createVerticalBox();
      
      for (int i = 0; i < values.length; i++) {
        Box box = Box.createHorizontalBox();
        StringConfigurer valueConfig = new StringConfigurer(null, "Value:  ", values[i] + "");
        StringConfigurer resultConfig = new StringConfigurer(null, "Result:  ", results[i] + "");
        box.add(valueConfig.getControls());
        box.add(resultConfig.getControls());
        valuesBox.add(box);
      }   
      
      switchBox.add(valuesBox);
    }
    
    protected void addNullValue() {
      String[] newValues = new String[values.length+1];
      String[] newResults = new String[results.length+1];
      System.arraycopy(values, 0, newValues, 0, values.length);
      System.arraycopy(results, 0, newResults, 0, results.length);
      newValues[newValues.length-1] = "";
      newResults[newResults.length-1] = "";
      values = newValues;
      results = newResults;
    }


    public Component getControls() {
      return box;
    }

    public String getState() {
      return "";
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(nameConfig.getValueString())
      .append(expressionConfig.getValueString())
      .append(defaultValueConfig.getValueString())
      .append(StringArrayConfigurer.arrayToString(values))
      .append(StringArrayConfigurer.arrayToString(results));
      return ID + se.getValue();
    }


    /*
     * Add an extra line 
     */
    public void actionPerformed(ActionEvent e) {
      
    }
   
  }
  

}
