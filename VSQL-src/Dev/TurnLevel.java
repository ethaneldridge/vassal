package Dev;

/*
 * $Id$
 * 
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License (LGPL) as published by
 * the Free Software Foundation.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, copies are available at
 * http://www.opensource.org.
 */

import java.awt.Color;
import java.awt.Component;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.SequenceEncoder;

public class TurnLevel extends AbstractConfigurable {

  protected static final String NAME = "name";
  protected static final String TYPE = "type";
  protected static final String START = "start";
  protected static final String INCR = "incr";
  protected static final String LIST = "list";

  protected static final String TYPE_COUNTER = "Counter";
  protected static final String TYPE_LIST = "List";

  // Module Configuration variable 
  protected String type = TYPE_COUNTER;
  protected int start = 0;
  protected int incr = 1;
  protected String[] list = new String[0];
  
  protected boolean rolledOver = false;
  
  // In play state variables
  protected int current = 0;
  protected int first = 0;
  protected boolean[] active = new boolean[0]; 

  // Other Variables
  protected IntConfigurer counterVal;
  protected StringEnumConfigurer listVal;
  protected StringEnumConfigurer firstVal;
  protected BooleanConfigurer activeVal[];
  
  protected boolean configMode = false;
  protected JPanel firstPanel;
  protected JPanel activePanel;
  
  public TurnLevel() {
    super();
  }

  protected boolean isCounter() {
    return type.equals(TYPE_COUNTER);
  }
  
  public void reset() {
    if (isCounter()) {
      current = start;
    }
    else {
      current = 0;
      first = 0;
      for (int i = 0; i < active.length; i++) {
        active[i] = true;
      }
    }
  }
  
  protected String getState() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(current);
    se.append(first);
    String s[] = new String[active.length];
    for (int i=0; i < s.length; i++) {
      s[i] = active[i] + "";
    }
    se.append(s);
    return se.getValue();
  }
  
  protected void setState(String code) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(code, ';');
    current = sd.nextInt(start);
    first = sd.nextInt(0);
    
    String[] s = sd.nextStringArray(0);
    active = new boolean[s.length];
    for (int i=0; i < s.length; i++) {
      active[i] = s[i].equals("true");
    }
  }
  
  public JPanel getDisplayControls() {
    JTextField text;
    JPanel p = new JPanel();
    p.add(new JLabel(getConfigureName() + ": "));
    if (isCounter()) {
      text = new JTextField(" " + getValueName()+" ");
    }
    else {
      text = new JTextField(getLongestName()/2+2);
      text.setText(" " + getValueName());
    }
    p.add(text);
    
    return p;
  }
  
  public JPanel getSetControls() {
    
    JPanel panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    
    JPanel p = new JPanel();
    Box box = Box.createVerticalBox();
    box.setAlignmentX(Component.RIGHT_ALIGNMENT);
    
    if (isCounter()) {

      p.add(new JLabel(getConfigureName() + ":  "));
      counterVal = new IntConfigurer(null, null, new Integer(current));
      counterVal.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent e) {
          current = ((Integer) counterVal.getValue()).intValue();      
        }});
      p.add(counterVal.getControls());
      box.add(p);
      panel.add(box);
    }
    
    else {

      p.add(new JLabel(getConfigureName() + ":  "));
      listVal = new StringEnumConfigurer(null, null, list);
      listVal.setValue(list[current]);
      listVal.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent e) {
          current = findListIndex(listVal.getValueString());
        }});
      p.add(listVal.getControls());
      box.add(p); 
      panel.add(box);
        
      box = Box.createVerticalBox();
      box.setAlignmentX(Component.RIGHT_ALIGNMENT);
      
      activePanel = new JPanel();
      JPanel left = new JPanel();
      left.setAlignmentY(Component.TOP_ALIGNMENT);
      left.setAlignmentX(Component.RIGHT_ALIGNMENT);
      left.add(new JLabel("Active:  "));
      activePanel.add(left);
      
      JPanel activeBox = new JPanel();
      activeBox.setLayout(new BoxLayout(activeBox, BoxLayout.Y_AXIS));
      //activeBox.setBorder(BorderFactory.createLineBorder(Color.black));
      activeBox.setAlignmentX(Component.RIGHT_ALIGNMENT);
      activeVal = new BooleanConfigurer[list.length];

      for (int i = 0; i < list.length; i++) {
        activeVal[i] = new ActiveConfigurer(i);
        activeVal[i].addPropertyChangeListener(new PropertyChangeListener() {
          public void propertyChange(PropertyChangeEvent e) {
           int index = ((ActiveConfigurer) e.getSource()).getIndex();
           active[index] = activeVal[index].booleanValue().booleanValue();
          }});
        activeBox.add(activeVal[i].getControls());
      }
      
      activePanel.add(activeBox);
      box.add(activePanel);
      panel.add(box);
      
      box = Box.createVerticalBox();
      box.setAlignmentX(Component.RIGHT_ALIGNMENT);
      firstPanel = new JPanel();
      firstPanel.add(new JLabel("First:  "));
      firstVal = new StringEnumConfigurer(null, null, list);
      firstVal.setValue(list[first]);
      firstVal.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent e) {
          first = findListIndex(firstVal.getValueString());
        }});
      firstPanel.add(firstVal.getControls());
      firstPanel.setAlignmentX(Component.LEFT_ALIGNMENT);

      box.add(firstPanel);
      panel.add(box);
         
    }
    

    return panel;
  }
  
  
  public void toggleConfigVisibility() {
    configMode = !configMode;
    updateVisibility();
  }
  
  public void setConfigVisibility(boolean b) {
    configMode = b;
    updateVisibility();
  }
  
  public void updateVisibility() {
    if (!isCounter()) {
      firstPanel.setVisible(configMode);
      activePanel.setVisible(configMode);
    }
  }
  
  public class ActiveConfigurer extends BooleanConfigurer {
  
    int index;
    
    public ActiveConfigurer(int index) {
      super(null, list[index], new Boolean(active[index]));
      this.index = index;
    }
    
    public int getIndex() {
      return index;
    }
  }
  
  protected int findListIndex(String s) {
    int index = -1;
    int i = 0;
    while (i < list.length && index < 0) {
      if (list[i].equals(s)) {
        index = i;
      }
      i++;
    }
    if (index < 0) {
      index = 0;
    }
    return index;
  }
  
  protected int getLongestName() {
    int m = 1;
    for (int i = 0; i < list.length; i++) {
      if (list[i] != null & list[i].length() > m) {
        m = list[i].length();
      }
    }
    return m;
  }
  
  public String getValueName() {
    if (isCounter()) {
      return current + "";
    }
    else {
      if (current >= 0 && current <= (list.length-1)) {
        return list[current];
      }
    }
    return "";
  }
  
  public void advance() {
    rolledOver = false;
    if (isCounter()) {
      current++;
    }
    else {
      int idx = current;
      boolean done = false;
      for (int i = 0; i < list.length && !done; i++) {
        idx++;
        if (idx >= list.length) {
          idx = 0;
        }
        if (idx == first) {
          rolledOver = true;
        }
        done = active[idx];
      }
      current = idx;
    }
  }
  
  public void retreat() {
    rolledOver = false;
    if (isCounter()) {
      current--;
    }
    else {
      int idx = current;
      boolean done = false;
      for (int i = 0; i < list.length && !done; i++) {
        if (idx == first) {
          rolledOver = true;
        }
        idx--;
        if (idx < 0) {
          idx = list.length-1;
        }
        done = active[idx];
      }
      current = idx;
    }
  }

  public boolean hasRolledOver() {
    return rolledOver;
  }
  
  public String[] getAttributeDescriptions() {
    return new String[] { "Name:  ", "Type:  ", "Start Value:  ", "Increment By:  ", "List:  " };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, TypeConfig.class, Integer.class, Integer.class, String[].class };
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, TYPE, START, INCR, LIST };
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (TYPE.equals(key)) {
      type = (String) value;
    }
    else if (START.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      start = ((Integer) value).intValue();
      current = start;
    }
    else if (INCR.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      incr = ((Integer) value).intValue();
    }
    else if (LIST.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      list = ((String[]) value);
      active = new boolean[list.length];
      for (int i = 0; i < active.length; i++) {
        active[i] = true;
      }
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (TYPE.equals(key)) {
      return type;
    }
    else if (START.equals(key)) {
      return start + "";
    }
    else if (INCR.equals(key)) {
      return incr + "";
    }
    else if (LIST.equals(key)) {
      return StringArrayConfigurer.arrayToString(list);
    }
    else
      return "";
  }

  public void removeFrom(Buildable parent) {
    ((TurnCounter) parent).removeLevel(this);
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void addTo(Buildable parent) {
    ((TurnCounter) parent).addLevel(this);
  }

  public static String getConfigureTypeName() {
    return "Turn Level";
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (START.equals(name) || INCR.equals(name)) {
      return counterCond;
    }
    else if (LIST.equals(name)) {
      return listCond;
    }
    else {
      return null;
    }
  }

  private VisibilityCondition counterCond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return isCounter();
    }
  };
  
  private VisibilityCondition listCond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return !isCounter();
    }
  };

  public static class TypeConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { TYPE_COUNTER, TYPE_LIST };
    }
  }

  
}
