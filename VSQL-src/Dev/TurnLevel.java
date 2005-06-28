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

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringEnum;
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
  
  public JPanel getControls() {
    JTextField text;
    JPanel p = new JPanel();
    p.add(new JLabel(getConfigureName() + ": "));
    if (isCounter()) {
      text = new JTextField(" " + getValueName()+" ");
    }
    else {
      text = new JTextField(getLongestName()+2);
      text.setText(" " + getValueName());
    }
    p.add(text);
    
    return p;
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
