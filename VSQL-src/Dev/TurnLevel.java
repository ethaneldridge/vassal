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
  protected static final String LOOP = "loop";
  protected static final String LOOP_LIMIT = "loopLimit";
  protected static final String LIST = "list";

  protected static final String TYPE_COUNTER = "Counter";
  protected static final String TYPE_LIST = "List";

  // Module Configuration variable 
  protected String type = TYPE_COUNTER;
  protected int start = 1;
  protected int incr = 1;
  protected boolean loop = false;
  protected int loopLimit = 10;
  protected String[] list = new String[0];
  
  protected boolean rolledOver = false;
  
  // In play state variables
  protected int value = 1;
  protected int first = 1;
  protected boolean[] active = new boolean[0]; 

  public TurnLevel() {
    super();
  }

  protected String getState() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(value);
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
    value = sd.nextInt(1);
    first = sd.nextInt(1);
    
    String[] s = sd.nextStringArray(0);
    active = new boolean[s.length];
    for (int i=0; i < s.length; i++) {
      active[i] = s[i].equals("true");
    }
  }
  
  public JPanel getControls() {
  
    JPanel p = new JPanel();
    p.add(new JLabel(getConfigureName() + ": "));
    JTextField text = new JTextField(" " + getValueName()+" ");
    p.add(text);
    
    return p;
  }
  
  public String getValueName() {
    if (type.equals(TYPE_COUNTER)) {
      return value + "";
    }
    else {
      if (value >= 0 && value <= (list.length-1)) {
        return list[value];
      }
    }
    return "";
  }
  
  public void advance() {
    rolledOver = false;
    if (type.equals(TYPE_COUNTER)) {
      value++;
      if (loop && value > loopLimit) {
        rolledOver = true;
        value = start;
      }
    }
    else {
      int idx = value;
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
      value = idx;
    }
  }
  
  public boolean hasRolledOver() {
    return rolledOver;
  }
  
  public String[] getAttributeDescriptions() {
    return new String[] { "Name:  ", "Type:  ", "Start Value:  ", "Increment By:  ", "Loop?", "Loop at Value:  ",
        "List:  " };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, TypeConfig.class, Integer.class, Integer.class, Boolean.class, Integer.class,
        String[].class };
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, TYPE, START, INCR, LOOP, LOOP_LIMIT, LIST };
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
    }
    else if (INCR.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      incr = ((Integer) value).intValue();
    }
    else if (LOOP.equals(key)) {
      if (value instanceof String) {
        value = new Boolean((String) value);
      }
      loop = ((Boolean) value).booleanValue();
    }
    else if (LOOP_LIMIT.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      loopLimit = ((Integer) value).intValue();
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
    else if (LOOP.equals(key)) {
      return loop + "";
    }
    else if (LOOP_LIMIT.equals(key)) {
      return loopLimit + "";
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
    if (START.equals(name) || INCR.equals(name) || LOOP.equals(name)) {
      return counterCond;
    }
    else if (LOOP_LIMIT.equals(name)) {
      return limitCond;
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
      return type.equals(TYPE_COUNTER);
    }
  };

  private VisibilityCondition limitCond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return type.equals(TYPE_COUNTER) && loop;
    }
  };
  
  private VisibilityCondition listCond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return type.equals(TYPE_LIST);
    }
  };

  public static class TypeConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { TYPE_COUNTER, TYPE_LIST };
    }
  }
  
}
