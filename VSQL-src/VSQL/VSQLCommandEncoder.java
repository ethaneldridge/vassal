/*
 * $Id$
 *
 * Copyright (c) 2005 by Brent Easton
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
package VSQL;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import VASL.build.module.ASLCommandEncoder;
import VASL.counters.Concealable;
import VASL.counters.Concealment;
import VASL.counters.TextInfo;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.configure.Configurer;
import VASSAL.configure.DoubleConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.counters.Decorator;
import VASSAL.counters.Embellishment;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Hideable;
import VASSAL.counters.ReportState;

/**
 * @author Brent Easton
 */
public class VSQLCommandEncoder extends ASLCommandEncoder {
  
  public VSQLCommandEncoder() {
    
    final IntConfigurer zoomLevels = new IntConfigurer(VSQLProperties.ZOOM_LEVELS, "Personal Number of Zoom Levels (0 for default):  ", new Integer(0));
    GameModule.getGameModule().getPrefs().addOption(VSQLProperties.VSQL, zoomLevels);

    final IntConfigurer zoomStart = new IntConfigurer(VSQLProperties.ZOOM_START, "Personal Starting Zoom Level (0 for default):  ", new Integer(0));
    GameModule.getGameModule().getPrefs().addOption(VSQLProperties.VSQL, zoomStart);

    final DoubleConfigurer zoomFactor = new DoubleConfigurer(VSQLProperties.ZOOM_FACTOR, "Personal Magnification Factor (0.0 for default):  ", new Double(0.0));
    GameModule.getGameModule().getPrefs().addOption(VSQLProperties.VSQL, zoomFactor);

    //final VSQLStringEnumConfigurer ruleLevel = new VSQLStringEnumConfigurer(VSQLProperties.RULE_LEVEL, "Rule Level", rule_levels);
    //GameModule.getGameModule().getPrefs().addOption(VSQLProperties.VSQL, ruleLevel);
  }
  
  /**
   * Create the rule-level preference
   */
  public void addTo(Buildable b) {
    super.addTo(b);
    
  }
  
 /**
  * Primary Command Encoder for VSQL. MarkMoved decorators are intercepted
  * and created as VSQLMarkMoved instead.
  */
  public Decorator createDecorator(String type, GamePiece inner) {
    if (type.startsWith(VSQLFootprint.ID)) {
      return new VSQLFootprint(type, inner);
    }
    else if (type.startsWith(VSQLMarkMoved.ID)) {
      return new VSQLMarkMoved(type, inner);
    }
    else if (type.startsWith(Embellishment.OLD_ID) || type.startsWith(Embellishment.ID)) {
      return new VSQLEmbellishment(type, inner);
    }
    else if (type.startsWith(VSQLTurreted.ID)) {
      return new VSQLTurreted(type, inner);
    }
    else if (type.startsWith(Hideable.ID)) {
      return new VSQLHideable(type, inner);
    }
    else if (type.startsWith(Concealment.ID)) {
      return new VSQLConcealment(type, inner);
    }
    else if (type.startsWith(Concealable.ID)) {
      return new VSQLConcealable(type, inner);
    }
    else if (type.startsWith(ReportState.ID)) {
      return new VSQLReportState(type, inner);
    }
    else if (type.startsWith(TextInfo.ID)) {
      return new VSQLTextInfo(type, inner);
    }
    else if (type.startsWith(TrackRotator.ID)) {
      return new TrackRotator(type, inner);
    }
    else if (type.startsWith(RestrictCommands.ID)) {
      return new RestrictCommands(type, inner);
    }
    else {
      return super.createDecorator(type, inner);
    }
  }
  
  public class VSQLStringEnumConfigurer extends Configurer {
    private String[] validValues;
    private JComboBox box;
    private JPanel panel;

    public VSQLStringEnumConfigurer(String key, String name, String[] validValues) {
      super(key, name);
      this.validValues = validValues;
    }

    public Component getControls() {
      if (panel == null) {
        panel = new JPanel();
        panel.add(new JLabel(name));
        box = new JComboBox(validValues);
        if (isValidValue(getValue())) {
          box.setSelectedItem(getValue());
        }
        else if (validValues.length > 0) {
          box.setSelectedIndex(0);
        }
        box.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            noUpdate = true;
            setValue(box.getSelectedItem());
            noUpdate = false;
          }
        });
        panel.add(box);
      }
      return panel;
    }

    public boolean isValidValue(Object o) {
      for (int i = 0; i < validValues.length; ++i) {
        if (validValues[i].equals(o)) {
          return true;
        }
      }
      return false;
    }

    public String[] getValidValues() {
      return validValues;
    }

    public void setValidValues(String[] s) {
      validValues = s;
      box.setModel(new DefaultComboBoxModel(validValues));
    }

    public void setValue(Object o) {
      if (validValues == null || isValidValue(o)) {
        super.setValue(o);
        if (!noUpdate && box != null) {
          box.setSelectedItem(o);
        }
      }
    }

    public String getValueString() {
      return box != null ? (String) box.getSelectedItem() : validValues[0];
    }

    public void setValue(String s) {
      setValue((Object) s);
    }

  }
}
