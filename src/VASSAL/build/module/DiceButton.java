/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
package VASSAL.build.module;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.AutoConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerWindow;
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.LaunchButton;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

/**
 * This component places a button into the controls window toolbar.
 * Pressing the button generates random numbers and displays the
 * result in the Chatter */
public class DiceButton extends AbstractConfigurable {
  protected java.util.Random ran;
  protected int nSides = 6, nDice = 2, plus = 0;
  protected boolean reportTotal = false;
  protected boolean promptAlways = false;
  protected LaunchButton launch;

  public static final String LABEL = "label";
  public static final String N_DICE = "nDice";
  public static final String N_SIDES = "nSides";
  public static final String PLUS = "plus";
  public static final String HOTKEY = "hotkey";
  public static final String REPORT_TOTAL = "reportTotal";
  public static final String PROMPT_ALWAYS = "prompt";

  public DiceButton() {
    ActionListener rollAction = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        if (promptAlways) {
          promptAlways = false; // Show the usu
          // Remove label, hotkey, and prompt controls
          AutoConfigurer ac = (AutoConfigurer) getConfigurer();
          ConfigurerWindow w = new ConfigurerWindow(ac, true);
          ac.getConfigurer(LABEL).getControls().setVisible(false);
          ac.getConfigurer(HOTKEY).getControls().setVisible(false);
          ac.getConfigurer(PROMPT_ALWAYS).getControls().setVisible(false);
          w.setVisible(true);
          ac.getConfigurer(LABEL).getControls().setVisible(true);
          ac.getConfigurer(HOTKEY).getControls().setVisible(true);
          ac.getConfigurer(PROMPT_ALWAYS).getControls().setVisible(true);
          DR();
          promptAlways = true;
        }
        else {
          DR();
        }
      }
    };
    launch = new LaunchButton(null, LABEL, HOTKEY, rollAction);
    setAttribute(LABEL, "2d6");
  }

  public static String getConfigureTypeName() {
    return "Dice Button";
  }

  /**
   * The text reported before the results of the roll
   */
  protected String getReportPrefix() {
    return " *** " + getConfigureName() + " = ";
  }

  /**
   * The text reported after the results of the roll;
   */
  protected String getReportSuffix() {
    return " ***  <"
      + GameModule.getGameModule().getChatter().getHandle() + ">";
  }

  /**
   * Forwards the result of the roll to the {@link Chatter#send}
   * method of the {@link Chatter} of the {@link GameModule}.  Format is
   * prefix+[comma-separated roll list]+suffix */
  protected void DR() {
    String val = getReportPrefix();
    int total = 0;
    for (int i = 0; i < nDice; ++i) {
      int roll = (int) (ran.nextFloat() * nSides + 1) + plus;
      if (reportTotal) {
        total += roll;
      }
      else {
        val += roll;
        if (i < nDice - 1)
          val += ",";
      }
    }

    if (reportTotal)
      val += total;

    val += getReportSuffix();
    GameModule.getGameModule().getChatter().send(val);
  }

  /**
   * The Attributes of a DiceButton are:
   *
   * <code>LABEL</code> the label of the button in the toolbar
   * <code>HOTKEY</code> the hotkey equivalent of the button
   * <code>N_DICE</code> the number of dice to roll on each button press
   * <code>N_SIDES</code> the number of sides of each die
   * <code>REPORT_TOTALL</code> If true, add the results of the dice together and report the total.  Otherwise, report the individual results
   */
  public String[] getAttributeNames() {
    String s[] = {LABEL, N_DICE, N_SIDES, PLUS, REPORT_TOTAL, HOTKEY, PROMPT_ALWAYS};
    return s;
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Button label",
                        "Number of dice",
                        "Number of sides per die",
                        "Add to each die",
                        "Report Total",
                        "Hotkey",
                        "Prompt for values when button pushed"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class,
                       Integer.class,
                       Integer.class,
                       Integer.class,
                       Boolean.class,
                       KeyStroke.class,
                       Boolean.class};
  }

  private VisibilityCondition cond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return !promptAlways;
    }
  };

  public VisibilityCondition getAttributeVisibility(String name) {
    if (N_DICE.equals(name)
      || N_SIDES.equals(name)
      || PLUS.equals(name)
      || REPORT_TOTAL.equals(name)) {
      return cond;
    }
    else {
      return null;
    }
  }

  /**
   * Expects to be added to a GameModule.  Adds the button to the
   * control window's toolbar and registers itself as a {@link
   * KeyStrokeListener} */
  public void addTo(Buildable parent) {
    ran = GameModule.getGameModule().getRNG();
    GameModule.getGameModule().getToolBar().add(getComponent());
  }

  /**
   * The component to be added to the control window toolbar
   */
  protected java.awt.Component getComponent() {
    return launch;
  }

  public void setAttribute(String key, Object o) {
    if (LABEL.equals(key)) {
      setConfigureName((String) o);
      launch.setAttribute(key, (String) o);
    }
    else if (N_DICE.equals(key)) {
      if (o instanceof Integer) {
        nDice = ((Integer) o).intValue();
      }
      else if (o instanceof String) {
        nDice = Integer.parseInt((String) o);
      }
    }
    else if (N_SIDES.equals(key)) {
      if (o instanceof Integer) {
        nSides = ((Integer) o).intValue();
      }
      else if (o instanceof String) {
        nSides = Integer.parseInt((String) o);
      }
    }
    else if (PLUS.equals(key)) {
      if (o instanceof Integer) {
        plus = ((Integer) o).intValue();
      }
      else if (o instanceof String) {
        plus = Integer.parseInt((String) o);
      }
    }
    else if (REPORT_TOTAL.equals(key)) {
      if (o instanceof Boolean) {
        reportTotal = ((Boolean) o).booleanValue();
      }
      else if (o instanceof String) {
        reportTotal = "true".equals(o);
      }
    }
    else if (PROMPT_ALWAYS.equals(key)) {
      if (o instanceof Boolean) {
        promptAlways = ((Boolean) o).booleanValue();
      }
      else if (o instanceof String) {
        promptAlways = "true".equals(o);
      }
    }
    else {
      launch.setAttribute(key, o);
    }
  }

  public String getAttributeValueString(String key) {
    if (N_DICE.equals(key)) {
      return "" + nDice;
    }
    else if (N_SIDES.equals(key)) {
      return "" + nSides;
    }
    else if (PLUS.equals(key)) {
      return "" + plus;
    }
    else if (REPORT_TOTAL.equals(key)) {
      return "" + reportTotal;
    }
    else if (PROMPT_ALWAYS.equals(key)) {
      return "" + promptAlways;
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void removeFrom(Buildable b) {
    GameModule.getGameModule().getToolBar().remove(getComponent());
    GameModule.getGameModule().getToolBar().revalidate();
  }

  public HelpFile getHelpFile() {
    File dir = new File("docs");
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "GameModule.htm"),"#DiceButton");
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }
}
