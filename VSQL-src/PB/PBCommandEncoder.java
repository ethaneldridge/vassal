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
package PB;

import java.awt.Color;
import java.awt.Component;

import javax.swing.JButton;
import javax.swing.JPanel;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import VASL.counters.ASLHighlighter;
import VASL.counters.ColorTable;
import VASL.counters.ColoredBox;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.command.Command;
import VASSAL.configure.ColorConfigurer;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;

public class PBCommandEncoder extends VASSAL.build.module.BasicCommandEncoder implements ColorTable {

  private Map map;
  private boolean promptForColors = true;

  public void addTo(Buildable b) {
    super.addTo(b);
    BasicPiece.setHighlighter(new ASLHighlighter());
    
    initColor("al", "Allied", new Color(140, 198, 140));
    initColor("ge", "German", new Color(181, 214, 214));
    initColor("ru", "Russian", new Color(255, 206, 57));
    initColor("ar", "Arab", new Color(204, 153, 102));
    initColor("is", "Israeli", new Color(255, 255, 255));
  }

  public void build(Element e) {
    super.build(e);
    promptForColors = !"true".equals(e.getAttribute("noColorPreferences"));
  }

  public Element getBuildElement(Document doc) {
    Element el = super.getBuildElement(doc);
    el.setAttribute("noColorPreferences", promptForColors ? "" : "true");
    return el;
  }

  public Command decode(String command) {
    Command c = super.decode(command);
    return c;
  }

  protected Decorator createDecorator(String type, GamePiece inner) {
    if (type.startsWith(ColoredBox.ID)) {
      return new ColoredBox(type, inner);
    }
    else if (type.startsWith(PBConcealable.ID)) {
      return new PBConcealable(type, inner);
    }
    else if (type.startsWith(PBConcealment.ID)) {
      return new PBConcealment(type, inner);
    }
    else {
      return super.createDecorator(type, inner);
    }
  }

  private void initColor(String key, String name, Color defaultColor) {
    ColorConfigurer c = new Col(key, name, defaultColor);
    GameModule.getGameModule().getPrefs().addOption(promptForColors ? "Nationality Colors" : null, c);
    if (c.getValue() == null) {
      c.setValue(defaultColor);
    }
  }

  public Color getColor(String s) {
    return (Color) GameModule.getGameModule().getPrefs().getValue(s);
  }

  public static class Col extends ColorConfigurer {
    private JPanel p;
    private Color defaultColor;

    public Col(String key, String name, Color c) {
      super(key, name, c);
      defaultColor = c;
    }

    public Component getControls() {
      if (p == null) {
        p = (JPanel) super.getControls();
        JButton b = new JButton("Default");
        p.add(b);
        b.addActionListener
            (new java.awt.event.ActionListener() {
              public void actionPerformed
                  (java.awt.event.ActionEvent e) {
                setValue(defaultColor);
              }
            });
      }
      return p;
    }
  }
}


