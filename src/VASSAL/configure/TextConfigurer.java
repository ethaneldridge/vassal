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
package VASSAL.configure;

import java.util.StringTokenizer;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.border.TitledBorder;

import VASSAL.tools.SequenceEncoder;

/**
 * A Configurer that allows multi-line string input via a JTextArea
 */
public class TextConfigurer extends Configurer {
  private JTextArea textArea;
  private JPanel p;

  public TextConfigurer(String key, String name) {
    this(key, name, "");
  }

  public TextConfigurer(String key, String name, String val) {
    super(key, name, val);
  }

  public String getValueString() {
    return escapeNewlines((String) getValue());
  }

  /**
   * Encodes a string by replacing newlines with '|' characters
   * @param s
   * @return
   */
  public static String escapeNewlines(String s) {
    SequenceEncoder se = new SequenceEncoder('|');
    StringTokenizer st = new StringTokenizer(s, "\n\r", true);
    boolean wasNewLine = true;
    while (st.hasMoreTokens()) {
      String token = st.nextToken();
      switch (token.charAt(0)) {
        case '\n':
          if (wasNewLine) {
            se.append("");
          }
          wasNewLine = true;
          break;
        case '\r':
          break;
        default:
          se.append(token);
          wasNewLine = false;
      }
    }
    return se.getValue() == null ? "" : se.getValue();
  }

  public void setValue(String s) {
    String text = restoreNewlines(s);
    setValue((Object) text);
  }

  public void setValue(Object o) {
    super.setValue(o);
    if (!noUpdate && textArea != null) {
      textArea.setText((String) o);
    }
  }

  /**
   * Restores a string by replacing '|' with newlines
   * @param s
   * @return
   */
  public static String restoreNewlines(String s) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, '|');
    String text = "";
    while (st.hasMoreTokens()) {
      text += st.nextToken();
      if (st.hasMoreTokens()) {
        text += "\n";
      }
    }
    return text;
  }

  public java.awt.Component getControls() {
    if (p == null) {
      p = new JPanel();
      p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));
      textArea = new JTextArea(6, 20);
      textArea.addKeyListener(new java.awt.event.KeyAdapter() {
        public void keyReleased(java.awt.event.KeyEvent evt) {
          noUpdate = true;
          setValue((Object) textArea.getText());
          noUpdate = false;
        }
      });
      textArea.setText((String) getValue());
      JScrollPane scroll = new JScrollPane(textArea);
      if (name != null) {
        scroll.setBorder(new TitledBorder(name));
      }
      p.add(scroll);
    }
    return p;
  }
}
