/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney & Brent Easton
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
/*
 * FormattedStringConfigurer.
 * Extended version of StringConfigure that provides a drop down list of options that can
 * be inserted into the string
 */
package VASSAL.configure;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

import javax.swing.JComboBox;

public class FormattedStringConfigurer
    extends StringConfigurer
    implements ActionListener, FocusListener {

  protected String[] optionList = new String[0];
  JComboBox dropList;

  public FormattedStringConfigurer(String key, String name) {
    super(key, name);
  }

  public FormattedStringConfigurer(
      String key,
      String name,
      String[] options) {
    this(key, name);
    setOptions(options);
  }

  public void setOptions(String[] t) {
    optionList = new String[t.length + 1];
    optionList[0] = "Insert";
    for (int i = 1; i < optionList.length; i++) {
      optionList[i] = t[i - 1];
    }
  }

  public java.awt.Component getControls() {
    if (p == null) {
      super.getControls();

      nameField.addFocusListener(this);
      dropList = new JComboBox((String[]) optionList);
      dropList.setSelectedIndex(0);
      dropList.setEnabled(false);
      dropList.addActionListener(this);

      p.add(dropList);
    }

    return p;
  }

  /*
   * Drop-down list has been clicked, insert selected option onto string
   */
  public void actionPerformed(ActionEvent arg0) {
    String item = "";

    int selectedItem = dropList.getSelectedIndex();

    if (selectedItem > 0) {
      item = '$' + optionList[selectedItem] + '$';
      String work = nameField.getText();

      // Cut out any selected text
      if (nameField.getSelectedText() != null) {
        int start = nameField.getSelectionStart();
        int end = nameField.getSelectionEnd();
        work = work.substring(0, start) + work.substring(end);
      }
      int pos = nameField.getCaretPosition();
      String news = work.substring(0, pos) + item + work.substring(pos);
      nameField.setText(news);

      // Update the text field and repaint it
      noUpdate = true;
      setValue(nameField.getText());
      noUpdate = false;
      nameField.repaint();
    }
    // Send focus back to text field
    nameField.requestFocusInWindow();
  }

  /*
   * Focus gained on text field, so enable insert drop-down
   * and make sure it says 'Insert'
   */
  public void focusGained(FocusEvent arg0) {
    dropList.setSelectedIndex(0);
    dropList.setEnabled(true);
    dropList.repaint();
  }

  /*
   * Focus lost on text field, so disable insert drop-down
   */
  public void focusLost(FocusEvent arg0) {
    dropList.setEnabled(false);

  }
}
