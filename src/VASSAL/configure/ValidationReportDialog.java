/*
 * $Id$
 *
 * Copyright (c) 2004 by Rodney Kinney
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

import VASSAL.build.GameModule;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.*;
import java.util.List;

/**
 * Dialog for reporting the results of validating a GameModule
 */
public class ValidationReportDialog extends JDialog {
  private CallBack callback;

  public ValidationReportDialog(ValidationReport report, CallBack cb) {
    super(GameModule.getGameModule().getFrame(), false);
    setTitle("Problems found in module");
    this.callback = cb;
    Box reportBox = Box.createVerticalBox();
    getContentPane().add(reportBox);
    JPanel buttonPanel = new JPanel();
    getContentPane().add(buttonPanel, BorderLayout.SOUTH);
    List warnings = report.getWarnings();
    switch (warnings.size()) {
      case 0:
        reportBox.add(new JLabel("No problems found"));
        buttonPanel.add(createOkButton());
        break;
        case 1:
        reportBox.add(new JLabel("A problem was found in this module."));
        reportBox.add(new JLabel(warnings.get(0).toString()+"."));
        buttonPanel.add(createOkButton());
        buttonPanel.add(createCancelButton());
        break;
        default:
        reportBox.add(new JLabel("The following problems were found in this module."));
        reportBox.add(new JLabel("If not fixed, they could cause bugs during game play."));
        JList list = new JList(warnings.toArray());
        list.setVisibleRowCount(Math.min(list.getVisibleRowCount(),warnings.size()));
        reportBox.add(new JScrollPane(list));
        buttonPanel.add(createOkButton());
        buttonPanel.add(createCancelButton());
    }
    pack();
    setLocationRelativeTo(null);
  }

  private JButton createCancelButton() {
    JButton cancel = new JButton("Cancel");
    cancel.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        callback.cancel();
        dispose();
      }
    });
    return cancel;
  }

  private JButton createOkButton() {
    JButton ok = new JButton("Ignore, save anyway");
    ok.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        callback.ok();
        dispose();
      }
    });
    return ok;
  }

  public static interface CallBack {
    void ok();
    void cancel();
  }
}
