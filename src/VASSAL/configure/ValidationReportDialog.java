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

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

/**
 * Dialog for reporting the results of validating a GameModule
 */
public class ValidationReportDialog extends JFrame {
  private CallBack callback;

  public ValidationReportDialog(ValidationReport report, CallBack cb) {
    setTitle("Problems found in module");
    this.callback = cb;
    getContentPane().setLayout(new BoxLayout(getContentPane(),BoxLayout.Y_AXIS));
    List warnings = report.getWarnings();
    switch (warnings.size()) {
      case 0:
        getContentPane().add(new JLabel("No problems found"));
        JButton b = new JButton("Ok");
        b.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            callback.ok();
            dispose();
          }
        });
        JPanel p = new JPanel();
        p.add(b);
        getContentPane().add(p);
        break;
        case 1:
        getContentPane().add(new JLabel("A problem was found in this module."));
        getContentPane().add(new JLabel(warnings.get(0).toString()+"."));
        getContentPane().add(createOkCancelButtons());
        break;
        default:
        getContentPane().add(new JLabel("The following problems were found in this module."));
        getContentPane().add(new JLabel("If not fixed, they could cause bugs during game play."));
        getContentPane().add(new JList(warnings.toArray()));
        getContentPane().add(createOkCancelButtons());
    }
    pack();
    setLocationRelativeTo(null);
  }

  private JPanel createOkCancelButtons() {
    JPanel p = new JPanel();
    JButton ok = new JButton("Ignore, save anyway");
    ok.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        callback.ok();
        dispose();
      }
    });
    p.add(ok);
    JButton cancel = new JButton("Cancel");
    cancel.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        callback.cancel();
        dispose();
      }
    });
    p.add(cancel);
    return p;
  }

  public static interface CallBack {
    void ok();
    void cancel();
  }
}
