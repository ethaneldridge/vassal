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
package VASSAL.tools;

import VASSAL.build.GameModule;

import javax.swing.*;
import java.awt.event.*;
import java.awt.*;

/**
 * Warns the user when an uncaught Exception occurs
 * Use this by calling System.setProperty("sun.awt.exception.handler","VASSAL.tools.ErrorLog");
 */
public class ErrorLog {
  private static boolean disabled = false;

  public void handle(Throwable t) {
    if (!disabled) {
      String type = t.getClass().getName().substring(t.getClass().getName().lastIndexOf(".") + 1);
      String msg = t.getMessage();
      if (msg == null
        || msg.length() == 0) {
        msg = type;
      }
      else {
        msg = type + "\n" + msg;
      }
      JButton okButton = new JButton("Ok");
      JButton disableButton = new JButton("Don't show this dialog again");
      String text = "An untrapped error has occurred.\n"
        + msg + "\n"
        + "Please send a report to support@vassalengine.org and attach the errorLog file.";
      if (t instanceof OutOfMemoryError) {
        text = "The application has run out of memory.\nTo decrease memory usage, try reducing the number of colors in your display.";
      }
      final JOptionPane pane = new JOptionPane
        (text,
         JOptionPane.DEFAULT_OPTION,
         JOptionPane.ERROR_MESSAGE,
         UIManager.getIcon("OptionPane.errorIcon"),
         new Object[]{okButton, disableButton},
         okButton);
      Component comp = GameModule.getGameModule() == null ? null : GameModule.getGameModule().getFrame();
      final JDialog dialog = pane.createDialog(comp, "Error");
      okButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          pane.setValue(Boolean.FALSE);
          dialog.dispose();
        }
      });
      disableButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          pane.setValue(Boolean.TRUE);
          dialog.dispose();
        }
      });
      Runnable runnable = new Runnable() {
        public void run() {
          dialog.show();
          disabled = Boolean.TRUE.equals(pane.getValue());
        }
      };
      SwingUtilities.invokeLater(runnable);
    }
    t.printStackTrace();
  }

  public static void main(String[] args) {
    ErrorLog log = new ErrorLog();
    while (!disabled) {
      log.handle(new RuntimeException("Warning!!!"));
    }
  }

  public static class Group extends ThreadGroup {
    private ErrorLog handler = new ErrorLog();

    public Group() {
      super("Main Thread");
    }

    public void uncaughtException(Thread t, Throwable e) {
      handler.handle(e);
    }
  }
}
