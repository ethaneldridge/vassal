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

import VASSAL.build.Configurable;
import VASSAL.build.GameModule;

import javax.swing.*;
import javax.swing.tree.TreePath;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.event.TreeSelectionListener;
import javax.swing.event.TreeSelectionEvent;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 * Dialog that prompts the user to select a component from the {@link ConfigureTree}
 */
public class ChooseComponentDialog extends JDialog implements TreeSelectionListener {
  private Configurable target;
  private Class targetClass;
  private JButton okButton;
  private VASSAL.configure.ConfigureTree tree;

  public ChooseComponentDialog(Frame owner, Class targetClass) {
    super(owner, true);
    this.targetClass = targetClass;
    setDefaultCloseOperation(DISPOSE_ON_CLOSE);
    getContentPane().setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
    tree = new VASSAL.configure.ConfigureTree(GameModule.getGameModule(), null) {
      public void mouseReleased(MouseEvent e) {
        super.mouseReleased(e);
      }
    };
    tree.addTreeSelectionListener(this);
    getContentPane().add(new JScrollPane(tree));
    Box b = Box.createHorizontalBox();
    okButton = new JButton("Ok");
    okButton.setEnabled(false);
    okButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        dispose();
      }
    });
    JButton cancelButton = new JButton("Cancel");
    cancelButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        target = null;
        dispose();
      }
    });
    b.add(okButton);
    b.add(cancelButton);
    getContentPane().add(b);
    pack();
  }

  public void valueChanged(TreeSelectionEvent e) {
    boolean enabled = false;
    target = null;
    TreePath path = tree.getSelectionPath();
    if (path != null) {
      Object selected = ((DefaultMutableTreeNode) path.getLastPathComponent()).getUserObject();
      enabled = isValidTarget(selected);
      if (enabled) {
        target = (Configurable) selected;
      }
    }
    okButton.setEnabled(enabled);
  }

  protected boolean isValidTarget(Object selected) {
    return targetClass.isInstance(selected);
  }

  public Configurable getTarget() {
    return target;
  }
}
