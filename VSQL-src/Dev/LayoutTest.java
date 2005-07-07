/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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
 
package Dev;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SpringLayout;
import javax.swing.border.BevelBorder;
import javax.swing.plaf.basic.BasicArrowButton;

/**
 * @author Brent
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class LayoutTest {

  
  private static void createAndShowGUI() {

    JDialog dialog = new JDialog();
    
    JPanel main = new JPanel();
    main.setLayout(new BoxLayout(main, BoxLayout.X_AXIS));
    
    JPanel box1 = new JPanel();
    //Box box1 = Box.createVerticalBox();
    box1.setMinimumSize(new Dimension(25, 50));
    box1.setMaximumSize(new Dimension(25, 999));
    box1.setLayout(new BoxLayout(box1, BoxLayout.Y_AXIS));
    //SpringLayout layout = new SpringLayout();
    //box1.setLayout(layout);
    
    //JButton b1 = new BasicArrowButton(BasicArrowButton.NORTH);
    //JButton b2 = new BasicArrowButton(BasicArrowButton.SOUTH);
    JLabel b1 = new JLabel();
    JLabel b2 = new JLabel();

    b1.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED ));
    b2.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED ));
    b1.setMinimumSize(new Dimension(20,20));
    b2.setMinimumSize(new Dimension(20,20));
    b1.setMaximumSize(new Dimension(20,20));
    b2.setMaximumSize(new Dimension(20,20));
    b1.setPreferredSize(new Dimension(20,20));
    b2.setPreferredSize(new Dimension(20,20));

    //layout.putConstraint(SpringLayout.NORTH, b1, 5, SpringLayout.NORTH, box1);
    //layout.putConstraint(SpringLayout.NORTH, b2, 5, SpringLayout.SOUTH, b1);
    //layout.putConstraint(SpringLayout.EAST, box1,        5,        SpringLayout.EAST, b1);
    //layout.putConstraint(SpringLayout.SOUTH, box1,        5,        SpringLayout.SOUTH, b2);


    box1.add(b1);
    box1.add(b2);
    main.add(box1);
    
    JPanel p = new JPanel();
    p.setBorder(BorderFactory.createLineBorder(Color.black));
    JLabel l = new JLabel("Some Text to Display");
    p.add(l);
    main.add(p);
    
    dialog.getContentPane().add(main);
    
    

    //Display the window.
    dialog.pack();
    dialog.setVisible(true);
}

  
  public static void main(String[] args) {
    javax.swing.SwingUtilities.invokeLater(new Runnable() {
      public void run() {
          createAndShowGUI();
      }
  });

    
  }

}
