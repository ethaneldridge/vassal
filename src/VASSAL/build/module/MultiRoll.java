/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Brent Easton
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


import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;

import javax.swing.*;
import javax.swing.border.*;

import VASSAL.build.GameModule;

/**
 * @author Brent Easton
 *
 * Dialog for defining a {@link DieManager.RollSet}
 * For use with internet dice rollers
 */
public class MultiRoll extends JDialog {

  private JButton rollButton = new JButton("Roll");
  private JButton canButton = new JButton("Cancel");

  private JPanel descPanel;
  private JTextField descText;
  private JPanel topPanel;
  private JPanel buttonPanel;
  private JPanel detailPanel;
  protected int lastSelectedRow, lastSelectedCol;
  private String description = "";

  protected RollRow[] rollRows;

  public static final int COL_IDX = 0;
  public static final int COL_ROLL = 1;
  public static final int COL_DESC = 2;
  public static final int COL_NDICE = 3;
  public static final int COL_NSIDES = 4;
  public static final int COL_ADD = 5;
  public static final int COL_TOTAL = 6;
  public static final int NUMCOLS = 7;

  public static final int MAX_ROLLS = 10;
  public static final int ROW_HEIGHT = 20;

  public static final int COL1_WIDTH = 31;
  public static final int COL2_WIDTH = 30;
  public static final int COL3_WIDTH = 137;
  public static final int COL4_WIDTH = 50;
  public static final int COL5_WIDTH = 50;
  public static final int COL6_WIDTH = 25;
  public static final int COL7_WIDTH = 35;

  protected DieManager dieManager;
  protected DieRoll[] rolls = new DieRoll[MAX_ROLLS];
  protected boolean[] useDie = new boolean[MAX_ROLLS];
  protected String verification = "";
  protected boolean rollCancelled = false;
  protected boolean singleRoll;

  public MultiRoll(DieManager d, int dfltNDice, int dfltNSides) {
    dieManager = d;
    for (int i = 0; i < MAX_ROLLS; i++) {
      rolls[i] = new DieRoll("", dfltNDice, dfltNSides);
    }
    initConfig(dfltNDice, dfltNSides);
    clearDie();
  }

  private void clearDie() {
    for (int i = 0; i < MAX_ROLLS; i++) {
      useDie[i] = false;
    }
  }

  public boolean wasCancelled() {
    return rollCancelled;
  }
  
  public void setDescription(String s) {
  	description = s;
  	descText.setText(s);
  }
  
  public String getDescription() {
  	return description;
  }
  
  public DieManager.RollSet getRollSet() {
    ArrayList l = new ArrayList();
    for (int i=0;i<MAX_ROLLS;++i) {
      if (useDie[i]) {
        l.add(rolls[i]);
      }
    }
    DieRoll[] rolls = (DieRoll[]) l.toArray(new DieRoll[l.size()]);
    return new DieManager.RollSet(getDescription(),rolls);
  }

  // Multi-roll Configuration code
  private void initConfig(int nd, int ns) {

    setModal(true);

    setTitle("Multi Roller");
    setSize(380, 206);
    setBackground(Color.gray);

    // Create a panel to hold all other components
    topPanel = new JPanel();
    topPanel.setLayout(new BoxLayout(topPanel, BoxLayout.PAGE_AXIS));

	descPanel = new JPanel();
	JLabel descLabel = new JLabel("Roll Description");
	descText = new JTextField(20);
	descText.setText(GameModule.getGameModule().getChatter().getInputField().getText());
	descText.addKeyListener(new java.awt.event.KeyAdapter() {
	  public void keyReleased(java.awt.event.KeyEvent e) {
		description = descText.getText();
	  }
	});
	descPanel.add(descLabel);
	descPanel.add(descText);
	topPanel.add(descPanel);
	
    detailPanel = new JPanel();
    detailPanel.setLayout(new BoxLayout(detailPanel, BoxLayout.PAGE_AXIS));
    detailPanel.setBorder(BorderFactory.createLineBorder(Color.black));

    detailPanel.add(new HeaderRow());

    rollRows = new RollRow[MAX_ROLLS];
    for (int i = 0; i < MAX_ROLLS; i++) {
      rollRows[i] = new RollRow(i, nd, ns);
      detailPanel.add(rollRows[i]);
    }

    topPanel.add(detailPanel);

    // Add Some buttons
    buttonPanel = new JPanel();
    rollButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent e) {
        rollCancelled = false;
        setVisible(false);
      }
    });
    canButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent e) {
        rollCancelled = true;
        setVisible(false);
      }
    });

    buttonPanel.add(rollButton);
    buttonPanel.add(canButton);

    getContentPane().add(topPanel, BorderLayout.PAGE_START);
    getContentPane().add(buttonPanel, BorderLayout.PAGE_END);

    pack();
  }

  class HeaderRow extends JPanel {

    public HeaderRow() {

      Border raisedbevel = BorderFactory.createRaisedBevelBorder();
      Border myBorder = raisedbevel;

      //setLayout(new BoxLayout(this, BoxLayout.LINE_AXIS));
      //setBorder(blackline);

      JLabel col1 = new JLabel("Roll");
      col1.setPreferredSize(new Dimension(COL1_WIDTH, ROW_HEIGHT));
      col1.setHorizontalAlignment(JTextField.CENTER);
      col1.setBorder(myBorder);

//			JLabel col2 = new JLabel("Roll");
//			col2.setPreferredSize(new Dimension(COL2_WIDTH, ROW_HEIGHT));
//			col2.setHorizontalAlignment(JTextField.CENTER);
//            col2.setBorder(myBorder);

      JLabel col3 = new JLabel("Description");
      col3.setPreferredSize(new Dimension(COL3_WIDTH, ROW_HEIGHT));
      col3.setBorder(myBorder);

      JLabel col4 = new JLabel("nDice");
      col4.setBorder(myBorder);
      col4.setHorizontalAlignment(JTextField.CENTER);
      col4.setPreferredSize(new Dimension(COL4_WIDTH, ROW_HEIGHT));

      JLabel col5 = new JLabel("nSides");
      col5.setBorder(myBorder);
      col5.setHorizontalAlignment(JTextField.CENTER);
      col5.setPreferredSize(new Dimension(COL5_WIDTH, ROW_HEIGHT));

      JLabel col6 = new JLabel("add");
      col6.setBorder(myBorder);
      col6.setPreferredSize(new Dimension(COL6_WIDTH, ROW_HEIGHT));

      JLabel col7 = new JLabel("Total");
      col7.setBorder(myBorder);
      col7.setPreferredSize(new Dimension(COL7_WIDTH, ROW_HEIGHT));

      add(col1);
      //add(col2);
      add(col3);
      add(col4);
      add(col5);
      add(col6);
      add(col7);
    }
  }

  class RollRow extends JPanel {

    int myRow;
    boolean selected;
    String description;
    int nDice, nSides, plus;
    boolean reportTotal;
    Dimension rowDim = new Dimension(40, ROW_HEIGHT);

    StateButton col1;
    JCheckBox col2, col7;
    JComboBox col4, col5;
    JTextField col3, col6;

    Border blackline = BorderFactory.createLineBorder(Color.black);
    Border raisedetched = BorderFactory.createEtchedBorder(EtchedBorder.RAISED);
    Border loweredetched = BorderFactory.createEtchedBorder(EtchedBorder.LOWERED);
    Border raisedbevel = BorderFactory.createRaisedBevelBorder();
    Border loweredbevel = BorderFactory.createLoweredBevelBorder();
    Border myBorder = raisedbevel;

    public RollRow(int row, int nd, int ns) {

      myRow = row;

      //setLayout(new BoxLayout(this, BoxLayout.LINE_AXIS));

      col1 = new StateButton((row + 1) + "");
      col1.setPreferredSize(new Dimension(COL1_WIDTH, ROW_HEIGHT));
      col1.setState(useDie[myRow]);
      col1.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          col1.switchState();
          useDie[myRow] = col1.getState();
        }
      });


      // Roll Number
//            col1 = new JLabel((row + 1) + "");
//            col1.setPreferredSize(new Dimension(COL1_WIDTH, ROW_HEIGHT));
//            col1.setHorizontalAlignment(JTextField.CENTER);
//            col1.setBorder(myBorder);
//
//            // Selection Checkbox
//            col2 = new JCheckBox();
//            col2.setBorder(myBorder);
//            col2.setPreferredSize(new Dimension(COL2_WIDTH, ROW_HEIGHT));
//            col2.setSelected(useDie[myRow]);
//            col2.setHorizontalAlignment(JCheckBox.CENTER);
//            col2.addItemListener(new ItemListener() {
//                public void itemStateChanged(ItemEvent e) {
//                    useDie[myRow] = (e.getStateChange() == ItemEvent.SELECTED);
//                }
//            });

      // Roll Description
      col3 = new JTextField(12);
      col3.setBorder(myBorder);
      col3.setPreferredSize(new Dimension(COL3_WIDTH, ROW_HEIGHT));
      col3.setText(rolls[myRow].getDescription() + "");
      col3.addKeyListener(new java.awt.event.KeyAdapter() {
        public void keyReleased(java.awt.event.KeyEvent e) {
          rolls[myRow].setDescription(col3.getText());
        }
      });

      // Number of Dice
      int allowableDice[] = dieManager.getServer().getnDiceList();
      String diceData[] = new String[allowableDice.length];
      int defaultNDIdx = 0;
      for (int i = 0; i < diceData.length; i++) {
        diceData[i] = allowableDice[i] + "";
        if (nd == allowableDice[i])
          defaultNDIdx = i;
      }
      col4 = new JComboBox(diceData);
      col4.setSelectedIndex(defaultNDIdx);
      col4.setPreferredSize(new Dimension(COL4_WIDTH, ROW_HEIGHT));
      col4.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          JComboBox cb = (JComboBox) e.getSource();
          rolls[myRow].setNumDice(Integer.valueOf((String) cb.getSelectedItem()).intValue());
        }
      });

      // Number of Sides
      int[] allowableSides = dieManager.getServer().getnSideList();
      String sideData[] = new String[allowableSides.length];
      int defaultNSIdx = 0;
      for (int i = 0; i < sideData.length; i++) {
        sideData[i] = allowableSides[i] + "";
        if (ns == allowableSides[i])
          defaultNSIdx = i;
      }
      col5 = new JComboBox(sideData);
      col5.setSelectedIndex(defaultNSIdx);
      col5.setPreferredSize(new Dimension(COL5_WIDTH, ROW_HEIGHT));
      col5.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          JComboBox cb = (JComboBox) e.getSource();
          rolls[myRow].setNumSides(Integer.valueOf((String) cb.getSelectedItem()).intValue());
        }
      });

      // Add to Total
      col6 = new JTextField(2);
      col6.setBorder(myBorder);
      col6.setPreferredSize(new Dimension(COL6_WIDTH, ROW_HEIGHT));
      col6.setText(rolls[myRow].getPlus() + "");
      col6.addKeyListener(new java.awt.event.KeyAdapter() {
        public void keyReleased(java.awt.event.KeyEvent e) {
          try {
            rolls[myRow].setPlus(Integer.valueOf(col3.getText()).intValue());
          }
          catch (Exception ev) {
            rolls[myRow].setPlus(0);
          }
        }
      });
      // Report Total Only
      col7 = new JCheckBox();
      col7.setBorder(myBorder);
      col7.setPreferredSize(new Dimension(COL7_WIDTH, ROW_HEIGHT));
      col7.setHorizontalAlignment(JCheckBox.CENTER);
      col7.setSelected(rolls[myRow].isReportTotal());
      col7.addItemListener(new ItemListener() {
        public void itemStateChanged(ItemEvent e) {
          rolls[myRow].setReportTotal((e.getStateChange() == ItemEvent.SELECTED));
        }
      });

      add(col1);
      //add(col2);
      add(col3);
      add(col4);
      add(col5);
      add(col6);
      add(col7);

    }
  }

  /*
   * An on/off button that changes state to show it's status
   */
  class StateButton extends JButton {
    boolean state = false;

    StateButton(String s, boolean b) {
      super(s);
      setHorizontalAlignment(JButton.CENTER);
      setState(b);

    }

    StateButton(String s) {
      this(s, false);
    }

    public void setState(boolean b) {
      state = b;
      if (state) {
        setBorder(BorderFactory.createLoweredBevelBorder());
      }
      else {
        setBorder(BorderFactory.createRaisedBevelBorder());
      }
    }

    public boolean getState() {
      return state;
    }

    public void switchState() {
      setState(!state);
    }
  }

}