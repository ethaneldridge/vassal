package VASSAL.tools;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

/*
 * $Id$
 *
 * Copyright (c) 2003 by Rodney Kinney
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

/**
 * A utility class for adding nested JSplitPane's
 */
public class ComponentSplitter {

  /** Create a new JSplitPane with <code>base</code> as the left component and <code>newComponent</code> as the right
   * Insert the JSplitPane into the parent in place of the original component */
  public JSplitPane splitRight(Component base, Component newComponent) {
    return split(base, newComponent, JSplitPane.HORIZONTAL_SPLIT, JSplitPane.LEFT, JSplitPane.RIGHT);
  }

  /** Create a new JSplitPane with <code>base</code> as the right component and <code>newComponent</code> as the left
   * Insert the JSplitPane into the parent in place of the original component */
  public JSplitPane splitLeft(Component base, Component newComponent) {
    return split(base, newComponent, JSplitPane.HORIZONTAL_SPLIT, JSplitPane.RIGHT, JSplitPane.LEFT);
  }

  /** Create a new JSplitPane with <code>base</code> as the top component and <code>newComponent</code> as the botom
   * Insert the JSplitPane into the parent in place of the original component */
  public JSplitPane splitBottom(Component base, Component newComponent) {
    return split(base, newComponent, JSplitPane.VERTICAL_SPLIT, JSplitPane.TOP, JSplitPane.BOTTOM);
  }

  /** Create a new JSplitPane with <code>base</code> as the bottom component and <code>newComponent</code> as the top
   * Insert the JSplitPane into the parent in place of the original component */
  public JSplitPane splitTop(Component base, Component newComponent) {
    return split(base, newComponent, JSplitPane.VERTICAL_SPLIT, JSplitPane.BOTTOM, JSplitPane.TOP);
  }

  public void toggleVisibility(Component c, boolean resizeFrame) {
    if (isVisible(c)) {
      hide(c,resizeFrame);
    }
    else {
      show(c, resizeFrame);
    }
  }

  public boolean isVisible(Component c) {
    JSplitPane split = (JSplitPane) SwingUtilities.getAncestorOfClass(JSplitPane.class, c);
    boolean visible = true;
    if (split != null) {
      int dividerLocation = split.getUI().getDividerLocation(split);
      if (SwingUtilities.isDescendingFrom(c, split.getLeftComponent())) {
        visible = dividerLocation > split.getInsets().top + 10;
      }
      else {
        switch (split.getOrientation()) {
          case JSplitPane.HORIZONTAL_SPLIT:
            visible = dividerLocation < split.getSize().width - split.getInsets().right - split.getDividerSize() - 10;
            break;
          case JSplitPane.VERTICAL_SPLIT:
            visible = dividerLocation < split.getSize().height - split.getInsets().bottom - split.getDividerSize() - 10;
        }
      }
    }
    return visible;
  }

  public void show(Component c, boolean resizeFrame) {
    if (resizeFrame) {
      Window w = SwingUtilities.getWindowAncestor(c);
      JSplitPane split = (JSplitPane) SwingUtilities.getAncestorOfClass(JSplitPane.class, c);
      if (w != null
        && split != null) {
        switch (split.getOrientation()) {
          case JSplitPane.HORIZONTAL_SPLIT:
            w.setSize(Math.min(w.getSize().width + c.getPreferredSize().width,
                                Toolkit.getDefaultToolkit().getScreenSize().width-w.getLocation().x), w.getSize().height);
            break;
          case JSplitPane.VERTICAL_SPLIT:
            w.setSize(w.getSize().width,
                      Math.min(w.getSize().height + c.getPreferredSize().height,
                               Toolkit.getDefaultToolkit().getScreenSize().height-w.getLocation().y));
            break;
        }
        int currentLoc = split.getUI().getDividerLocation(split);
        w.validate();
        split.setDividerLocation(currentLoc);
      }
    }

    JSplitPane split = (JSplitPane) SwingUtilities.getAncestorOfClass(JSplitPane.class, c);
    if (split != null) {
      int currentLoc = split.getUI().getDividerLocation(split);
      int newLoc = getPreferredVisibleDividerLocation(c, split);
      if (newLoc != currentLoc) {
        split.setDividerLocation(newLoc);
        split.setLastDividerLocation(currentLoc);
      }
    }
  }

  public int getPreferredVisibleDividerLocation(Component c, JSplitPane split) {
    int loc = 0;
    if (SwingUtilities.isDescendingFrom(c, split.getLeftComponent())) {
      switch (split.getOrientation()) {
        case JSplitPane.HORIZONTAL_SPLIT:
          loc = split.getInsets().left + c.getPreferredSize().width;
          break;
        case JSplitPane.VERTICAL_SPLIT:
          loc = split.getInsets().top + c.getPreferredSize().height;
      }
    }
    else {
      switch (split.getOrientation()) {
        case JSplitPane.HORIZONTAL_SPLIT:
          loc = split.getSize().width - split.getInsets().right - split.getDividerSize() - c.getPreferredSize().width;
          break;
        case JSplitPane.VERTICAL_SPLIT:
          loc = split.getSize().height - split.getInsets().bottom - split.getDividerSize() - c.getPreferredSize().height;
      }
    }
    return loc;
  }

  public void hide(Component c, boolean resizeFrame) {
    if (resizeFrame) {
      Window w = SwingUtilities.getWindowAncestor(c);
      JSplitPane split = (JSplitPane) SwingUtilities.getAncestorOfClass(JSplitPane.class, c);
      if (w != null
        && split != null) {
        switch (split.getOrientation()) {
          case JSplitPane.HORIZONTAL_SPLIT:
          int delta = SwingUtilities.isDescendingFrom(c,split.getLeftComponent()) ?
            ((JComponent)split.getLeftComponent()).getVisibleRect().width :
            ((JComponent)split.getRightComponent()).getVisibleRect().width;
            w.setSize(w.getSize().width - delta, w.getSize().height);
            break;
          case JSplitPane.VERTICAL_SPLIT:
            delta = SwingUtilities.isDescendingFrom(c,split.getLeftComponent()) ?
              ((JComponent)split.getLeftComponent()).getVisibleRect().height :
              ((JComponent)split.getRightComponent()).getVisibleRect().height;
            w.setSize(w.getSize().width, w.getSize().height - delta);
            break;
        }
      }
      w.validate();
    }
    JSplitPane split = (JSplitPane) SwingUtilities.getAncestorOfClass(JSplitPane.class, c);
    if (split != null) {
      if (SwingUtilities.isDescendingFrom(c, split.getLeftComponent())) {
        split.setDividerLocation(0.0);
      }
      else {
        split.setDividerLocation(1.0);
      }
    }
  }

  public Component getSplitAncestor(Component c, int index) {
    Component next = SwingUtilities.getAncestorOfClass(JSplitPane.class, c);
    int count = -1;
    while (next != null
      && (index < 0 || count++ < index)) {
      c = next;
      next = SwingUtilities.getAncestorOfClass(JSplitPane.class, c);
    }
    return c;
  }

  private JSplitPane split(Component base, final Component newComponent, int orientation, String basePosition, String newPosition) {
    int index = -1;
    Container parent = base.getParent();
    if (base.getParent() != null) {
      for (int i = 0,n = base.getParent().getComponentCount(); i < n; ++i) {
        if (base == base.getParent().getComponent(i)) {
          index = i;
          break;
        }
      }
    }
    if (newComponent instanceof JComponent) {
      ((JComponent) newComponent).setMinimumSize(new Dimension(0, 0));
    }
    final JSplitPane split = new JSplitPane(orientation);
    split.setBorder(null);
    split.setOneTouchExpandable(true);
    split.add(base, basePosition);
    split.add(newComponent, newPosition);
    split.addComponentListener(new ComponentAdapter() {
      public void componentResized(ComponentEvent e) {
        hide(newComponent,false);
        split.setLastDividerLocation(getPreferredVisibleDividerLocation(newComponent, split));
        split.removeComponentListener(this);
      }
    });
    split.setResizeWeight(JSplitPane.TOP.equals(newPosition) || JSplitPane.LEFT.equals(newPosition) ? 0.0 : 1.0);
    if (index >= 0) {
      parent.add(split, index);
    }
    return split;
  }

  public static void main(String[] args) {
    JFrame f = new JFrame();
    f.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
    f.addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
        System.exit(0);
      }
    });

    JTextField status = new JTextField("status");
    status.setEditable(false);
    f.getContentPane().setLayout(new BorderLayout());
    Box box = Box.createVerticalBox();
    box.add(status);
    JPanel main = new JPanel(new BorderLayout());
    f.getContentPane().add(main, BorderLayout.CENTER);
    JToolBar toolbar = new JToolBar();
    toolbar.setFloatable(false);
    toolbar.setAlignmentX(0.0F);
    box.add(toolbar);

    f.getContentPane().add(box, BorderLayout.NORTH);

    final JLabel smallLeft = new JLabel(new ImageIcon("small.gif"));
    final JLabel smallRight = new JLabel(new ImageIcon("small.gif"));
    final JLabel large = new JLabel(new ImageIcon("large.jpg"));

    JPanel text = new JPanel();
    text.setLayout(new BoxLayout(text, BoxLayout.Y_AXIS));
    text.add(new JScrollPane(new JTextArea(15, 60)));
    JTextField input = new JTextField(60);
    input.setMaximumSize
      (new Dimension(input.getMaximumSize().width,
                     input.getPreferredSize().height));
    text.add(input);

    final ComponentSplitter splitter = new ComponentSplitter();
    splitter.splitRight(main, smallRight);
    splitter.splitLeft(main, smallLeft);
    splitter.splitBottom(splitter.getSplitAncestor(main, -1), new JScrollPane(large)).setResizeWeight(0.0);

    main.add(text, BorderLayout.CENTER);

    toolbar.add(new AbstractAction("Left") {
      public void actionPerformed(ActionEvent e) {
        splitter.toggleVisibility(smallLeft, false);
      }
    });
    toolbar.add(new AbstractAction("Right") {
      public void actionPerformed(ActionEvent e) {
        splitter.toggleVisibility(smallRight, false);
      }
    });
    toolbar.add(new AbstractAction("Bottom") {
      public void actionPerformed(ActionEvent e) {
        splitter.toggleVisibility(large, true);
      }
    });
    Dimension d = large.getPreferredSize();
    large.setPreferredSize(new Dimension(0,0));
    f.pack();
    large.setPreferredSize(d);
    f.setVisible(true);
  }
}
