package VASSAL.tools;

import javax.swing.*;
import javax.swing.plaf.basic.BasicSplitPaneUI;
import java.awt.*;
import java.awt.event.*;
import java.util.Vector;
import java.util.Enumeration;
import java.util.Hashtable;

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
  private Insets insets;

  public ComponentSplitter(Insets insets) {
    this.insets = insets;
  }

  public ComponentSplitter() {
    this(new Insets(0, 0, Toolkit.getDefaultToolkit().getScreenSize().height, Toolkit.getDefaultToolkit().getScreenSize().width));
  }

  /** Create a new JSplitPane with <code>base</code> as the left component and <code>newComponent</code> as the right
   * Insert the JSplitPane into the parent in place of the original component */
  public SplitPane splitRight(Component base, Component newComponent, boolean resizeOnVisibilityChange) {
    return split(base, newComponent, SplitPane.HIDE_RIGHT, resizeOnVisibilityChange);
  }

  /** Create a new JSplitPane with <code>base</code> as the right component and <code>newComponent</code> as the left
   * Insert the JSplitPane into the parent in place of the original component */
  public SplitPane splitLeft(Component base, Component newComponent, boolean resizeOnVisibilityChange) {
    return split(base, newComponent, SplitPane.HIDE_LEFT, resizeOnVisibilityChange);
  }

  /** Create a new JSplitPane with <code>base</code> as the top component and <code>newComponent</code> as the botom
   * Insert the JSplitPane into the parent in place of the original component */
  public SplitPane splitBottom(Component base, Component newComponent, boolean resizeOnVisibilityChange) {
    return split(base, newComponent, SplitPane.HIDE_BOTTOM, resizeOnVisibilityChange);
  }

  /** Create a new JSplitPane with <code>base</code> as the bottom component and <code>newComponent</code> as the top
   * Insert the JSplitPane into the parent in place of the original component */
  public SplitPane splitTop(Component base, Component newComponent, boolean resizeOnVisibilityChange) {
    return split(base, newComponent, SplitPane.HIDE_TOP, resizeOnVisibilityChange);
  }

  public Component getSplitAncestor(Component c, int index) {
    Component next = SwingUtilities.getAncestorOfClass(SplitPane.class, c);
    int count = -1;
    while (next != null
        && (index < 0 || count++ < index)) {
      c = next;
      next = SwingUtilities.getAncestorOfClass(SplitPane.class, c);
    }
    return c;
  }

  private SplitPane split(Component base, final Component newComponent, int hideablePosition, boolean resize) {
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
    final SplitPane split = new SplitPane(newComponent, base, hideablePosition, resize);
    if (index >= 0) {
      parent.add(split, index);
    }
    return split;
  }

  /**
   * Extension of JSplitPane with utility methods for showing/hiding one of the components
   */
  public static class SplitPane extends JSplitPane {
    private boolean resizeOnVisibilityChange;
    private int hideablePosition;
    public static final int HIDE_TOP = 0;
    public static final int HIDE_BOTTOM = 1;
    public static final int HIDE_LEFT = 2;
    public static final int HIDE_RIGHT = 3;
    private Vector transverseShowing = new Vector();
    private Hashtable transverseSizes = new Hashtable();
    private int emptyLoc;
    private Dimension emptySize;

    public SplitPane(Component hideableComponent, Component baseComponent, int targetPosition, boolean resizeOnVisibilityChange) {
      super(HIDE_TOP == targetPosition || HIDE_BOTTOM == targetPosition ? VERTICAL_SPLIT : HORIZONTAL_SPLIT);
      this.resizeOnVisibilityChange = resizeOnVisibilityChange;
      this.hideablePosition = targetPosition;
      if (hideableComponent instanceof JComponent) {
        ((JComponent) hideableComponent).setMinimumSize(new Dimension(0, 0));
      }
      switch (targetPosition) {
        case HIDE_LEFT:
          setLeftComponent(hideableComponent);
          setRightComponent(baseComponent);
          break;
        case HIDE_RIGHT:
          setRightComponent(hideableComponent);
          setLeftComponent(baseComponent);
          break;
        case HIDE_TOP:
          setTopComponent(hideableComponent);
          setBottomComponent(baseComponent);
          break;
        case HIDE_BOTTOM:
          setBottomComponent(hideableComponent);
          setTopComponent(baseComponent);
      }
      setBorder(null);
      setResizeWeight(HIDE_LEFT == targetPosition || HIDE_TOP == targetPosition ? 0.0 : 1.0);
      hideComponent();
    }

    public void toggleVisibility() {
      if (getHideableComponent().isVisible()) {
        hideComponent();
      }
      else {
        showComponent();
      }
    }

    /**
     * @return the Component that can be shown/hidden
     */
    public JComponent getHideableComponent() {
      JComponent c = null;
      switch (hideablePosition) {
        case HIDE_LEFT:
          c = (JComponent) getLeftComponent();
          break;
        case HIDE_RIGHT:
          c = (JComponent) getRightComponent();
          break;
        case HIDE_TOP:
          c = (JComponent) getTopComponent();
          break;
        case HIDE_BOTTOM:
          c = (JComponent) getBottomComponent();
      }
      return c;
    }

    /**
     * @return the Component that remains always visible
     */
    public Component getBaseComponent() {
      Component c = null;
      switch (hideablePosition) {
        case HIDE_LEFT:
          c = getRightComponent();
          break;
        case HIDE_RIGHT:
          c = getLeftComponent();
          break;
        case HIDE_TOP:
          c = getBottomComponent();
          break;
        case HIDE_BOTTOM:
          c = getTopComponent();
      }
      return c;
    }


    public void hideComponent() {
      if (resizeOnVisibilityChange) {
        Container ancestor = getTopLevelAncestor();
        if (ancestor != null) {
          switch (hideablePosition) {
            case HIDE_LEFT:
            case HIDE_RIGHT:
              ancestor.setSize(new Dimension(ancestor.getSize().width - getHideableComponent().getSize().width, ancestor.getSize().height - getDividerSize()));
              break;
            case HIDE_TOP:
            case HIDE_BOTTOM:
              ancestor.setSize(new Dimension(ancestor.getSize().width, ancestor.getSize().height - getHideableComponent().getSize().height - getDividerSize()));
              break;
          }
          ancestor.validate();
        }
      }
      ((BasicSplitPaneUI) getUI()).getDivider().setVisible(false);
      getHideableComponent().setVisible(false);
      switch (hideablePosition) {
        case HIDE_LEFT:
        case HIDE_TOP:
          setDividerLocation(0.0);
          break;
        case HIDE_RIGHT:
        case HIDE_BOTTOM:
          setDividerLocation(1.0);
      }
      SplitPane split = getTransverseSplit();
      if (split != null) {
        split.hideTransverseComponent(this);
      }
    }

    protected void showTransverseComponent(SplitPane split) {
      if (getHideableComponent().isVisible()) {
        int currentLoc = getUI().getDividerLocation(this);
        if (transverseShowing.isEmpty()) {
          emptyLoc = currentLoc;
        }
        transverseShowing.addElement(split);
        switch (getOrientation()) {
          case VERTICAL_SPLIT:
            if (SwingUtilities.isDescendingFrom(split, getTopComponent())) {
              setDividerLocation(Math.max(currentLoc, split.getHideableComponent().getPreferredSize().height));
            }
            else {
              setDividerLocation(Math.min(currentLoc, getSize().height - split.getHideableComponent().getPreferredSize().height));
            }
            break;
          case HORIZONTAL_SPLIT:
            if (SwingUtilities.isDescendingFrom(split, getLeftComponent())) {
              setDividerLocation(Math.max(currentLoc, split.getHideableComponent().getPreferredSize().width));
            }
            else {
              setDividerLocation(Math.min(currentLoc, getSize().width - split.getHideableComponent().getPreferredSize().width));
            }
        }
      }
      else if (resizeOnVisibilityChange
          && getTopLevelAncestor() != null) {
        Dimension d = getTopLevelAncestor().getSize();
        if (transverseSizes.isEmpty()) {
          emptySize = new Dimension(d);
        }
        switch (getOrientation()) {
          case VERTICAL_SPLIT:
            d.height += Math.max(0, split.getHideableComponent().getPreferredSize().height - getBaseComponent().getSize().height);
            break;
          case HORIZONTAL_SPLIT:
            d.width += Math.max(0, split.getHideableComponent().getPreferredSize().width - getBaseComponent().getSize().width);
        }
        transverseSizes.put(split,d);
        getTopLevelAncestor().setSize(d);
        getTopLevelAncestor().validate();
      }
    }

    protected void hideTransverseComponent(SplitPane split) {
      if (getHideableComponent().isVisible()) {
        transverseShowing.removeElement(split);
        if (transverseShowing.isEmpty()) {
          setDividerLocation(emptyLoc);
        }
        else {
          int newPos = emptyLoc;
          for (Enumeration e = transverseShowing.elements(); e.hasMoreElements();) {
            SplitPane pane = (SplitPane) e.nextElement();
            switch (getOrientation()) {
              case VERTICAL_SPLIT:
                if (SwingUtilities.isDescendingFrom(split, getTopComponent())) {
                  newPos = Math.max(newPos, pane.getHideableComponent().getPreferredSize().height);
                }
                else {
                  newPos = Math.min(newPos, getSize().height - pane.getHideableComponent().getPreferredSize().height);
                }
                break;
              case HORIZONTAL_SPLIT:
                if (SwingUtilities.isDescendingFrom(split, getLeftComponent())) {
                  newPos = Math.max(newPos, pane.getHideableComponent().getPreferredSize().width);
                }
                else {
                  newPos = Math.min(newPos, getSize().width - pane.getHideableComponent().getPreferredSize().width);
                }
            }
          }
          setDividerLocation(newPos);
        }
      }
      else if (resizeOnVisibilityChange
        && getTopLevelAncestor() != null) {
        transverseSizes.remove(split);
        if (transverseSizes.isEmpty()) {
          getTopLevelAncestor().setSize(emptySize);
          getTopLevelAncestor().validate();
        }
        else {
          Dimension newSize = new Dimension(emptySize);
          for (Enumeration e = transverseSizes.elements(); e.hasMoreElements();) {
            Dimension d = (Dimension) e.nextElement();
            switch (getOrientation()) {
              case VERTICAL_SPLIT:
                  newSize.height = Math.max(newSize.height, d.height);
                break;
              case HORIZONTAL_SPLIT:
                  newSize.width = Math.max(newSize.width, d.width);
            }
          }
          getTopLevelAncestor().setSize(newSize);
          getTopLevelAncestor().validate();
        }
      }
    }

    public void showComponent() {
      if (resizeOnVisibilityChange) {
        Container ancestor = getTopLevelAncestor();
        if (ancestor != null) {
          int newLoc = 0;
          switch (getOrientation()) {
            case JSplitPane.HORIZONTAL_SPLIT:
              ancestor.setSize(Math.min(ancestor.getSize().width + getHideableComponent().getPreferredSize().width,
                                        Toolkit.getDefaultToolkit().getScreenSize().width - ancestor.getLocation().x), ancestor.getSize().height);
              newLoc = getBaseComponent().getSize().width;
              break;
            case JSplitPane.VERTICAL_SPLIT:
              ancestor.setSize(ancestor.getSize().width,
                               Math.min(ancestor.getSize().height + getHideableComponent().getPreferredSize().height,
                                        Toolkit.getDefaultToolkit().getScreenSize().height - ancestor.getLocation().y));
              newLoc = getBaseComponent().getSize().height;
              break;
          }
          ancestor.validate();
          getHideableComponent().setVisible(true);
          ((BasicSplitPaneUI) getUI()).getDivider().setVisible(true);
          setDividerLocation(newLoc);
        }
      }
      else {
        getHideableComponent().setVisible(true);
        ((BasicSplitPaneUI) getUI()).getDivider().setVisible(true);
        setDividerLocation(getPreferredVisibleDividerLocation());
        SplitPane split = getTransverseSplit();
        if (split != null) {
          split.showTransverseComponent(this);
        }
      }
    }

    public int getPreferredVisibleDividerLocation() {
      int loc = 0;
      switch (hideablePosition) {
        case HIDE_LEFT:
          loc = getInsets().left + getLeftComponent().getPreferredSize().width;
          break;
        case HIDE_RIGHT:
          loc = getSize().width - getInsets().right - getDividerSize() - getRightComponent().getPreferredSize().width;
          break;
        case HIDE_TOP:
          loc = getInsets().top + getLeftComponent().getPreferredSize().height;
          break;
        case HIDE_BOTTOM:
          loc = getSize().height - getInsets().bottom - getDividerSize() - getRightComponent().getPreferredSize().height;
      }
      return loc;
    }

    /**
     * Return the first JSplitPane ancestor with a different orientation from this JSplitPane
     * @return
     */
    public SplitPane getTransverseSplit() {
      SplitPane split = null;
      for (Component c = getParent(); c != null; c = c.getParent()) {
        if (c instanceof SplitPane) {
          SplitPane p = (SplitPane) c;
          if (p.getOrientation() != getOrientation()) {
            split = p;
            break;
          }
        }
      }
      return split;
    }

    public Dimension getPreferredSize() {
      Dimension d = null;
      if (getHideableComponent() == null
          || getHideableComponent().isVisible()) {
        d = super.getPreferredSize();
      }
      else {
        switch (hideablePosition) {
          case HIDE_LEFT:
            d = getRightComponent().getPreferredSize();
            break;
          case HIDE_RIGHT:
            d = getLeftComponent().getPreferredSize();
            break;
          case HIDE_TOP:
            d = getBottomComponent().getPreferredSize();
            break;
          case HIDE_BOTTOM:
            d = getTopComponent().getPreferredSize();
        }
      }
      return d;
    }
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
    final JLabel smallRight = new JLabel(new ImageIcon("smallRight.gif"));
    final JLabel large = new JLabel(new ImageIcon("large.jpg"));

    JPanel text = new JPanel();
    text.setLayout(new BoxLayout(text, BoxLayout.Y_AXIS));
    text.add(new JScrollPane(new JTextArea(15, 60)));
    JTextField input = new JTextField(60);
    input.setMaximumSize
        (new Dimension(input.getMaximumSize().width,
                       input.getPreferredSize().height));
    text.add(input);

    ComponentSplitter splitter = new ComponentSplitter();
    final SplitPane splitRight = splitter.splitRight(main, smallRight, false);
    final SplitPane splitLeft = splitter.splitLeft(main, smallLeft, false);
    final SplitPane splitBottom = splitter.splitBottom(splitter.getSplitAncestor(main, -1), new JScrollPane(large), true);
    splitBottom.setResizeWeight(0.0);

    main.add(text, BorderLayout.CENTER);

    toolbar.add(new AbstractAction("Left") {
      public void actionPerformed(ActionEvent e) {
        splitLeft.toggleVisibility();
      }
    });
    toolbar.add(new AbstractAction("Right") {
      public void actionPerformed(ActionEvent e) {
        splitRight.toggleVisibility();
      }
    });
    toolbar.add(new AbstractAction("Bottom") {
      public void actionPerformed(ActionEvent e) {
        splitBottom.toggleVisibility();
      }
    });
    f.pack();
    f.setSize(new Dimension(Math.min(f.getWidth(), splitter.insets.right), Math.min(f.getHeight(), splitter.insets.bottom)));
    f.setVisible(true);
  }
}
