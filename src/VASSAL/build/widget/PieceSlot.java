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
package VASSAL.build.widget;

import VASSAL.build.*;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.documentation.HelpWindowExtension;
import VASSAL.build.module.documentation.HelpWindow;
import VASSAL.build.module.map.MenuDisplayer;
import VASSAL.command.AddPiece;
import VASSAL.command.Command;
import VASSAL.configure.Configurer;
import VASSAL.counters.*;
import VASSAL.Info;

import javax.swing.*;
import java.awt.*;
import java.awt.datatransfer.StringSelection;
import java.awt.dnd.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.net.MalformedURLException;

/**
 * A Component that displays a GamePiece.
 *
 * Can be added to any Widget but cannot contain any children
 * Keyboard input on a PieceSlot is forwarded to the {@link GamePiece#keyEvent}
 * method for the PieceSlot's GamePiece.  Clicking on a PieceSlot
 * initiates a drag
 */
public class PieceSlot extends Widget implements MouseListener, KeyListener {
  private GamePiece c;
  private String name;
  private String pieceDefinition;
  protected static Font FONT = new Font("Dialog", 0, 12);
  private javax.swing.JPanel panel;
  private int width, height;

  public PieceSlot() {
    panel = new PieceSlot.Panel();
    panel.addMouseListener(this);
    panel.addKeyListener(this);
  }

  private class Panel extends javax.swing.JPanel {
    public void paint(Graphics g) {
      PieceSlot.this.paint(g);

    }

    public Dimension getPreferredSize() {
      return PieceSlot.this.getPreferredSize();
    }

    public boolean isManagingFocus() {
      return true;
    }
  }

  public PieceSlot(GamePiece p) {
    this();
    setPiece(p);
  }

  public void setPiece(GamePiece p) {
    c = p;
    if (c != null) {
      c.setPosition(new Point(panel.getSize().width / 2, panel.getSize().height / 2));
      name = Decorator.getInnermost(c).getName();
    }
    panel.revalidate();
    panel.repaint();
    pieceDefinition = c == null ? null
        : GameModule.getGameModule().encode(new AddPiece(c));

  }

  public GamePiece getPiece() {
    if (c == null
        && pieceDefinition != null) {
      AddPiece comm = (AddPiece) GameModule.getGameModule().decode(pieceDefinition);
      if (comm == null) {
        System.err.println("Couldn't build piece " + pieceDefinition);
        pieceDefinition = null;
      }
      else {
        c = comm.getTarget();
        c.setState(comm.getState());
        c.setPosition(new Point(panel.getSize().width / 2, panel.getSize().height / 2));
      }
    }
    return c;
  }

  public void paint(Graphics g) {
    g.clearRect(0, 0, panel.getSize().width, panel.getSize().height);
    if (getPiece() == null) {
      FontMetrics fm = g.getFontMetrics();
      g.drawRect(0, 0, panel.getSize().width - 1, panel.getSize().height - 1);
      g.setFont(FONT);
      g.drawString(" nil ", panel.getSize().width / 2
                            - fm.stringWidth(" nil ") / 2, panel.getSize().height / 2);
    }
    else {
      getPiece().draw(g, panel.getSize().width / 2, panel.getSize().height / 2, panel, 1.0);
      if (Boolean.TRUE.equals(getPiece().getProperty(Properties.SELECTED))) {
        BasicPiece.getHighlighter().draw
            (getPiece(), g, panel.getSize().width / 2, panel.getSize().height / 2, panel, 1.0);
      }
    }
  }

  public Dimension getPreferredSize() {
    if (c != null && panel.getGraphics() != null) {
      c.draw(panel.getGraphics(), 0, 0, panel, 1.0);
      return c.boundingBox().getSize();
    }
    else {
      return new Dimension(width, height);
    }
  }

  public void mousePressed(MouseEvent e) {
    KeyBuffer.getBuffer().clear();
    if (getPiece() != null) {
      KeyBuffer.getBuffer().add(getPiece());
    }
    if (!Info.isDndEnabled()) {
      startDrag();
      panel.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
    }
  }

  protected void startDrag() {
    if (getPiece() != null) {
      DragBuffer.getBuffer().clear();
      GamePiece newPiece
          = ((AddPiece) GameModule.getGameModule().decode
          (GameModule.getGameModule().encode
           (new AddPiece(getPiece())))).getTarget();
      newPiece.setState(getPiece().getState());
      DragBuffer.getBuffer().add(newPiece);
    }
  }

  public void mouseReleased(MouseEvent e) {
    if (getPiece() != null
        && e.isMetaDown()) {
      JPopupMenu popup = MenuDisplayer.createPopup(getPiece());
      popup.addPopupMenuListener(new javax.swing.event.PopupMenuListener() {
        public void popupMenuCanceled
            (javax.swing.event.PopupMenuEvent evt) {
          panel.repaint();
        }

        public void popupMenuWillBecomeInvisible
            (javax.swing.event.PopupMenuEvent evt) {
          panel.repaint();
        }

        public void popupMenuWillBecomeVisible
            (javax.swing.event.PopupMenuEvent evt) {
        }
      });
      popup.show(panel, e.getX(), e.getY());
    }
    panel.requestFocus();
    panel.repaint();
  }

  public void mouseClicked(MouseEvent e) {
  }

  public void mouseEntered(MouseEvent e) {
  }

  public void mouseExited(MouseEvent e) {
    KeyBuffer.getBuffer().remove(getPiece());
    panel.repaint();
  }

  public void keyPressed(KeyEvent e) {
    KeyBuffer.getBuffer().keyCommand(javax.swing.KeyStroke.getKeyStrokeForEvent(e));
    e.consume();
    panel.repaint();
  }

  public void keyTyped(KeyEvent e) {
    KeyBuffer.getBuffer().keyCommand(javax.swing.KeyStroke.getKeyStrokeForEvent(e));
    e.consume();
    panel.repaint();
  }

  public void keyReleased(KeyEvent e) {
    KeyBuffer.getBuffer().keyCommand(javax.swing.KeyStroke.getKeyStrokeForEvent(e));
    e.consume();
    panel.repaint();
  }

  public static String getConfigureTypeName() {
    return "Single piece";
  }

  public java.awt.Component getComponent() {
    return panel;
  }

  /**
   * When building a PieceSlot, the text contents of the XML element
   * are parsed into a String.  The String is decoded using
   * {@link GameModule#decode}.  The resulting {@link Command}
   * should be an instance of {@link AddPiece}.  The piece referred to
   * in the Command becomes the piece contained in the PieceSlot
   */
  public void build(org.w3c.dom.Element e) {
    if (e != null) {
      name = e.getAttribute("entryName");
      if (name.length() == 0) {
        name = null;
      }
      try {
        width = Integer.parseInt(e.getAttribute("width"));
        height = Integer.parseInt(e.getAttribute("height"));
      }
      catch (NumberFormatException ex) {
        width = 60;
        height = 60;
      }
      pieceDefinition = Builder.getText(e);
    }
  }

  public void addTo(Buildable parent) {
    if (Info.isDndEnabled()) {
      DragGestureListener l = new DragGestureListener() {
        public void dragGestureRecognized(DragGestureEvent dge) {
          startDrag();
          dge.startDrag(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR), new StringSelection(""), new DragSourceListener() {
            public void dragEnter(DragSourceDragEvent dsde) {
            }

            public void dragOver(DragSourceDragEvent dsde) {
            }

            public void dropActionChanged(DragSourceDragEvent dsde) {
            }

            public void dragExit(DragSourceEvent dse) {
            }

            public void dragDropEnd(DragSourceDropEvent dsde) {
            }
          });
        }
      };
      DragSource.getDefaultDragSource().createDefaultDragGestureRecognizer(panel, DnDConstants.ACTION_MOVE, l);
    }
    else {
      DragBuffer.getBuffer().addDragSource(getComponent());
    }
  }

  public org.w3c.dom.Element getBuildElement(org.w3c.dom.Document doc) {
    org.w3c.dom.Element el = doc.createElement(getClass().getName());
    String s = getConfigureName();
    if (s != null) {
      el.setAttribute("entryName", s);
    }
    el.setAttribute("width", getPreferredSize().width + "");
    el.setAttribute("height", getPreferredSize().height + "");
    el.appendChild(doc.createTextNode(c == null ? pieceDefinition : GameModule.getGameModule().encode(new AddPiece(c))));
    return el;
  }

  public void removeFrom(Buildable parent) {
  }

  public String getConfigureName() {
    if (name != null) {
      return name;
    }
    else if (getPiece() != null) {
      return Decorator.getInnermost(getPiece()).getName();
    }
    else {
      return null;
    }
  }

  public HelpFile getHelpFile() {
    File dir = new File("docs");
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "GamePiece.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public String[] getAttributeNames() {
    return null;
  }

  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  public Class[] getAttributeTypes() {
    return new Class[0];
  }

  public void setAttribute(String name, Object value) {
  }

  public String getAttributeValueString(String name) {
    return null;
  }

  /** @return an array of Configurer objects representing
   * the attributes of this Configurable object
   */
  public Configurer[] getAttributeConfigurers() {
    return new Configurer[0];
  }

  /** @return an array of Configurer objects representing
   * the Buildable children of this Configurable object
   */
  public Configurable[] getConfigureComponents() {
    return new Configurable[0];
  }

  /** @return an array of Configurer objects representing
   * all possible classes of Buildable children of this Configurable object
   */
  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public Configurer getConfigurer() {
    return new MyConfigurer(this);
  }

  private static class MyConfigurer extends Configurer implements HelpWindowExtension {
    private PieceDefiner definer;

    public MyConfigurer(PieceSlot slot) {
      super(null, slot.getConfigureName(), slot);
      definer = new PieceDefiner();
      definer.setPiece(slot.getPiece());
    }

    public void setBaseWindow(HelpWindow w) {
      definer.setBaseWindow(w);
    }

    public String getValueString() {
      return null;
    }

    public void setValue(String s) {
      throw new RuntimeException("Cannot set from String");
    }

    public Object getValue() {
      PieceSlot slot = (PieceSlot) super.getValue();
      if (slot != null) {
        slot.setPiece(definer.getPiece());
      }
      return slot;
    }

    public java.awt.Component getControls() {
      return definer;
    }
  }
}

