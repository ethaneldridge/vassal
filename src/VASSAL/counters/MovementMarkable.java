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
/*
 * Created by IntelliJ IDEA.
 * User: unknown
 * Date: Dec 30, 2002
 * Time: 12:42:01 PM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.counters;

import VASSAL.build.GameModule;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.ChangeTracker;
import VASSAL.configure.Configurer;
import VASSAL.configure.ImageConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.tools.SequenceEncoder;

import javax.swing.*;
import java.awt.*;
import java.io.File;
import java.net.MalformedURLException;
import java.util.Enumeration;

/**
 * A GamePiece with this trait will automatically be marked whenever it is moved.  A marked piece is
 * indicated by drawing a specified image at a specified location
 */
public class MovementMarkable extends Decorator implements EditablePiece {
  public static final String ID = "markmoved;";

  private static final KeyStroke markStroke = KeyStroke.getKeyStroke('M', java.awt.event.InputEvent.CTRL_MASK);
  private String markImage;
  private int xOffset = 0;
  private int yOffset = 0;
  private Dimension imageSize;
  private boolean hasMoved = false;

  public MovementMarkable() {
    this(ID + "moved;0;0", null);
  }

  public MovementMarkable(String type, GamePiece p) {
    mySetType(type);
    setInner(p);
  }

  public boolean isMoved() {
    return hasMoved;
  }

  public void setMoved(boolean b) {
    hasMoved = b;
  }

  public void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    markImage = st.nextToken();
    xOffset = Integer.parseInt(st.nextToken());
    yOffset = Integer.parseInt(st.nextToken());
  }

  public void mySetState(String newState) {
    hasMoved = "true".equals(newState);
  }

  public String myGetState() {
    return "" + hasMoved;
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(markImage).append("" + xOffset).append("" + yOffset);
    return ID + se.getValue();
  }

  protected KeyCommand[] myGetKeyCommands() {
    return new KeyCommand[]{new KeyCommand("Mark Moved", markStroke, Decorator.getOutermost(this))};
  }

  public Command myKeyEvent(javax.swing.KeyStroke stroke) {
    if (stroke.equals(markStroke)) {
      ChangeTracker c = new ChangeTracker(this);
      hasMoved = !hasMoved;
      return c.getChangeCommand();
    }
    else {
      return null;
    }
  }

  public Rectangle selectionBounds() {
    return getInner().selectionBounds();
  }

  public Rectangle boundingBox() {
    Rectangle r = getInner().boundingBox();
    Rectangle r2 = getInner().selectionBounds();
    if (imageSize != null) {
      Rectangle r3 = new Rectangle(getPosition().x + xOffset, getPosition().y + yOffset, imageSize.width, imageSize.height);
      r2 = r2.union(r3);
    }
    return r.union(r2);
  }

  public String getName() {
    return getInner().getName();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    getInner().draw(g, x, y, obs, zoom);
    if (hasMoved) {
      try {
        Image im =
          GameModule.getGameModule().getDataArchive().getCachedImage(markImage);
        g.drawImage(im,
                    x + (int) Math.round(zoom * xOffset),
                    y + (int) Math.round(zoom * yOffset),
                    (int) Math.round(zoom * im.getWidth(obs)),
                    (int) Math.round(zoom * im.getHeight(obs)),
                    obs);
        if (imageSize == null) {
          JLabel l = new JLabel();
          l.setIcon(new ImageIcon(im));
          imageSize = l.getPreferredSize();
        }
      }
      catch (java.io.IOException ex) {
        ex.printStackTrace();
      }
    }
  }

  public String getDescription() {
    return "Can be marked moved";
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    File dir = new File("docs");
    dir = new File(dir,"ReferenceManual");
    try {
      return new HelpFile(null,new File(dir,"MarkMoved.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public Object getProperty(Object key) {
    if (Properties.MOVED.equals(key)) {
      return new Boolean(isMoved());
    }
    else {
      return super.getProperty(key);
    }
  }

  public void setProperty(Object key, Object val) {
    if (Properties.MOVED.equals(key)) {
      setMoved(Boolean.TRUE.equals(val));
    }
    else {
      super.setProperty(key, val);
    }
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  private static class Ed implements PieceEditor {
    private ImageConfigurer image;
    private IntConfigurer xOff;
    private IntConfigurer yOff;
    private Box box;

    private Ed(MovementMarkable p) {
      box = Box.createVerticalBox();
      image = new ImageConfigurer(null, "Marker Image:  ", GameModule.getGameModule().getArchiveWriter());
      ((Configurer) image).setValue(p.markImage);
      xOff = new IntConfigurer(null, "Horizontal Offset:  ", new Integer(p.xOffset));
      yOff = new IntConfigurer(null, "Vertical Offset:  ", new Integer(p.yOffset));
      box.add(image.getControls());
      box.add(xOff.getControls());
      box.add(yOff.getControls());
    }

    public Component getControls() {
      boolean enabled = false;
      for (Enumeration e = GameModule.getGameModule().getComponents(Map.class); e.hasMoreElements();) {
        Map m = (Map) e.nextElement();
        String value = m.getAttributeValueString(Map.MARK_MOVED);
        enabled = enabled
          || GlobalOptions.ALWAYS.equals(value)
          || GlobalOptions.PROMPT.equals(value);
      }
      if (!enabled) {
        Runnable runnable = new Runnable() {
          public void run() {
            JOptionPane.showMessageDialog(box, "You must enable the \"Mark Pieces that Move\" option in one or more Map Windows", "Option not enabled", JOptionPane.WARNING_MESSAGE);
          }
        };
        SwingUtilities.invokeLater(runnable);
      }
      return box;
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(image.getValueString()).append(xOff.getValueString()).append(yOff.getValueString());
      return ID + se.getValue();
    }

    public String getState() {
      return "false";
    }
  }
}
