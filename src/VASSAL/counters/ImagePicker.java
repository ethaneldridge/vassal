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
package VASSAL.counters;

import VASSAL.build.GameModule;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

public class ImagePicker extends javax.swing.JPanel implements MouseListener, ItemListener {
  private String imageName = " ";
  protected static Font FONT = new Font("Dialog", 0, 11);
  private JTextArea noImage;
  private JComboBox select;
  private ImageIcon icon;
  private JLabel imageView;

  public ImagePicker() {
    noImage = new JTextArea(1,10);
    noImage.setFont(FONT);
    noImage.setText("double-click to add new image");
    noImage.addMouseListener(this);
    noImage.setEditable(false);
    noImage.setLineWrap(true);
    noImage.setWrapStyleWord(true);
    icon = new ImageIcon();
    imageView = new JLabel(icon);
    imageView.addMouseListener(this);
    select = new JComboBox(GameModule.getGameModule().getDataArchive().getImageNames());
    select.addItemListener(this);
    setLayout(new BoxLayout(this,BoxLayout.Y_AXIS));
    add(noImage);
    add(select);
  }

  public String getImageName() {
    return imageName;
  }

  public void setImageName(String name) {
    imageName = setSuffix(name);
    remove(0);
    if (name == null) {
      add(noImage,0);
    }
    else {
      try {
        icon.setImage(getImage());
        name = imageName;
        add(imageView,0);
      }
      catch (java.io.IOException e) {
        name = null;
        add(noImage,0);
      }
    }
    select.removeItemListener(this);
    select.setSelectedItem(name == null ? null : name+".gif");
    select.addItemListener(this);
    revalidate();
    Window w = (Window) SwingUtilities.getAncestorOfClass(Window.class,this);
    if (w != null) {
      w.pack();
    }
    repaint();
  }

  private String setSuffix(String name) {
    String s = name;
    if (s != null
      && s.indexOf(".gif") > 0) {
      s = s.substring(0, s.indexOf(".gif"));
    }
    return s;
  }

  private Image getImage() throws java.io.IOException {
    return imageName == null ? null
      : GameModule.getGameModule()
      .getDataArchive().getCachedImage(imageName + ".gif");
  }

  /*
  public void paint(Graphics g) {
    try {
      g.clearRect(0, 0, getWidth(), getHeight());
      g.drawImage(getImage(), 0, 0, this);
    }
    catch (java.io.IOException ex) {
      noImage.paint(g);
    }
  }

  public Dimension getPreferredSize() {
    try {
      Image im = getImage();
      if (im == null) {
        return noImage.getSize();
      }
      if (im.getWidth(this) < 0
        || im.getHeight(this) < 0) {
        throw new java.io.IOException("Image not yet loaded");
      }
      return new Dimension(im.getWidth(this),
                           im.getHeight(this));
    }
    catch (java.io.IOException ex) {
      //	  return new Dimension(60,60);
      return noImage.getSize();
    }
  }
   */

  public void mouseEntered(MouseEvent e) {
  }

  public void mouseExited(MouseEvent e) {
  }

  public void mouseClicked(MouseEvent e) {
  }

  public void mousePressed(MouseEvent e) {
  }

  public void mouseReleased(MouseEvent e) {
    if (e.getClickCount() > 1) {
      pickImage();
    }
  }

  public void itemStateChanged(ItemEvent e) {
    setImageName((String) select.getSelectedItem());
  }

  public void pickImage() {
    javax.swing.JFileChooser fc = GameModule.getGameModule().getFileChooser();
    if (fc.showOpenDialog(null)
      == javax.swing.JFileChooser.APPROVE_OPTION
      && fc.getSelectedFile().exists()) {
      String name = fc.getName(fc.getSelectedFile());
      GameModule.getGameModule().getArchiveWriter()
        .addImage(fc.getSelectedFile().getPath(), setSuffix(name) + ".gif");
      select.setModel(new DefaultComboBoxModel(GameModule.getGameModule().getDataArchive().getImageNames()));
      setImageName(name);
    }
    else {
      setImageName(" ");
    }
    repaint();
  }
}
