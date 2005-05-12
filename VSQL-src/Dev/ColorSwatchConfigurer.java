/*
 * $Id$
 * 
 * Copyright (c) 2000-2003 by Rodney Kinney
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License (LGPL) as published by
 * the Free Software Foundation.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, copies are available at
 * http://www.opensource.org.
 */
package Dev;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.image.BufferedImage;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.ListCellRenderer;

import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.tools.SequenceEncoder;

public class ColorSwatchConfigurer extends Configurer {

  protected JPanel p;
  protected JPanel swatchPanel;
  protected JComboBox swatches;

  public ColorSwatchConfigurer(String key, String name) {
    super(key, name);
  }

  public ColorSwatchConfigurer(String key, String name, ColorSwatch swatch) {
    this(key, name);
    setValue(swatch);
  }

  public ColorSwatchConfigurer(String key, String name, String swatchName) {
    this(key, name, GenericsContainer.getColorSwatch(swatchName));
  }

  public String getValueString() {
    return (String) value;
  }

  public Color getValueColor() {
    return ((ColorSwatch) value).getColor();
  }

  public ColorSwatch getValueColorSwatch() {
    return (ColorSwatch) value;
  }

  public java.awt.Component getControls() {
    if (p == null) {
      p = new JPanel();
      swatchPanel = new JPanel();

      p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));

      Box box = Box.createHorizontalBox();
      box.add(new JLabel(name));
      buildSwatches();

      box.add(swatchPanel);
      p.add(box);

    }
    return p;
  }

  protected void buildSwatches() {
    if (swatchPanel == null) {
      return;
    }
    
    if (swatches != null) {
      swatchPanel.remove(swatches);
    }

    swatches = new JComboBox();
    String[] s = GenericsContainer.getColorNames();
    for (int i = 0; i < s.length; ++i) {
      swatches.addItem(s[i]);
    }
    swatches.setSelectedItem(value == null ? "White" : ((ColorSwatch) value).getConfigureName());
    swatchPanel.add(swatches);

    ItemListener l = new ItemListener() {
      public void itemStateChanged(ItemEvent evt) {
        updateValue();
      }
    };

    swatches.addItemListener(l);

    ComboBoxRenderer renderer = new ComboBoxRenderer();
    swatches.setRenderer(renderer);

  }

  protected void updateValue() {
    setValue(GenericsContainer.getColorSwatch((String) swatches.getSelectedItem()));
  }

  public void setValue(String s) {
    setValue(decode(s));
    buildSwatches();
  }

  public static ColorSwatch decode(String s) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, '|');
    return new ColorSwatch(sd.nextToken(), sd.nextColor(Color.WHITE));
  }

  public static String encode(ColorSwatch f) {
    SequenceEncoder se = new SequenceEncoder(f.getConfigureName(), '|');
    se.append(ColorConfigurer.colorToString(f.getColor()));
    return se.getValue();
  }

  class ComboBoxRenderer extends JLabel implements ListCellRenderer {

    public ComboBoxRenderer() {
      setOpaque(true);
      setHorizontalAlignment(CENTER);
      setVerticalAlignment(CENTER);
    }

    /*
     * This method finds the image and text corresponding to the selected value
     * and returns the label, set up to display the text and image.
     */
    public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
        boolean cellHasFocus) {

      ColorSwatch swatch = GenericsContainer.getColorSwatch((String) value);

      if (isSelected) {
        setBackground(list.getSelectionBackground());
        setForeground(list.getSelectionForeground());
      }
      else {
        setBackground(list.getBackground());
        setForeground(list.getForeground());
      }

      //Set the icon and text. If icon was null, say so.
      //String name = (String) list.get
      BufferedImage bi = new BufferedImage(20, 10, BufferedImage.TYPE_INT_RGB);
      Graphics g = bi.getGraphics();
      g.setColor(swatch.getColor());
      g.fillRect(0, 0, 20, 10);
      ImageIcon icon = new ImageIcon(bi);
      
      setIcon(icon);
      setText((String) value);
      setFont(list.getFont());

      return this;
    }

  }

}
