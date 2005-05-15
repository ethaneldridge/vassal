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

import java.awt.Component;
import java.awt.Font;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.ListCellRenderer;

import VASSAL.configure.Configurer;
import VASSAL.tools.SequenceEncoder;

public class FontStyleConfigurer extends Configurer {

  protected JPanel p;
  protected JPanel fontPanel;
  protected JComboBox fonts;

  public FontStyleConfigurer(String key, String name) {
    super(key, name);
  }

  public FontStyleConfigurer(String key, String name, FontStyle fontStyle) {
    this(key, name);
    setValue(fontStyle);
  }

//  public FontStyleConfigurer(String key, String name, String fontName) {
//    this(key, name, GenericsContainer.getStyledFont(fontName));
//  }

  public String getValueString() {
    return (String) value;
  }

  public Font getValueFont() {
    return ((FontStyle) value).getFont();
  }

  public FontStyle getValueStyledFont() {
    return (FontStyle) value;
  }

  public java.awt.Component getControls() {
    if (p == null) {
      p = new JPanel();
      fontPanel = new JPanel();

      p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));

      Box box = Box.createHorizontalBox();
      box.add(new JLabel(name));
      buildFonts();

      box.add(fontPanel);
      p.add(box);

    }
    return p;
  }

  protected void buildFonts() {
    if (fontPanel == null) {
      return;
    }
    
    if (fonts != null) {
      fontPanel.remove(fonts);
    }

    fonts = new JComboBox();
    String[] s = GenericsContainer.getFontNames();
    for (int i = 0; i < s.length; ++i) {
      fonts.addItem(s[i]);
    }
    fonts.setSelectedItem(value == null ? "White" : ((FontStyle) value).getConfigureName());
    fontPanel.add(fonts);

    ItemListener l = new ItemListener() {
      public void itemStateChanged(ItemEvent evt) {
        updateValue();
      }
    };

    fonts.addItemListener(l);

    FontRenderer renderer = new FontRenderer();
    fonts.setRenderer(renderer);

  }

  protected void updateValue() {
    setValue(GenericsContainer.getStyledFont((String) fonts.getSelectedItem()));
  }

  public void setValue(String s) {
    setValue(decode(s));
    buildFonts();
  }

  public static FontStyle decode(String s) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, '|');
    return new FontStyle(sd.nextToken(""), FontConfigurer.decode(sd.nextToken("")));
  }

  public static String encode(FontStyle f) {
    SequenceEncoder se = new SequenceEncoder(f.getConfigureName(), '|');
    se.append(FontConfigurer.encode(f.getFont()));
    return se.getValue();
  }

  class FontRenderer extends JLabel implements ListCellRenderer {

    public FontRenderer() {
      setOpaque(true);
      setHorizontalAlignment(LEFT);
      setVerticalAlignment(CENTER);
    }

    /*
     * This method finds the image and text corresponding to the selected value
     * and returns the label, set up to display the text and image.
     */
    public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
        boolean cellHasFocus) {

      FontStyle fontStyle = GenericsContainer.getFontStyleByName((String) value);
      
      //ColorSwatch swatch = GenericsContainer.getColorSwatch((String) value);

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
//      BufferedImage bi = new BufferedImage(25, 12, BufferedImage.TYPE_INT_RGB);
//      Graphics g = bi.getGraphics();
//      g.setColor(swatch.getColor());
//      g.fillRect(0, 0, 25, 12);
//      g.setColor(Color.black);
//      g.drawRect(0, 0, 24, 11);
//      ImageIcon icon = new ImageIcon(bi);
      
      //setIcon(icon);
      setText((String) value);
      setFont(fontStyle.getFont());

      return this;
    }

  }

}
