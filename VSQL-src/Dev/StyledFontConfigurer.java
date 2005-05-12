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
package Dev;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.tools.SequenceEncoder;

/**
 * A Configurer for {@link Font}values
 */
public class StyledFontConfigurer extends Configurer {

  protected JPanel p;
  protected IntConfigurer size;
  protected BooleanConfigurer bold;
  protected BooleanConfigurer italic;
  protected ColorSwatchConfigurer fgColor;
  protected ColorSwatchConfigurer bgColor;
  protected JComboBox family;
  protected JTextField demo;

  public StyledFontConfigurer(String key, String name) {
    super(key, name);
  }

  public StyledFontConfigurer(String key, String name, StyledFont f) {
    super(key, name);
    setValue(f);
  }

  public String getValueString() {
    return encode((StyledFont) value);
  }

  public void setValue(String s) {
    setValue(decode(s));
  }

  public java.awt.Component getControls() {
    if (p == null) {
      p = new JPanel();

      p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));
      
      Box box = Box.createHorizontalBox();
      box.add(new JLabel("Font Family:  "));

      family = new JComboBox();
      String[] s = GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
      for (int i = 0; i < s.length; ++i) {
        family.addItem(s[i]);
      }
      family.setSelectedItem(value == null ? "SansSerif" : (getFontValue().getFamily()));
      box.add(family);
      p.add(box);

      size = new IntConfigurer(null, "Size:  ", new Integer(getFontValue().getSize()));
      p.add(size.getControls());

      box = Box.createHorizontalBox();
      bold = new BooleanConfigurer(null, "Bold", new Boolean(isBold()));
      box.add(bold.getControls());
      italic = new BooleanConfigurer(null, "Italic", new Boolean(isItalic()));
      box.add(italic.getControls());
      p.add(box);

      fgColor = new ColorSwatchConfigurer(null, "Font Color:  ", getFontValue().getFgColorName());
      p.add(fgColor.getControls());
      bgColor = new ColorSwatchConfigurer(null, "Background Color:  ", getFontValue().getBgColorName());
      p.add(bgColor.getControls());

      box = Box.createHorizontalBox();
      box.add(new JLabel("Sample:  "));
      demo = new JTextField("The quick brown fox", 20);
      demo.setPreferredSize(new Dimension(100, 25));
      demo.setEditable(false);
      box.add(demo);
      p.add(box);
      
      updateValue();

      ItemListener l = new ItemListener() {
        public void itemStateChanged(ItemEvent evt) {
          updateValue();
        }
      };
      family.addItemListener(l);

      PropertyChangeListener pc = new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          updateValue();
        }
      };
      size.addPropertyChangeListener(pc);
      bold.addPropertyChangeListener(pc);
      italic.addPropertyChangeListener(pc);
      fgColor.addPropertyChangeListener(pc);
      bgColor.addPropertyChangeListener(pc);
    }
    return p;
  }

  protected void updateValue() {

    int style = Font.PLAIN + 
       (bold.booleanValue().booleanValue() ? Font.BOLD : 0) + 
       (italic.booleanValue().booleanValue() ? Font.ITALIC : 0); 
   
    Font font = new Font((String) family.getSelectedItem(), style, Integer.parseInt(size.getValueString()));

    setValue(new StyledFont(font, fgColor.getValueColorSwatch(), bgColor.getValueColorSwatch()));

    demo.setFont(font);
    demo.setBackground(bgColor.getValueColor());
    demo.setForeground(fgColor.getValueColor());
    
  }

  protected StyledFont getFontValue() {
    return (StyledFont) getValue();
  }

  public static StyledFont decode(String s) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ';');
    return new StyledFont(
        sd.nextToken("Dialog"), 
        sd.nextInt(Font.PLAIN), 
        sd.nextInt(10), 
        sd.nextToken(ColorSwatch.BLACK), 
        sd.nextToken(ColorSwatch.CLEAR));
  }

  public static String encode(StyledFont f) {
    SequenceEncoder se = new SequenceEncoder(f.getName(), ';');
    se.append(f.getStyle());
    se.append(f.getSize());
    se.append(f.getFgColorName());
    se.append(f.getBgColorName());
    return se.getValue();
  }

  public boolean isBold() {
    int style = ((StyledFont) getFontValue()).getStyle();
    return style == Font.BOLD || style == (Font.BOLD + Font.ITALIC);
  }

  public boolean isItalic() {
    int style = ((StyledFont) getFontValue()).getStyle();
    return style == Font.ITALIC || style == (Font.BOLD + Font.ITALIC);
  }

  public boolean isBgTransparent() {
    return ((StyledFont) getFontValue()).getBgColor() == null;
  }

}
