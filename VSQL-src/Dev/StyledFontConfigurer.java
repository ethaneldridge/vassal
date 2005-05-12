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

import java.awt.Color;
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

import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.tools.SequenceEncoder;

/**
 * A Configurer for {@link Font} values
 */
public class StyledFontConfigurer extends Configurer {
  
  protected JPanel p;
  protected IntConfigurer size;
  protected BooleanConfigurer bold;
  protected BooleanConfigurer italic;
  protected ColorConfigurer fgColor;
  protected ColorConfigurer bgColor;
  protected JComboBox family;

  public StyledFontConfigurer (String key, String name) {
    super(key, name);
  }

  public StyledFontConfigurer (String key, String name, StyledFont f) {
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
      p.add(new JLabel(name));
      
      Box box = Box.createHorizontalBox();
      family = new JComboBox();
      String[] s = GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
      for (int i = 0; i < s.length; ++i) {
        family.addItem(s[i]);
      }
      family.setSelectedItem(value == null ? "SansSerif" : (getFontValue().getFamily()));
      box.add(family);

      size = new IntConfigurer(null, "Size", new Integer(getFontValue().getSize()));
      box.add(size.getControls());
      p.add(box);

      box = Box.createHorizontalBox();
      bold = new BooleanConfigurer(null, "Bold", new Boolean(isBold()));
      box.add(bold.getControls());
      italic = new BooleanConfigurer(null, "Italic", new Boolean(isItalic()));
      box.add(italic.getControls());
      p.add(box);
      
      box = Box.createHorizontalBox();
      fgColor = new ColorConfigurer(null, "Font Color", getFontValue().getFgColor());
      box.add(fgColor.getControls());
      bgColor = new ColorConfigurer(null, "Background Color", getFontValue().getBgColor());
      box.add(bgColor.getControls());
      p.add(box);
      
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

    setValue(new StyledFont(
        (String) family.getSelectedItem(),
        Font.PLAIN,
        Integer.parseInt(size.getValueString()),
        (Color) fgColor.getValue(),
        (Color) bgColor.getValue()));
  }
  
  protected StyledFont getFontValue() {
    return (StyledFont) getValue();
  }

  public static StyledFont decode(String s) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, '|');
    return new StyledFont(
        sd.nextToken("Dialog"),
        sd.nextInt(10),
        sd.nextInt(Font.PLAIN),
        sd.nextColor(Color.BLACK),
        sd.nextColor(null)
        );
  }

  public static String encode(StyledFont f) {
    SequenceEncoder se = new SequenceEncoder(f.getName(), '|');
    se.append(f.getSize());
    se.append(f.getStyle());
    se.append(ColorConfigurer.colorToString(f.getFgColor()));
    se.append(ColorConfigurer.colorToString(f.getBgColor())+"");
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

