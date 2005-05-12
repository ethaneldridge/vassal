/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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

public class StyledFont extends Font {
  
  protected ColorSwatch fgColor;
  protected ColorSwatch bgColor;
  
  public StyledFont(String name, int style, int size) {
    super(name, style, size);
    fgColor = GenericsContainer.getColorSwatch(ColorSwatch.BLACK);
    bgColor = GenericsContainer.getColorSwatch(ColorSwatch.CLEAR);
  }
  
  public StyledFont(String name, int style, int size, ColorSwatch fgColor, ColorSwatch bgColor) {
    this(name, style, size);
    this.fgColor = fgColor;
    this.bgColor = bgColor;
  }
  
  public StyledFont(String name, int style, int size, String fgColor, String bgColor) {
    this(name, style, size);
    this.fgColor = GenericsContainer.getColorSwatch(fgColor);
    this.bgColor = GenericsContainer.getColorSwatch(bgColor);
  }
  
  public StyledFont(Font font, ColorSwatch fgColor, ColorSwatch bgColor) {
    this(font.getFamily(), font.getStyle(), font.getSize(), fgColor, bgColor);
  }
  
  public StyledFont(Font font, String fgColorName, String bgColorName) {
    this(font.getFamily(), font.getStyle(), font.getSize(), 
        GenericsContainer.getColorSwatch(fgColorName), 
        GenericsContainer.getColorSwatch(bgColorName));
  }

//  protected void setFgColor(Color fgColor) {
//    this.fgColor = fgColor;
//  }

  protected Color getFgColor() {
    return fgColor.getColor();
  }

  protected String getFgColorName() {
    return fgColor.getConfigureName();
  }
  
//  protected void setBgColor(Color bgColor) {
//    this.bgColor = bgColor;
//  }

  protected Color getBgColor() {
    return bgColor.getColor();
  }
  
  protected String getBgColorName() {
    return bgColor.getConfigureName();
  }

  protected boolean isBgTransparent() {
    return bgColor == null;
  }
  
}