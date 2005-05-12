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
  
  protected Color fgColor;
  protected Color bgColor;
  
  public StyledFont(String name, int style, int size) {
    super(name, style, size);
    fgColor = Color.BLACK;
    bgColor = Color.WHITE;
  }
  
  public StyledFont(String name, int style, int size, Color fgColor, Color bgColor) {
    this(name, style, size);
    this.setFgColor(fgColor);
    this.setBgColor(bgColor);
  }

  protected void setFgColor(Color fgColor) {
    this.fgColor = fgColor;
  }

  protected Color getFgColor() {
    return fgColor;
  }

  protected void setBgColor(Color bgColor) {
    this.bgColor = bgColor;
  }

  protected Color getBgColor() {
    return bgColor;
  }

  protected boolean isBgTransparent() {
    return bgColor == null;
  }
  
}