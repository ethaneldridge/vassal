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
 
package Generic;

import VASSAL.tools.SequenceEncoder;


class SchemeElement {

  private String name;
  private ColorSwatch fgColor;
  private ColorSwatch bgColor;

  public SchemeElement(String s, ColorSwatch fg, ColorSwatch bg) {
    name = s;
    fgColor = fg;
    bgColor = bg;
  }

  public SchemeElement(String s) {
    decode(s);
  }

  public String encode() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(name);
    se.append(fgColor.encode());
    se.append(bgColor.encode());
    return se.getValue();
  }

  public void decode(String s) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ';');
    name = sd.nextToken("");
    fgColor = new ColorSwatch(sd.nextToken(ColorSwatch.getBlack().encode()));
    bgColor = new ColorSwatch(sd.nextToken(ColorSwatch.getClear().encode()));
  }

  protected void setName(String name) {
    this.name = name;
  }

  protected String getName() {
    return name;
  }

  protected void setFgColor(ColorSwatch fgColor) {
    this.fgColor = fgColor;
  }

  protected ColorSwatch getFgColor() {
    return fgColor;
  }

  protected void setBgColor(ColorSwatch bgColor) {
    this.bgColor = bgColor;
  }

  protected ColorSwatch getBgColor() {
    return bgColor;
  }

}