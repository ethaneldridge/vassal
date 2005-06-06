/*
 * $Id$
 * 
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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

package Generic;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.io.File;
import java.io.IOException;

import VASSAL.build.GameModule;
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.DataArchive;
import VASSAL.tools.SequenceEncoder;

public class ImageItem extends Item {

  public static final String TYPE = "Image";
  
  protected static final String IMAGE = "image";

  protected String imageName = "";
  protected Image image = null;
  protected Rectangle imageBounds = new Rectangle();

  public ImageItem() {
    super();
  }

  public ImageItem(CounterLayout l) {
    super(l);
  }
  
  public String[] getAttributeDescriptions() {
    String a[] = new String[] { "Image:  " };
    String b[] = super.getAttributeDescriptions();
    String c[] = new String[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 2);
    System.arraycopy(a, 0, c, 2, a.length);
    System.arraycopy(b, 2, c, a.length+2, b.length-2);
    return c;
  }

  public Class[] getAttributeTypes() {
    Class a[] = new Class[] { Image.class };
    Class b[] = super.getAttributeTypes();
    Class c[] = new Class[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 2);
    System.arraycopy(a, 0, c, 2, a.length);
    System.arraycopy(b, 2, c, a.length+2, b.length-2);
    return c;
  }

  public String[] getAttributeNames() {
    String a[] = new String[] { IMAGE };
    String b[] = super.getAttributeNames();
    String c[] = new String[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 2);
    System.arraycopy(a, 0, c, 2, a.length);
    System.arraycopy(b, 2, c, a.length+2, b.length-2);
    return c;
  }
  
  public void setAttribute(String key, Object o) {
    if (IMAGE.equals(key)) {
      if (o instanceof String) {
        imageName = (String) o;
      }
      else {
        imageName = ((File) o).getName();
      }
    }
    else {
      super.setAttribute(key, o);
    }
    
    if (layout != null) {
      layout.refresh();
    }
    
  }
  
  public String getAttributeValueString(String key) {
    
    if (IMAGE.equals(key)) {
      return imageName;
    }  
    else {
      return super.getAttributeValueString(key);
    }
  }
  
  public VisibilityCondition getAttributeVisibility(String name) {
    if (ROTATION.equals(name)) {
       return falseCond;
     }
     else {
       return super.getAttributeVisibility(name);
     }
   }

  private VisibilityCondition falseCond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return false;
    }
  };
  
  public void draw(Graphics g, SchemeElement se, ImageDefn defn) {

    Color fg = se.getFgColor().getColor();
    Color bg = se.getBgColor().getColor();
    
    Point origin = getOrigin();
    
    ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
    //Labeler.drawLabel(g, s, origin.x, origin.y, f, align, Labeler.CENTER, fg, bg, null, getRotation());
    loadImage();
    g.drawImage(image, origin.x + imageBounds.x, origin.y + imageBounds.y, null);
  }
  
  public String getType() {
    return TYPE;
  }
  
  protected void loadImage() {
    
    if (imageName.trim().length() > 0) {
      try {
        image = GameModule.getGameModule().getDataArchive().getCachedImage(imageName);
        imageBounds = DataArchive.getImageBounds(image);
      }
      catch (IOException e) {
        image = null;
        imageBounds = new Rectangle();
      }
    }
    else {
      image = null;
      imageBounds = new Rectangle();
    }
  }
  
  public static Item decode(CounterLayout l, String s) {
    
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ';');
    
    ImageItem item = new ImageItem(l);
    
    sd.nextToken();
    item.imageName = sd.nextToken();
    
    return item;
  }
  
  public String encode() {
   
    SequenceEncoder se1 = new SequenceEncoder(TYPE, ';');
    
    se1.append(imageName);
   
    SequenceEncoder se2 = new SequenceEncoder(se1.getValue(), '|');
    se2.append(super.encode());
    
    return se2.getValue();
  }
  
  
}
