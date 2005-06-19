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

package AutoImage;

import javax.swing.KeyStroke;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyCommand;

public abstract class ItemInstance extends AbstractConfigurable {

  public static final String FG_COLOR = "fgColor";
  public static final String BG_COLOR = "bgColor";
  
  protected String type = "";
  protected String location = "";
  protected ColorSwatch bgColor = ColorSwatch.getClear();
  protected ColorSwatch fgColor = ColorSwatch.getBlack();
  protected ImageDefn defn;
  protected Item item;
  private String state = "";
  
  protected InstanceConfigurer myConfig = null;

  public ItemInstance(String nam, String typ, String loc) {
    setName(nam);
    setType(typ);
    setLocation(loc);
  }

  public ItemInstance() {
    this("", "", "");
  }

  public ItemInstance(ImageDefn defn) {
    this();
    this.defn = defn;
  }

  /*
   * Generate a copy of the instance for use by the Generic trait.
   */
  
  public ItemInstance statefulCopy() {
    return this;
  }
  
  protected void setItem() {
    if (defn != null) {
      Layout layout = defn.getLayout();
      if (layout != null) {
        item = layout.getItem(name);
      }
    }
  }
  
  public Item getItem() {
    if (item == null) {
      setItem();
    }
    return item;
  }
  
  public void setConfig(InstanceConfigurer i) {
    myConfig = i;
  }
  
  public abstract String encode();

  public static ItemInstance newDefaultInstance(String name, String type, String location) {

    if (type.equals(SymbolItem.TYPE)) {
      return new SymbolItemInstance(name, type, location, Symbol.NatoUnitSymbolSet.SZ_DIVISION,
          Symbol.NatoUnitSymbolSet.INFANTRY, Symbol.NatoUnitSymbolSet.NONE);
    }
    else if (type.equals(TextItem.TYPE)) {
      return new TextItemInstance(name, type, location, null);
    }
    return null;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }

  public void setType(String type) {
    this.type = type;
  }

  public String getType() {
    return type;
  }

  public void setLocation(String location) {
    this.location = location;
  }

  public String getLocation() {
    return location;
  }

  public void setBgColor(ColorSwatch bgColor) {
    this.bgColor = bgColor;
  }

  public ColorSwatch getBgColor() {
    return bgColor;
  }

  public void setFgColor(ColorSwatch fgColor) {
    this.fgColor = fgColor;
  }

  public ColorSwatch getFgColor() {
    return fgColor;
  }

  public void setState(String state) {
    this.state = state;
  }

  public String getState() {
    return state;
  }

  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  public Class[] getAttributeTypes() {
    return new Class[0];
  }

  public String[] getAttributeNames() {
    return new String[0];
  }

  public void setAttribute(String key, Object value) {

  }

  public String getAttributeValueString(String key) {
    return null;
  }

  public void removeFrom(Buildable parent) {

  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return null;
  }

  public void addTo(Buildable parent) {
    if (parent instanceof ImageDefn) {
      defn = (ImageDefn) parent;
    }
  }

  public int getKeyCommandCount() {
    return 0;
    
  }
  
  public KeyCommand[] getKeyCommands(GamePiece target) {
    return new KeyCommand[getKeyCommandCount()];
  }
  
  public void keyEvent(KeyStroke stroke) {
    return;
  }

  public String getSuffix() {
    return "";
  }

  public String formatName(String name) {
    return name;
  }
  
}
