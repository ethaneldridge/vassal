/*
 * Created on 14/05/2005
 * 
 * TODO To change the template for this generated file go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
package Dev;

import java.awt.Font;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;

public class Item extends AbstractConfigurable {

  protected static final String NAME = "name";
  protected static final String TYPE = "type";
  protected static final String WIDTH = "width";
  protected static final String HEIGHT = "height";
  protected static final String LOCATION = "location";
  protected static final String X_OFFSET = "xoffset";
  protected static final String Y_OFFSET = "yoffset";
  protected static final String BG_COLOR = "bgColor";
  protected static final String FG_COLOR = "fgColor";
  protected static final String BORDER_COLOR = "borderColor";
  protected static final String BORDER_WIDTH = "borderWidth";
  protected static final String FONT = "font";
  protected static final String TEXT = "text";
  protected static final String SYMBOL_SET = "symbolSet";
  protected static final String IMAGE = "image";

  protected static final String TYPE_TEXT = "Text";
  protected static final String TYPE_LABEL = "Label";
  protected static final String TYPE_IMAGE = "Image";
  protected static final String TYPE_SYMBOL = "Symbol";
  
  protected static final String SYMBOL_NATO = "NATO Units";
  protected static final String SYMBOL_NATO_SIZE = "NATO Unit Sizes";

  protected static final String N = "Top";
  protected static final String S = "Bottom";
  protected static final String E = "Right";
  protected static final String W = "Left";
  protected static final String NE = "Top Right";
  protected static final String NW = "Top Left";
  protected static final String SE = "Bottom Right";
  protected static final String SW = "Bottom Left";
  protected static final String CENTER = "Center";

  protected String type, location;
  protected int width, height, xoffset, yoffset;
  double borderWidth;
  protected ColorSwatch bgColor = new ColorSwatch();
  protected ColorSwatch fgColor = new ColorSwatch();
  protected ColorSwatch borderColor = new ColorSwatch();

  protected Font font;
  protected String symbolSet;
  protected String text;
  protected String imageName;

  public Item() {
    super();
  }

  public Item(String name) {
    this();
    setConfigureName(name);
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Name:  ", "Type:  ", "Font:  ", "Text:  ", "Symbol Set:  ", "Image:  ", "Width:  ",
        "Height:  ", "Location:  ", "X Offset:  ", "Y Offset:  ", "Background Color:  ", "Foreground Color:  ",
        "Border Width:  ", "Border Color:  " };

  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, TypeConfig.class, FontConfig.class, String.class, SymbolConfig.class,
        Image.class, Integer.class, Integer.class, LocationConfig.class, Integer.class, Integer.class,
        BgColorConfig.class, FgColorConfig.class, Double.class, BorderColorConfig.class };
  }

  public static class TypeConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { TYPE_TEXT, TYPE_LABEL, TYPE_SYMBOL, TYPE_IMAGE };
    }
  }

  public static class SymbolConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { SYMBOL_NATO, SYMBOL_NATO_SIZE };
    }
  }
  
  public static class LocationConfig extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { CENTER, N, S, E, W, NE, NW, SE, SW };
    }
  }

  public static class FontConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FontStyleConfigurer(key, name, ((Item) c).font);
    }
  }
  
  public static class BgColorConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((Item) c).bgColor);
    }
  }

  public static class FgColorConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((Item) c).fgColor);
    }
  }

  public static class BorderColorConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((Item) c).borderColor);
    }
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, TYPE, FONT, TEXT, SYMBOL_SET, IMAGE, WIDTH, HEIGHT, LOCATION, X_OFFSET, Y_OFFSET,
        BG_COLOR, FG_COLOR, BORDER_WIDTH, BORDER_COLOR };
  }

  public void setAttribute(String key, Object o) {
    if (NAME.equals(key)) {
      setConfigureName((String) o);
    }
    else if (TYPE.equals(key)) {
      type = (String) o;
    }
    else if (WIDTH.equals(key)) {
      if (o instanceof String) {
        o = Integer.getInteger((String) o);
      }
      width = ((Integer) o).intValue();
    }
    else if (HEIGHT.equals(key)) {
      if (o instanceof String) {
        o = Integer.getInteger((String) o);
      }
      height = ((Integer) o).intValue();
    }
    else if (LOCATION.equals(key)) {
      location = (String) o;
    }
    else if (X_OFFSET.equals(key)) {
      if (o instanceof String) {
        o = Integer.getInteger((String) o);
      }
      xoffset = ((Integer) o).intValue();
    }
    else if (Y_OFFSET.equals(key)) {
      if (o instanceof String) {
        o = Integer.getInteger((String) o);
      }
      yoffset = ((Integer) o).intValue();
    }
    else if (BG_COLOR.equals(key)) {
      if (o instanceof String) {
        o = GenericsContainer.getColorSwatch((String) o);
      }
      bgColor = (ColorSwatch) o;
    }
    else if (FG_COLOR.equals(key)) {
      if (o instanceof String) {
        o = GenericsContainer.getColorSwatch((String) o);
      }
      fgColor = (ColorSwatch) o;
    }
    else if (BORDER_WIDTH.equals(key)) {
      if (o instanceof String) {
        o = new Double(Double.parseDouble((String) o));
      }
      borderWidth = ((Double) o).doubleValue();
    }
    else if (BORDER_COLOR.equals(key)) {
      if (o instanceof String) {
        o = GenericsContainer.getColorSwatch((String) o);
      }
      borderColor = (ColorSwatch) o;
    }

  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (TYPE.equals(key)) {
      return type + "";
    }
    else if (WIDTH.equals(key)) {
      return width + "";
    }
    else if (HEIGHT.equals(key)) {
      return height + "";
    }
    else if (LOCATION.equals(key)) {
      return location + "";
    }
    else if (X_OFFSET.equals(key)) {
      return xoffset + "";
    }
    else if (Y_OFFSET.equals(key)) {
      return yoffset + "";
    }
    else if (BG_COLOR.equals(key)) {
      return bgColor.getConfigureName();
    }
    else if (FG_COLOR.equals(key)) {
      return fgColor.getConfigureName();
    }
    else if (BORDER_WIDTH.equals(key)) {
      return borderWidth + "";
    }
    else if (BORDER_COLOR.equals(key)) {
      return borderColor.getConfigureName();
    }
    else
      return null;
  }

  public void removeFrom(Buildable parent) {

  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void addTo(Buildable parent) {

  }

  private VisibilityCondition borderCond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return borderWidth > 0.0f;
    }
  };

public VisibilityCondition getAttributeVisibility(String name) {
    if (BORDER_COLOR.equals(name)) {
      return borderCond;
    }
    else if ()
    else {
      return null;
    }
  }}
