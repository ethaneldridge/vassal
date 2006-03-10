/*
 * FormattedString.java
 * A String that can include options of the form $optionName$. Option values
 * are maintained in a property list and getText returns the string will all
 * options replaced by their value
 */

package VASSAL.tools;

import java.util.HashMap;

import VASSAL.build.GameModule;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.counters.GamePiece;

public class FormattedString {

  private String formatString;
  private HashMap props = new HashMap();

  public FormattedString() {
    setFormat("");
  }

  public FormattedString(String s) {
    setFormat(s);
  }

  public void setFormat(String s) {
    formatString = s;
  }

  public String getFormat() {
    return formatString;
  }

  public void setProperty(String name, String value) {
    props.put(name, value);
  }

  public void clearProperties() {
    props.clear();
  }

  /**
   * Return the resulting string after substituting properties
   * @return
   */
  public String getText() {
    return getText(GameModule.getGameModule());
  }

  /**
   * @deprecated use @link #getText(PropertySource)
   */
  public String getText(GamePiece piece) {
    return getText((PropertySource)piece);
  }
  /**
   * Return the resulting string after substituting properties
   * Also, if any property keys match a property in the given GamePiece,
   * substitute the value of that property
   * @see GamePiece#getProperty
   * @param piece
   * @return
   */
  public String getText(PropertySource piece) {
    StringBuffer buffer = new StringBuffer();
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(formatString, '$');
    boolean isProperty = true;
    while (st.hasMoreTokens()) {
      String token = st.nextToken();
      isProperty = !isProperty;
      if (token.length() > 0) {
        /*
         * Only even numbered tokens with at least one token after them are valid $propertName$ strings.
         */
        if (!isProperty || ! st.hasMoreTokens()) {
          buffer.append(token);
        }
        else if (props.containsKey(token)) {
          String value = (String) props.get(token);
          if (value != null) {
            buffer.append(value);
          }
        }
        else if (piece != null) {
          Object value = piece.getProperty(token);
          if (value != null) {
            buffer.append(value.toString());
          }
          else {
            buffer.append(token);
          } 
        }
        else {
          buffer.append(token);
        }
      }
    }

    return buffer.toString();
  }

}