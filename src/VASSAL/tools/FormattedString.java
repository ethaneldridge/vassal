/*
 * FormattedString.java
 * A String that can include options of the form $optionName$. Option values
 * are maintained in a property list and getText returns the string will all
 * options replaced by their value
 */

package VASSAL.tools;

import java.util.HashMap;

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

  public String getText() {
    StringBuffer buffer = new StringBuffer();
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(formatString, '$');
    while (st.hasMoreTokens()) {
      String token = st.nextToken();
      String value = (String) props.get(token);
      if (value != null) {
        buffer.append(value);
      }
      else if (!props.containsKey(token)) {
        buffer.append(token);
      }
    }

    return buffer.toString();
  }

}