/*
 * FormattedString.java
 * A String that can include options of the form $optionName$. Option values
 * are maintained in a property list and getText returns the string will all
 * options replaced by their value
 */

package VASSAL.tools;

import java.util.Properties;
import java.util.StringTokenizer;

public class FormattedString {

	private String formatString;
	private Properties props = new Properties();

    public FormattedString () {
    	setFormat("");
    }
    
	public FormattedString (String s) {
		setFormat(s);	
	}
	
	public void setFormat(String s) {
		formatString = s;
	}
	
	public String getFormat() {
		return formatString;
	}

	public void setProperty(String name, String value) {
    if (value != null) {
      props.setProperty(name, value);
    }
    else {
      props.remove(name);
    }
  }
	
	public String getText() {
		StringBuffer buffer = new StringBuffer();
    StringTokenizer st = new StringTokenizer(formatString,"$",true);
    boolean expectingVariable = false;
    while (st.hasMoreTokens()) {
      String token = st.nextToken();
      if ("$".equals(token)) {
        expectingVariable = !expectingVariable;
      }
      else if (expectingVariable) {
        String value = props.getProperty(token);
        if (value != null) {
          buffer.append(value);
        }
        else {
          if (!props.containsKey(token)) {
            expectingVariable = false;
          }
        }
      }
      else {
        buffer.append(token);
      }
    }
/*
		Enumeration e = p.keys();
		while (e.hasMoreElements()) {
			String key = (String) e.nextElement();
			String val = p.getProperty(key);
      if (val != null) {
        s = s.replaceAll("\\$"+key+"\\$", "");
      }
      else {
        s = s.replaceAll("\\$"+key+"\\$", val);
      }
    }
*/

		return buffer.toString();
	}
	
}