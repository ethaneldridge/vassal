/*
 * FormattedString.java
 * A String that can include options of the form $optionName$. Option values
 * are maintained in a property list and getText returns the string will all
 * options replaced by their value
 */

package VASSAL.tools;

import java.util.Enumeration;
import java.util.Properties;

public class FormattedString {
	
	String formatString;
	Properties props = new Properties();

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
		return getText(props);
	}
	
	public String getText(Properties p) {
		String s = formatString;
		
		Enumeration e = p.keys();
		while (e.hasMoreElements()) {
			String key = (String) e.nextElement();
			String val = p.getProperty(key);
      if (val != null) {
        s = s.replaceAll("\\$"+key+"\\$", val);
      }
    }
		
		return s; 
	}
	
}