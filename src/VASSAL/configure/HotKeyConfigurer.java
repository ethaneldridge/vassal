/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
package VASSAL.configure;

import javax.swing.*;
import java.awt.event.*;

/**
 * A Configurer for {@link KeyStroke} values
 */
public class HotKeyConfigurer extends Configurer implements KeyListener {
    private JTextField tf;
    private JPanel p;

    public HotKeyConfigurer(String key, String name) {
	this(key,name,KeyStroke.getKeyStroke((char)0));
    }
    public HotKeyConfigurer(String key, String name, KeyStroke val) {
	super(key,name,val);
    }

    public void setValue(Object o) {
	super.setValue(o);
	if (tf != null
	    && !tf.getText().equals(keyToString())) {
	    tf.setText(keyToString());
	}
    }

    public String keyToString() {
	return getString((KeyStroke)getValue());
    }

    public String getValueString() {
	//	return getString((KeyStroke)getValue());
	return encode((KeyStroke)getValue());
    }
    public void setValue(String s) {
	setValue(s == null ? null : decode(s));
    }
    public java.awt.Component getControls() {
	if (p == null) {
	    p = new JPanel();
	    p.setLayout(new BoxLayout(p,BoxLayout.X_AXIS));
	    tf = new JTextField(8);
	    tf.setText(keyToString());
	    tf.addKeyListener(this);	    
	    p.add(new JLabel(getName()));
	    p.add(tf);
	}
	return p;
    }
    
    public void keyTyped(KeyEvent e){
    }

    public void keyPressed(KeyEvent e){
	switch (e.getKeyCode()) {
	case KeyEvent.VK_DELETE:
	case KeyEvent.VK_BACK_SPACE:
	    setValue(null);
	case KeyEvent.VK_SHIFT:
	case KeyEvent.VK_CONTROL:
	case KeyEvent.VK_META:
	case KeyEvent.VK_ALT:
	    break;
	default:
	    setValue(KeyStroke.getKeyStrokeForEvent(e));
	} 
    }

    public void keyReleased(KeyEvent e) {
	tf.setText(getString((KeyStroke)getValue()));
    }
    /**
     * A plain text representation of a KeyStroke.  Doesn't differ much
     * from {@link KeyEvent#getKeyText}
     */
    public static String getString(KeyStroke k) {
	if (k == null) {
	    return null;
	}
	String s = KeyEvent.getKeyText(k.getKeyCode());
	/*
	switch (",./;=[\\]".indexOf(k.getKeyCode())) {
	case 0:
	    s = "COMMA";
	    break;
	case 1:
	    s = "PERIOD";
	    break;
	case 2:
	    s = "SLASH";
	    break;
	case 3:
	    s = "SEMICOLON";
	    break;
	case 4:
	    s = "EQUALS";
	    break;
	case 5:
	    s = "OPEN_BRACKET";
	    break;
	case 6:
	    s = "BACK_SLASH";
	    break;
	case 7:
	    s = "CLOSE_BRACKET";
	    break;
	default:
	}
	*/
	s = s.replace(' ','_');
	if ((k.getModifiers() & KeyEvent.SHIFT_MASK) > 0) {
	    s = KeyEvent.getKeyText(KeyEvent.VK_SHIFT)+" "+s;
	}
	if ((k.getModifiers() & KeyEvent.CTRL_MASK) > 0) {
	    s = KeyEvent.getKeyText(KeyEvent.VK_CONTROL)+" "+s;
	}
	if ((k.getModifiers() & KeyEvent.META_MASK) > 0) {
	    s = KeyEvent.getKeyText(KeyEvent.VK_META)+" "+s;
	}
	if ((k.getModifiers() & KeyEvent.ALT_MASK) > 0) {
	    s = KeyEvent.getKeyText(KeyEvent.VK_ALT)+" "+s;
	}
	return s.toUpperCase();
    }
    /**
     * Decode a String into a KeyStroke
     */
    public static KeyStroke decode(String s) {
	int index = s.indexOf(",");
	try {
	    return KeyStroke.getKeyStroke
		(Integer.parseInt(s.substring(0,index)),
		 Integer.parseInt(s.substring(index+1)));
	}
	catch (Exception e) {
	    return null;
	}
	
    }
    /**
     * Encode a KeyStroke into a String 
     */
    public static String encode(KeyStroke stroke) {
	return stroke == null ? null : stroke.getKeyCode()+","+stroke.getModifiers();
    }

}
