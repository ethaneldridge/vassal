package VASSAL.configure;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

import javax.swing.JComboBox;

public class FormattedStringConfigurer
	extends StringConfigurer
	implements ActionListener, FocusListener {

	protected String[] thingyList = new String[] {};
	JComboBox dropList;

	public FormattedStringConfigurer(String key, String name) {
		super(key, name);
	}

	public FormattedStringConfigurer(
		String key,
		String name,
		String[] things) {
		this(key, name);
		setThings(things);
	}

	public void setThings(String[] t) {
		thingyList = new String[t.length + 1];
		thingyList[0] = "Insert";
		for (int i = 1; i < thingyList.length; i++) {
			thingyList[i] = t[i - 1];
		}
	}

	public java.awt.Component getControls() {
		super.getControls();

		nameField.addFocusListener(this);
		dropList = new JComboBox((String[]) thingyList);
		dropList.setSelectedIndex(0);
		dropList.setEnabled(false);
		dropList.addActionListener(this);

		p.add(dropList);

		return p;
	}

    /*
     * Drop-down list has been clicked, so save current p
     * 
     */
	public void actionPerformed(ActionEvent arg0) {
		String item = "";
		
		int selectedItem = dropList.getSelectedIndex();
		
		if (selectedItem > 0) {
			item = '$' + thingyList[selectedItem] + '$';
			String work = nameField.getText();
			
			// Cut out any selected text
			if (nameField.getSelectedText() != null) {
				int start = nameField.getSelectionStart();
				int end = nameField.getSelectionEnd();
				work = work.substring(0,start) + work.substring(end);
			}
			int pos = nameField.getCaretPosition();
			String news = work.substring(0,pos) + item + work.substring(pos);
			nameField.setText(news);
			
			// Select newly inserted text
			nameField.setSelectionStart(0);
			nameField.setSelectionEnd(pos+item.length());
			nameField.setSelectionStart(pos);
			nameField.repaint();
		}
        nameField.requestFocusInWindow();
	}

	/* 
	 * Focus gained on text field, so enable insert drop-down
	 * and make sure it says 'Insert'
	 */
	public void focusGained(FocusEvent arg0) {
		dropList.setSelectedIndex(0);
		dropList.setEnabled(true);
		dropList.repaint();
	}

	/* 
	 * Focus lost on text field, so disable insert drop-down
	 */
	public void focusLost(FocusEvent arg0) {
		dropList.setEnabled(false);
		
	}
}