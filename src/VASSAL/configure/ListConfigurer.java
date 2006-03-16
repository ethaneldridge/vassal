package VASSAL.configure;

import java.awt.Component;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.SwingUtilities;

import VASSAL.tools.SequenceEncoder;

/**
 * Configures a variable-length list of objects
 * 
 * @author rkinney
 * 
 */
public abstract class ListConfigurer extends Configurer implements PropertyChangeListener {
  protected Box controls;
  protected Box configControls;
  private List configurers = new ArrayList();

  public ListConfigurer(String key, String name) {
    super(key, name, new ArrayList());
  }

  public ListConfigurer(String key, String name, List val) {
    super(key, name, val);
  }

  public String getValueString() {
    SequenceEncoder se = new SequenceEncoder(',');
    for (Iterator iter = configurers.iterator(); iter.hasNext();) {
      Configurer c = (Configurer) iter.next();
      se.append(c.getValueString());
    }
    return se.getValue();
  }

  public void setValue(String s) {
    configurers.clear();
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
    while (sd.hasMoreTokens()) {
      Configurer c = buildChildConfigurer();
      c.addPropertyChangeListener(this);
      configurers.add(c);
    }
    updateControls();
  }

  protected void updateValue() {
    noUpdate = true;
    ArrayList newArray = new ArrayList();
    for (Iterator iter = configurers.iterator(); iter.hasNext();) {
      Configurer c = (Configurer) iter.next();
      newArray.add(c.getValue());
    }
    setValue(newArray);
    noUpdate = false;
  }
  
  public void setValue(Object o) {
    if (o == null) {
      o = new ArrayList();
    }
    super.setValue(o);
    if (!noUpdate && controls != null) {
      updateControls();
    }
  }

  public Component getControls() {
    if (controls == null) {
      controls = Box.createVerticalBox();
      controls.setBorder(BorderFactory.createTitledBorder(getName()));
      configControls = Box.createVerticalBox();
      controls.add(configControls);
      JButton addButton = new JButton("New");
      addButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          Configurer c = buildChildConfigurer();
          c.addPropertyChangeListener(ListConfigurer.this);
          configurers.add(c);
          updateValue();
          repack();
        }
      });
      controls.add(addButton);
      updateControls();
    }
    return controls;
  }

  public List getListValue() {
    return (List) getValue();
  }

  /**
   * The objects in the list are specified by the Configurer returned here
   * 
   * @return
   */
  protected abstract Configurer buildChildConfigurer();

  public void propertyChange(PropertyChangeEvent evt) {
    updateValue();
  }

  protected void updateControls() {
    for (Iterator iter = configurers.iterator(); iter.hasNext();) {
      Configurer c = (Configurer) iter.next();
      c.removePropertyChangeListener(this);
    }
    configurers.clear();
    configControls.removeAll();

    for (Iterator iter = getListValue().iterator(); iter.hasNext();) {
      Object value = (Object) iter.next();
      final Configurer c = buildChildConfigurer();
      c.addPropertyChangeListener(this);
      c.setValue(value);
      configurers.add(c);
      final Box b = Box.createHorizontalBox();
      JButton delButton = new JButton("Remove");
      delButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          controls.remove(b);
          configurers.remove(c);
          updateValue();
          repack();
        }
      });
      b.add(c.getControls());
      configControls.add(b);
    }
  }

  protected void repack() {
    Window w = SwingUtilities.getWindowAncestor(controls);
    if (w != null) {
      w.pack();
    }
  }
}
