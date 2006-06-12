/*
 * $Id$
 * 
 * Copyright (c) 2000-2005 by Rodney Kinney and Brent Easton
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
package VSQL;

import VASSAL.build.AbstractBuildable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.noteswindow.SecretNotesController;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.Configurer;
import VASSAL.configure.TextConfigurer;
import VASSAL.tools.KeyStrokeListener;
import VASSAL.tools.SequenceEncoder;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import java.awt.*;
import java.awt.event.*;
import java.util.Hashtable;
import java.util.Enumeration;
import java.util.StringTokenizer;

public class VSQLScenInfo extends AbstractBuildable implements GameComponent, CommandEncoder {

  protected final String PUBLIC = "PUBLIC_NOTES";
  protected final String OBA = "OBA_INFO";

  // private JTextField AxisELR, AxisSAN, AlliedELR, AlliedSAN;
  protected FixedTextConfigurer notes;
  protected FixedTextConfigurer publicNotes;
  protected Hashtable privateNotes = new Hashtable();
  protected JButton launch;
  protected JFrame frame;
  protected OBATable oba;

  // private TurnMarker turn;

  protected KeyStrokeListener keyListener;
  protected AbstractAction launchAction;

  // private int axisSAN, alliedSAN;
  protected TextConfigurer myPrivate;
  protected SecretNotesController secretNotes;
  protected String lastState = "";

  public VSQLScenInfo() {

    frame = new JFrame("Notes");
    frame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);

    launch = new JButton("Info");
    launch.setAlignmentY(0.0F);
    launch.setToolTipText("Scenario Info Window [F7]");
    launchAction = new AbstractAction() {
      private static final long serialVersionUID = 1L;

      public void actionPerformed(ActionEvent e) {
        captureState();
        frame.setVisible(!frame.isShowing());
      }
    };
    launch.addActionListener(launchAction);
    launchAction.setEnabled(false);
    launch.setEnabled(false);

    keyListener = new KeyStrokeListener(launchAction);
    keyListener.setKeyStroke(KeyStroke.getKeyStroke(KeyEvent.VK_F7, 0, false));

    frame.getContentPane().setLayout(new BoxLayout(frame.getContentPane(), BoxLayout.Y_AXIS));

    Dimension base = new Dimension(300, 50);

    notes = new FixedTextConfigurer(null, "Scenario Information: ");
    publicNotes = new FixedTextConfigurer(null, "Public Notes: ");
    myPrivate = new TextConfigurer(null, "Private notes: ");
    secretNotes = new SecretNotesController();
    oba = new OBATable();

    JTabbedPane tab = new JTabbedPane();
    tab.setPreferredSize(new Dimension(650, 650));

    JPanel box = new JPanel();
    box.setLayout(new BorderLayout());
    box.add(notes.getControls(), BorderLayout.CENTER);
    JScrollPane scroll = new JScrollPane(oba, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    scroll.setBorder(new TitledBorder("OBA Information:"));
    oba.setPreferredScrollableViewportSize(new Dimension(400, 70));
    box.add(scroll, BorderLayout.SOUTH);

    tab.addTab("Scenario", null, box, "Scenario");
    tab.addTab("Public", null, publicNotes.getControls(), "Public");
    tab.addTab("Private", null, myPrivate.getControls(), "Private");
    tab.addTab("Delayed", null, secretNotes.getControls(), "Delayed");

    frame.getContentPane().add(tab);

    JPanel p = new JPanel();
    JButton saveButton = new JButton("Save");
    saveButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        save();
        frame.setVisible(false);
      }
    });
    p.add(saveButton);

    JButton canButton = new JButton("Cancel");
    canButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        cancel();
        frame.setVisible(false);
      }
    });
    p.add(canButton);
    p.setMaximumSize(base);

    frame.getContentPane().add(p);

    frame.addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
        save();
        frame.setVisible(false);
      }
    });

    frame.pack();
  }

  protected void cancel() {
    setState(lastState);
    secretNotes.restoreState();
  }

  private void save() {
    privateNotes.put(GameModule.getUserId(), myPrivate.getValue());
    Command c = new SetInfo(getState(), this);
    c.append(secretNotes.save());
    GameModule.getGameModule().sendAndLog(c);
  }

  public void addTo(Buildable b) {
    GameModule.getGameModule().getToolBar().add(launch);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    GameModule.getGameModule().addKeyStrokeListener(keyListener);
    GameModule.getGameModule().addCommandEncoder(this);
    GameModule.getGameModule().addCommandEncoder(secretNotes);
    GameModule.getGameModule().getGameState().addGameComponent(secretNotes);
  }

  public void removeFrom(Buildable b) {
    GameModule.getGameModule().getToolBar().remove(launch);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    GameModule.getGameModule().removeCommandEncoder(this);
    GameModule.getGameModule().removeCommandEncoder(secretNotes);
    GameModule.getGameModule().getGameState().removeGameComponent(secretNotes);
  }

  /**
   * Capture this object's state, to be restored if the user hits "Cancel"
   */
  protected void captureState() {
    lastState = getState();
    secretNotes.captureState();
  }

  public void setState(String in) {
    reset();

    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(in, '\t');
    try {
      st.nextToken();  // Read throgh old SAN stuff
      st.nextToken();
      st.nextToken();
      st.nextToken();
      st.nextToken();
      st.nextToken();
      st.nextToken();
    }
    catch (Exception e) {
    }

    if (st.hasMoreTokens()) {
      notes.setValue(st.nextToken());
    }
    while (st.hasMoreTokens()) {
      String id = st.nextToken();
      String encodedNotes = st.nextToken();

      if (id.equals(PUBLIC)) {
        publicNotes.setValue(encodedNotes);
      }
      else if (id.equals(OBA)) {
        oba.setState(encodedNotes);
      }
      else {
        StringBuffer buffer = new StringBuffer();
        SequenceEncoder.Decoder st2 = new SequenceEncoder.Decoder(encodedNotes, '|');
        while (st2.hasMoreTokens()) {
          buffer.append(st2.nextToken());
          if (st2.hasMoreTokens()) {
            buffer.append('\n');
          }
        }
        if (id.equals(GameModule.getUserId())) {
          myPrivate.setValue(buffer.toString());
        }
        privateNotes.put(id, buffer.toString());
      }
    }
  }

  public static final String COMMAND_PREFIX = "INFO\t";
  public static final String OBA_COMMAND_PREFIX = "OBA\t";

  public Command decode(String command) {
    Command comm = null;
    if (command.startsWith(COMMAND_PREFIX)) {
      comm = new SetInfo(command.substring(COMMAND_PREFIX.length()), this);
    }
    else {
      comm = secretNotes.decode(command);
    }
    return comm;
  }

  public String encode(Command c) {
    String s = null;
    if (c instanceof SetInfo) {
      s = COMMAND_PREFIX + ((SetInfo) c).getState();
    }
    else {
      s = secretNotes.encode(c);
    }
    return s;
  }

  public void setup(boolean show) {
    launch.setEnabled(show);
    launchAction.setEnabled(show);
    if (!show) {
      reset();
      frame.setVisible(false);
    }
  }

  public Command getRestoreCommand() {
    Command c = new SetInfo(getState(), this);
    c.append(secretNotes.getRestoreCommand());
    return c;
  }

  public void reset() {
    notes.setValue("");
    publicNotes.setValue("");
    myPrivate.setValue("");
    privateNotes.clear();
  }

  public String[] getAttributeNames() {
    return new String[0];
  }

  public void setAttribute(String name, Object value) {
  }

  public String getAttributeValueString(String name) {
    return null;
  }

  public String getState() {

    SequenceEncoder se = new SequenceEncoder('\t');
    se.append("").append("").append("" + "").append("").append("").append("").append("");
    se.append(notes.getValueString());

    for (Enumeration e = privateNotes.keys(); e.hasMoreElements();) {
      String id = (String) e.nextElement();
      String notes = (String) privateNotes.get(id);
      if (notes != null && notes.length() > 0) {
        SequenceEncoder se2 = new SequenceEncoder('|');
        StringTokenizer st = new StringTokenizer(notes, "\r\n");
        while (st.hasMoreTokens()) {
          se2.append(st.nextToken());
        }
        se.append(id);
        se.append(se2.getValue());
      }
    }

    se.append(PUBLIC);
    se.append(publicNotes.getValueString());
    se.append(OBA);
    se.append(oba.getState());

    return se.getValue();
  }

  public static class SetInfo extends Command {
    private VSQLScenInfo info;
    private String oldState;
    private String newState;

    public SetInfo(String value, VSQLScenInfo info) {
      newState = value;
      oldState = info.getState();
      this.info = info;
    }

    public String getState() {
      return newState;
    }

    protected void executeCommand() {
      info.setState(newState);
    }

    protected Command myUndoCommand() {
      return new SetInfo(oldState, info);
    }
  }

  public class FixedTextConfigurer extends Configurer {
    private JTextArea textArea;
    private JPanel p;

    public FixedTextConfigurer(String key, String name) {
      this(key, name, "");
    }

    public FixedTextConfigurer(String key, String name, String val) {
      super(key, name, val);
    }

    public String getValueString() {
      return TextConfigurer.escapeNewlines((String) getValue());
    }

    public void setValue(String s) {
      String text = TextConfigurer.restoreNewlines(s);
      setValue((Object) text);
    }

    public void setValue(Object o) {
      super.setValue(o);
      if (!noUpdate && textArea != null) {
        textArea.setText((String) o);
      }
    }

    public java.awt.Component getControls() {
      if (p == null) {
        p = new JPanel();
        p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));
        textArea = new JTextArea(6, 20);
        textArea.setFont(new Font("monospaced", Font.PLAIN, 12));
        textArea.setLineWrap(true);
        textArea.setWrapStyleWord(true);
        textArea.addKeyListener(new java.awt.event.KeyAdapter() {
          public void keyReleased(java.awt.event.KeyEvent evt) {
            noUpdate = true;
            setValue((Object) textArea.getText());
            noUpdate = false;
          }
        });
        textArea.setText((String) getValue());
        JScrollPane scroll = new JScrollPane(textArea);
        if (name != null) {
          scroll.setBorder(new TitledBorder(name));
        }
        p.add(scroll);
      }
      return p;
    }
  }

  protected static int MAX_ITEMS = 4;

  public class OBATable extends JTable {

    private static final long serialVersionUID = 1L;

    protected String[] side;
    protected String[] caliber;
    protected String[] notes;
    protected int[] remaining;

    public OBATable() {

      side = new String[MAX_ITEMS];
      caliber = new String[MAX_ITEMS];
      notes = new String[MAX_ITEMS];
      remaining = new int[MAX_ITEMS];

      for (int i = 0; i < MAX_ITEMS; i++) {
        side[i] = "";
        caliber[i] = "";
        notes[i] = "";
        remaining[i] = 0;
      }

      setModel(new MyTableModel());
      getColumnModel().getColumn(0).setPreferredWidth(80);
      getColumnModel().getColumn(1).setPreferredWidth(120);
      getColumnModel().getColumn(2).setPreferredWidth(300);
      getColumnModel().getColumn(3).setPreferredWidth(80);


      JComboBox sideBox = new JComboBox();
      sideBox.addItem("");
      sideBox.addItem("Allied");
      sideBox.addItem("Axis");
      getColumnModel().getColumn(0).setCellEditor(new DefaultCellEditor(sideBox));

      JComboBox calBox = new JComboBox();
      calBox.addItem("");
      calBox.addItem("70mm");
      calBox.addItem("80mm");
      calBox.addItem("100mm");
      calBox.addItem("120mm");
      calBox.addItem("150mm");
      calBox.addItem("Other");
      getColumnModel().getColumn(1).setCellEditor(new DefaultCellEditor(calBox));

      getColumnModel().getColumn(3).setCellEditor(new SpinnerEditor());
    }

    public TableCellRenderer getCellRenderer(int row, int column) {
      if (column == 3 && side[row].equals("")) {
          return new BlankRenderer();
      }
      return super.getCellRenderer(row, column);
    }

    class BlankRenderer implements TableCellRenderer {

      public Component getTableCellRendererComponent(JTable arg0, Object arg1, boolean arg2, boolean arg3, int arg4, int arg5) {
        return null;
      }
      
    }
    
    class SpinnerEditor extends AbstractCellEditor implements TableCellEditor {

      private static final long serialVersionUID = 1L;
      SpinnerModel spinnerModel;
      JSpinner spinner;

      public SpinnerEditor() {
        spinnerModel = new SpinnerNumberModel(0, 0, 9, 1);
        spinner = new JSpinner(spinnerModel);
      }

      public Object getCellEditorValue() {
        return spinner.getValue();
      }

      public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
        spinner.setValue((Integer) value);
        return spinner;
      }
    }

    public String getState() {
      SequenceEncoder se = new SequenceEncoder('|');
      for (int i = 0; i < MAX_ITEMS; i++) {
        SequenceEncoder se2 = new SequenceEncoder(',');
        se2.append(side[i]);
        se2.append(caliber[i]);
        se2.append(notes[i]);
        se2.append(remaining[i]);
        se.append(se2.getValue());
      }
      return se.getValue();
    }

    public void setState(String state) {
      SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(state, '|');
      for (int i = 0; i < MAX_ITEMS; i++) {
        String s = sd.nextToken("");
        SequenceEncoder.Decoder sd2 = new SequenceEncoder.Decoder(s, ',');
        side[i] = sd2.nextToken("");
        caliber[i] = sd2.nextToken("");
        notes[i] = sd2.nextToken("");
        remaining[i] = sd2.nextInt(0);
      }
    }

    public class MyTableModel extends AbstractTableModel {
      private static final long serialVersionUID = 1L;
      private String[] columnNames = new String[] {"Side", "Caliber", "Notes", "Remaining"};

      public int getColumnCount() {
        return columnNames.length;
      }

      public int getRowCount() {
        return MAX_ITEMS;
      }

      public String getColumnName(int col) {
        return columnNames[col];
      }

      public Object getValueAt(int row, int col) {
        if (col == 0) {
          return side[row];
        }
        else if (col == 1) {
          return caliber[row];
        }
        else if (col == 2) {
          return notes[row];
        }
        else if (col == 3) {
          return new Integer(remaining[row]);
        }
        return null;
      }

      public void setValueAt(Object o, int row, int col) {
        if (col == 0) {
          side[row] = (String) o;
          if (side[row].equals("")) {
            caliber[row] = "";
            notes[row] = "";
            remaining[row] = 0;
          }
        }
        else if (col == 1) {
          caliber[row] = (String) o;
        }
        else if (col == 2) {
          notes[row] = (String) o;
        }
        else if (col >= 3) {
          remaining[row] = ((Integer) o).intValue();
        }
        fireTableRowsUpdated(row, row);
      }

      public Class getColumnClass(int c) {
        if (c == 3) {
          return Integer.class;
        }
        return String.class;
      }

      public boolean isCellEditable(int row, int col) {
        if (col==0) {
          return true;
        }
        else {
          return (!side[row].equals(""));
        }
      }

    }
  }
}
