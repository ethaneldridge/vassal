package VASSAL.build.module;

/*
 * @author Brent Easton
 *
 * Describes a Request for multiple Die Rolls.
 *
 */

import java.awt.*;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.table.*;

import VASSAL.build.GameModule;

public class MultiRoll extends JDialog implements ListSelectionListener {

    private final Boolean TRUE = new Boolean(true);
    private final Boolean FALSE = new Boolean(false);

    private JButton rollButton = new JButton("Roll");
    private JButton canButton = new JButton("Cancel");

    private JPanel topPanel;
    private JPanel buttonPanel;
    private JPanel descPanel;
    private JTable table;
    private JScrollPane scrollPane;
    protected JTextField descText;
    protected int lastSelectedRow, lastSelectedCol;

    public static final int COL_IDX = 0;
    public static final int COL_ROLL = 1;
    public static final int COL_DESC = 2;
    public static final int COL_NDICE = 3;
    public static final int COL_NSIDES = 4;
    public static final int COL_ADD = 5;
    public static final int COL_TOTAL = 6;
    public static final int NUMCOLS = 7;

    public static final int MAX_ROLLS = 10;

    protected String description;
    protected DieRoll[] rolls = new DieRoll[MAX_ROLLS];
    protected boolean[] useDie = new boolean[MAX_ROLLS];
    protected String verification = "";
    protected boolean rollCancelled = false;
    protected boolean singleRoll;

    private static final String DESC = "description";

    public MultiRoll(int dfltNDice, int dfltNSides) {
        initConfig();
        clearDie();
        setSingleRoll(false);
        for (int i = 0; i < MAX_ROLLS; i++) {
            rolls[i] = new DieRoll("", dfltNDice, dfltNSides);
        }
    }

    private void clearDie() {
        for (int i = 0; i < MAX_ROLLS; i++) {
            useDie[i] = false;
        }
    }

    // Getters/Setters
    public void setSingleRoll(String desc, int nd, int ns, int p, boolean r) {
        clearDie();
        rolls[0] = new DieRoll(desc, nd, ns, p, r);
        useDie[0] = true;
        singleRoll = true;
        description = desc;
    }

    public void setSingleRoll(boolean b) {
        singleRoll = b;
    }

    public boolean wasCancelled() {
        return rollCancelled;
    }

    public void setDescription(String d) {
        description = d;
    }

    public String getDescription() {
        return description;
    }

    public boolean getSingleRoll() {
        return singleRoll;
    }

    public int getMaxDescLength() {
        int len = 0;
        for (int i = 0; i < MAX_ROLLS; i++) {
            if (useDie[i]) {
                if (rolls[i].description.length() > len) {
                    len = rolls[i].description.length();
                }
            }
        }
        return len;
    }

    public void refresh() {
        table.repaint();
    }

    // Multi-roll Configuration code
    private void initConfig() {

        setModal(true);

        setTitle("Multi Roller");
        setSize(380, 206);
        setBackground(Color.gray);

        // Create a panel to hold all other components
        topPanel = new JPanel();
        topPanel.setLayout(new BorderLayout());

        // Create a new table instance
        table = new JTable(new MyTableModel());

        table.getColumnModel().getColumn(COL_IDX).setPreferredWidth(10);
        table.getColumnModel().getColumn(COL_ROLL).setPreferredWidth(10);
        table.getColumnModel().getColumn(COL_DESC).setPreferredWidth(100);
        table.getColumnModel().getColumn(COL_NDICE).setPreferredWidth(30);
        table.getColumnModel().getColumn(COL_NSIDES).setPreferredWidth(30);
        table.getColumnModel().getColumn(COL_ADD).setPreferredWidth(30);
        table.getColumnModel().getColumn(COL_TOTAL).setPreferredWidth(30);

        // Is this neater?
        //
        //        TableColumn nDiceColumn = table.getColumnModel().getColumn(COL_NDICE);
        //        JComboBox comboBox = new JComboBox();
        //        comboBox.addItem("1");
        //        comboBox.addItem("2");
        //        comboBox.addItem("3");
        //        comboBox.addItem("4");
        //        comboBox.addItem("5");
        //        comboBox.addItem("6");
        //		comboBox.addItem("7");
        //		comboBox.addItem("8");
        //        nDiceColumn.setCellEditor(new DefaultCellEditor(comboBox));

        // Handle the listener
        ListSelectionModel selectionModel = table.getSelectionModel();
        selectionModel.addListSelectionListener(this);

        descPanel = new JPanel();
        JLabel descLabel = new JLabel("Roll Description");
        descText = new JTextField(30);
        descText.setText(description);

        descPanel.add(descLabel, BorderLayout.LINE_START);
        descPanel.add(descText, BorderLayout.LINE_END);
        topPanel.add(descPanel, BorderLayout.PAGE_START);

        descText.addKeyListener(new KeyAdapter() {
            public void keyReleased(KeyEvent evt) {
                setDescription(descText.getText());
            }
        });

        // Add the table to a scrolling pane
        scrollPane = new JScrollPane(table, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollPane.setPreferredSize(new Dimension(360, 180));
        topPanel.add(scrollPane, BorderLayout.CENTER);
        // topPanel.add(table);

        // Add Some buttons
        buttonPanel = new JPanel();
        rollButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent e) {
                rollCancelled = false;
                setVisible(false);
            }
        });
        canButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent e) {
                rollCancelled = true;
                setVisible(false);
            }
        });
        buttonPanel.add(rollButton);
        buttonPanel.add(canButton);

        getContentPane().add(topPanel, BorderLayout.PAGE_START);
        getContentPane().add(buttonPanel, BorderLayout.PAGE_END);

        pack();
    }

    class MyTableModel extends AbstractTableModel {

        // Create columns names
        private String columnNames[] = { "", "Roll", "Description", "nDice", "nSides", "add", "Total" };

        public int getColumnCount() {
            return 7;
        }

        public String getColumnName(int col) {
            return columnNames[col];
        }

        public Class getColumnClass(int col) {
            switch (col) {
                case COL_IDX :
                    return Integer.class;
                case COL_ROLL :
                    return Boolean.class;
                case COL_DESC :
                    return String.class;
                case COL_NDICE :
                    return Integer.class;
                case COL_NSIDES :
                    return Integer.class;
                case COL_ADD :
                    return Integer.class;
                case COL_TOTAL :
                    return Boolean.class;
                default :
                    return String.class;
            }
        }

        public int getRowCount() {
            return 10;
        }

        public Object getValueAt(int row, int col) {
            Object val = null;

            switch (col) {
                case COL_IDX :
                    return new Integer(row + 1);
                case COL_ROLL :
                    return new Boolean(useDie[row]);
                case COL_DESC :
                    return rolls[row].description;
                case COL_NDICE :
                    return new Integer(rolls[row].numDice);
                case COL_NSIDES :
                    return new Integer(rolls[row].numSides);
                case COL_ADD :
                    return new Integer(rolls[row].plus);
                case COL_TOTAL :
                    return new Boolean(rolls[row].reportTotal);
                default :
                    break;
            }
            return null;

        }

        public boolean isCellEditable(int row, int col) {
            return (col > 0) ? true : false;
        }

        public void setValueAt(Object value, int row, int col) {
            switch (col) {
                case COL_ROLL :
                    useDie[row] = ((Boolean) value).booleanValue();
                    break;
                case COL_DESC :
                    rolls[row].description = (String) value;
                    break;
                case COL_NDICE :
                    rolls[row].setNumDice(((Integer) value).intValue());
                    break;
                case COL_NSIDES :
                    rolls[row].numSides = ((Integer) value).intValue();
                    break;
                case COL_ADD :
                    rolls[row].plus = ((Integer) value).intValue();
                    break;
                case COL_TOTAL :
                    rolls[row].reportTotal = ((Boolean) value).booleanValue();
                    break;
                default :
                    break;
            }
            fireTableCellUpdated(row, col);
        }

    }

    // Handler for list selection changes
    public void valueChanged(ListSelectionEvent event) {
        // See if this is a valid table selection
        if (event.getSource() == table.getSelectionModel() && event.getFirstIndex() >= 0) {
            // Get the data model for this table
            TableModel model = (TableModel) table.getModel();

            //			// Determine the selected item
            //			String string = (String)model.getValueAt(
            //									table.getSelectedRow(),
            //									table.getSelectedColumn() );
            //
            //			// Display the selected item
            //			System.out.println( "Value selected = " + string );
        }
    }

}