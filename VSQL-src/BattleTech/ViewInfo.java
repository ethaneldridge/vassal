package BattleTech;

import java.awt.Component;
import java.awt.Container;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.event.*;
import java.awt.*;
import javax.swing.JSplitPane;
import javax.swing.JSpinner;
import javax.swing.*;


import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.counters.Decorator;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.ImagePicker;
import VASSAL.counters.KeyCommand;
import VASSAL.counters.PieceEditor;
import VASSAL.tools.DataArchive;
import VASSAL.tools.SequenceEncoder;

import BattleTech.MessageBox;

public class ViewInfo extends Decorator implements EditablePiece, ActionListener, ChangeListener
{
	// declaration of constants
	static final int HEIGHT = 650;
	static final int WIDTH = 450;
	
	// declaration of the window vars
	public static final String ID = "vinfo;";
	private KeyCommand[] command;
	private String commandName;
	private KeyStroke key;
	protected String imageName;	
	protected JDialog frame;
	protected Image image;
	protected Rectangle imageBounds;
	private JPanel jPanel1 = new JPanel();
	private  JPanel jPanel2 = new JPanel();
	private JPanel jPanel3 = new JPanel();
	private JSplitPane jSplitPane1 = new JSplitPane();
	private JLabel jLabel1 = new JLabel();
	private JLabel jLabel2 = new JLabel();
	private JLabel jLabel3 = new JLabel();
	private JLabel jLabel4 = new JLabel();
    private JLabel jLabel5 = new JLabel();
	private JTextField gunskill = new JTextField();
	private JTextField warriorname = new JTextField();
	private JTextField pilotskill = new JTextField();
    private JSpinner hitsTakenSpinner = new JSpinner();
    private String[] totalWarriorHits = {"1","2","3","4","5","6"};
    private String[] heatbuildup = {"1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30"};    
    private JLabel jLabel6 = new JLabel();
    private JLabel jLabel7 = new JLabel();
    private JSpinner jSpinner2 = new JSpinner();
    private JLabel jLabel8 = new JLabel();
    private JLabel jLabel9 = new JLabel();
    private JLabel jLabel10 = new JLabel();
    private JLabel jLabel11 = new JLabel();
    private JRadioButton jRadioButton1 = new JRadioButton();
    private JRadioButton jRadioButton2 = new JRadioButton();
    private JRadioButton jRadioButton3 = new JRadioButton();
    private JRadioButton jRadioButton4 = new JRadioButton();
    private JRadioButton jRadioButton5 = new JRadioButton();
    private JRadioButton jRadioButton6 = new JRadioButton();
    private JRadioButton jRadioButton7 = new JRadioButton();
    private JRadioButton jRadioButton8 = new JRadioButton();

 	// declaration of the data vars
	private String warriorName = "";
	private String gunSkill = "";
	private String pilotSkill = "";
	private String warriorHitsTaken = "";
	private String heatBuildup = "";
	  	  
	public ViewInfo() 
	{
		this(ID + "ViewInfo;I;", null);
	}
	  
	public ViewInfo(String type, GamePiece inner) 
	{
		mySetType(type);
		setInner(inner);
	}

	public void mySetType(String type) 
	{
		type = type.substring(ID.length());
		SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
		this.commandName = st.nextToken();
		this.key = st.nextKeyStroke('I');
		this.imageName = st.nextToken();

		if (this.imageName.trim().length() > 0) 
		{
			try 
			{
				this.image = GameModule.getGameModule().getDataArchive().getCachedImage(this.imageName+".gif");
				this.imageBounds = DataArchive.getImageBounds(this.image);
		    }
		    catch (IOException e) 
		    {
		    	this.image = null;
		    	this.imageBounds = new Rectangle();
		    }
		}
		else 
		{
			this.image = null;
			this.imageBounds = new Rectangle();
		}
		
	    command = null;
	}

	public String myGetType() 
	{
		SequenceEncoder se = new SequenceEncoder(';');
		se.append(this.commandName).append(this.key).append(this.imageName);
		return ID + se.getValue();
	}

	protected KeyCommand[] myGetKeyCommands() 
	{
		if (command == null) 
		{
			if (this.commandName.length() > 0 && key != null) 
			{
				command = new KeyCommand[] { new KeyCommand(this.commandName, key, Decorator.getOutermost(this))};
			}
			else 
			{
				command = new KeyCommand[0];
			}
		}
			
		if (command.length > 0) 
		{
			command[0].setEnabled(getMap() != null);
		}
			
		return command;
	}

	public Command myKeyEvent(KeyStroke stroke) 
	{
		myGetKeyCommands();
		
		if (command[0].matches(stroke)) 
		{
			// schermke oppuppen met volledige img in
	        Container pane;

	        VASSAL.build.module.Map map = piece.getMap();
	        Frame parent = null;
	        if (map != null && map.getView() != null) 
	        {
	        	Container topWin = map.getView().getTopLevelAncestor();
	        	if (topWin instanceof JFrame) 
	        	{
	        		parent = (Frame) topWin;
	        	}
	        }

	        //frame = new JDialog();
	        frame = new JDialog(GameModule.getGameModule().getFrame());

	        pane = frame.getContentPane();
	        pane.setLayout(new FlowLayout());	        
	        buildDialog(pane);
	        
	        Point p = GameModule.getGameModule().getFrame().getLocation();
	        if (getMap() != null) 
	        {
	          p = getMap().getView().getLocationOnScreen();
	          Point p2 = getMap().componentCoordinates(getPosition());
	          p.translate(p2.x, p2.y);
	        }

	        frame.setLocation(p.x, p.y);
	        frame.addWindowListener(new WindowAdapter() 
	        {
	        	public void windowClosing(WindowEvent evt) 
	        	{
	        	}
	        }
	        );

	        frame.pack();
	        frame.setSize(HEIGHT,WIDTH);
	        frame.setTitle(getName());
	        frame.setResizable(true);
	        frame.setVisible(true);
		}
		return null;
	}

	public void mySetState(String s) {
	}

	public Rectangle boundingBox() 
	{
		Rectangle r = piece.boundingBox();
		return r;
	}	  

	public void draw(Graphics g, int x, int y, Component obs, double zoom) 
	{
		piece.draw(g, x, y, obs, zoom);
    }

	public String getName() 
	{
		return piece.getName();
	}

	public Shape getShape() 
	{
		Rectangle r = piece.getShape().getBounds();
	    return r;
	}

	public PieceEditor getEditor() 
	{
		return new Ed(this);
	}

	public String getDescription() 
	{
		return "Has Full Info";
	}

	public VASSAL.build.module.documentation.HelpFile getHelpFile() 
	{
		return null;
	}
	  
	public String myGetState() 
	{
		return "";
	}

	public static class Ed implements PieceEditor 
	{
		private StringConfigurer nameInput;
		private HotKeyConfigurer keyInput;
		private ImagePicker picker;
		private JPanel controls;

		public Ed(ViewInfo vi) 
		{
			controls = new JPanel();
			controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

			picker = new ImagePicker();
			picker.setImageName(vi.imageName);
			controls.add(picker);

			nameInput = new StringConfigurer(null, "Command name:  ", vi.commandName);
			controls.add(nameInput.getControls());
		      
			keyInput = new HotKeyConfigurer(null,"Keyboard Command:  ",vi.key);
			controls.add(keyInput.getControls());
		}

		public Component getControls() 
		{
			return controls;
		}

		public String getType() 
		{
			SequenceEncoder se = new SequenceEncoder(';');
			se.append(nameInput.getValueString()).append((KeyStroke)keyInput.getValue()).append(picker.getImageName());
			return ID + se.getValue();
		}

		public String getState() 
		{
			return "";
		}
	}

	public void buildDialog(Container pane)
	{

        jPanel1.setMinimumSize(new Dimension(610, 410));
        jPanel1.setPreferredSize(new Dimension(610, 410));
        jPanel1.setLayout(new BorderLayout());
        
        jSplitPane1.setBackground(Color.red);
        jSplitPane1.setBorder(BorderFactory.createLineBorder(Color.black));
//        jSplitPane1.setMaximumSize(new Dimension(10, 10));
//        jSplitPane1.setMinimumSize(new Dimension(10, 10));
//        jSplitPane1.setPreferredSize(new Dimension(10, 10));
        jSplitPane1.setLastDividerLocation(300);
        
        jPanel2.setBackground(Color.orange);
//        jPanel2.setMaximumSize(new Dimension(10, 10));
//        jPanel2.setMinimumSize(new Dimension(10, 10));
//        jPanel2.setPreferredSize(new Dimension(10, 10));
        jPanel2.setLayout(null);
        
        jPanel3.setBackground(Color.green);
//        jPanel3.setMaximumSize(new Dimension(10, 10));
//        jPanel3.setMinimumSize(new Dimension(10, 10));
//        jPanel3.setPreferredSize(new Dimension(10, 10));
        jPanel3.setLayout(new FlowLayout());

        JLabel fullimage = new JLabel();
        ImageIcon icon = new ImageIcon(image);
        fullimage.setIcon(icon);
        jPanel3.add(fullimage);
        
        jLabel1.setBorder(BorderFactory.createEtchedBorder());
        jLabel1.setText("Warrior Data");
        jLabel1.setBounds(new Rectangle(3, 4, 100, 21));
        
        warriorname.setText(this.warriorName);
        warriorname.setBounds(new Rectangle(66, 30, 115, 21));
        warriorname.addActionListener(this);
        
        jLabel2.setBorder(BorderFactory.createEtchedBorder());
        jLabel2.setText("Name :");
        jLabel2.setBounds(new Rectangle(3, 30, 57, 21));
        
        jLabel3.setBorder(BorderFactory.createEtchedBorder());
        jLabel3.setText("Piloting");
        jLabel3.setBounds(new Rectangle(99, 56, 57, 21));
        
        gunskill.addActionListener(this);
        gunskill.setText(gunSkill);
        gunskill.setBounds(new Rectangle(160, 56, 22, 21));
        
        jLabel4.setBorder(BorderFactory.createEtchedBorder());
        jLabel4.setText("Gunnery");
        jLabel4.setBounds(new Rectangle(3, 56, 57, 21));
        
        pilotskill.addActionListener(this);
        pilotskill.setText(pilotSkill);
        pilotskill.setBounds(new Rectangle(66, 56, 22, 21));

        SpinnerModel model = new SpinnerListModel(totalWarriorHits);
        hitsTakenSpinner = new JSpinner(model);
        JSpinner.DefaultEditor editor = new JSpinner.DefaultEditor(hitsTakenSpinner);
        hitsTakenSpinner.setEditor(editor);
        hitsTakenSpinner.addChangeListener(this);
        hitsTakenSpinner.setBounds(new Rectangle(85, 81, 40, 21));
         
        jLabel5.setBorder(BorderFactory.createEtchedBorder());
        jLabel5.setText("Hits Taken");
        jLabel5.setBounds(new Rectangle(2, 81, 80, 21));

        jLabel6.setBorder(BorderFactory.createEtchedBorder());
        jLabel6.setText("Mech Data");
        jLabel6.setBounds(new Rectangle(2, 124, 73, 21));
        jLabel7.setBorder(BorderFactory.createEtchedBorder());
        jLabel7.setText("Mech Heat Buildup");
        jLabel7.setBounds(new Rectangle(2, 151, 120, 21));
        
        SpinnerModel model2 = new SpinnerListModel(heatbuildup);
        jSpinner2 = new JSpinner(model2);
        JSpinner.DefaultEditor editor2 = new JSpinner.DefaultEditor(jSpinner2);
        jSpinner2.setEditor(editor2);
        jSpinner2.addChangeListener(this);
        jSpinner2.setBounds(new Rectangle(125, 151, 43, 21));        

        jLabel8.setBorder(BorderFactory.createEtchedBorder());
        jLabel8.setText("Engine hits");
        jLabel8.setBounds(new Rectangle(2, 176, 80, 21));
        jLabel9.setBorder(BorderFactory.createEtchedBorder());
        jLabel9.setText("Gyro hits");
        jLabel9.setBounds(new Rectangle(2, 201, 80, 21));
        jLabel10.setBorder(BorderFactory.createEtchedBorder());
        jLabel10.setText("Sensor hits");
        jLabel10.setBounds(new Rectangle(2, 225, 80, 21));
        jLabel11.setBorder(BorderFactory.createEtchedBorder());
        jLabel11.setText("Life support");
        jLabel11.setBounds(new Rectangle(2, 250, 80, 21));
        jRadioButton1.setActionCommand("");
        jRadioButton1.setBounds(new Rectangle(85, 176, 21, 23));
        jRadioButton2.setActionCommand("");
        jRadioButton2.setText("jRadioButton2");
        jRadioButton2.setBounds(new Rectangle(113, 176, 21, 23));
        jRadioButton3.setActionCommand("");
        jRadioButton3.setText("jRadioButton2");
        jRadioButton3.setBounds(new Rectangle(85, 201, 21, 23));
        jRadioButton4.setActionCommand("");
        jRadioButton4.setText("jRadioButton2");
        jRadioButton4.setBounds(new Rectangle(113, 201, 21, 23));
        jRadioButton5.setActionCommand("");
        jRadioButton5.setText("jRadioButton2");
        jRadioButton5.setBounds(new Rectangle(85, 225, 21, 23));
        jRadioButton6.setActionCommand("");
        jRadioButton6.setText("jRadioButton2");
        jRadioButton6.setBounds(new Rectangle(113, 225, 21, 23));
        jRadioButton7.setActionCommand("");
        jRadioButton7.setText("jRadioButton2");
        jRadioButton7.setBounds(new Rectangle(85, 250, 21, 23));
        jRadioButton8.setActionCommand("");
        jRadioButton8.setText("jRadioButton2");
        jRadioButton8.setBounds(new Rectangle(139, 176, 21, 23));
        
        jPanel2.add(jLabel1);
        jPanel2.add(jLabel2);
        jPanel2.add(jLabel3);
        jPanel2.add(jLabel4);
        jPanel2.add(jLabel5);
        jPanel2.add(warriorname);
        jPanel2.add(pilotskill);
        jPanel2.add(gunskill);
        jPanel2.add(hitsTakenSpinner);
        jPanel2.add(jLabel6);
        jPanel2.add(jLabel7);
        jPanel2.add(jSpinner2);
        jPanel2.add(jLabel8);
        jPanel2.add(jLabel9);
        jPanel2.add(jLabel10);
        jPanel2.add(jLabel11);
        jPanel2.add(jRadioButton1);
        jPanel2.add(jRadioButton2);
        jPanel2.add(jRadioButton4);
        jPanel2.add(jRadioButton3);
        jPanel2.add(jRadioButton5);
        jPanel2.add(jRadioButton6);
        jPanel2.add(jRadioButton7);
        jPanel2.add(jRadioButton8);

        jSplitPane1.add(jPanel2, JSplitPane.RIGHT);
        jSplitPane1.add(jPanel3, JSplitPane.LEFT);
        jSplitPane1.setDividerLocation(300);

        jPanel1.add(jSplitPane1, java.awt.BorderLayout.CENTER);
        pane.add(jPanel1);
  	}

    public void actionPerformed(ActionEvent e) 
    {
    	if(e.getSource().equals(warriorname))
    	{
    		this.warriorName = warriorname.getText();
    	}
    	if(e.getSource().equals(gunskill))
    	{
    		this.gunSkill = gunskill.getText();
    	}
    	if(e.getSource().equals(pilotskill))
    	{
    		this.pilotSkill = pilotskill.getText();
    	}
    }

    public void stateChanged(ChangeEvent e) 
    {
    	if(e.getSource().equals(hitsTakenSpinner))
    	{
    		warriorHitsTaken = hitsTakenSpinner.getValue().toString();
    	}
    	if(e.getSource().equals(jSpinner2))
    	{
    		heatBuildup = jSpinner2.getValue().toString();
    	}
    }
}
//	MessageBox box = new MessageBox(this);
//	box.setTitle("Test");
//	box.ask(this.warriorName);