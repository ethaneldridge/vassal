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

import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

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

public class ViewInfo extends Decorator implements EditablePiece
{
	static final int HEIGHT = 400;
	static final int WIDTH = 300;
	
	public static final String ID = "vinfo;";
	private KeyCommand[] command;
	private String commandName;
	private KeyStroke key;
	protected String imageName;	
	protected JDialog frame;
	protected Image image;
	protected Rectangle imageBounds;

	  	  
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
		System.err.println(this.imageName);
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

			System.err.println(this.imageName);
			
	        JLabel label = new JLabel();
	        ImageIcon icon = new ImageIcon(image);
	        label.setIcon(icon);
	        pane.add(label);

	        frame.pack();
	        //frame.setAlwaysOnTop(true);
	        frame.setSize(WIDTH,HEIGHT);
	        frame.setTitle(getName());
	        frame.setResizable(false);
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
		
}