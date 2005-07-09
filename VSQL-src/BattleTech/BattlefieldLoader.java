package BattleTech;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.command.Command;
import VASSAL.command.*;

import javax.swing.*;

import java.awt.*;
import java.util.*;
import java.awt.event.*;

import BattleTech.MyImagePicker;
import BattleTech.BattleTechBoard;

public class BattlefieldLoader extends AbstractConfigurable implements CommandEncoder, GameComponent 
{
    private JButton importButton; // Adds an increment to the tension counter
    private JButton exitButton;
    protected JDialog controls;
    protected String imageName = "";
    protected Image image;
    protected MyImagePicker picker;
    protected Mymap map;

    
    public void addTo(Buildable parent) 
    {
    	GameModule mod = (GameModule)parent;

    	mod.addCommandEncoder(this);
    	mod.getGameState().addGameComponent(this);

    	
    	importButton = new JButton("Load Battlefield");
    	importButton.setAlignmentY(0.0F);
    	importButton.addActionListener(new ActionListener() 
    	{
    		public void actionPerformed(ActionEvent evt) 
    		{
    			LoadPlayfieldDialog();
    		}
    	});
    	mod.getToolBar().add(importButton);
    }
    
    public void setAttribute(String key, Object value) 
    {
    }

    public String[] getAttributeNames() 
    {
    	return new String[]{};
    }

    public String[] getAttributeDescriptions() 
    {
    	return new String[]{};
    }

    public Class[] getAttributeTypes() 
    {
    	return new Class[]{};
    }
    
    public String getAttributeValueString(String key) 
    {
    	return "";
    }

    public void removeFrom(Buildable parent) 
    {
    	GameModule mod = (GameModule)parent;

    	mod.removeCommandEncoder(this);
    	mod.getGameState().removeGameComponent(this);

    	mod.getToolBar().remove(importButton);
    }

    public VASSAL.build.module.documentation.HelpFile getHelpFile() 
    {
    	return null;
    }

    public Class[] getAllowableConfigureComponents() 
    {
    	return new Class[0];
    }

    public void setup(boolean gameStarting) 
    {
    }

    public Command getRestoreCommand() 
    {
    	return null;
    }

    public static final String COMMAND_PREFIX = "IMPORT:";
    public String encode(Command c) 
    {
    	return null;
    }

    public Command decode(String s) 
    {
    	return null;
    }
    
    public void LoadPlayfieldDialog()
    {
    	Container pane;
		controls = new JDialog(GameModule.getGameModule().getFrame());
		pane = controls.getContentPane();
        pane.setLayout(new FlowLayout());	
				
		picker = new MyImagePicker();
		pane.add(picker);
				
		exitButton = new JButton("Exit");
    	exitButton.addActionListener(new ActionListener() 
    	{
    		public void actionPerformed(ActionEvent evt) 
    	    {
   		      Enumeration e = GameModule.getGameModule().getComponents(Map.class);
   		      
   		      map = (Mymap) e.nextElement();

   		      
   		      String boardName = "Battlefield";
   		      BattleTechBoard myboard = map.getTechBoardByName(boardName);
   		         
   		      /*
   		      //board.setAttribute(Board.IMAGE, newImageFileName);
   		      //map = (Component) map;
   		      
   		      board.fixImage(map); */
   		      controls.dispose();
    	    }
    	});

    	pane.add(exitButton);
    	controls.setSize(200,150);
    	controls.setResizable(false);
    	controls.setVisible(true);
	}
}
