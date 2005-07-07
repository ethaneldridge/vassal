package BattleTech;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;

public class BattlefieldLoader extends AbstractConfigurable implements CommandEncoder, GameComponent 
{
    private JButton importButton; // Adds an increment to the tension counter

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
    	return null;
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
}


