package BattleTech;

import java.awt.Image;
import java.io.IOException;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JOptionPane;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.boardPicker.Board;

public class BattleTechBoard extends Board 
{
  
	  public void fixImage(Map map) 
	  {
		  Cleanup.init();
		  Cleanup.getInstance().addBoard(this);
		  
		  if (imageFile == null)
		  {
			  return;
		  }
		    
		  try 
		  {
		      try 
		      {
		    	  if(imageFile.equalsIgnoreCase("test.gif"))
		    	  {
		    		  boardImage = GameModule.getGameModule().getDataArchive().getImage
		    		  (GameModule.getGameModule().getDataArchive().getFileStream
		    				  ("images/" + imageFile));
		    	  }
		      }
		      catch (IOException e) 
		      {
		        boardImage = null;
		      }

		      if (boardImage == null && imageFile != null) 
		      {
		    	  JOptionPane.showMessageDialog
		            (null,
		             "Error reading board image " + imageFile + " in "
		             + GameModule.getGameModule().getDataArchive().getName(),
		             "Not Found",
		             JOptionPane.ERROR_MESSAGE);
		    	  return;
		      }

		      Icon icon = new ImageIcon(boardImage);
		      boundaries.setSize(icon.getIconWidth(),
		                         icon.getIconHeight());
		    }
		    catch (OutOfMemoryError err) 
		    {
		      JOptionPane.showMessageDialog
		          (null,
		           "Insufficient memory to load board " + getName()
		           + "\nTry setting your display to use fewer colors",
		           "Out of memory",
		           JOptionPane.ERROR_MESSAGE);
		    }
	  }
	  
	  public void setBoardImage(Image im)
	  {
		  this.boardImage = im;
	  }
}