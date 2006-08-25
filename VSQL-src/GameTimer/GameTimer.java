package GameTimer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.awt.Image;

import javax.swing.BorderFactory;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.DiceButton.IconConfig;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.ColorConfigurer;
import VASSAL.tools.LaunchButton;

public class GameTimer extends AbstractConfigurable implements GameComponent {

  protected LaunchButton launch;
  protected ClockWindow clockWindow;
  protected Color color = Color.white;

  public static final String VERSION = "1.0";
  public static final String HOTKEY = "hotkey";
  public static final String BUTTON_TEXT = "text";
  public static final String NAME = "name";
  public static final String ICON = "icon";
  public static final String COLOR = "color";
  
  public GameTimer() {
    super();
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        clockWindow.setVisible(!clockWindow.isVisible());
      }
    };
    launch = new LaunchButton(null, BUTTON_TEXT, HOTKEY, ICON, al);
    setAttribute(NAME, "Game Timer");
    setAttribute(BUTTON_TEXT, "Game Timer");
    launch.setToolTipText(getConfigureName());
    launch.setEnabled(false);
    
    clockWindow = new ClockWindow();    
    clockWindow.setColor(color);
    clockWindow.pack(); 
  }
  
  public static String getConfigureTypeName() {
    return "Game Timer v"+VERSION;
  }
  
  public String[] getAttributeDescriptions() {
    return new String[] { "Name:  ", "Button text:  ", "Button icon:  ", "Hotkey:  ", "Bakground Color:  " };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, String.class, IconConfig.class, KeyStroke.class, Color.class };
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, BUTTON_TEXT, ICON, HOTKEY, COLOR};
  }

  public void setAttribute(String key, Object o) {

    if (NAME.equals(key)) {
      setConfigureName((String) o);
      launch.setToolTipText((String) o);
    }
    else if (COLOR.equals(key)) {
      if (o instanceof String) {
        o = ColorConfigurer.stringToColor((String) o);
      }
      color = (Color) o;
      clockWindow.setColor(color);
    }
    else {
      launch.setAttribute(key, o);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (COLOR.equals(key)) {
      return ColorConfigurer.colorToString(color);
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }

  public void removeFrom(Buildable parent) {
    GameModule.getGameModule().getToolBar().remove(getComponent());
    GameModule.getGameModule().getGameState().removeGameComponent(this);
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void addTo(Buildable parent) {
    launch.setAlignmentY(0.0F);
    GameModule.getGameModule().getToolBar().add(getComponent());
    GameModule.getGameModule().getGameState().addGameComponent(this);
    
  }

  protected Component getComponent() {
    return launch;
  }
  
  public void setup(boolean gameStarting) {
    launch.setEnabled(gameStarting);   
  }

  public Command getRestoreCommand() {
    return null;
  }
  
  protected class ClockWindow extends JDialog {

    private static final long serialVersionUID = 1L;
    protected JPanel clockPanel;
    protected Clock clocks[];
    
    public ClockWindow() {
      super(GameModule.getGameModule().getFrame());
      initComponents();
    }
    
    public void setColor(Color color) {
      clockPanel.setBackground(color);
    }
    
    protected void initComponents() {

      setTitle(getConfigureName());
      
      clocks = new Clock[2];
      clocks[0] = new Clock();
      clocks[1] = new Clock();
      
      clockPanel = new JPanel();
      clockPanel.add(clocks[0].getControls());
      clockPanel.add(clocks[1].getControls());
    }
  }
  
  protected class Clock {

    protected JPanel holdingPanel;
    protected JPanel clockPanel;
    
    public Clock() {
      
    }
    
    public Component getControls() {
      if (holdingPanel == null) {
        holdingPanel = new JPanel();
        holdingPanel.setLayout(new BorderLayout());
        holdingPanel.setPreferredSize(new Dimension(40, 40));
        
        clockPanel = new JPanel();
        clockPanel.setPreferredSize(new Dimension(30, 30));
        clockPanel.setBorder(BorderFactory.createLineBorder(Color.black));
        holdingPanel.add(clockPanel, BorderLayout.CENTER);
        
      }
      SVGImage image = new SVGImage();
      BufferedImage image2 = new BufferedImage(50, 50, BufferedImage.TYPE_4BYTE_ABGR);
      Graphics g = image2.getGraphics();
      boolean done = g.drawImage(image, 0, 0, null);
      
      return holdingPanel;
    }
  }
  
  protected class SVGImage extends Image {
    
    BufferedImage image;

    public SVGImage() {
      image = new BufferedImage(50, 50, BufferedImage.TYPE_4BYTE_ABGR);
    }
    public void flush() {
      image.flush();
    }

    public Graphics getGraphics() {
      return image.getGraphics();
    }

    public int getHeight(ImageObserver arg0) {
      return image.getHeight(arg0);
    }

    public int getWidth(ImageObserver arg0) {
      return image.getWidth(arg0);
    }

    public ImageProducer getSource() {
      return image.getSource();
    }

    public Object getProperty(String arg0, ImageObserver arg1) {
      return image.getProperty(arg0, arg1);
    }
    
  }
}
