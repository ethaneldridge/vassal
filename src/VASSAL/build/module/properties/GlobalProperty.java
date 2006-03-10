package VASSAL.build.module.properties;

import java.beans.PropertyChangeSupport;

import javax.swing.JToolBar;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.ToolBarComponent;

/**
 * Adds a global property to a Map or Module
 * @author rkinney
 *
 */
public class GlobalProperty extends AbstractConfigurable implements ToolBarComponent, GameComponent, CommandEncoder {
  protected PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(this);

  public static final String NAME = "name";
  public static final String INITIAL_VALUE = "initialValue";
  public static final String DESCRIPTION = "description";

  private static final String COMMAND_PREFIX = "GlobalProperty\t";
  private ToolBarComponent toolbarComponent;
  protected Property property = new Property(null, null);
  protected String description;
  private String initialValue;


  public String[] getAttributeDescriptions() {
    return new String[] {"Name", "Initial value", "Description"};
  }

  public Class[] getAttributeTypes() {
    return new Class[] {String.class, String.class, String.class};
  }

  public String[] getAttributeNames() {
    return new String[] {NAME, INITIAL_VALUE, DESCRIPTION};
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      String oldName = getConfigureName();
      propertyChangeSupport.firePropertyChange(oldName,property.getValue(),null); // Clear the value under the old key
      setConfigureName((String) value);
      property = createProperty((String)value);
      propertyChangeSupport.firePropertyChange(getConfigureName(),null,property.getValue());
    }
    else if (INITIAL_VALUE.equals(key)) {
      initialValue = (String) value;
      setPropertyValue(initialValue);
    }
    else if (DESCRIPTION.equals(key)) {
      description = (String) value;
    }
  }

  protected Property createProperty(String key) {
    return new Property(key, property.getValue());
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (INITIAL_VALUE.equals(key)) {
      return initialValue;
    }
    else if (DESCRIPTION.equals(key)) {
      return description;
    }
    return null;
  }

  public void removeFrom(Buildable parent) {
    propertyChangeSupport.removePropertyChangeListener(((GlobalPropertiesContainer)parent).getPropertyListener());
    GameModule.getGameModule().removeCommandEncoder(this);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[]{ChangePropertyButton.class};
  }

  public void addTo(Buildable parent) {
    // Initialize property with current values
    propertyChangeSupport.addPropertyChangeListener(((GlobalPropertiesContainer)parent).getPropertyListener());
    propertyChangeSupport.firePropertyChange(property.getKey(),null,property.getValue());
    toolbarComponent = (ToolBarComponent)parent;
    GameModule.getGameModule().addCommandEncoder(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
  }
  
  public String getPropertyValue() {
    return property.getValue();
  }
  
  public void setPropertyValue(String value) {
    String oldValue = property.getValue();
    property.setValue(value);
    propertyChangeSupport.firePropertyChange(property.getKey(),oldValue,property.getValue());
  }

  public String prompt(String promptText) {
    return property.prompt(getToolBar().getTopLevelAncestor(),promptText);
  }

  public JToolBar getToolBar() {
    return toolbarComponent.getToolBar();
  }
  
  public void setup(boolean gameStarting) {
    return;
  }

  public Command getRestoreCommand() {
    return new SetGlobalProperty(this, "", getPropertyValue());
  }

  public Command decode(String command) {
    Command comm = null;

    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(command, ';');
    String prefix = sd.nextToken("");
    if (prefix.equals(COMMAND_PREFIX)) {
      String marker = sd.nextToken("");
      if (marker.equals(getConfigureName())) {
        comm = new SetGlobalProperty(this, getPropertyValue(), sd.nextToken(""));
      }
    }
    return comm;
  }

  public String encode(Command c) {
    String s = null;
    if (c instanceof SetGlobalProperty ) {
      if (((SetGlobalProperty) c).getTargetName().equals(getConfigureName())) {
        SequenceEncoder se = new SequenceEncoder(COMMAND_PREFIX, ';');
        se.append(getConfigureName());
        se.append(((SetGlobalProperty) c).newValue);
        s = se.getValue();
      }
    }
    return s;
  }
  /**
   * Command to pass a new Global property value to other players or into the
   * logfile.
   */
  protected static class SetGlobalProperty extends Command {

    protected String newValue;
    protected String oldValue;
    protected GlobalProperty target;
    protected String targetName;

    protected SetGlobalProperty(GlobalProperty target, String oldV, String newV) {

      oldValue = oldV;
      newValue = newV;
      this.target = target;
    }

    public String getTargetName() {
      return target == null ? "" : target.getConfigureName();
    }
    
    protected void executeCommand() {
      target.setPropertyValue(newValue);
    }

    protected Command myUndoCommand() {
      return new SetGlobalProperty(target, newValue, oldValue);
    }
  }


}
