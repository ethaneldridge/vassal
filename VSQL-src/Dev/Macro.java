/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
/*
 * Created by IntelliJ IDEA.
 * User: rkinney
 * Date: Oct 2, 2002
 * Time: 6:30:35 AM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package Dev;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.InputEvent;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.KeyStrokeArrayConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.counters.Decorator;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyCommand;
import VASSAL.counters.PieceEditor;
import VASSAL.counters.PieceFilter;
import VASSAL.counters.PropertiesPieceFilter;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;

/**
 * A GamePiece with this trait will echo the piece's current name when any of a given key commands are pressed
 * (and after they take effect)
 */
public class Macro extends Decorator implements EditablePiece {
  public static final String ID = "macro;";
  protected KeyStroke[] watchKeys;
  protected String propertyMatch;
  protected KeyStroke[] actionKeys;

  public Macro() {
    this(ID, null);
  }

  public Macro(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  public String getName() {
    return piece.getName();
  }

  protected KeyCommand[] myGetKeyCommands() {
    return new KeyCommand[0];
  }

  public String myGetState() {
    return "";
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(KeyStrokeArrayConfigurer.encode(watchKeys)).append(propertyMatch).append(KeyStrokeArrayConfigurer.encode(actionKeys));
    return ID + se.getValue();
  }

  public Command myKeyEvent(KeyStroke stroke) {
 
    // 1. Does it match one of our keystrokes?
    boolean seen = false;
    for (int i = 0; i < watchKeys.length && !seen; i++) {
      seen = stroke.equals(watchKeys[i]);
    }
    if (!seen) {
      return null;
    }
    
    // 2. Check the Property Filter?
    GamePiece outer = Decorator.getOutermost(this);
    if (propertyMatch != null && propertyMatch.length() > 0) {
      PieceFilter filter = PropertiesPieceFilter.parse(new FormattedString(propertyMatch).getText(outer));
      if (!filter.accept(outer)) {
        return null;
      }
    }
    
    // 3. Issue the outgoing keystrokes
    Command c = new NullCommand();
    for (int i = 0; i < actionKeys.length; i++) {
      c.append(outer.keyEvent(actionKeys[i]));
    }
    return c;
  }

  public void mySetState(String newState) {
  }

  public Shape getShape() {
    return piece.getShape();
  }

  public String getDescription() {
    return "Macro";
  }

  public HelpFile getHelpFile() {
      return null;
  }

  public void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    
    String keys = st.nextToken("");
    if (keys.indexOf(',') > 0) {
      watchKeys = KeyStrokeArrayConfigurer.decode(keys);
    }
    else {
      watchKeys = new KeyStroke[keys.length()];
      for (int i = 0; i < watchKeys.length; i++) {
        watchKeys[i] = KeyStroke.getKeyStroke(keys.charAt(i),InputEvent.CTRL_MASK);
      }
    }
    
    propertyMatch = st.nextToken("");
  
    keys = st.nextToken("");
    if (keys.indexOf(',') > 0) {
      actionKeys = KeyStrokeArrayConfigurer.decode(keys);
    }
    else {
      watchKeys = new KeyStroke[keys.length()];
      for (int i = 0; i < watchKeys.length; i++) {
        actionKeys[i] = KeyStroke.getKeyStroke(keys.charAt(i),InputEvent.CTRL_MASK);
      }
    }
    
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public static class Ed implements PieceEditor {

    private KeyStrokeArrayConfigurer watchKeys;
    private KeyStrokeArrayConfigurer actionKeys;
    private StringConfigurer propertyMatch;
    private JPanel box;

    public Ed(Macro piece) {

      box = new JPanel();
      box.setLayout(new BoxLayout(box, BoxLayout.Y_AXIS));
      
      watchKeys = new KeyStrokeArrayConfigurer(null, "Watch for these keystrokes:  ", piece.watchKeys);
      box.add(watchKeys.getControls());
   
      propertyMatch = new StringConfigurer(null, "And match these properties:  ", piece.propertyMatch);
      box.add(propertyMatch.getControls());
   
      actionKeys = new KeyStrokeArrayConfigurer(null, "To perform these keystrokes:  ", piece.watchKeys);
      box.add(actionKeys.getControls());      

    }

    public Component getControls() {
      return box;
    }

    public String getState() {
      return "";
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(watchKeys.getValueString()).append(propertyMatch.getValueString()).append(actionKeys.getValueString());
      return ID + se.getValue();
    }
  }
}
