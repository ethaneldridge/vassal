/*
 * $Id$
 *
 * Copyright (c) 2000-2006 by Rodney Kinney
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
package VASSAL.counters;

import java.applet.AudioClip;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.AudioClipConfigurer;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;

/**
 * A trait that plays a sound clip
 * @author rkinney
 *
 */
public class PlaySound extends Decorator implements EditablePiece {
  public static final String ID = "playSound;";
  protected String menuText;
  protected KeyStroke stroke;
  protected KeyCommand command;
  protected KeyCommand[] commands;
  protected FormattedString format;
  
  public PlaySound() {
    this(ID,null);
  }
  
  public PlaySound(String type, GamePiece piece) {
    mySetType(type);
    setInner(piece);
  }

  public void mySetState(String newState) {
  }

  public String myGetState() {
    return "";
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(format.getFormat()).append(menuText).append(stroke);
    return ID+se.getValue();
  }

  protected KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      command = new KeyCommand(menuText,stroke,Decorator.getOutermost(this));
      commands = new KeyCommand[]{command};
    }
    return commands;
  }

  public Command myKeyEvent(KeyStroke stroke) {
    if (command.matches(stroke)) {
        try {
          AudioClip clip = GameModule.getGameModule().getDataArchive().getCachedAudioClip(format.getText(Decorator.getOutermost(this)));
          if (clip != null) {
            clip.play();
          }
        }
        catch (IOException e) {
          return null;
        }
    }
    return null;
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g,x,y,obs,zoom);
  }

  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  public Shape getShape() {
    return piece.getShape();
  }

  public String getName() {
    return piece.getName();
  }

  public String getDescription() {
    return "Play Sound";
  }

  public void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type,';');
    st.nextToken();
    format = new FormattedString(st.nextToken(""));
    menuText = st.nextToken("Play Sound");
    stroke = st.nextKeyStroke('P');
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "GlobalKeyCommand.htm"));
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }
  
  public static class Ed implements PieceEditor {
    private StringConfigurer menuConfig;
    private HotKeyConfigurer keyConfig;
    private AudioClipConfigurer soundConfig;
    private JPanel panel;
    
    public Ed(PlaySound p) {
      menuConfig = new StringConfigurer(null,"Menu Text",p.menuText);
      keyConfig = new HotKeyConfigurer(null,"Keyboard Command",p.stroke);
      soundConfig = new AudioClipConfigurer(null,"Sound Clip",GameModule.getGameModule().getArchiveWriter());
      soundConfig.setValue(p.format.getFormat());
      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel,BoxLayout.Y_AXIS));
      panel.add(menuConfig.getControls());
      panel.add(keyConfig.getControls());
      panel.add(soundConfig.getControls());
    }

    public Component getControls() {
      return panel;
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(soundConfig.getValueString()).append(menuConfig.getValueString()).append(keyConfig.getValueString());
      return ID+se.getValue();
    }

    public String getState() {
      return "";
    }
  }
}
