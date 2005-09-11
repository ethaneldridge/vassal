/*
 * $Id$
 *
 * Copyright (c) 2005 by Scott Giese, Rodney Kinney
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

import java.awt.*;
import java.io.File;
import java.net.MalformedURLException;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;

import javax.swing.*;

import VASSAL.build.module.Map;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.HexGrid;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.build.module.map.boardPicker.board.SquareGrid;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.ChangeTracker;
import VASSAL.configure.*;
import VASSAL.counters.Decorator;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyCommand;
import VASSAL.counters.PieceEditor;
import VASSAL.tools.SequenceEncoder;

/**
 * @author Scott Giese sgiese@sprintmail.com
 * 
 * Displays a transparency surrounding the GamePiece which represents the Area of Effect of the GamePiece
 */
public class AreaOfEffect extends Decorator implements EditablePiece {
	public static final String ID = "AreaOfEffect;";
	private static final Color defaultTransparencyColor = Color.GRAY;
	private static final float defaultTransparencyLevel = 0.3F;
	private static final int defaultRadius = 1;
	
	private Color transparencyColor;
	private float transparencyLevel;
	private int radius;
  private boolean alwaysActive;
  private boolean active;
  private String activateCommand;
  private KeyStroke activateKey;
  private KeyCommand[] commands;

	public AreaOfEffect() {
		this(ID + ColorConfigurer.colorToString(defaultTransparencyColor), null);
	}
	
	public AreaOfEffect(String type, GamePiece inner) {
		mySetType(type);
		setInner(inner);
	}

	public String getDescription() {
		return "Area Of Effect";
	}

	public String myGetType() {
		SequenceEncoder se = new SequenceEncoder(';');
		se.append(transparencyColor);
		se.append((int) (transparencyLevel * 100));
		se.append(radius);
    se.append(alwaysActive);
    se.append(activateCommand);
    se.append(activateKey);

		return ID + se.getValue();
	}

	public void mySetType(String type) {
	    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
	    st.nextToken();		// Discard ID
	    transparencyColor = st.nextColor(defaultTransparencyColor);
	    transparencyLevel = st.nextInt((int) (defaultTransparencyLevel * 100)) / 100.0F;
	    radius = st.nextInt(defaultRadius);
      alwaysActive = st.nextBoolean(true);
      activateCommand = st.nextToken("Show Area");
      activateKey = st.nextKeyStroke(null);
      commands = null;
	}

	// State does not change during the game
	public String myGetState() {
		return alwaysActive ? "" : String.valueOf(active);
	}
	
	// State does not change during the game
	public void mySetState(String newState) {
    if (!alwaysActive) {
      active = "true".equals(newState);
    }
	}

	public Rectangle boundingBox() {
		// TODO: Need the context of the parent Component, because the transparency is only drawn
		// on a Map.View object.  Because this context is not known, the bounding box returned by
		// this method does not encompass the bounds of the transparency.  The result of this is
		// that portions of the transparency will not be drawn after scrolling the Map window.
		return piece.boundingBox();
	}

	public Shape getShape() {
		return piece.getShape();
	}

	public String getName() {
		return piece.getName();
	}

	public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    if (alwaysActive || active) {
      // The transparency is only draw on a Map.View component.  Only the GamePiece is drawn within
      // other windows (Counter Palette, etc.)
      if (obs instanceof Map.View) {
        if (g instanceof Graphics2D) {
          Graphics2D g2d = (Graphics2D) g;

          float fZoom = (float)zoom;

          Color oldColor = g2d.getColor();
          g2d.setColor(transparencyColor);

          Composite oldComposite = g2d.getComposite();
          g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, transparencyLevel));

          // Determine the type of grid used (Hex or Square)
          Map map = getMap();
          // Always draw the area centered on the piece's current position
          // (For instance, don't draw it at an offset if it's in an expected stack)
          Point position = getPosition();
          int zoomedX = Math.round(fZoom*position.x);
          int zoomedY = Math.round(fZoom*position.y);
          Board board = map.findBoard(position);
          MapGrid grid = board.getGrid();

          if (grid instanceof HexGrid) {
            HexGrid hexGrid = (HexGrid) grid;

            // Ideally, the MapGrid interface would expose a GetShape() method; but since it
            // does not... calculate the bounds/polygon that makes up one hex location

            // Distance between hex centers in the horizontal direction
            float dxHex = (float) hexGrid.getHexWidth();

            // Distance between hex centers in the vertical direction
            float dyHex = (float) hexGrid.getHexSize();

            // Because hexes use 60 degree angles, we can bisect the width of the hex into 4
            // equal parts to determine where the vertices are located in the horizontal
            // direction.  This gives us five vertices in the horizontal direction; West,
            // North-West, Top-Center, North-East, East.  Because adjacent hex columns overlap,
            // we discard one of these 4 equal parts, giving us 3 equal parts between hex centers.
            // This is the value dxHex retrieved above.  The top of the hex makes up 2/3 of
            // these 3 parts, therefore, the upper, North-West vertice is calculated as
            // 1/3 of dxHex, left of center (x - dxHex / 3).  The North-East vertice
            // is calculated as 1/3 dxHex, right of center (x + dxHex / 3).
            float dxOneThird = dxHex / 3.0F;

            // Hexes have three vertices in the vertical direction; Top, Middle, and Bottom
            float dyOneHalf = dyHex / 2.0F;

            // Draw the transparency at the GamePiece location
            drawPolygon(zoomedX, zoomedY, fZoom*dxOneThird, fZoom*dyOneHalf, g2d);

            // Each radius unit is drawn as a ring surrounding the GamePiece location
            for (int i = 1; i <= radius; i++) {
              drawPolygonRing(zoomedX, zoomedY, fZoom*dxHex, fZoom*dyHex, i, g2d);
            }
          }
          else if (grid instanceof SquareGrid) {
            SquareGrid squareGrid = (SquareGrid) grid;
            float dxSquare = (float) squareGrid.getDx()*fZoom;
            float dySquare = (float) squareGrid.getDy()*fZoom;

            // Squares are much easier than hexes.  Just draw a bigger square!
            g2d.fillRect(zoomedX - Math.round(dxSquare * radius + dxSquare / 2), zoomedY - Math.round(dySquare * radius + dySquare / 2), Math.round(dxSquare * (2 * radius + 1)), Math.round(dySquare * (2 * radius + 1)));
          }
          else {
            int circleRadius = Math.round(fZoom*radius);
            g2d.fillOval(zoomedX-circleRadius, zoomedY-circleRadius,circleRadius*2,circleRadius*2);
          }

          g2d.setColor(oldColor);
          g2d.setComposite(oldComposite);
        }
      }
    }

    // Draw the GamePiece
		piece.draw(g, x, y, obs, zoom);
	}
	
	// This method draws a ring of transparency hexes centered around the GamePiece hex.  The ring
	// size is determined by the distance parameter.
	private void drawPolygonRing(int x, int y, float dxHex, float dyHex, int distance, Graphics2D g2d) {
		float dxOneThird = dxHex / 3.0F;
		float dyOneHalf = dyHex / 2.0F;
		
		int dx = x;
		int dy = y;
		
		// Travel North
		dy = Math.round(dy - (dyHex * distance));
		drawPolygon(dx, dy, dxOneThird, dyOneHalf, g2d);
		
		// Travel South-East
		for (int i = 0; i < distance; i++) {
			dx = Math.round(dx + dxHex);
			dy = Math.round(dy + dyOneHalf);
			drawPolygon(dx, dy, dxOneThird, dyOneHalf, g2d);
		}
		
		// Travel South
		for (int i = 0; i < distance; i++) {
			dy = Math.round(dy + dyHex);
			drawPolygon(dx, dy, dxOneThird, dyOneHalf, g2d);
		}
		
		// Travel South-West
		for (int i = 0; i < distance; i++) {
			dx = Math.round(dx - dxHex);
			dy = Math.round(dy + dyOneHalf);
			drawPolygon(dx, dy, dxOneThird, dyOneHalf, g2d);
		}
		
		// Travel North-West
		for (int i = 0; i < distance; i++) {
			dx = Math.round(dx - dxHex);
			dy = Math.round(dy - dyOneHalf);
			drawPolygon(dx, dy, dxOneThird, dyOneHalf, g2d);
		}

		// Travel North
		for (int i = 0; i < distance; i++) {
			dy = Math.round(dy - dyHex);
			drawPolygon(dx, dy, dxOneThird, dyOneHalf, g2d);
		}

		// Travel North-East
		for (int i = 0; i < distance - 1; i++) {
			dx = Math.round(dx + dxHex);
			dy = Math.round(dy - dyOneHalf);
			drawPolygon(dx, dy, dxOneThird, dyOneHalf, g2d);
		}
	}
	
	// This method draws the transparency within a single hex location.
	private void drawPolygon(int x, int y, float dxOneThird, float dyOneHalf, Graphics2D g2d) {
		Polygon hex = new Polygon();
		hex.addPoint(Math.round(x - dxOneThird), Math.round(y - dyOneHalf));	// North-West
		hex.addPoint(Math.round(x + dxOneThird), Math.round(y - dyOneHalf));	// North-East
		hex.addPoint(Math.round(x + dxOneThird * 2), Math.round(y));			// East
		hex.addPoint(Math.round(x + dxOneThird), Math.round(y + dyOneHalf));	// South-East
		hex.addPoint(Math.round(x - dxOneThird), Math.round(y + dyOneHalf));	// South-West
		hex.addPoint(Math.round(x - dxOneThird * 2), Math.round(y));			// West
		g2d.fill(hex);
	}

	// No hot-keys
	protected KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      if (alwaysActive) {
        commands = new KeyCommand[0];
      }
      else {
        commands = new KeyCommand[]{new KeyCommand(activateCommand,activateKey,Decorator.getOutermost(this))};
      }
    }
		return commands;
	}

	// No hot-keys
	public Command myKeyEvent(KeyStroke stroke) {
    Command c = null;
    myGetKeyCommands();
    if (commands.length > 0
    && commands[0].matches(stroke)) {
      ChangeTracker t = new ChangeTracker(this);
      active = !active;
      c = t.getChangeCommand();
    }
		return c;
	}
	
	public HelpFile getHelpFile() {
		File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
	    dir = new File(dir, "ReferenceManual");
	    try {
	      return new HelpFile(null, new File(dir, "AreaOfEffect.htm"));
	    }
	    catch (MalformedURLException ex) {
	      return null;
	    }
	}
	
	public PieceEditor getEditor() {
		return new TraitEditor(this);
	}
	
	private static class TraitEditor implements PieceEditor {
		private JPanel panel;
		private ColorConfigurer transparencyColorValue;
		private IntConfigurer transparencyValue;
		private IntConfigurer radiusValue;
    private BooleanConfigurer alwaysActive;
    private StringConfigurer activateCommand;
    private HotKeyConfigurer activateKey;

		private TraitEditor(AreaOfEffect trait) {
			panel = new JPanel();
			panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
			
			panel.add(new JLabel("Contributed by Scott Giese (sgiese@sprintmail.com)", JLabel.CENTER));
			panel.add(new JSeparator());
			panel.add(new JLabel(" "));
			
			transparencyColorValue = new ColorConfigurer(null, "Transparency Color: ", trait.transparencyColor);
			panel.add(transparencyColorValue.getControls());
			transparencyValue = new IntConfigurer(null, "Transparency Level (1-100): ", new Integer((int) (trait.transparencyLevel * 100)));
			panel.add(transparencyValue.getControls());
			radiusValue = new IntConfigurer(null, "Radius: ", new Integer(trait.radius));
			panel.add(radiusValue.getControls());

      alwaysActive = new BooleanConfigurer(null,"Always visible",trait.alwaysActive ? Boolean.TRUE : Boolean.FALSE);
      activateCommand = new StringConfigurer(null,"Toggle visible command: ",trait.activateCommand);
      activateKey = new HotKeyConfigurer(null,"Toggle visible keyboard shortcut: ", trait.activateKey);
      alwaysActive.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          updateVisibility();
        }
      });
      updateVisibility();

      panel.add(alwaysActive.getControls());
      panel.add(activateCommand.getControls());
      panel.add(activateKey.getControls());
		}

    private void updateVisibility() {
      boolean alwaysActiveSelected = Boolean.TRUE.equals(alwaysActive.getValue());
      activateCommand.getControls().setVisible(!alwaysActiveSelected);
      activateKey.getControls().setVisible(!alwaysActiveSelected);
      Window w = SwingUtilities.getWindowAncestor(alwaysActive.getControls());
      if (w != null) {
        w.pack();
      }
    }

    public Component getControls() {
			return panel;
		}

		public String getState() {
			return "true";
		}

		public String getType() {
      boolean alwaysActiveSelected = Boolean.TRUE.equals(alwaysActive.getValue());
			SequenceEncoder se = new SequenceEncoder(';');
			se.append(transparencyColorValue.getValueString());
			se.append(transparencyValue.getValueString());
			se.append(radiusValue.getValueString());
      se.append(alwaysActiveSelected);
      se.append(activateCommand.getValueString());
      se.append((KeyStroke)activateKey.getValue());

			return AreaOfEffect.ID + se.getValue();
		}
	}
}
