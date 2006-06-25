/*
 * $Id$
 * 
 * Copyright (c) 2000-2005 by Brent Easton and Rodney Kinney
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License (LGPL) as published by
 * the Free Software Foundation.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, copies are available at
 * http://www.opensource.org.
 */
package VSQL;

import java.util.Enumeration;

import javax.swing.JCheckBox;

import CASL.Map.GameMap;
import CASL.VASL.VASLThread;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.HexGrid;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.preferences.Prefs;

/*
 * 
 * @author Brent Easton
 * 
 * Provide SQL specific support - SQL LOS rules - SQL Preferences
 */
public class VSQLThread extends VASLThread {

  protected SQLGameMap SQLMap;
  protected VSQLMap vmap;
  protected int state = STATE_OFF;
  
  public static final int STATE_OFF = 0;
  public static final int STATE_VEH = 1;
  public static final int STATE_ALL = 2;
  public static final int STATE_NONE = 3;

 
  public VSQLThread() {
    super();

    final BooleanConfigurer snapCenter = new BooleanConfigurer(VSQLProperties.SNAP_OPTION, "Position all counters in center of hex?");
    final JCheckBox snapBox = findBox(snapCenter.getControls());
    GameModule.getGameModule().getPrefs().addOption(VSQLProperties.VSQL, snapCenter);



    java.awt.event.ItemListener l = new java.awt.event.ItemListener() {
      public void itemStateChanged(java.awt.event.ItemEvent evt) {
        setSnap(snapBox.isSelected());
      }
    };
    snapBox.addItemListener(l);

    setSnap();
  }

  public void addTo(Buildable buildable) {
    super.addTo(buildable);
    vmap = (VSQLMap) buildable;
  }
  
  protected void launch() {
    // Cycle through the piece visibility states for each time you hit the 
    // LOS button
    state += 1;
    if (state > STATE_NONE) {
      state = STATE_VEH;
    }
    
    boolean vis;
    
    if (state == STATE_OFF || state == STATE_ALL) {
      vis = true;
    }
    else {
      vis = false;
    }
    
    vmap.setPiecesVisible(vis);
    vmap.repaint();
    //if (!visible && hideCounters) {
    //  vmap.hidePieces();
      //map.setPiecesVisible(false);
    //}
    super.launch();
  }
  
  public boolean isActive() {
    boolean vis = super.isVisible();
    return vis && (state != STATE_NONE);
  }
  
  protected GameMap createCASLMap(int w, int h) {
    SQLGameMap s = new SQLGameMap(w, h);
    setSnap();
    return s;
  }

  // Catch LOS key release and reset grid snap
  public void mouseReleased(java.awt.event.MouseEvent e) {
    if (!retainAfterRelease && e.getWhen() != lastRelease) {
      vmap.setPiecesVisible(true);
      state = STATE_OFF;
    }
    super.mouseReleased(e);
    setSnap();
  }

  /*
   * Set snap option - Centers only, or edges also
   */
  private void setSnap() {

    Prefs prefs = GameModule.getGameModule().getPrefs();
    boolean centerSnapOnly = ((Boolean) prefs.getValue(VSQLProperties.SNAP_OPTION)).booleanValue();
    setSnap(centerSnapOnly);
  }

  private void setSnap(boolean centerSnap) {
    if (map != null) {
      for (Enumeration e = map.getAllBoards(); e.hasMoreElements();) {
        Board b = (Board) e.nextElement();
        HexGrid hg = (HexGrid) b.getGrid();
        hg.setEdgesLegal(!centerSnap);
      }
    }
  }

  protected String initCaslMap() {
    String r = super.initCaslMap();
    result = new SQLLOSResult();
    setSnap();
    return r;
  }

 
}