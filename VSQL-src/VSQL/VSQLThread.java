/*
 * $Id$
 *
 * Copyright (c) 2000-2004 by Brent Easton and Rodney Kinney
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
package VSQL;

import java.util.Enumeration;

import javax.swing.JCheckBox;

import VASSAL.build.GameModule;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.HexGrid;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.preferences.Prefs;
import CASL.Map.GameMap;
import CASL.VASL.VASLThread;

/*
 * 
 * @author Brent Easton
 *
 * Provide SQL specific support
 *  - SQL LOS rules
 *  - SQL Preferences
 */
public class VSQLThread extends VASLThread {

	protected SQLGameMap SQLMap;

	public static final String VSQL = "VSQL";
	public static final String RULE_LEVEL = "rulelevel";
	public static final String SNAP_OPTION = "snapoption";

	public static final String[] rule_levels = new String[] {"SL", "COI", "COD", "GI"};
	
	public VSQLThread() {
		super();


		
		final BooleanConfigurer snapCenter = new BooleanConfigurer(SNAP_OPTION, "Snap Counters To Hex Grid Center Only?");
		final JCheckBox snapBox = findBox(snapCenter.getControls());
		GameModule.getGameModule().getPrefs().addOption(VSQL, snapCenter);
		
//		final StringEnumConfigurer ruleLevel = new StringEnumConfigurer(RULE_LEVEL, "Rule Level", rule_levels);
//		GameModule.getGameModule().getPrefs().addOption(VSQL, ruleLevel);
		
		java.awt.event.ItemListener l = new java.awt.event.ItemListener() {
			public void itemStateChanged(java.awt.event.ItemEvent evt) {
				setSnap(snapBox.isSelected());
			}
		};
		snapBox.addItemListener(l);
		
		

		setSnap();
	}

	protected GameMap createCASLMap(int w, int h) {
		SQLGameMap s = new SQLGameMap(w, h);
		setSnap();
		return s;
	}

	// Catch LOS key release and reset grid snap
	public void mouseReleased(java.awt.event.MouseEvent e) {
		super.mouseReleased(e);
		setSnap();
	}

	/* 
	 * Set snap option - Centers only, or edges also
	 */
	private void setSnap() {

		Prefs prefs = GameModule.getGameModule().getPrefs();
		boolean centerSnapOnly = ((Boolean) prefs.getValue(SNAP_OPTION)).booleanValue();
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