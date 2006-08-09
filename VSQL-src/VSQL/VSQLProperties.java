/*
 * $Id$
 * 
 * Copyright (c) 2005 by Brent Easton
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

public class VSQLProperties {
  
  public static final String VSQL = "VSQL";
  
  // Preferences
  public static final String SNAP_OPTION = "snapoption";
  public static final String ZOOM_LEVELS = "zoomLevels";
  public static final String ZOOM_START = "zoomStart";
  public static final String ZOOM_FACTOR = "zoomFactor";
  public static final String RULE_LEVEL = "rulelevel";
  public static final String SL = "SL";
  public static final String COI = "COI";
  public static final String COD = "COD";
  public static final String GI = "GI";
  public static final String[] RULE_LEVELS = new String[] { SL, COI, COD, GI };
  
  // Political
  public static final String SIDE = "SIDE";
  public static final String NATION = "NATION";
  
  // Night Vision public properties
  public static final String NV_SIDE = "NVS";
  public static final String NV_RANGE = "NVR";
  public static final String LIGHT = "LIGHT";
  
  // Vehicle CA
  public static final String VEHICLE_CA = "VEHICLE_CA";
  
  // Number of Squads/Crews stacked with a leader
  public static final String STACKED_COUNT = "STACKED_COUNT";
  
  // Unit Types
  public static final String UNIT_TYPE = "TYPE";
  public static final String VEHICLE = "Vehicle";
  public static final String INFANTRY = "Infantry";
  public static final String GUN = "Gun";
  public static final String SW = "SW";
  
  // Unit Sub-types
  public static final String UNIT_SUB_TYPE = "STYPE";
  public static final String LEADER = "Leader";
  public static final String SQUAD = "Squad";
  public static final String CREW = "Crew";
  public static final String SNIPER = "Sniper";
  public static final String TRUCK_OR_JEEP = "Truck-Jeep";
  public static final String ARMORED_CAR = "Armored Car";
  public static final String MOTOR_CYCLE = "Motor Cycle";
  public static final String BUNKER = "Bunker";
}