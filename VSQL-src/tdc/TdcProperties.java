/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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
 
package tdc;

/**
 * Define the Properties/Markers used in TDC
 * @author Brent Easton
 *
 */
public interface TdcProperties {
  
  public static final String STEP = "Step";            // Step Count Marker
  public static final String ACTIVE = "Active";        // Unit can be Activated Marker
  public static final String 
  	IS_INDEPENDENT = "isIndependent";                  // Units belongs to Indepndent Formation Marker
 
  public static final String RANGE = "Range";          // Command or Fire Range Marker
  public static final String RANGE_TYPE = "RangeType"; // Range Type Marker
  public static final String ARTILLERY = "Artillery";  // Artillery Range Type
  public static final String MORTAR = "Mortar";        // Mortar Range Type

  public static final String ARMY = "Army";           // Owning Army Marker
  public static final String FORMATION = "Formation"; // Owning Formation Marker
  public static final String FORMATION2 = "Formation2"; // Secondary command Formation Marker
  public static final String DIVISION = "Division";   // Owning Division Marker
  public static final String DIVISION2 = "Division2";   // Secondary command Division Marker

  public static final String TYPE = "Type";         // Unit Type Marker
  public static final String BRIDGE = "Bridge";     // Bridge Type
  public static final String CHIT = "Chit";         // Chit Type
  public static final String ENGINEER = "Engineer"; // Engineer Type
  
  public static final String CLASS = "Class";       // Unit Class Marker
  public static final String LEADER = "Leader";     // Leader Class
  public static final String INFANTRY = "Infantry"; // Infantry Class
  public static final String VEHICLE = "Vehicle";   // Vehicle Class
  public static final String GUN = "Gun";           // Gun Class
  
  public static final String MOVE = "Move";   // Movement type Marker
  public static final String LEG = "Leg";     // Leg Movement
  public static final String TRACK = "Track"; // Track Movement
  public static final String WHEEL = "Wheel"; // Wheel Movement

}
