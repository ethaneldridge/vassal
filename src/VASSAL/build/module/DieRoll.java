/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Brent Easton
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
package VASSAL.build.module;


/**
 * @author Brent Easton
 *
 * Describes a single roll of one or more identical dice.
 * For use with internet dice rollers
 */
public class DieRoll {

  protected String description = "";
  protected int numDice;
  protected int numSides;
  protected int plus;
  protected boolean reportTotal;
  protected int[] result;

  public DieRoll(String d, int dice, int sides, int add, boolean r) {
    description = d;
    numDice = dice;
    numSides = sides;
    plus = add;
    reportTotal = r;
    result = new int[dice];
  }


  public DieRoll(String d, int dice, int sides, int add) {
    this(d, dice, sides, add, false);
  }

  public DieRoll(String d, int dice, int sides) {
    this(d, dice, sides, 0);
  }

  public int[] getResults() {
    return result;
  }

  public int getResult(int pos) {
    return result[pos];
  }

  public void setResult(int pos, int res) {
    result[pos] = res;
  }

  public void setNumDice(int nd) {
    numDice = nd;
    result = new int[nd];
  }

  public void refresh() {
    if (numDice != result.length) {
      result = new int[numDice];
    }
  }
}