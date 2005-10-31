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
 
package Dev;

import org.nfunk.jep.ParseException;
import org.nfunk.jep.function.Comparative;

/**
 * @author Brent
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class VassalComparative extends Comparative {

	public VassalComparative(int id_in)
	{
		super(id_in);
	}
	
	public boolean lt(Object param1, Object param2)	throws ParseException {
	  return super.lt(param1, param2);
	}

	public boolean gt(Object param1, Object param2)	throws ParseException {
	  return super.lt(param1, param2);
	}
	
	public boolean le(Object param1, Object param2)	throws ParseException {
	  return super.le(param1, param2);
	}
	
	public boolean ge(Object param1, Object param2)	throws ParseException {
	  return super.ge(param1, param2);
	}
	
	public boolean eq(Object param1, Object param2)	throws ParseException {
	  return super.eq(param1, param2);
	}
	public boolean ne(Object param1, Object param2)	throws ParseException {
	  return super.ne(param1, param2);
	}
}
