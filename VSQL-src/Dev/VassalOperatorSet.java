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

import org.nfunk.jep.Operator;
import org.nfunk.jep.OperatorSet;
import org.nfunk.jep.function.Comparative;

/**
 * @author Brent
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class VassalOperatorSet extends OperatorSet {

	public VassalOperatorSet() {
	  super();
	  OP_GT =  new Operator(">",new VassalComparative(Comparative.GT));
	  OP_LT =  new Operator("<",new VassalComparative(Comparative.LT));
	  OP_EQ =  new Operator("==",new VassalComparative(Comparative.EQ));
	  OP_LE =  new Operator("<=",new VassalComparative(Comparative.LE));
	  OP_GE =  new Operator(">=",new VassalComparative(Comparative.GE));
	  OP_NE =  new Operator("!=",new VassalComparative(Comparative.NE));
	}
	
}
