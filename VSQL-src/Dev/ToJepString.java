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

import java.util.Stack;

import org.nfunk.jep.ParseException;
import org.nfunk.jep.function.PostfixMathCommand;

/**
 * @author Brent
 * 
 * TODO To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
public class ToJepString extends PostfixMathCommand {

  public void run(Stack inStack) throws ParseException {

    // check the stack
    checkStack(inStack);

    // get the parameter from the stack
    Object param = inStack.pop();

    // check whether the argument is of the right type
    if (param instanceof Double) {
      // calculate the result
      String s = ConditionalMarker.doubleToString((Double) param);
      // push the result on the inStack
      inStack.push(s);
    }
    else if (param instanceof String) {
      inStack.push(param);
    }
    else {
      throw new ParseException("Invalid parameter type");
    }
  }

}
