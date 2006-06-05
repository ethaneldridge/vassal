/*
 * $Id$
 *
 * Copyright (c) 2005 by Brent Easton
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

import VASL.build.module.ASLCommandEncoder;
import VASL.counters.Concealable;
import VASL.counters.Concealment;
import VASL.counters.TextInfo;
import VASSAL.build.Buildable;
import VASSAL.counters.Decorator;
import VASSAL.counters.Embellishment;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Hideable;
import VASSAL.counters.ReportState;

/**
 * @author Brent Easton
 */
public class VSQLCommandEncoder extends ASLCommandEncoder {

  /**
   * Create the rule-level preference
   */
  public void addTo(Buildable b) {
    super.addTo(b);
    
  }
  
 /**
  * Primary Command Encoder for VSQL. MarkMoved decorators are intercepted
  * and created as VSQLMarkMoved instead.
  */
  public Decorator createDecorator(String type, GamePiece inner) {
    if (type.startsWith(VSQLFootprint.ID)) {
      return new VSQLFootprint(type, inner);
    }
    else if (type.startsWith(VSQLMarkMoved.ID)) {
      return new VSQLMarkMoved(type, inner);
    }
    else if (type.startsWith(Embellishment.OLD_ID) || type.startsWith(Embellishment.ID)) {
      return new VSQLEmbellishment(type, inner);
    }
    else if (type.startsWith(VSQLTurreted.ID)) {
      return new VSQLTurreted(type, inner);
    }
    else if (type.startsWith(Hideable.ID)) {
      return new VSQLHideable(type, inner);
    }
    else if (type.startsWith(Concealment.ID)) {
      return new VSQLConcealment(type, inner);
    }
    else if (type.startsWith(Concealable.ID)) {
      return new VSQLConcealable(type, inner);
    }
    else if (type.startsWith(ReportState.ID)) {
      return new VSQLReportState(type, inner);
    }
    else if (type.startsWith(TextInfo.ID)) {
      return new VSQLTextInfo(type, inner);
    }
    else if (type.startsWith(TrackRotator.ID)) {
      return new TrackRotator(type, inner);
    }
    else {
      return super.createDecorator(type, inner);
    }
  }
}
