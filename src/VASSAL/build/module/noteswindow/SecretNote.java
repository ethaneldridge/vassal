package VASSAL.build.module.noteswindow;

/*
 * $Id$
 *
 * Copyright (c) 2004 by Rodney Kinney
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

/**
 * Represents a text note with an owner and hidden/revealed status.
 * This is an immutable object
 */
public class SecretNote {
    private String owner;
    private String name;
    private String text;
    private boolean hidden = true;

    public SecretNote(String name, String owner, String text, boolean hidden) {
      this.name = name;
      this.owner = owner;
      this.text = text;
      this.hidden = hidden;
    }

    public boolean isHidden() {
      return hidden;
    }

    public String getName() {
      return name;
    }

    public String getOwner() {
      return owner;
    }

  /**
   * Two SecretNotes with the same owner and name are considered equal
   * @param o
   * @return
   */
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof SecretNote)) return false;

    final SecretNote secretNote = (SecretNote) o;

    if (name != null ? !name.equals(secretNote.name) : secretNote.name != null) return false;
    if (owner != null ? !owner.equals(secretNote.owner) : secretNote.owner != null) return false;

    return true;
  }

  public int hashCode() {
    int result;
    result = (owner != null ? owner.hashCode() : 0);
    result = 29 * result + (name != null ? name.hashCode() : 0);
    return result;
  }

  public String getText() {
      return text;
    }

  }
