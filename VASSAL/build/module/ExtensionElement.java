/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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

import VASSAL.build.*;
import VASSAL.configure.PropertiesWindow;
import VASSAL.tools.SequenceEncoder;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import VASSAL.configure.ConfigureTree;

import java.util.Vector;

/**
 * An element of a {@link ModuleExtension} that extends an
 * individual {@link VASSAL.build.Buildable} component of the {@link VASSAL.build.GameModule}
 */
public class ExtensionElement implements Buildable {
  /**
   * An identifier for the component to be extended
   */
  public static final String TARGET = "target";
  private Buildable extension;
  private Configurable[] targetPath;

  public ExtensionElement() {
  }

  public ExtensionElement(Buildable extension, Configurable[] targetPath) {
    this.extension = extension;
    this.targetPath = targetPath;
  }

  public void add(Buildable child) {
    extension = child;
  }

  public void build(Element e) {
    targetPath = getPath(e.getAttribute(TARGET));
    Element childElement = null;
    for (Node n = e.getFirstChild(); n != null; n = n.getNextSibling()) {
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        childElement = (Element) n;
        break;
      }
    }
    if (childElement != null) {
      try {
        extension = Builder.create(childElement);
      }
      catch (Exception e1) {
        String msg = e1.getMessage();
        if (msg == null) {
          msg = e1.getClass().getName().substring(e1.getClass().getName().lastIndexOf('.') + 1);
        }
        throw new IllegalBuildException(msg);
      }
    }
  }

  public Buildable getExtension() {
    return extension;
  }

  public Configurable[] getTargetPath() {
    return targetPath;
  }

  private Configurable[] getPath(String id) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(id, '/');
    Vector v = new Vector();
    if (id.length() > 0) {
      addToPath(GameModule.getGameModule(), st, v);
    }
    Configurable[] path = new Configurable[v.size()];
    for (int i = 0; i < path.length; ++i) {
      path[i] = (Configurable) v.elementAt(i);
    }
    return path;
  }

  private void addToPath(Configurable parent, SequenceEncoder.Decoder st, Vector path) {
    if (st.hasMoreTokens()) {
      String id = st.nextToken();
      String name = null;
      SequenceEncoder.Decoder st2 = new SequenceEncoder.Decoder(id, ':');
      String className = st2.nextToken();
      if (st2.hasMoreTokens()) {
        name = st2.nextToken();
      }
      Configurable[] children = parent.getConfigureComponents();
      Configurable match = null;
      int i = -1;
      while (++i < children.length) {
        if (className.equals(children[i].getClass().getName())) {
          match = children[i];
          if (name == null ? children[i].getConfigureName() == null
            : name.equals(children[i].getConfigureName())) {
            break;
          }
        }
      }
      if (match != null) {
        path.addElement(match);
        addToPath(match, st, path);
      }
      else {
        String msgName = name;
        if (msgName == null) {
          msgName = className.substring(className.lastIndexOf('.') + 1);
        }
        throw new IllegalBuildException("Could not find " + msgName + " in " + VASSAL.configure.ConfigureTree.getConfigureName(parent.getClass()));
      }
    }
  }

  private String getId(Configurable[] targetPath) {
    SequenceEncoder se = new SequenceEncoder('/');
    for (int i = 0; i < targetPath.length; ++i) {
      String name = targetPath[i].getConfigureName();
      SequenceEncoder se2 = new SequenceEncoder(targetPath[i].getClass().getName(), ':');
      if (name != null) {
        se2.append(name);
      }
      se.append(se2.getValue());
    }
    return se.getValue() == null ? "" : se.getValue();
  }

  public Element getBuildElement(Document doc) {
    Element el = doc.createElement(getClass().getName());
    el.setAttribute(TARGET, getId(targetPath));
    el.appendChild(extension.getBuildElement(doc));
    return el;
  }

  public void addTo(Buildable parent) {
    Configurable target = targetPath.length == 0 ? GameModule.getGameModule() : targetPath[targetPath.length - 1];
    extension.addTo(target);
    target.add(extension);
  }
}
