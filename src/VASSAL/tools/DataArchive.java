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
package VASSAL.tools;

import VASSAL.build.module.documentation.HelpFile;

import javax.swing.*;
import java.awt.*;
import java.io.*;
import java.net.URL;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;

/**
 * Wrapper around a Zip archive with methods to cache images
 */
public class DataArchive extends ClassLoader {
  protected ZipFile archive = null;
  protected Vector extensions = new Vector();
  private Hashtable imageCache = new Hashtable();
  protected String[] imageNames;
  public static final String IMAGE_DIR = "images/";
  MediaTracker tracker = new MediaTracker(new JLabel());

  protected DataArchive() {
  }

  public DataArchive(String zipName) throws IOException {
    archive = new ZipFile(zipName);
  }

  public String getName() {
    return archive == null ? "data archive" : archive.getName();
  }

  public ZipFile getArchive() {
    return archive;
  }

  public static Image findImage(File zip, String file) throws IOException {
    return getImage(getFileStream(zip, file));
  }

  public static InputStream getFileStream(File zip, String file) throws IOException {
    try {
      ZipFile z = new ZipFile(zip);
      return z.getInputStream(z.getEntry(file));
    }
    catch (Exception e) {
      throw new IOException("Couldn't locate " + file + " in " + zip.getName()
                            + ": " + e.getMessage());
    }
  }

  public static Image findImage(File dir, String zip, String file)
    throws IOException {
    /*
     ** Looks for entry "file" in ZipFile "zip" in directory "dir"
     ** If no such zipfile, look for "file" in "dir"
     */
    if ((new File(dir, zip)).exists()) {
      return getImage(getFileStream(dir, zip, file));
    }
    else if ((new File(dir, file)).exists()) {
      return Toolkit.getDefaultToolkit().getImage
        (dir.getPath() + File.separatorChar + file);
    }
    else {
      throw new IOException("Image " + file + " not found in " + dir
                            + File.separator + zip);
    }
  }

  /*
   ** Find an image from the archive
   * Once an image is found, cache it in Hashtable
   */
  public Image getCachedImage(String file) throws IOException {
    file = IMAGE_DIR + file;
    if (imageCache.get(file) != null) {
      return (Image) imageCache.get(file);
    }
    else {
      Image im = getImage(getFileStream(file));
      imageCache.put(file, im);
      return im;
    }
  }

  /**
   *
   * @param im
   * @return the boundaries of this image, where (0,0) is the center of the image
   */
  public static Rectangle getImageBounds(Image im) {
    ImageIcon icon = new ImageIcon(im);
    return new Rectangle(-icon.getIconWidth()/2,-icon.getIconHeight()/2,icon.getIconWidth(),icon.getIconHeight());
  }

/*
  private Shape getImageShape(String imageName) {
    Shape s = (Shape) imageShapes.get(imageName);
    if (s == null) {
      Area a = new Area();
      try {
        Image im = getCachedImage(imageName);
        ImageIcon icon = new ImageIcon(im);
        int width = icon.getIconWidth();
        int height = icon.getIconHeight();
        int[] pixels = new int[width * height];
        PixelGrabber pg = new PixelGrabber(im, 0, 0, width, height, pixels, 0, width);
        long time = System.currentTimeMillis();
        pg.grabPixels();
        System.err.println("Grab "+imageName+" took "+(System.currentTimeMillis()-time));
        time = System.currentTimeMillis();
        for (int j = 0; j < height; ++j) {
          for (int i = 0; i < width; ++i) {
            if (((pixels[i + j * width] >> 24) & 0xff) > 0) {
              a.add(new Area(new Rectangle(i, j, 1, 1)));
            }
          }
        }
        System.err.println("Build shape "+imageName+" took "+(System.currentTimeMillis()-time));
      }
      catch (IOException e) {
      }
      catch (InterruptedException e) {

      }
      s = a;
      imageShapes.put(imageName,s);
    }
    return s;
  }
*/

  public void unCacheImage(String file) {
    imageCache.remove(IMAGE_DIR + file);
  }

  public static Image getImage(InputStream in) throws IOException {
    return Toolkit.getDefaultToolkit().createImage(getBytes(in));
  }

  /**
   * Read all available bytes from the given InputStream
   */
  public static byte[] getBytes(InputStream in) throws IOException {
    BufferedInputStream bufIn = new BufferedInputStream(in);
    int nLen = bufIn.available();
    int nCurBytes = 0;
    byte buffer[] = null;
    byte abyte0[] = new byte[nLen];

    while ((nCurBytes = bufIn.read(abyte0, 0, abyte0.length)) > 0) {
      if (buffer == null) {
        buffer = new byte[nCurBytes];
        System.arraycopy(abyte0, 0, buffer, 0, nCurBytes);
      }
      else {
        byte oldbuf[] = buffer;

        buffer = new byte[oldbuf.length + nCurBytes];

        System.arraycopy(oldbuf, 0, buffer, 0, oldbuf.length);
        System.arraycopy(abyte0, 0, buffer, oldbuf.length, nCurBytes);
      }
    }
    return buffer != null ? buffer : new byte[0];
  }

  /**
   * Get an inputstream from the given filename in the archive
   */
  public InputStream getFileStream(String file) throws IOException {
    InputStream stream = null;
    ZipEntry entry = archive.getEntry(file);
    if (entry != null) {
      stream = archive.getInputStream(entry);
    }
    else {
      for (int i = 0; i < extensions.size() && stream == null; ++i) {
        DataArchive ext = (DataArchive) extensions.elementAt(i);
        try {
          stream = ext.getFileStream(file);
        }
        catch (IOException e) {
          // Not found in this extension.  Try the next.
        }
      }
    }
    if (stream == null) {
      throw new IOException("\'" + file + "\' not found in " + archive.getName());
    }
    return stream;
  }

  public URL getURL(String fileName) throws IOException {
    if (archive == null) {
      throw new IOException("Must save before accessing contents");
    }
    String archiveURL = HelpFile.toURL(new File(archive.getName())).toString();
    return new URL("jar:" + archiveURL + "!/" + fileName);
  }

  /**
   * DataArchives can extend other archives.  The extensions will be searched for data if not found in the parent archive
   * @param ext the extension
   */
  public void addExtension(DataArchive ext) {
    extensions.addElement(ext);
  }

  /**
   * Return the writeable instance of DataArchive, either this or one of its extensions
   * (At most one archive should be being edited at a time)
   * @return
   */
  public ArchiveWriter getWriter() {
    ArchiveWriter writer = null;
    if (this instanceof ArchiveWriter) {
      writer = (ArchiveWriter) this;
    }
    else {
      for (Enumeration e = extensions.elements(); e.hasMoreElements();) {
        Object o = e.nextElement();
        if (o instanceof ArchiveWriter) {
          writer = (ArchiveWriter) o;
          break;
        }
      }
    }
    return writer;
  }

  public static InputStream getFileStream(File dir, String zipName, String file) {
    try {
      if ((new File(dir, zipName)).exists()) {
        ZipFile zip = new ZipFile(new File(dir, zipName));
        return zip.getInputStream(zip.getEntry(file));
      }
      else {
        return new FileInputStream(new File(dir, file));
      }
    }
    catch (Exception e) {
      return null;
    }
  }

  public synchronized Class loadClass(String name,
                                      boolean resolve) throws ClassNotFoundException {
    Class c;
    try {
      c = findSystemClass(name);
    }
    catch (Exception noClass) {
      c = findLoadedClass(name);
    }
    if (c == null) {
      return findClass(name);
    }
    if (resolve) {
      resolveClass(c);
    }
    return c;
  }

  protected Class findClass(String name) throws ClassNotFoundException {
    try {
      String slashname = name.replace('.', '/');
      InputStream in = getFileStream(slashname + ".class");
      byte[] data = getBytes(in);
      return defineClass(slashname, data, 0, data.length);
    }
    catch (IOException e) {
      throw new ClassNotFoundException("Unable to load " + name + "\n" + e.getMessage());
    }
  }

  public String[] getImageNames() {
    if (isNameCacheStale()) {
      Vector v = new Vector();
      listImageNames(v);
      Sort.quicksort(v, new Sort.Alpha());
      imageNames = new String[v.size()];
      for (int i = 0; i < imageNames.length; ++i) {
        imageNames[i] = (String) v.elementAt(i);
      }
    }
    return imageNames;
  }

  protected boolean isNameCacheStale() {
    boolean isStale = imageNames == null;
    for (Enumeration e = extensions.elements(); e.hasMoreElements() && !isStale;) {
      isStale = ((DataArchive) e.nextElement()).imageNames == null;
    }
    return isStale;
  }

  /**
   * Place the names of the image files stored in this DataArchive into the argument Vector
   * @param v
   */
  protected void listImageNames(Vector v) {
    if (archive != null) {
      try {
        ZipInputStream zis
          = new ZipInputStream(new FileInputStream(archive.getName()));

        ZipEntry entry = null;
        while ((entry = zis.getNextEntry()) != null) {
          if (entry.getName().startsWith(IMAGE_DIR)) {
            v.addElement(entry.getName().substring(IMAGE_DIR.length()));
          }
        }
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }
    for (Enumeration e = extensions.elements(); e.hasMoreElements();) {
      ((DataArchive) e.nextElement()).listImageNames(v);
    }
  }
}
