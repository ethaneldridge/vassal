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

import VASSAL.build.GameModule;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.BooleanConfigurer;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.AffineTransform;
import java.awt.image.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.*;
import java.net.URL;
import java.security.AllPermission;
import java.security.CodeSource;
import java.security.PermissionCollection;
import java.security.SecureClassLoader;
import java.util.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;

/**
 * Wrapper around a Zip archive with methods to cache images
 */
public class DataArchive extends SecureClassLoader {
  protected ZipFile archive = null;
  protected Vector extensions = new Vector();
  private Hashtable imageCache = new Hashtable();
  private Hashtable scaledImageCache = new Hashtable();
  protected String[] imageNames;
  public static final String IMAGE_DIR = "images/";
  private BooleanConfigurer smoothPrefs;
  private CodeSource cs;

  protected DataArchive() {
    super(DataArchive.class.getClassLoader());
  }

  public DataArchive(String zipName) throws IOException {
    this();
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
   * Return a scaled instance of the image.
   * The image will be retrieved from cache if available, cached otherwise
   * @param base
   * @param scale
   * @return
   */
  public Image getScaledImage(Image base, double scale) {
    return getScaledImage(base, scale, false, true);
  }

  /**
   * Return a scaled instance of the image, optionally rotated by 180 degrees.
   * The image will be retrieved from cache if available, cached otherwise
   * @param base
   * @param scale
   * @param reversed
   * @param forceSmoothing If true, force smoothing.  This usually yields better results, but can be slow for large images
   * @return
   */
  public Image getScaledImage(Image base, double scale, boolean reversed, boolean forceSmoothing) {
    if (base == null) {
      return null;
    }
    Dimension d = getImageBounds(base).getSize();
    d.width *= scale;
    d.height *= scale;
    ScaledCacheKey key = new ScaledCacheKey(base, d, reversed);
    Image scaled = (Image) scaledImageCache.get(key);
    if (scaled == null) {
      scaled = createScaledInstance(base, d, reversed, forceSmoothing);

      new ImageIcon(scaled); // Wait for the image to load
      scaledImageCache.put(key, scaled);
    }
    return scaled;
  }

  /**
   * Create a new scaled instance of the argument image, optionally reversed
   * @param im
   * @param size
   * @param reversed
   * @param forceSmoothing If true, force a smoothing algorithm.  May be too slow for large images
   * @return
   */
  protected Image createScaledInstance(Image im, Dimension size, boolean reversed, boolean forceSmoothing) {
    Dimension fullSize = getImageBounds(im).getSize();
    if (fullSize.equals(size) && !reversed) {
      return im;
    }
    if (smoothPrefs == null) {
      smoothPrefs = (BooleanConfigurer) GameModule.getGameModule().getPrefs().getOption(GlobalOptions.SCALER_ALGORITHM);
      if (smoothPrefs == null) {
        smoothPrefs = new BooleanConfigurer(null, null, Boolean.TRUE);
      }
      smoothPrefs.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          scaledImageCache.clear();
        }
      });
    }
    boolean smooth = Boolean.TRUE.equals(smoothPrefs.getValue());
    if (reversed) {
      BufferedImage rev = new BufferedImage(size.width, size.height, BufferedImage.TYPE_4BYTE_ABGR);
      Graphics2D g2d = rev.createGraphics();
      if (smooth) {
        g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
      }
      double zoom = (double) size.width / fullSize.width;
      AffineTransform t = AffineTransform.getRotateInstance(Math.PI, size.width * .5, size.height * .5);
      t.scale(zoom, zoom);
      g2d.drawImage(im, t, null);
      return rev;
    }
    else if (smooth && forceSmoothing) {
      return improvedScaling(im, size.width, size.height);
    }
    else {
      return im.getScaledInstance(size.width, size.height, smooth ? Image.SCALE_AREA_AVERAGING : Image.SCALE_DEFAULT);
    }
  }

  /**
   *
   * @param im
   * @return the boundaries of this image, where (0,0) is the center of the image
   */
  public static Rectangle getImageBounds(Image im) {
    ImageIcon icon = new ImageIcon(im);
    return new Rectangle(-icon.getIconWidth() / 2, -icon.getIconHeight() / 2, icon.getIconWidth(), icon.getIconHeight());
  }

  public Image improvedScaling(Image img, int width, int height) {
    ImageFilter filter;

    filter = new ImprovedAveragingScaleFilter(img.getWidth(null),
                                              img.getHeight(null),
                                              width, height);

    ImageProducer prod;
    prod = new FilteredImageSource(img.getSource(), filter);
    return Toolkit.getDefaultToolkit().createImage(prod);
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

  public void unCacheImage(Image im) {
    ArrayList toClear = new ArrayList();
    for (Iterator iterator = scaledImageCache.keySet().iterator(); iterator.hasNext();) {
      ScaledCacheKey key = (ScaledCacheKey) iterator.next();
      if (im.equals(key.base)) {
        toClear.add(key);
      }
    }
    for (Iterator iterator = toClear.iterator(); iterator.hasNext();) {
      ScaledCacheKey scaledCacheKey = (ScaledCacheKey) iterator.next();
      scaledImageCache.remove(scaledCacheKey);
    }
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
    URL url = null;
    ZipEntry entry = archive.getEntry(fileName);
    if (entry != null) {
      String archiveURL = HelpFile.toURL(new File(archive.getName())).toString();
      url = new URL("jar:" + archiveURL + "!/" + fileName);
    }
    else {
      for (int i = 0; i < extensions.size() && url == null; ++i) {
        DataArchive ext = (DataArchive) extensions.elementAt(i);
        try {
          url = ext.getURL(fileName);
        }
        catch (IOException e) {
          // Not found in this extension.  Try the next.
        }
      }
    }
    if (url == null) {
      throw new IOException("\'" + fileName + "\' not found in " + archive.getName());
    }
    return url;
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
//      c = findSystemClass(name);
      c = Class.forName(name);
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

  protected PermissionCollection getPermissions(CodeSource codesource) {
    PermissionCollection p = super.getPermissions(codesource);
    p.add(new AllPermission());
    return p;
  }

  protected Class findClass(String name) throws ClassNotFoundException {
    if (cs == null) {
      cs = new CodeSource(null, null);
    }
    try {
      String slashname = name.replace('.', '/');
      InputStream in = getFileStream(slashname + ".class");
      byte[] data = getBytes(in);
      return defineClass(name, data, 0, data.length, cs);
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

  private static class ScaledCacheKey {
    private Image base;
    private Dimension bounds;
    private boolean reversed;

    public ScaledCacheKey(Image base, Dimension bounds, boolean reversed) {
      this.bounds = bounds;
      this.base = base;
      this.reversed = reversed;
    }

    public boolean equals(Object o) {
      if (this == o) return true;
      if (!(o instanceof ScaledCacheKey)) return false;

      final ScaledCacheKey scaledCacheKey = (ScaledCacheKey) o;

      if (reversed != scaledCacheKey.reversed) return false;
      if (!base.equals(scaledCacheKey.base)) return false;
      if (!bounds.equals(scaledCacheKey.bounds)) return false;

      return true;
    }

    public int hashCode() {
      int result;
      result = base.hashCode();
      result = 29 * result + bounds.hashCode();
      result = 29 * result + (reversed ? 1 : 0);
      return result;
    }
  }
}
