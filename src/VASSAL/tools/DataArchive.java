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

import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.GameModule;
import VASSAL.configure.BooleanConfigurer;

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
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;

/**
 * Wrapper around a Zip archive with methods to cache images
 */
public class DataArchive extends ClassLoader {
  protected ZipFile archive = null;
  protected Vector extensions = new Vector();
  private Hashtable imageCache = new Hashtable();
  private Hashtable scaledImageCache = new Hashtable();
  protected String[] imageNames;
  public static final String IMAGE_DIR = "images/";
  private BooleanConfigurer smoothPrefs;

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
   * Return a scaled instance of the image.  Each zoom factory of each image is stored in a cache
   * @param base
   * @param scale
   * @return
   */
  public Image getScaledImage(Image base, double scale) {
    Dimension d = getImageBounds(base).getSize();
    d.width *= scale;
    d.height *= scale;
    ScaledCacheKey key = new ScaledCacheKey(base,d);
    Image scaled = (Image) scaledImageCache.get(key);
    if (scaled == null) {
      scaled = getScaledInstance(base, d);
      new ImageIcon(scaled); // Wait for the image to load
      scaledImageCache.put(key,scaled);
    }
    return scaled;
  }

  private Image getScaledInstance(Image im, Dimension size) {
    if (smoothPrefs == null) {
      smoothPrefs = (BooleanConfigurer) GameModule.getGameModule().getPrefs().getOption(GlobalOptions.SCALER_ALGORITHM);
      smoothPrefs.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          scaledImageCache.clear();
        }
      });
    }
    int algorithm = Boolean.TRUE.equals(smoothPrefs.getValue()) ? Image.SCALE_AREA_AVERAGING : Image.SCALE_DEFAULT;
    return im.getScaledInstance(size.width,size.height,algorithm);
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

  public static class Scaler {
    static final int IMAGE_ID = 0;
    public static final int SCALE_AREA_AVERAGING = 16;
    public static final int SCALE_SMOOTH = 4;
    public static final int SCALE_FAST = 2;

    private Hashtable scaledImageCache = new Hashtable();

    private int scalingMethod;

    public Scaler() {
    }

    public void clearCache() {
      scaledImageCache.clear();
    }

    private void setScalingMethod() {
      // this can be 16,8,4 or 2 depending on scaling alg
      if (GlobalOptions.getInstance().isAveragedScaling()) {
        scalingMethod = SCALE_AREA_AVERAGING;
      }
      else {
        scalingMethod = SCALE_FAST;
      }
    }

    public Image scale(Image img, double zoom, Component obs) {
      return scaleImage(img, zoom, obs);
    }

    public Image scale(Image img, String pieceID, double zoom, Component obs) {
      if (pieceID != null) {
        // Check whether the scaled image is in the cache
        String hashKey = pieceID + String.valueOf(zoom);
        if (scaledImageCache.containsKey(hashKey)) {
          return (Image) scaledImageCache.get(hashKey);
        }
        img = scaleImage(img, zoom, obs);
        scaledImageCache.put(hashKey, img);
      }
      else {
        img = scaleImage(img, zoom, obs);
      }
      return img;
    }

/*
	private Image JAIscaleImage( Image img,  double zoom, Component obs  )
	{

		// Create an RGB color model
		int[] bits = { 8, 8, 8 };
		ColorModel colorModel = new
				ComponentColorModel(
					ColorSpace.getInstance(ColorSpace.CS_sRGB),
						bits,	false, false,
						Transparency.OPAQUE,
						DataBuffer.TYPE_BYTE);

		// Possible Interpolation methods are (in order of quality):
		// INTERP_NEAREST, INTERP_BILINEAR, INTERP_BICUBIC, INTERP_BICUBIC2
		Interpolation interp = Interpolation.getInstance(
									Interpolation.INTERP_BICUBIC);
		ParameterBlock pb = new ParameterBlock();
		pb.add(img);
		PlanarImage image1 = (PlanarImage)JAI.create("awtImage", pb);

		//		Create the ParameterBlock.
		pb = new ParameterBlock();
		pb.addSource(image1).add(colorModel);
		//		Perform the color conversion.
		PlanarImage image2 = JAI.create("ColorConvert", pb);

				ParameterBlock params = new ParameterBlock();
		params.addSource(image2);
		params.add(new Float(zoom)); // x scale factor
		params.add(new Float(zoom)); // y scale factor
		params.add(0.0F); // x translate
		params.add(0.0F); // y translate
		params.add(interp); // interpolation method

		PlanarImage image3 = (PlanarImage)JAI.create("scale", params);
		img = (Image)image3.getAsBufferedImage();
		return img;
	}
*/
    private Image scaleImage(Image img, double zoom, Component obs) {
      setScalingMethod();
      int width = img.getWidth(obs);
      int height = img.getHeight(obs);
      MediaTracker t = new MediaTracker(obs);
      Image scaled = img;
      try {
        scaled = scaled.getScaledInstance(
            (int) (zoom * width),
            (int) (zoom * height),
            scalingMethod);

        t.addImage(scaled, IMAGE_ID);
        try {
          t.waitForID(IMAGE_ID);
        }
        catch (InterruptedException e) {
        }
      }
      catch (IllegalArgumentException c) {
      }
      try {
        Thread.sleep(2);
      }
      catch (InterruptedException e) {
      }
      return scaled;
    }
  }
  private static class ScaledCacheKey {
    private Image base;
    private Dimension bounds;

    public ScaledCacheKey(Image base, Dimension bounds) {
      this.bounds = bounds;
      this.base = base;
    }

    public boolean equals(Object o) {
      if (this == o) return true;
      if (!(o instanceof ScaledCacheKey)) return false;

      final ScaledCacheKey scaledCacheKey = (ScaledCacheKey) o;

      if (!bounds.equals(scaledCacheKey.bounds)) return false;
      if (!base.equals(scaledCacheKey.base)) return false;

      return true;
    }

    public int hashCode() {
      int result;
      result = base.hashCode();
      result = 29 * result + bounds.hashCode();
      return result;
    }
  }
}
