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
package VASSAL.build.module.map;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.ColorModel;
import java.awt.image.PixelGrabber;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.MalformedURLException;

import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JWindow;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.border.BevelBorder;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.counters.GamePiece;
import VASSAL.tools.BackgroundTask;
import VASSAL.tools.LaunchButton;

import com.keypoint.PngEncoder;

/**
 * This allows the user to capture a snapshot of the entire map into a PNG file
 */
public class ImageSaver extends AbstractConfigurable {
  private LaunchButton launch;
  private Map map;
  private boolean promptToSplit = false;

  public ImageSaver() {
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        writeMapAsImage();
      }
    };
    launch = new LaunchButton("Capture image", null, HOTKEY, al);
    launch.setToolTipText("Save Map as PNG file");
  }

  public ImageSaver(Map m) {
    map = m;
  }

  /**
   * Expects to be added to a {@link Map}.  Adds a button to the map
   * window toolbar that initiates the capture */
  public void addTo(Buildable b) {
    map = (Map) b;
    map.getToolBar().add(launch);
    java.net.URL image = getClass().getResource("/images/camera.gif");
    if (image != null) {
      launch.setIcon(new ImageIcon(image));
      launch.setText("");
    }
  }

  public void removeFrom(Buildable b) {
    map = (Map) b;
    map.getToolBar().remove(launch);
    map.getToolBar().revalidate();
  }

  private static final String HOTKEY = "hotkey";

  public String[] getAttributeNames() {
    return new String[]{launch.getHotkeyAttribute()};
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Hotkey"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{KeyStroke.class};
  }

  public void setAttribute(String key, Object value) {
    launch.setAttribute(key, value);
  }

  public String getAttributeValueString(String key) {
    return launch.getAttributeValueString(key);
  }

  /**
   * Outputs a snapshot of the Map to a PNG file.  Displays a file
   * dialog to prompt the user for the file */
  public void writeMapAsImage() {
    int sections = 1;
    if (promptToSplit) {
      String s = JOptionPane.showInputDialog("Divide map into how many sections?\n(Using more sections requires less memory)");
      if (s == null) {
        return;
      }
      try {
        sections = Integer.parseInt(s);
      }
      catch (NumberFormatException ex) {
      }
    }
    JFileChooser fc
      = GameModule.getGameModule().getFileChooser();
    fc.setSelectedFile(new File(fc.getCurrentDirectory(),
                                GameModule.getGameModule().getGameName()
                                + "Map.png"));
    if (fc.showSaveDialog(null) !=
      JFileChooser.CANCEL_OPTION) {
      final int sectionCount = sections;
      final String fileName = fc.getSelectedFile().getPath();
      final JWindow w = new JWindow((Frame) SwingUtilities.getAncestorOfClass(Frame.class,map.getView()));
      final JLabel text = new JLabel("Saving Map Image ...");
      text.setFont(new Font("Dialog",Font.PLAIN,48));
      text.setBackground(Color.white);
      text.setForeground(Color.black);
      text.setBorder(new BevelBorder(BevelBorder.RAISED,Color.lightGray,Color.darkGray));
      w.getContentPane().setBackground(Color.white);
      w.getContentPane().add(text);
      w.pack();
      Rectangle r = map.getView().getTopLevelAncestor().getBounds();
      w.setLocation(r.x + r.width / 2 - w.getSize().width / 2, r.y + r.height / 2 - w.getSize().height / 2);
      BackgroundTask task = new BackgroundTask() {
        private Throwable error;

        public void doFirst() {
          try {
            FileOutputStream[] p = new FileOutputStream[sectionCount];
            for (int i = 0; i < sectionCount; ++i) {
              String sectionName = fileName;
              if (sectionCount > 1) {
                if (fileName.lastIndexOf(".") >= 0) {
                  sectionName = fileName.substring(0, fileName.lastIndexOf("."))
                    + (i + 1) + fileName.substring(fileName.lastIndexOf("."));
                }
                else {
                  sectionName = fileName + (i + 1);
                }
              }
              p[i] = new FileOutputStream(sectionName);
            }
            writeImage(p);
          }
          catch (Throwable err) {
            error = err;
          }
        }

        public void doLater() {
          if (error instanceof OutOfMemoryError) {
            JOptionPane.showMessageDialog(map.getView().getTopLevelAncestor(), "Insufficient memory\n"+
                                                                               "Zooming out will reduce memory requirements\n"+
                                                                               "Otherwise, try again and you will be prompted to split the map\n"+
                                                                               "into a number of sections", "Error saving map image", JOptionPane.ERROR_MESSAGE);
            promptToSplit = true;
          }
          else if (error != null) {
            error.printStackTrace();
            String msg = error.getMessage();
            if (msg == null || msg.length() == 0) {
              msg = error.getClass().getName();
              msg = msg.substring(msg.lastIndexOf(".") + 1);
            }
            JOptionPane.showMessageDialog(map.getView().getTopLevelAncestor(), msg, "Error saving map image", JOptionPane.ERROR_MESSAGE);
          }
          w.dispose();
        }
      };
      Timer t = new Timer(1000, new ActionListener() {
        boolean toggle;
        public void actionPerformed(ActionEvent e) {
          if (toggle) {
            text.setText("Saving Map Image");
          }
          else {
            text.setText("Saving Map Image ...");
          }
          toggle = !toggle;
        }
      });
      w.setVisible(true);
      task.start();
      t.start();
    }
  }

  /**
   * Write a PNG-encoded snapshot of the map to the given OutputStreams,
   * dividing the map into vertical sections, one per stream
   */
  public void writeImage(OutputStream[] out) throws IOException {
    Dimension buffer = map.getEdgeBuffer();
    int totalWidth = (int) ((map.mapSize().width - 2 * buffer.width)
      * map.getZoom());
    int totalHeight = (int) ((map.mapSize().height - 2 * buffer.height)
      * map.getZoom());
    for (int i = 0; i < out.length; ++i) {
      int height = totalHeight / out.length;
      if (i == out.length - 1) {
        height = totalHeight - height * (out.length - 1);
      }

      Image output = map.getView().createImage(totalWidth, height);
      Graphics gg = output.getGraphics();
      map.paint(gg, -(int) (map.getZoom() * buffer.width),
                -(int) (map.getZoom() * buffer.height) + height * i);
      try {
        MediaTracker t = new MediaTracker(map.getView());
        t.addImage(output, 0);
        t.waitForID(0);
      }
      catch (Exception e) {
        e.printStackTrace();
      }
//      writeGif(output,out[i]);
      writePNG(output, out[i]);
      out[i].close();
    }
  }

/*
  private void writeGif(Image output, OutputStream out) throws IOException {
    Acme.JPM.Encoders.GifEncoder e = new Acme.JPM.Encoders.GifEncoder(output, out);
    e.encode();
  }
*/

  private void writePNG(Image output, OutputStream out) throws IOException {
    PngEncoder enc = new PngEncoder(output);
    enc.setCompressionLevel(7);
    out.write(enc.pngEncode());
  }

  /**
   * Write a snapshot of the map to the given OutputStream in PPM
   * format.  This results in a larger file (in a less familiar
   * format), but requires less memory */
  public void writePPM(OutputStream p) throws Exception {
    int outW = (int) (map.mapSize().width * map.getZoom());
    int outH = (int) (map.mapSize().height * map.getZoom());

    Dimension buffer = map.getEdgeBuffer();

    int MAXSIZE = 500000;
    int chunk = MAXSIZE / outW;
    int offsetY = (int) (map.getZoom() * buffer.height);
    int offsetX = (int) (map.getZoom() * buffer.width);

    p.write(("P6\n" + outW + " " + outH + "\n255\n").getBytes());

    while (offsetY < outH) {
      if (offsetY + chunk > outH) {
        chunk = outH - offsetY;
      }

      Image output = map.getView().createImage(outW, chunk);
      Graphics gg = output.getGraphics();
      map.drawBoards(gg, offsetX, -offsetY, map.getZoom(), map.getView());
      GamePiece stack[] = map.getPieces();
      for (int i = 0; i < stack.length; ++i)
        stack[i].draw(gg,
                      (int) (map.getZoom()
                             * stack[i].getPosition().x),
                      (int) (map.getZoom()
                             * stack[i].getPosition().y) - offsetY,
                      map.getView(), map.getZoom());

      int pix[] = new int[outW * chunk];

      PixelGrabber pg = new PixelGrabber(output,
                                         0, 0, outW, chunk, true);
      pg.grabPixels();
      pix = (int[]) pg.getPixels();
      ColorModel model = pg.getColorModel();

      byte out[] = new byte[outW * chunk * 3];

      int n = 0, nbyte = 0;
      for (int j = 0; j < chunk; ++j) {
        for (int i = 0; i < outW; ++i) {
          out[nbyte++] = (byte) model.getRed(pix[n]);
          out[nbyte++] = (byte) model.getGreen(pix[n]);
          out[nbyte++] = (byte) model.getBlue(pix[n]);
          n++;
        }
      }
      p.write(out);
      offsetY += chunk;
    }
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "Map.htm"), "#ImageCapture");
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public org.w3c.dom.Element getBuildElement(org.w3c.dom.Document doc) {
    return doc.createElement(getClass().getName());
  }

  public static String getConfigureTypeName() {
    return "Image Capture Tool";
  }

  public String getConfigureName() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }
}
