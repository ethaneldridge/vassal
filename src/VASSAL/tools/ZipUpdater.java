/*
 * $Id$
 *
 * Copyright (c) 2003 by Rodney Kinney
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

import javax.swing.*;
import java.io.*;
import java.util.Enumeration;
import java.util.Properties;
import java.util.jar.JarOutputStream;
import java.util.zip.CRC32;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

/**
 * Automatically builds a .jar file that will update a Zip archive.
 * Usage:  java VASSAL.tools.ZipUpdater <oldArchiveName> <newArchiveName>
 * will create a file named update<oldArchiveName>.jar Executing this jar (by double-clicking or
 * typing "java -jar update<oldArchiveName>.jar") will update the old archive so that its contents are identical to
 * the new archive.
 * User: rkinney
 * Date: Oct 23, 2003
 */
public class ZipUpdater {
  public static final String CHECKSUM_RESOURCE = "checksums";
  public static final String TARGET_ARCHIVE = "target";
  public static final String ENTRIES_DIR = "entries/";
  private String inputArchiveName;
  private ZipFile input;
  private Properties checkSums;

  public ZipUpdater(String input) throws IOException {
    this.inputArchiveName = input;
    if (!new File(inputArchiveName).exists()) {
      throw new IOException("Could not find file " + inputArchiveName);
    }
  }

  private long getCrc(ZipFile file, ZipEntry entry) throws IOException {
    long crc = -1;
    if (entry != null) {
      crc = entry.getCrc();
      if (crc < 0) {
        CRC32 checksum = new CRC32();
        InputStream in = file.getInputStream(entry);
        byte[] buffer = new byte[1024];
        int count;
        while ((count = in.read(buffer)) > 0) {
          checksum.update(buffer,0,count);
        }
        crc = checksum.getValue();
      }
    }
    return crc;
  }

  private long copyEntry(ZipOutputStream output, ZipEntry newEntry) throws IOException {
    return writeEntry(input.getInputStream(new ZipEntry(newEntry.getName())), output, newEntry);
  }

  private long replaceEntry(ZipOutputStream output, ZipEntry newEntry) throws IOException {
    return writeEntry(getClass().getResourceAsStream("/"+ENTRIES_DIR + newEntry.getName()), output, newEntry);
  }

  private long writeEntry(InputStream zis, ZipOutputStream output, ZipEntry newEntry) throws IOException {
    ByteArrayOutputStream byteStream = new ByteArrayOutputStream();
    int count = 0;
    byte[] buffer = new byte[1024];
    while ((count = zis.read(buffer, 0, 1024)) >= 0) {
      byteStream.write(buffer, 0, count);
    }
    byte[] contents = byteStream.toByteArray();
    CRC32 checksum = new CRC32();
    checksum.update(contents);
    if (newEntry.getMethod() == ZipEntry.STORED) {
      newEntry.setSize(contents.length);
      newEntry.setCrc(checksum.getValue());
    }
    output.putNextEntry(newEntry);
    output.write(contents, 0, contents.length);
    return checksum.getValue();
  }

  public void write() throws IOException {
    checkSums = new Properties();
    checkSums.load(ZipUpdater.class.getResourceAsStream("/"+CHECKSUM_RESOURCE));

    input = new ZipFile(inputArchiveName);

    File tempFile = File.createTempFile("VSL", ".zip");
    ZipOutputStream output = new ZipOutputStream(new FileOutputStream(tempFile));
    for (Enumeration e = checkSums.keys(); e.hasMoreElements();) {
      String entryName = (String) e.nextElement();
      long targetSum;
      try {
        targetSum = Long.parseLong(checkSums.getProperty(entryName, "<none>"));
      }
      catch (NumberFormatException invalid) {
        throw new IOException("Invalid checksum " + checkSums.getProperty(entryName, "<none>") + " for entry " + entryName);
      }
      ZipEntry entry = input.getEntry(entryName);
      ZipEntry newEntry = new ZipEntry(entryName);
      newEntry.setMethod(entry != null ? entry.getMethod() : ZipEntry.DEFLATED);
      if (targetSum == getCrc(input, entry)) {
        if (targetSum != copyEntry(output, newEntry)) {
          throw new IOException("Checksum mismatch for entry " + entry.getName());
        }
      }
      else {
        if (targetSum != replaceEntry(output, newEntry)) {
          throw new IOException("Checksum mismatch for entry " + entry.getName());
        }
      }
    }
    input.close();
    output.close();

    File f = new File(inputArchiveName);
    int index = inputArchiveName.lastIndexOf(".");
    String backup = index < 0 || index == inputArchiveName.length() - 1
      ? inputArchiveName + "Backup" : inputArchiveName.substring(0, index) + "Backup" + inputArchiveName.substring(index);
    if (!f.renameTo(new File(backup))) {
      throw new IOException("Unable to create backup file " + backup + ".\nUpdated file is in " + tempFile.getPath());
    }
    if (!tempFile.renameTo(f)) {
      throw new IOException("Unable to write to file " + inputArchiveName + ".\nUpdated file is in " + tempFile.getPath());
    }
  }

  public void createUpdater(String goalArchiveName) throws IOException {
    checkSums = new Properties();
    int index = inputArchiveName.indexOf(".");
    String jarName;
    if (index >= 0) {
      jarName = "update" + inputArchiveName.substring(0, index) + ".jar";
    }
    else {
      jarName = "update" + inputArchiveName;
    }
    input = new ZipFile(inputArchiveName);
    ZipFile goal = new ZipFile(goalArchiveName);
    JarOutputStream out = new JarOutputStream(new FileOutputStream(jarName));
    for (Enumeration e = goal.entries(); e.hasMoreElements();) {
      ZipEntry entry = (ZipEntry) e.nextElement();
      long goalCrc = getCrc(goal, entry);
      long inputCrc = getCrc(input, input.getEntry(entry.getName()));
      if (goalCrc != inputCrc) {
        ZipEntry outputEntry = new ZipEntry(ENTRIES_DIR + entry.getName());
        outputEntry.setMethod(entry.getMethod());
        writeEntry(goal.getInputStream(entry), out, outputEntry);
      }
      checkSums.put(entry.getName(), goalCrc + "");
    }

    ZipEntry manifestEntry = new ZipEntry("META-INF/MANIFEST.MF");
    manifestEntry.setMethod(ZipEntry.DEFLATED);
    StringBuffer buffer = new StringBuffer();
    buffer.append("Manifest-Version: 1.0\n")
      .append("Main-Class: VASSAL.tools.ZipUpdater\n");
    writeEntry(new ByteArrayInputStream(buffer.toString().getBytes()), out, manifestEntry);

    ZipEntry nameEntry = new ZipEntry(TARGET_ARCHIVE);
    nameEntry.setMethod(ZipEntry.DEFLATED);
    writeEntry(new ByteArrayInputStream(inputArchiveName.getBytes()), out, nameEntry);

    ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
    checkSums.store(byteOut, null);
    ZipEntry sumEntry = new ZipEntry(CHECKSUM_RESOURCE);
    sumEntry.setMethod(ZipEntry.DEFLATED);
    writeEntry(new ByteArrayInputStream(byteOut.toByteArray()), out, sumEntry);

    String className = getClass().getName().replace('.', '/') + ".class";
    ZipEntry classEntry = new ZipEntry(className);
    classEntry.setMethod(ZipEntry.DEFLATED);
    writeEntry(getClass().getResourceAsStream("/" + className), out, classEntry);

    out.close();
  }

  public static void main(String[] args) {
    String archiveName = "<unknown>";
    try {
      if (args.length > 1) {
        String base = args[0];
        String goal = args[1];
        ZipUpdater updater = new ZipUpdater(base);
        updater.createUpdater(goal);
      }
      else {
        BufferedReader r = new BufferedReader(new InputStreamReader(ZipUpdater.class.getResourceAsStream("/"+TARGET_ARCHIVE)));
        archiveName = r.readLine();
        ZipUpdater updater = new ZipUpdater(archiveName);
        updater.write();
      }
    }
    catch (IOException e) {
      e.printStackTrace();
      JOptionPane.showMessageDialog(null, "Unable to update " + archiveName + ".\n" + e.getMessage(), "Update failed", JOptionPane.ERROR_MESSAGE);
    }
  }

}
