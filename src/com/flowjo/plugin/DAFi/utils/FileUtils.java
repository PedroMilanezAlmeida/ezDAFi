package com.flowjo.plugin.DAFi.utils;

import com.opencsv.CSVReader;
import com.opencsv.CSVWriter;
import org.apache.commons.io.FilenameUtils;

import javax.swing.*;
import java.awt.*;
import java.io.*;
import java.util.HashMap;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

public class FileUtils {

    public static HashMap<String, String> getFileExtensionsMap() {
        HashMap<String, String> fileExtensionsMap = new HashMap<>();
        fileExtensionsMap.put(com.flowjo.plugin.DAFi.utils.FJSML.FORMATS.FILE.FCS.UPPERCASE, com.flowjo.plugin.DAFi.utils.FJSML.FORMATS.FILE.FCS.EXTENSION);
        fileExtensionsMap.put(com.flowjo.plugin.DAFi.utils.FJSML.FORMATS.FILE.CSV.UPPERCASE, com.flowjo.plugin.DAFi.utils.FJSML.FORMATS.FILE.CSV.EXTENSION);
        fileExtensionsMap.put(com.flowjo.plugin.DAFi.utils.FJSML.FORMATS.FILE.TSV.UPPERCASE, com.flowjo.plugin.DAFi.utils.FJSML.FORMATS.FILE.TSV.EXTENSION);
        fileExtensionsMap.put(com.flowjo.plugin.DAFi.utils.FJSML.FORMATS.FILE.TXT.UPPERCASE, com.flowjo.plugin.DAFi.utils.FJSML.FORMATS.FILE.TXT.EXTENSION);
        fileExtensionsMap.put(com.flowjo.plugin.DAFi.utils.FJSML.FORMATS.FILE.H5.UPPERCASE, com.flowjo.plugin.DAFi.utils.FJSML.FORMATS.FILE.H5.EXTENSION);
        return fileExtensionsMap;
    }

    public static String cleanExtensions(String filePath) {
        try {
            File file = new File(filePath);
            if (file == null) return null;
            return cleanExtensions(file);
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }
        return null;
    }

    public static String cleanExtensions(File file) {
        try {
            if (file == null) return null;
            String fileName = file.getName();
            for (String extension : getFileExtensionsMap().values()) {
                fileName = fileName.replace(extension, "");
            }
            return fileName;
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }
        return null;
    }

    public static File createTempDirectory()
            throws IOException {
        final File temp;

        temp = File.createTempFile("temp", Long.toString(System.nanoTime()));

        if (!(temp.delete())) {
            throw new IOException("Could not delete temp file: " + temp.getAbsolutePath());
        }

        if (!(temp.mkdir())) {
            throw new IOException("Could not create temp directory: " + temp.getAbsolutePath());
        }

        return (temp);
    }

    public static File getRandomCSV(File outputFolder) {
        return new File(outputFolder, StringUtils.getSaltString() + com.flowjo.plugin.DAFi.utils.FJSML.FORMATS.FILE.CSV.EXTENSION);
    }

    public static File getRandomCSV(File outputFolder, String prefix) {
        return new File(outputFolder, prefix + StringUtils.getSaltString() + com.flowjo.plugin.DAFi.utils.FJSML.FORMATS.IMAGE.PNG.EXTENSION);
    }

    public static File writeCSV(List<String[]> data, File outputFolder, String prefix) {
        return writeCSV(data, getRandomCSV(outputFolder, prefix));
    }

    public static File writeCSV(List<String[]> data, File csvFile) {
        try (Writer writer = new FileWriter(csvFile);
             CSVWriter csvWriter = new CSVWriter(writer, CSVWriter.DEFAULT_SEPARATOR, CSVWriter.NO_QUOTE_CHARACTER,
                     CSVWriter.DEFAULT_ESCAPE_CHARACTER, CSVWriter.DEFAULT_LINE_END)) {
            csvWriter.writeAll(data);
            if (csvFile.exists()) {
                return csvFile;
            }
        } catch (IOException ex) {
            System.out.println(ex.getMessage());
        }
        return null;
    }

    public static void openFile(File file) {
        try {
            Desktop.getDesktop().open(file);
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }

    }

    public static File checkFileExtension(File selectedFile, String fileExtension) {
        if (!FilenameUtils.getExtension(selectedFile.getName()).equalsIgnoreCase(fileExtension)) {
            selectedFile = new File(selectedFile.toString() + "." + fileExtension);
        }
        return selectedFile;
    }

    public static File getDesktopFile() {
        File desktop = new File(System.getProperty(com.flowjo.plugin.DAFi.utils.FJSML.SYSTEM_ATTRIBUTES.USER_HOME), com.flowjo.plugin.DAFi.utils.FJSML.SYSTEM_ATTRIBUTES.DESKTOP);
        if (!desktop.exists()) {
            desktop = new File("");
        }
        return desktop;
    }

    public static JFileChooser getFileChooser() {
        final JFileChooser fileChooser = new JFileChooser();
        fileChooser.setCurrentDirectory(FileUtils.getDesktopFile());
        return fileChooser;
    }

    public static File askCSVFile() {
        final JFileChooser fileChooser = new JFileChooser();
        fileChooser.setCurrentDirectory(FileUtils.getDesktopFile());
        int returnVal = fileChooser.showOpenDialog(null);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            return fileChooser.getSelectedFile();
        }
        return null;
    }

    public static List<String[]> readCSV(File csvFile) {
        try {
            CSVReader reader = new CSVReader(new FileReader(csvFile));
            List<String[]> allRows = reader.readAll();
            return allRows;
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }

        return null;
    }

    public static File zipFile(File fileToZip) throws IOException {
        File zippedFile = new File(fileToZip.getParentFile(), fileToZip.getName() + ".zip");
        FileOutputStream fos = new FileOutputStream(zippedFile);
        ZipOutputStream zipOut = new ZipOutputStream(fos);

        zipFileUtil(fileToZip, fileToZip.getName(), zipOut);
        zipOut.close();
        fos.close();
        return zippedFile;
    }

    public static void zipFileUtil(File fileToZip, String fileName, ZipOutputStream zipOut) throws IOException {
        if (fileToZip.isHidden()) {
            return;
        }
        if (fileToZip.isDirectory()) {
            if (fileName.endsWith("/")) {
                zipOut.putNextEntry(new ZipEntry(fileName));
                zipOut.closeEntry();
            } else {
                zipOut.putNextEntry(new ZipEntry(fileName + "/"));
                zipOut.closeEntry();
            }
            File[] children = fileToZip.listFiles();
            for (File childFile : children) {
                zipFileUtil(childFile, fileName + "/" + childFile.getName(), zipOut);
            }
            return;
        }
        FileInputStream fis = new FileInputStream(fileToZip);
        ZipEntry zipEntry = new ZipEntry(fileName);
        zipOut.putNextEntry(zipEntry);
        byte[] bytes = new byte[1024];
        int length;
        while ((length = fis.read(bytes)) >= 0) {
            zipOut.write(bytes, 0, length);
        }
        fis.close();
    }

    public static File getRandomFile(File outputFolder) {
        return new File(outputFolder, StringUtils.getSaltString(6));
    }

    public static File getRandomFile(File outputFolder, String prefix) {
        return new File(outputFolder, prefix + StringUtils.getSaltString(8));
    }

    public static File getRandomPNG() {
        return getRandomPNG(getDesktopFile(), StringUtils.getSaltString(6));
    }

    public static File getRandomPNG(File outputFolder) {
        return getRandomPNG(outputFolder, StringUtils.getSaltString(6));
    }

    public static File getRandomPNG(File outputFolder, String prefix) {
        return new File(outputFolder, prefix.replace("\\", "_").replace(" ", "").replace("\\", "_").replace("/", "_").trim() + com.flowjo.plugin.DAFi.utils.FJSML.FORMATS.IMAGE.PNG.EXTENSION);

    }
}
