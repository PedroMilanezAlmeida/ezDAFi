package com.flowjo.plugin.DAFi.utils;

import com.opencsv.CSVReader;
import com.opencsv.CSVWriter;
import com.opencsv.exceptions.CsvException;
import com.treestar.flowjo.application.engine.EngineRequestElement;
import com.treestar.flowjo.core.nodes.PopNode;
import com.treestar.flowjo.engine.FEML;
import com.treestar.flowjo.engine.Query;
import com.treestar.flowjo.engine.SimpleQueryCallback;
import com.treestar.flowjo.engine.utility.ExternalPopulationAlgorithmUtility;
import com.treestar.lib.fjml.FJML;
import com.treestar.lib.xml.SElement;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class ExportUtils {


    public static File appendPopulationNumber(File file, String popName, double number) {
        try {
//            System.out.println("Start addind pop number");
            CSVReader reader = new CSVReader(new BufferedReader(new FileReader(file)));
//            System.out.println("Read file");
            List<String[]> lines = reader.readAll();
//            System.out.println("Lines :"+lines.size());
            List<String[]> newLines = new ArrayList<>();
            for (int i1 = 0; i1 < lines.size(); i1++) {
                String[] line = lines.get(i1);
                String[] newLine = new String[line.length + 1];
                int i;
                for (i = 0; i < line.length; i++) {
                    String string = line[i];
                    newLine[i] = string;
                }
                if (i1 == 0) {
                    newLine[i++] = "Class";
                } else {
                    newLine[i++] = number + "";
                }
                newLines.add(newLine);
            }
            newLines.remove(newLines.size() - 1);
            File newFile = new File(file.getAbsolutePath().replace(".csv", "").trim() + "." + popName + ".csv");
            CSVWriter csvWriter = new CSVWriter(new FileWriter(newFile));
            csvWriter.writeAll(newLines, false);
            csvWriter.flushQuietly();
            reader.close();
            if (file.exists() && newFile.exists()) {
                file.delete();
            }
            return newFile;
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (CsvException e) {
            e.printStackTrace();
        }
        return null;
    }

    public static boolean startsWithDashOne(String string) {
        return string.startsWith("1_");
    }

    public static File removeLeadingDashOneFromCellId(File file) {
        try {
            CSVReader reader = new CSVReader(new BufferedReader(new FileReader(file)));
            List<String[]> lines = reader.readAll();
            List<String[]> newLines = new ArrayList<>();
            for (int i1 = 0; i1 < lines.size(); i1++) {
                String[] line = lines.get(i1);
//                String[] newLine = new String[line.length + 1];
                String string = line[0];
                if (startsWithDashOne(string)) {
                    line[0] = string.substring(2, string.length());
                } else {
                    line[0] = string;
                }

                newLines.add(line);
            }
            File newFile = new File(file.getAbsolutePath().replace(".csv", "").trim() + ".cleaned.csv");
            CSVWriter csvWriter = new CSVWriter(new FileWriter(newFile));
            csvWriter.writeAll(newLines, false);
            csvWriter.flushQuietly();
            reader.close();
            if (file.exists() && newFile.exists()) {
                file.delete();
            }
            return newFile;
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (CsvException e) {
            e.printStackTrace();
        }
        return null;
    }

    public static File exportParameters(PopNode pop, List<String> parameters, File outputFolder) {
        if (pop == null) {
            return null;
        }
        if (pop.getCount() > 0) {
            if (outputFolder == null) {
                try {
                    Path tempDirWithPrefix = Files.createTempDirectory("ServerTmpFolder");
                    outputFolder = tempDirWithPrefix.toFile();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }

            if (outputFolder == null) {
                return null;
            }
            File outFile = FileUtils.getRandomCSV(outputFolder);
            outFile.setWritable(true);
            outFile.setReadable(true);
            File sampleFile = pop.getSample().getLocalFileRef();
            sampleFile.setReadable(true);

            EngineRequestElement fcmlElem = new EngineRequestElement("Export");

            if (parameters.contains("SampleID")) {
                parameters.remove("SampleID");
                System.out.println("Removed sampleID");
            }

            int numEvents = pop.getCount();
            ExternalPopulationAlgorithmUtility util = new ExternalPopulationAlgorithmUtility();
            SElement exportRequest = util.composeParameterExportQuery(sampleFile, outFile, fcmlElem, numEvents, parameters, null);

            SElement fcmlQueryElement = exportRequest.findAnyChildWithName("FcmlQuery");
            fcmlQueryElement.addContent(pop.getSample().getDataSetTag());

            SElement gateElement = new SElement(FEML.Gate);
            if (!pop.isSampleNode()) {
                SElement gateQueryElement = new SElement("Tmp");
                pop.writeGateQuery(gateQueryElement, null);
                gateElement = gateQueryElement.getChild(FJML.Gate);
                gateElement.setString(FJML.path, pop.getAnalysisPathString());
            }
            fcmlQueryElement.addContent(gateElement);

            SimpleQueryCallback callback = new SimpleQueryCallback();
            Query query = new Query(exportRequest, callback);
            query.setNoCaching();
            query.executeQuery();

            if (outFile.exists()) {
                return outFile;
            }

        }
        return null;
    }

    // COMMENTED OUT THE CODE BELOW SINCE FlowJoUtils.getPopulationNodesByIds IS NOT AVAILABLE.

    public static List exportPopulationsFiles(String workspaceID, String sampleID, List<String> populationIDs, List<String> parameterIDs, File outputFolder) {
        List<PopNode> populationNodes = FlowJoUtils.getPopulationNodesByIds(workspaceID, sampleID, populationIDs);
      return exportPopulationsFiles(populationNodes, parameterIDs, outputFolder);
    }

    public static List exportPopulationsFiles(List<PopNode> populationNodes, List<String> parameterIDs, File outputFolder) {
      List<String> exportedFiles = new ArrayList<>();
      for (int i = 0; i < populationNodes.size(); i++) {
          PopNode popNode = populationNodes.get(i);
          File outputFile = ExportUtils.exportParameters(popNode, parameterIDs, outputFolder);
          if (outputFile != null && outputFile.exists() && outputFile.isFile()) {
              outputFile.setReadable(true);
              String popName = popNode.getName();
              File annotatedFile = ExportUtils.appendPopulationNumber(outputFile, popName, i);
              if (annotatedFile != null && annotatedFile.exists()) {
                  annotatedFile.setReadable(true);
                  exportedFiles.add(annotatedFile.getAbsolutePath());
              }
          }
      }
      return exportedFiles;
    }

}


