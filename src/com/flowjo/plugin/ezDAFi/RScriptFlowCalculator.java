package com.flowjo.plugin.ezDAFi;

/*
    Modified: MiguelV
    Date:   Nov th 2017
    Description:
    The RFlowCalculator class executes a script using the R path in the preferences.
    This class extends RFlowCalculator to use Rscript or Rscript.exe instead of
    R CMD BATCH on Linux based systems or R.exe in Windows machines.

    Reason: The R community has recommend we use Rscript instead of R CMD BATCH
    Speed and depecration.
 */

import java.awt.*;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import com.treestar.flowjo.engine.EngineManager;
import com.treestar.flowjo.engine.FEML;
import com.treestar.flowjo.engine.utility.RFlowCalculator;
import com.treestar.lib.file.FJFileRef;
import com.treestar.lib.file.FJFileRefFactory;
import com.treestar.lib.fjml.FJML;
import com.treestar.lib.fjml.types.FileTypes;
import com.treestar.lib.parsing.interpreter.ParseUtil;

public class RScriptFlowCalculator extends RFlowCalculator {
    public static final String R_PATH_CANT_FIND = "Could not find R, please install R and set the path in your preferences.";
    public static final String R_PATH_CANT_FIND_OR_EXECUTE = "R path in preferences can't be found or can't be executed.";
    public static final String UNKNOWN_OS = "Unknown OS; don't know how to find R.";
    public static final String FAILED_EXECUTE = "Failed to execute command.";
    public static final String FINISHED_EXECUTING = "Finished executing script, process returned ";
    public static final String CANNOT_EXE_EMPTY_COMMANDS = "RFlowCalculator cannot execute empty commands.";
    public static final String SPACE = " ";
    public static final String UNIX_FILE_SEPARATOR = "/";
    public static final String WINDOWS_FILE_SEPARATOR = "\\";
    public static final String VANILLA_OPTION = " --vanilla";
    public static final String SLAVE_OPTION = " --slave";

    public static final String[] winRPathsToTest = {
            "C:\\Program Files\\R",
            "C:\\Program Files (x86)\\R",
            "D:\\Program Files\\R",
            "D:\\Program Files (x86)\\R"
    };

    public static final String[] unixRPathsToTest = {
            "/Library/Frameworks/R.framework/Resources/bin/R",
            "/usr/local/bin/R",
            "/usr/bin/R",
            "/bin/R",
            "/usr/lib64/R",
            "/usr/include/R",
            "/usr/share/R"
    };

    protected static boolean verbose = true;  // Not really used (replaced by non-static field) but keeping for backwards compatibility
    protected String fScriptOutput = "";
    protected File fScriptFile = null;
    protected static File fOutFile = null;
    protected static File fOutFileLastLines = null;
    protected File fScriptFileTempLink = null;

    protected boolean fRScriptMode = false;   // Default is R CMD BATCH (backwards compatible), but Rscript is an option if a plugin chooses that instead
    protected boolean fVerboseOutput = true;  // Do we want verbose debug output (yes by default... it's not that verbose)
    protected boolean fDebugOutput = true;    // Do we want debug output (yes by default)
    protected boolean fAddVanilla = true;     // Should we be adding --vanilla to the script (normally we should)

    public RScriptFlowCalculator() {
    }

    public void setRScriptMode(boolean rScriptMode) {
        fRScriptMode = rScriptMode;
    }

    public void setVerboseOutput(boolean verboseOutput) {
        fVerboseOutput = verboseOutput;
    }

    public void setDebugOutput(boolean debugOutput) {
        fRScriptMode = debugOutput;
    }

    public void setAddVanilla(boolean addVanilla) {
        fAddVanilla = addVanilla;
    }

    public void executeRBatch(File scriptFile) {
        fScriptFile = scriptFile;
        executeRBatch(fScriptFile.getName());
    }

    protected void executeRBatch(String name) {
        List<String> cmds = new ArrayList<>();
        cmds = buildRCMDs();
        executeCMDs(cmds, true);
        deleteTempSymLink(fScriptFileTempLink);
    }

    private boolean executeCMDs(List<String> arguments, boolean captureOutput) {
        if (arguments == null || arguments.isEmpty()) {
            System.err.println(CANNOT_EXE_EMPTY_COMMANDS);
        }

        String[] cmd = null;
        boolean ret = false;

        try {
            String allArgs = "";
            allArgs = arguments.stream().map((str) -> str + " ").reduce(allArgs, String::concat);

            if (isWindows())
                cmd = new String[]{"cmd", "/C", allArgs};
            else
                cmd = new String[]{"/bin/sh", "-c", allArgs};

            int result = -99;

            ProcessBuilder pb = new ProcessBuilder(cmd);
            pb.redirectErrorStream(true);
            Process process = pb.start();
            BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
            String line;
            StringBuilder outputBuilder = new StringBuilder();
            while ((line = reader.readLine()) != null) {
                outputBuilder.append(line);
                if (fDebugOutput) System.out.println(line);
            }
            if (captureOutput)
                fScriptOutput = outputBuilder.toString();

            try {
                result = process.waitFor();
            } catch (InterruptedException ex) {
            }

            if (fDebugOutput) {
                System.out.println(FINISHED_EXECUTING + result);
            }

            ret = true;
        } catch (Exception e) {
            System.err.println(FAILED_EXECUTE);
            if (cmd != null)
                for (String cmd1 : cmd) {
                    System.err.print(cmd1 + " ");
                }
            System.err.println();
            e.printStackTrace();
        }

        return ret;
    }

    public static boolean isWindows() {
        return EngineManager.isWindows() || EngineManager.isWindows7() || EngineManager.isVista();
    }

    public static boolean isUNIX() {
        return EngineManager.isMac() || EngineManager.isLinux();
    }

    public static boolean endswithRWindows(String path) {
        if (path == null || !path.contains(File.separator))
            return false;
        String last = path.substring(path.lastIndexOf(File.separator), path.length());
        last = last.toLowerCase();
        return last.endsWith("r.exe") || last.endsWith("rscript.exe");
    }

    public static boolean endswithRUNIX(String path) {
        if (path == null || !path.contains(File.separator))
            return false;
        String last = path.substring(path.lastIndexOf(File.separator), path.length());
        last = last.toLowerCase();
        return last.endsWith("r") || last.endsWith("rscript");
    }

    private boolean checkRExecutable(File rPath) {
        return rPath != null && rPath.exists() && rPath.canExecute();
    }

    private File checkRPath(File path) {
        if (path == null)
            return null;

        String rPath = path.getAbsolutePath();

        if (isWindows()) {
            if (!endswithRWindows(rPath)) {
                if (!rPath.endsWith(File.separator)) {
                    rPath += File.separator;
                }
                rPath += "R.exe";
            }

            if (fRScriptMode) {
                if (rPath.endsWith("R.exe")) {
                    rPath = rPath.replaceAll("R.exe", "Rscript.exe");
                } else if (rPath.endsWith("r.exe")) {
                    rPath = rPath.replaceAll("r.exe", "Rscript.exe");
                }
            } else {
                if (rPath.endsWith("Rscript.exe")) {
                    rPath = rPath.replaceAll("Rscript.exe", "R.exe");
                } else if (rPath.endsWith("rscript.exe")) {
                    rPath = rPath.replaceAll("rscript.exe", "R.exe");
                }
            }
        } else {
            if (isUNIX()) {
                if (!endswithRUNIX(rPath)) {
                    if (!rPath.endsWith(File.separator)) {
                        rPath += File.separator;
                    }
                    rPath += "R";
                }

                if (fRScriptMode) {
                    if (rPath.endsWith("R")) {
                        rPath += "script";
                    }
                } else {
                    if (rPath.endsWith("script")) {
                        rPath = rPath.replaceAll("rscript", "r");
                    }
                }
            }
        }

        File rPathFile = new File(rPath);
        if (checkRExecutable(rPathFile)) {
            return rPathFile;
        }
        return path;
    }

    public static String findRInDefaultLocations() {

        if (isWindows()) {
            for (String Rpath : winRPathsToTest) {
                try {
                    File file = new File(Rpath);
                    if (file.exists() && file.isDirectory()) {
                        File[] rDirs = file.listFiles(new FilenameFilter() {
                            @Override
                            public boolean accept(File current, String name) {
                                return name.startsWith("R") && new File(current, name).isDirectory();
                            }
                        });
                        try {
                            for (File rDir : rDirs) {
                                File rBinDir = new File(rDir, "bin");
                                if (rBinDir.exists() && rBinDir.isDirectory()) {
                                    File rBin = new File(rBinDir, "R.exe");
                                    if (rBin.isFile() && rBin.canExecute()) {
                                        return rBin.getAbsolutePath();
                                    }
                                }
                            }
                        } catch (Exception e) {
                        }
                        ;
                    }
                } catch (Exception e) {
                }
                ;
            }
        } else {
            for (String Rpath : unixRPathsToTest) {
                try {
                    File file = new File(Rpath);
                    if (file.isFile() && file.canExecute()) {
                        return file.getAbsolutePath();
                    }
                } catch (Exception e) {
                }
                ;
            }
        }

        return null;
    }

    public static String findExecutableOnPath(String name) {
        for (String dirname : System.getenv("PATH").split(File.pathSeparator)) {
            File file = new File(dirname, name);
            if (file.isFile() && file.canExecute()) {
                return file.getAbsolutePath();
            }
        }
        return null;
    }

    @SuppressWarnings("unused")
    private String getRPath() {
        File path = null;
        if (isWindows()) {
            String rExe = EngineManager.getInstance().getRExe();
            if (fDebugOutput && fVerboseOutput)
                System.out.println("EngineManager instance getRExe() called from getRPath() returned " + rExe);
            if (rExe == null || rExe.isEmpty()) {
                rExe = EngineManager.getInstance().getRPath();
                if (fDebugOutput && fVerboseOutput)
                    System.out.println("EngineManager instance getRPath() called from getRPath() returned " + rExe);
            }
            if (rExe == null || rExe.isEmpty()) {
                rExe = findRInDefaultLocations();
                if (fDebugOutput && fVerboseOutput)
                    System.out.println("findRInDefaultLocations returned " + rExe);
                if (rExe == null || rExe.isEmpty()) {
                    rExe = findExecutableOnPath("R.exe");
                    if (fDebugOutput && fVerboseOutput)
                        System.out.println("findExecutableOnPath returned " + rExe);
                    if (rExe == null || rExe.isEmpty()) {
                        System.err.println(R_PATH_CANT_FIND_OR_EXECUTE);
                        return null;
                    }
                }
            }
            File rExeFile = new File(rExe);
            if (checkRExecutable(rExeFile)) {
                rExeFile = checkRPath(rExeFile);
                if (checkRExecutable(rExeFile)) {
                    return rExeFile.getAbsolutePath();
                }
            } else {
                System.err.println(R_PATH_CANT_FIND_OR_EXECUTE);
            }
        } else if (isUNIX()) {
            String rPath = EngineManager.getInstance().getRPath();
            if (fDebugOutput && fVerboseOutput)
                System.out.println("EngineManager instance getRPath() called from getRPath() returned " + rPath);
            if (rPath == null || rPath.isEmpty()) {
                rPath = findRInDefaultLocations();
                if (fDebugOutput && fVerboseOutput)
                    System.out.println("findRInDefaultLocations returned " + rPath);
                if (rPath == null || rPath.isEmpty()) {
                    rPath = findExecutableOnPath("R");
                    if (fDebugOutput && fVerboseOutput)
                        System.out.println("findExecutableOnPath returned " + rPath);
                    if (rPath == null || rPath.isEmpty()) {
                        System.err.println(R_PATH_CANT_FIND_OR_EXECUTE);
                        return null;
                    }
                }
            }
            File rPathFile = new File(rPath);
            if (checkRExecutable(rPathFile)) {
                rPathFile = checkRPath(rPathFile);
                if (checkRExecutable(rPathFile)) {
                    return rPathFile.getAbsolutePath();
                }
            } else {
                System.err.println(R_PATH_CANT_FIND_OR_EXECUTE);
                return null;
            }
        } else {
            System.err.println(UNKNOWN_OS);
            return null;
        }

        if (path == null) {
            System.err.println(R_PATH_CANT_FIND);
            return null;
        }

        if (path.exists() && path.canExecute())
            return path.getAbsolutePath();

        return null;
    }

    public static String wrapQuotes(String path) {
        if (isWindows()) {
            String cleanPath = "";
            String[] paths = path.split("\\\\");
            for (int i = 0; i < paths.length; i++) {
                String split = paths[i];
                if (split.contains(SPACE)) {
                    cleanPath += "\"" + split + "\"";
                } else {
                    cleanPath += split;
                }
                if (i < paths.length - 1) {
                    cleanPath += "\\\\";
                }
            }
            return cleanPath;
        } else if (isUNIX()) {
            return "\"" + path + "\"";
        }
        return path;
    }

    private List<String> buildRCMDs() {
        List<String> cmds = new ArrayList<>();
        String path = getRPath();
        if (path == null) {
            return null;
        }
        if (isWindows()) {
            cmds.add(wrapQuotes(path));
        } else {
            cmds.add((path));
        }
        if (fRScriptMode) {
            cmds.add(" " + wrapQuotes(fScriptFile.getAbsolutePath()));
            if (fAddVanilla)
                cmds.add(VANILLA_OPTION);
        } else {


            fScriptFileTempLink = createTempSymLink(fScriptFile);

            fOutFile = new File(fScriptFile.getAbsolutePath() + ".txt"); // changed ".Rout" to "R.txt" to allow auto open with default txt editor

            //fOutFileLastLines created to try to write the last few lines of the R script into a separate file that opens every time the plugin runs
            //this may facilitate communicating errors to the user.
            fOutFileLastLines = new File(fScriptFile.getAbsolutePath() + ".LastLines.txt"); // changed ".Rout" to "R.txt" to allow auto open with default txt editor

            cmds.add(" CMD BATCH");
            if (fScriptFileTempLink == null)
                cmds.add(" " + wrapQuotes(fScriptFile.getAbsolutePath())); // R CMD BATCH "/x/path with space/x.R" "xx.Rout" still doesn't work with some R versions... cannot have the space
            else
                cmds.add(" " + wrapQuotes(fScriptFileTempLink.getAbsolutePath())); // So using a symlink instead.

            cmds.add(" " + wrapQuotes(fOutFile.getAbsolutePath()));
            if (fAddVanilla)
                cmds.add(VANILLA_OPTION);
            if (isWindows()) {
                cmds.add(SLAVE_OPTION);
            }
        }

        if (fDebugOutput) {
            cmds.forEach((cmd) ->
            {
                System.out.println(cmd);
            });
        }
        return cmds;
    }

    // Works for OSX (and Linux) only
    private File createTempSymLink(File scriptFile) {
        if (isWindows()) return null;

        try {
            Path path;
            path = Files.createTempFile("RFlowCalculator" + System.currentTimeMillis(), ".R");
            File ret = path.toFile();
            if (ret.exists()) {
                try {
                    ret.delete();    // Only need the path, don't want the file to be there.
                } catch (Exception x) {
                }
                ;
            }

            List<String> cmds = new ArrayList<>();
            cmds.add(" ln -s");
            cmds.add(" " + wrapQuotes(scriptFile.getAbsolutePath()));
            cmds.add(" " + wrapQuotes(ret.getAbsolutePath()));

            if (fDebugOutput) {
                cmds.forEach((cmd) ->
                {
                    System.out.println(cmd);
                });
            }
            executeCMDs(cmds, false);
            return ret;
        } catch (Exception e) {
            e.printStackTrace();
        }

        return null;
    }

    // Works for OSX (and Linux) only
    private void deleteTempSymLink(File scriptFileTempLink) {
        if (isWindows() || scriptFileTempLink == null) return;

        try {
            List<String> cmds = new ArrayList<>();
            cmds.add(" rm -f");
            cmds.add(" " + wrapQuotes(scriptFileTempLink.getAbsolutePath()));

            if (fDebugOutput) {
                cmds.forEach((cmd) ->
                {
                    System.out.println(cmd);
                });
            }
            executeCMDs(cmds, false);
        } catch (Exception e) {
            e.printStackTrace();
        }

    }


    public void deleteScriptFile() {
        if (fScriptFile != null && fScriptFile.exists() && fScriptFile.delete() && fVerboseOutput)
            System.out.println("Deleted " + fScriptFile.getAbsolutePath());
    }

    public void deleteROutFile() {
        if (fOutFile != null && fOutFile.exists() && fOutFile.delete() && fVerboseOutput)
            System.out.println("Deleted " + fOutFile.getAbsolutePath());
    }

    public File renameROutFile() {
        if (fOutFile == null || !fOutFile.exists())
            return null;
        String fileName = fOutFile.getAbsolutePath();
        fileName = fileName.replaceAll(".R.txt", FileTypes.TXT_SUFFIX);
        File newFile = new File(fileName);
        fOutFile.renameTo(newFile);
        fOutFile = newFile;
        return fOutFile;
    }

    public FJFileRef generateFinalCsvFile(FJFileRef clusterNumFileRef, FJFileRef origEventNumFileRef, int numEvents, String popName, String outputFolder, boolean isMultiColumn) throws IOException {
        File clusterNumFile = clusterNumFileRef.getLocalFile();
        File origEventNumFile = origEventNumFileRef.getLocalFile();
        if (!clusterNumFile.exists() || !origEventNumFile.exists())
            return null;
        String noClust = Integer.toString(0);
        File outFile = new File(outputFolder, popName + FEML.EPA_Suffix);
        Writer output = new BufferedWriter(new FileWriter(outFile));
        BufferedReader clusterNumFileReader = new BufferedReader(new FileReader(clusterNumFile));
        BufferedReader origEventNumFileReader = new BufferedReader(new FileReader(origEventNumFile));
        String clusterVal = clusterNumFileReader.readLine();
        String origEventNumVal = origEventNumFileReader.readLine();
        int colCt = 0;
        int columnIndex = 0;
        if (isMultiColumn) {
            StringTokenizer tokenizer = new StringTokenizer(origEventNumVal, ",");
            while (tokenizer.hasMoreTokens()) {
                String token = tokenizer.nextToken();
                if (token.contains(FJML.EventNumberDP))
                    break;
                colCt++;
            }
            columnIndex = colCt;
        }

        int ct = 0;
        while ((clusterVal = clusterNumFileReader.readLine()) != null) {
            origEventNumVal = origEventNumFileReader.readLine();
            if (isMultiColumn) {
                colCt = 0;
                StringTokenizer tokenizer = new StringTokenizer(origEventNumVal, ",");
                while (tokenizer.hasMoreTokens()) {
                    String token = tokenizer.nextToken();
                    if (colCt == columnIndex) {
                        origEventNumVal = token;
                        break;
                    }
                    colCt++;
                }
            }
            int clusterNum = (int) ParseUtil.getDouble(clusterVal);
            int origEventNum = (int) ParseUtil.getDouble(origEventNumVal);
            while (ct < origEventNum - 1) {
                output.write(noClust);
                output.write("\n");
                ct++;
            }
            output.write(Integer.toString(clusterNum * 100));
            output.write("\n");
            ct++;
        }
        while (ct < numEvents) {
            output.write(noClust);
            output.write("\n");
            ct++;
        }
        clusterNumFileReader.close();
        origEventNumFileReader.close();
        output.close();

        FJFileRef fjFileRef = null;
        try {
            fjFileRef = FJFileRefFactory.make(outFile.getAbsolutePath());
            if (outFile.exists()) {
                clusterNumFile.delete();
                if (!isMultiColumn)
                    origEventNumFile.delete();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return fjFileRef;
    }

    private boolean fWriteHeaderForDerivedParameterCSVFile = true;

    public void setWriteHeaderForDerivedParameterCSVFile(boolean b) {
        fWriteHeaderForDerivedParameterCSVFile = b;
    }

    public FJFileRef generateDerivedParameterCSVFile(FJFileRef inputCSVFileRef, int numEvents, String popName, String outputFolder) throws IOException {
        File inputCSVFile = inputCSVFileRef.getLocalFile();
        if (!inputCSVFile.exists()) // return early if input file does not exist
            return null;

        BufferedReader inputCSVFileReader = new BufferedReader(new FileReader(inputCSVFile));
        // read the first header line of the input CSV sample file
        String csvLine = inputCSVFileReader.readLine();
        // determine which column is the event number column
        int eventNumColumnIndex = -1; // the column index of the event number column
        int colCt = 0;
        String headerLine = ""; // the column header to write in the new file
        String noEventLine = ""; // the line to write when there is no derived parameter value
        StringTokenizer tokenizer = new StringTokenizer(csvLine, ",");
        while (tokenizer.hasMoreTokens()) {
            String token = tokenizer.nextToken();
            if (token.contains(FJML.EventNumberDP))
                eventNumColumnIndex = colCt;
            else if (eventNumColumnIndex >= 0) {
                headerLine += token + ",";
                noEventLine += "0,";
            }
            colCt++;
        }
        if (headerLine.endsWith(",")) // get rid of trailing comma of header line
            headerLine = headerLine.substring(0, headerLine.length() - 1);
        headerLine += "\n";
        if (noEventLine.endsWith(",")) // get rid of trailing comma of no parameter value line
            noEventLine = noEventLine.substring(0, noEventLine.length() - 1);
        noEventLine += "\n";

        File outFile = new File(outputFolder, popName + FEML.EPA_Suffix);
        Writer output = new BufferedWriter(new FileWriter(outFile));
        if (fWriteHeaderForDerivedParameterCSVFile)
            output.write(headerLine);
        int ct = 0;
        int eventNum = 0;
        String separator = colCt == 2 ? "" : ","; // if only 2 columns, one is event number, other is single column, so don't need the trailing comma
        while ((csvLine = inputCSVFileReader.readLine()) != null) {
            tokenizer = new StringTokenizer(csvLine, ",");
            colCt = 0;
            String line = "";
            while (tokenizer.hasMoreTokens()) {
                String token = tokenizer.nextToken();
                if (colCt == eventNumColumnIndex) // get the event number as integer
                    eventNum = (int) ParseUtil.getDouble(token);
                else if (colCt > eventNumColumnIndex) // it's a column after the event number, make it a derived parameter value
                    line += token + separator;
                colCt++;
            }
            if (eventNum < 0)
                break;
            while (ct < eventNum - 1 && ct < numEvents) {
                output.write(noEventLine);
                ct++;
            }
            output.write(line);
            output.write("\n");
            ct++;
        }
        while (ct < numEvents) {
            output.write(noEventLine);
            ct++;
        }
        inputCSVFileReader.close();
        output.close();

        FJFileRef fjFileRef = null;
        try {
            fjFileRef = FJFileRefFactory.make(outFile.getAbsolutePath());
        } catch (IOException e) {
            e.printStackTrace();
        }
        return fjFileRef;
    }

}