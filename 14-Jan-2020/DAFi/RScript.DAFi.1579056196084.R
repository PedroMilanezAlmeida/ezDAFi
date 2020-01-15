
tryCatch(suppressMessages(library("foreach")),
         error = function(e){
           install.packages(pkgs =  "foreach",
                            repos = 'http://cran.us.r-project.org')
           suppressMessages(library("foreach"))
         })
tryCatch(suppressMessages(library("dplyr")),
         error = function(e){
           install.packages(pkgs =  "dplyr",
                            repos = 'http://cran.us.r-project.org')
           suppressMessages(library("dplyr"))
         })
tryCatch(suppressMessages(library("XML")),
         error = function(e){
           install.packages(pkgs =  "XML",
                            repos = 'http://cran.us.r-project.org')
           suppressMessages(library("XML"))
         })
tryCatch(suppressMessages(library("FlowSOM")),
         error = function(e){
           if (!requireNamespace("BiocManager",
                                 quietly = TRUE))
             install.packages("BiocManager",
                              repos = 'http://cran.us.r-project.org')
           BiocManager::install("FlowSOM",
                                update = FALSE,
                                ask = FALSE)
           suppressMessages(library("FlowSOM"))
         })
tryCatch(suppressMessages(library("flowWorkspace")),
         error = function(e){
           if (!requireNamespace("BiocManager",
                                 quietly = TRUE))
             install.packages("BiocManager",
                              repos = 'http://cran.us.r-project.org')
           BiocManager::install("flowWorkspace",
                                update = FALSE,
                                ask = FALSE)
           suppressMessages(library("flowWorkspace"))
         })
tryCatch(suppressMessages(library("CytoML")),
         error = function(e){
           if (!requireNamespace("BiocManager",
                                 quietly = TRUE))
             install.packages("BiocManager",
                              repos = 'http://cran.us.r-project.org')
           BiocManager::install("CytoML",
                                update = FALSE,
                                ask = FALSE)
           suppressMessages(library("CytoML"))
         })
tryCatch(suppressMessages(library("flowUtils")),
         error = function(e){
           if (!requireNamespace("BiocManager",
                                 quietly = TRUE))
             install.packages("BiocManager",
                              repos = 'http://cran.us.r-project.org')
           BiocManager::install("flowUtils",
                                update = FALSE,
                                ask = FALSE)
           suppressMessages(library("flowUtils"))
         })


populationName <- "DAFi_CD3..CD45.subset"
minPopSize <- 500

## Code to read gates from wsp file
popOfInt <- populationName
popOfInt

#find and load wsp file
wspNames <- dirname(path = "C:/Users/pedro/OneDrive/Desktop/DAFi/14-Jan-2020/DAFi/T4  Monogenic-disease-HC_036-003.DAFi_CD3..CD45.subset.ExtNode.csv") %>%
  dirname(path = .) %>%
  paste0(.,
         ".wsp")
wspNames
ws <- openWorkspace(wspNames)
##find raw .fcs files
#find path of all fcs files in workspace
sampleFCS_paths <- xpathApply(ws@doc,
                              file.path("/Workspace/SampleList/Sample","DataSet"),
                              function(x)
                                xmlGetAttr(x,"uri") %>%
                                gsub(pattern = "%20", replacement = " ", x = .) %>%
                                gsub(pattern = "file:", replacement = "", x = .)) %>%
  unlist
sampleFCS_paths
sampleFCS_names <- sampleFCS_paths %>%
  basename(.) %>%
  gsub(pattern = ".fcs",
       replacement = "",
       x = .)
sampleFCS_names
#find name of fcs file used here
nameSearch <- sapply(sampleFCS_names,
                     function(name)
                       grep(pattern = name,
                            x = basename("C:/Users/pedro/OneDrive/Desktop/DAFi/14-Jan-2020/DAFi/T4  Monogenic-disease-HC_036-003.DAFi_CD3..CD45.subset.ExtNode.csv"))) %>%
  unlist(.)
nameSearchRes <- names(nameSearch)[nameSearch %>%
                                     names(.) %>% 
                                     nchar(.) %>% 
                                     which.max(.)]
sampleFCS <- paste0(nameSearchRes,
                    ".fcs")
#sampleFCS <- basename("C:/Users/pedro/OneDrive/Desktop/DAFi/14-Jan-2020/DAFi/T4  Monogenic-disease-HC_036-003.DAFi_CD3..CD45.subset.ExtNode.csv") %>%
#  gsub(pattern = excess.sampleFCS,
#       replacement = "",
#       x = .,
#       fixed = TRUE) %>%
#  paste0(.,
#         ".fcs")
#sampleFCS
#find path to fcs file used here
sampleFCS_path <- sampleFCS_paths[basename(sampleFCS_paths) == sampleFCS]
sampleFCS_path

if(Sys.info()["sysname"] == "Windows"){
  sampleFCS_path <- substring(sampleFCS_path, 2)
}

#parse wsp and fcs files into a GatingSet object

pathFCS <- tryCatch(
  data.frame(sampleID = getSamples(ws)$sampleID[getSamples(ws)$name == sampleFCS],
             file = sampleFCS_path),
  error = function(e) {
    FIL <- read.FCS(sampleFCS_path)@description$`$FIL`
    data.frame(sampleID = getSamples(ws)$sampleID[getSamples(ws)$name == FIL],
               file = sampleFCS_path)
  })

gs <- parseWorkspace(ws,
                     name = 1,
                     path = pathFCS,
                     isNcdf = FALSE)

#TODO: check for special characters in gates names and stop calc

orig.parNames <- getData(gs[[1]]) %>%
  parameters(.) %>%
  pData(.) %>%
  .$name

parNames <- c("FSC-A","SSC-A","FJComp-R710-A","FJComp-R780-A","FJComp-V450-A","FJComp-V545-A","FJComp-V605-A","FJComp-V655-A","FJComp-V800-A","FJComp-G610-A","FJComp-B515-A","FJComp-B710-A","FJComp-R660-A","FJComp-G560-A","FJComp-G660-A","FJComp-G710-A","FJComp-G780-A")

## In CSV files, the parameter names are often like FJComp-xxx while in parNames we may be getting Comp-dsfdsdxxx
FJCompToComp <- function(char_vec) {
  if (any(grepl("FJComp", char_vec))) {
    ## If it looks like there is FJComp-xxx in parNames but no FJComp-xxx in the column names of the FCS file, then
    ## rename FJComp-xxx to Comp-xxx in the parNames and we will be looking for those instead.
    new_char_vec <- gsub("^\\FJComp-", "Comp-", char_vec)
    return(new_char_vec)
  } else {
    return(char_vec)
  }
}
changeFJSpecialChar <- function(char_vec, cor_char_vec) {
  new_char_vec <- unlist(lapply(char_vec, function(name) {
    if (name %in% cor_char_vec | name == "EventNumberDP") {
      name
    } else {
      # Let's try [] to <>
      name2 <- gsub("[", "<", name, fixed=TRUE)
      name2 <- gsub("]", ">", name2, fixed=TRUE)
      if (name2 %in% cor_char_vec) {
        name2 # Worked, return it
      } else {
        # Previous fix did not do it, _ =/ on the original names
        name2 <- gsub("_", "/", name, fixed=TRUE)
        if (name2 %in% cor_char_vec) {
          name2 # Worked, return it
        } else {
          # That did not work either, let's try both [] =on top of the previous fix (_ =/)
          name3 <- gsub("[", "<", name2, fixed=TRUE)
          name3 <- gsub("]", ">", name3, fixed=TRUE)
          if (name3 %in% cor_char_vec) {
            name3 # Worked, finally, return it
          } else {
            ## Maybe we read a wrong dataset?
            cat(paste("The input FCS file does not contain the provided input parameter, missing", name, "\n"))
            "MISSINGPARAMETER"
          }
        }
      }
    }
  }))
  return(new_char_vec)
}

parNames <- FJCompToComp(parNames)
parNames <- changeFJSpecialChar(parNames, orig.parNames)

parIndices <- match(parNames, getData(gs, "root") %>%
                      .[[1]] %>%
                      parameters(.) %>%
                      .$name)

if (length(parNames) == 0 || length(parIndices) == 0)
  stop("Something seems wrong, it's like the input FCS file does not contain the provided input parameters.", call.=FALSE)

eventsCount <- getData(gs, "root") %>%
  .[[1]] %>%
  dim %>%
  .[1]

if (length(parNames) == 0)
  stop("Some input parameters need to be selected!", call.=FALSE)

if (length(parNames) == 0 || "MISSINGPARAMETER" %in% parNames)
  stop("The input file is missing some of the specified parameters.", call.=FALSE)
if (eventsCount == 0)
  stop("R failed to read the input file.", call.=FALSE)

popOfInt_full_path <- getNodes(gs)[basename(getNodes(gs)) %in%
                                       popOfInt]

#get info about gating hierarchy for each pop of interest
names_gates_SOM <- foreach(pop = seq_along(basename(popOfInt_full_path))) %do% {
    strsplit(x = getNodes(gs)[grepl(pattern = paste0("/",
                                                     basename(popOfInt_full_path)[pop],
                                                     "/"),
                                    x = getNodes(gs),
                                    fixed = TRUE)],
             split = paste0(getNodes(gs)[grepl(pattern = paste0("/",
                                                                basename(popOfInt_full_path)[pop],
                                                                "$"),
                                               x = getNodes(gs),
                                               fixed = FALSE)][1],
                            "/"),
             fixed = TRUE) %>%
      lapply(tail, 1) %>%
      unlist
  }
names(names_gates_SOM) <- basename(popOfInt_full_path)
names_gates_SOM
named list()
#to add recursive analysis, run whole DAFi process for each
#non-terminal gate, adding the results to GatingSet as boolean filter

#find all gates down the gating hierarchy starting from the selected pop
names_gates_of_int <- foreach(pop = seq_along(basename(popOfInt_full_path)),
                                .final = unlist) %do% {
                                  getNodes(gs)[grepl(pattern = paste0("/",
                                                                      basename(popOfInt_full_path)[pop],
                                                                      "/"),
                                                     x = getNodes(gs),
                                                     fixed = TRUE)]
                                }
if(length(names_gates_of_int) == 0){
  stop("There are no sub-populations in this gate.", call.=FALSE)
  }
Error: There are no sub-populations in this gate.
Execution halted
