#######################################################################
# Copyright (c) 2020 Pedro Milanez-Almeida, Ph.D., NIAID/NIH
#
# License
# The software is distributed under the terms of the
# Artistic License 2.0
# http://www.r-project.org/Licenses/Artistic-2.0
#
# Disclaimer
# This software and documentation come with no warranties of any kind.
# This software is provided "as is" and any express or implied
# warranties, including, but not limited to, the implied warranties of
# merchantability and fitness for a particular purpose are disclaimed.
# In no event shall the  copyright holder be liable for any direct,
# indirect, incidental, special, exemplary, or consequential damages
# (including but not limited to, procurement of substitute goods or
# services; loss of use, data or profits; or business interruption)
# however caused and on any theory of liability, whether in contract,
# strict liability, or tort arising in any way out of the use of this
# software.
######################################################################

#test whether R version older than 3.6.2
Rver.maj <- version$major
Rver.min.1 <- strsplit(x = version$minor, 
                       split = ".",
                       fixed = TRUE,
                       perl = FALSE, 
                       useBytes = FALSE)[[1]][1]
Rver.min.2 <- strsplit(x = version$minor, 
                       split = ".",
                       fixed = TRUE,
                       perl = FALSE, 
                       useBytes = FALSE)[[1]][2]

if(Rver.maj < 3){
  stop(paste0("The plugin cannot run with R versions older than 3.6.2. ",
              "Your version is: ",
              paste0(version$major, ".", version$minor),
              ". Please, update R and try again."))
} else if(Rver.maj == 3 & 
          Rver.min.1 < 6){
  stop(paste0("The plugin cannot run with R versions older than 3.6.2. ",
              "Your version is: ",
              paste0(version$major, ".", version$minor),
              ". Please, update R and try again."))
} else if(Rver.maj == 3 & 
          Rver.min.1 == 6 &
          Rver.min.2 < 2) {
  stop(paste0("The plugin cannot run with R versions older than 3.6.2. ",
              "Your version is: ",
              paste0(version$major, ".", version$minor),
              ". Please, update R and try again."))
}

tryCatch(suppressMessages(library("BiocManager")),
         error = function(e){
           if (!requireNamespace("BiocManager",
                                 quietly = TRUE))
             install.packages("BiocManager",
                              repos = 'http://cran.us.r-project.org')
           suppressMessages(library("BiocManager"))
         })

Bioc.ver.maj <- strsplit(x = as.character(BiocManager::version()), 
                         split = ".",
                         fixed = TRUE,
                         perl = FALSE, 
                         useBytes = FALSE)[[1]][1]
Bioc.ver.min <- strsplit(x = as.character(BiocManager::version()), 
                         split = ".",
                         fixed = TRUE,
                         perl = FALSE, 
                         useBytes = FALSE)[[1]][2]

if(Bioc.ver.maj < 3){
  stop(paste0("The plugin cannot run with Bioconductor releases older than 3.10. ",
              "Your version is: ",
              BiocManager::version(),
              ". Please, update Bioconductor (visit https://www.bioconductor.org/install/) and try again."))
} else if(Bioc.ver.maj == 3 & 
          Bioc.ver.min < 10){
  stop("The plugin cannot run with Bioconductor releases older than 3.10. ",
       "Your version is: ",
       BiocManager::version(),
       ". Please, update Bioconductor (visit https://www.bioconductor.org/install/) and try again.")
}


## This will load required packages and, if not found, install them without updating old Bioc pckgs
tryCatch(suppressMessages(library("foreach")),
         error = function(e){
           install.packages(pkgs =  "foreach",
                            repos = 'http://cran.us.r-project.org')
           suppressMessages(library("foreach"))
         })
tryCatch(suppressMessages(library("magrittr")),
         error = function(e){
           install.packages(pkgs =  "magrittr",
                            repos = 'http://cran.us.r-project.org')
           suppressMessages(library("magrittr"))
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
tryCatch(suppressMessages(library("flowCore")),
         error = function(e){
           if (!requireNamespace("BiocManager",
                                 quietly = TRUE))
             install.packages("BiocManager",
                              repos = 'http://cran.us.r-project.org')
           BiocManager::install("flowCore",
                                update = FALSE,
                                ask = FALSE)
           suppressMessages(library("flowCore"))
         })

sessionInfo()

minPopSize <- FJ_PAR_MINPOPSIZE
#sampleFCS <- "FJ_SAMPLE_NODE_NAME"

#find parent
#parentFileName <- "FJ_PARENT_NAME"
#parentFilePath <- paste0(dirname("FJ_DATA_FILE_PATH"),
#                        "/",
#                        parentFileName)
#parentFilePath
## Code to read gates from wsp file
popOfInt <- "FJ_POPULATION_NAME"
popOfInt

#find and load wsp file
wspDir <- "FJ_PARM_WSPDIR"
wspName <- "FJ_PARM_WSPNAME"

wspName <- paste0(wspDir, 
                  "/",
                  wspName)
#dirname(path = "FJ_DATA_FILE_PATH") %>%
#dirname(path = .) %>%
#paste0(.,
#       ".wsp")
wspName

ws <- CytoML::open_flowjo_xml(wspName)
##find raw .fcs files
#find path of all fcs files in workspace
sampleFCS_paths <- XML::xpathApply(ws@doc,
                                   file.path("/Workspace/SampleList/Sample","DataSet"),
                                   function(x)
                                     XML::xmlGetAttr(x,"uri") %>%
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
                            x = basename("FJ_DATA_FILE_PATH"),
                            fixed = TRUE)) %>%
  unlist(.)
nameSearch
nameSearchRes <- names(nameSearch)[nameSearch %>%
                                     names(.) %>% 
                                     nchar(.) %>% 
                                     which.max(.)]
nameSearchRes
sampleFCS <- paste0(nameSearchRes,
                    ".fcs")
sampleFCS
#gsFileName <- paste0(dirname("FJ_DATA_FILE_PATH"),
#                     "/",
#                    basename(wspName),
#                    ".",
#                    sampleFCS,
#                    ".gs")

#if(file.exists(gsFileName)){
# gs <- load_gs(gsFileName)
#} else {
sampleFCS_path <- sampleFCS_paths[basename(sampleFCS_paths) == sampleFCS]
sampleFCS_path

sampleID <- which(sampleFCS_paths == sampleFCS_path)

# the following is meant to add support for acs files on windows
# TODO: test on Mac!
nchar_wspDir <- nchar(wspDir)
wspDir_last4 <- substr(wspDir, 
                       nchar_wspDir - 4 + 1,
                       nchar_wspDir)
if(wspDir_last4 != ".acs" & Sys.info()["sysname"] == "Windows") {
  sampleFCS_path <- substring(sampleFCS_path, 2)
}

if(wspDir_last4 == ".acs"){
  sampleFCS_path <- paste0(wspDir,
                           "/",
                           sampleFCS_path)
}

#parse wsp and fcs files into a GatingSet object
pathFCS <- #tryCatch(
  #data.frame(sampleID = CytoML::fj_ws_get_samples(ws)$sampleID[CytoML::fj_ws_get_samples(ws)$name == sampleFCS],
  #          file = sampleFCS_path),
  #error = function(e) {
  #FIL <- flowCore::read.FCS(sampleFCS_path)@description$`$FIL`
  data.frame(sampleID = sampleID,#CytoML::fj_ws_get_samples(ws)$sampleID[CytoML::fj_ws_get_samples(ws)$name == FIL],
             file = sampleFCS_path)
#})
pathFCS$sampleID <- as.numeric(pathFCS$sampleID)
pathFCS
#in case two samples have the same sample name, altough they came from different fcs files,
#the plugin will fail. In this case, return just the first row of pathFCS:
#if(duplicated(pathFCS$file) %>%
#  any){
# pathFCS <- data.frame(sampleID = sampleID,
#                       file = pathFCS[1,"file"])
# pathFCS$sampleID <- as.numeric(pathFCS$sampleID)
#}

gs <- CytoML::flowjo_to_gatingset(ws,
                                  name = 1,
                                  path = pathFCS,
                                  isNcdf = FALSE)

# Close the flowjo workspace connection
CytoML::flowjo_ws_close(ws)

#}

orig.parNames <- flowWorkspace::gh_pop_get_data(gs[[1]]) %>%
  flowCore::parameters(.) %>%
  flowCore::pData(.) %>%
  .$name

parNames <- c(FJ_PARAMS_LIST)

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
        # Previous fix did not do it, _ => / on the original names
        name2 <- gsub("_", "/", name, fixed=TRUE)
        if (name2 %in% cor_char_vec) {
          name2 # Worked, return it
        } else {
          # That did not work either, let's try both [] => on top of the previous fix (_ => /)
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

parIndices <- match(parNames, orig.parNames)

if (length(parNames) == 0 || length(parIndices) == 0){
  stop("Something seems wrong, it's like the input FCS file does not contain the provided input parameters.", call.=FALSE)
}
eventsCount <- flowWorkspace::gh_pop_get_data(gs[[1]]) %>%
  dim %>%
  .[1]

if (length(parNames) == 0){
  stop("Some input parameters need to be selected!", call.=FALSE)
}
if (length(parNames) == 0 || "MISSINGPARAMETER" %in% parNames){
  stop("The input file is missing some of the specified parameters.", call.=FALSE)
}
if (eventsCount == 0){
  stop("R failed to read the input file.", call.=FALSE)
}
popOfInt_full_path <- flowWorkspace::gs_get_pop_paths(gs)[basename(flowWorkspace::gs_get_pop_paths(gs)) %in%
                                                            popOfInt]

#get info about gating hierarchy for each pop of interest
names_gates_SOM <- foreach::foreach(pop = seq_along(basename(popOfInt_full_path))) %do% {
  strsplit(x = flowWorkspace::gs_get_pop_paths(gs)[grepl(pattern = paste0("/",
                                                                          basename(popOfInt_full_path)[pop],
                                                                          "/"),
                                                         x = flowWorkspace::gs_get_pop_paths(gs),
                                                         fixed = TRUE)],
           split = paste0(flowWorkspace::gs_get_pop_paths(gs)[grepl(pattern = paste0("/",
                                                                                     basename(popOfInt_full_path)[pop],
                                                                                     "$"),
                                                                    x = flowWorkspace::gs_get_pop_paths(gs),
                                                                    fixed = FALSE)][1],
                          "/"),
           fixed = TRUE) %>%
    lapply(tail, 1) %>%
    unlist
}
names(names_gates_SOM) <- basename(popOfInt_full_path)
names_gates_SOM

if(length(names_gates_SOM) == 1 &
   is.null(names_gates_SOM[[1]])) {
  stop("It looks like the selected population has no children for DAFi to refine. DAFi requires the selected population to have at least one child gate.",
       call. = FALSE)
}

# TODO: CHANGE CODE TO BE ABLE TO HANDLE WHEN PLUGIN IS CALLED ON ROOT
if(substr(popOfInt, nchar(popOfInt) - 4 + 1, nchar(popOfInt)) == ".fcs" &
   popOfInt == sampleFCS){
  stop("The plugin cannot handle the root population yet. Please select the next downstream gate from the root and rerun DAFi.",
       call. = FALSE)
}

##### CODE TO ADD ABILITY TO RUN DAFI ON SELECTED POP WITHOUT CHILD
# if the selected population has no children
# this will make it possible to DAFi the parent pop
# and refine the selected one
#if(length(names_gates_SOM) == 1 &
#  is.null(names_gates_SOM[[1]])) {
# noChildMode <- TRUE
# popOfInt <- basename(dirname(popOfInt_full_path))
# names_gates_of_int <- popOfInt_full_path
# names_gates_SOM <- basename(dirname(popOfInt_full_path))
# popOfInt_full_path <- dirname(popOfInt_full_path)
# names_gates_to_SOM <- as.list(popOfInt_full_path)
# names(names_gates_to_SOM)[1] <- popOfInt_full_path
# pops_to_SOM <- names_gates_to_SOM
#} else {
#  noChildMode <- FALSE
#if doing recursive analysis, run whole DAFi process for each
#non-terminal gate, adding the results to GatingSet as boolean filter
#importantly, we can still run children only analysis despite these changes, see below

#find all gates down the gating hierarchy starting from the selected pop
names_gates_of_int <- foreach::foreach(pop = seq_along(basename(popOfInt_full_path)),
                                       .final = unlist) %do% {
                                         flowWorkspace::gs_get_pop_paths(gs)[grepl(pattern = paste0("/",
                                                                                                    basename(popOfInt_full_path)[pop],
                                                                                                    "/"),
                                                                                   x = flowWorkspace::gs_get_pop_paths(gs),
                                                                                   fixed = TRUE)]
                                       }

#find non-terminal gates down the gating hierarchy, which will all be used in clustering
names_gates_non_term <- unlist(names_gates_of_int,
                               use.names = FALSE)[lapply(unlist(names_gates_of_int, 
                                                                use.names = FALSE),
                                                         function(gate_of_int)
                                                           grepl(pattern = paste0("/",
                                                                                  basename(gate_of_int),
                                                                                  "/"),
                                                                 x = unlist(names_gates_of_int, use.names = FALSE),
                                                                 fixed = TRUE) %>%
                                                           any) %>%
                                                    unlist]
#change names of non-terminal gates to reflect the fact they will be DAFi-refined
names_gates_non_term_to_SOM <- as.list(names_gates_non_term)

names(names_gates_non_term_to_SOM) <-  unlist(names_gates_non_term_to_SOM,
                                              use.names = FALSE) %>%
  gsub(pattern = popOfInt_full_path,
       replacement = "",
       x = .,
       fixed = TRUE) %>%
  strsplit(.,
           split = "/") %>%
  lapply(.,
         function(pop)
           pop[-1]  %>%
           gsub(pattern = "^/",
                replacement = "",
                x = .) %>%
           paste0("DAFi_",
                  .) %>%
           paste0(.,
                  collapse = "/") %>%
           paste0(popOfInt_full_path,
                  "/",
                  .)) %>%
  unlist(.)
#gates that will be used in clustering and whose children will be refined
names_gates_to_SOM <- c(popOfInt_full_path,
                        names_gates_non_term_to_SOM)
names(names_gates_to_SOM)[1] <- popOfInt_full_path
#find their position in the hiearchy
tree_pos_gate_to_SOM <- strsplit(x = unlist(names_gates_to_SOM,
                                            use.names = FALSE),
                                 split = "/",
                                 fixed = TRUE) %>%
  lapply(length) %>%
  unlist(.)
if(FJ_PAR_CHILDREN){
  pops_to_SOM <- names_gates_to_SOM[tree_pos_gate_to_SOM == min(tree_pos_gate_to_SOM)]
} else {
  pops_to_SOM <- names_gates_to_SOM[order(tree_pos_gate_to_SOM)] #order is very important to ensure hierarchy of gates
}
#drop empty gates
pops_to_SOM <- pops_to_SOM[
  lapply(pops_to_SOM,
         function(pop_to_SOM)
           !identical(pop_to_SOM, character(0))) %>%
    unlist(., use.names = FALSE)]
#}
#actual DAFi
for(pop_to_SOM in seq_along(pops_to_SOM)){
  print(pops_to_SOM[pop_to_SOM] %>% names(.))
  for(fSample in seq_along(gs)) {
    if(dim(flowWorkspace::gh_pop_get_data(gs[[fSample]],
                                          pops_to_SOM[pop_to_SOM] %>%
                                          names(.))[,parIndices])[1] > minPopSize) { # in case a subpop is smaller than min #events, SOM is not applied
      ## Code to read the GatingSet data from each population that will be analyzed with DAFi
      if (nchar("FJ_PAR_APPLY_ON_PREV") > 5) { ## Expected either "None" or a valid file path
        load("FJ_PAR_APPLY_ON_PREV")
        fSOM <- FlowSOM::NewData(fSOM, flowWorkspace::gh_pop_get_data(gs[[fSample]],
                                                                      pops_to_SOM[pop_to_SOM] %>%
                                                                        names(.))[,parIndices]);
      } else {
        fSOM <- FlowSOM::ReadInput(flowWorkspace::gh_pop_get_data(gs[[fSample]],
                                                                  pops_to_SOM[pop_to_SOM] %>%
                                                                    names(.))[,parIndices],
                                   compensate = FALSE,
                                   transform = FALSE,
                                   scale = FJ_PAR_SCALE,
                                   silent = TRUE)
      }
      if(FJ_PAR_SOM){ #if user decides to use self-organizing maps
        ## Code to generate SOM centroids
        set.seed(2020)
        fSOM <- FlowSOM::BuildSOM(fSOM,
                                  colsToUse = parNames,
                                  silent = TRUE,
                                  xdim = FJ_PAR_XDIM,
                                  ydim = FJ_PAR_YDIM)
        ## Code to gate flowSOM results
        #retrieve codes
        if(FJ_PAR_SCALE) {
          codes <- t(apply(fSOM$map$codes,
                           1,
                           function(centroid)
                             centroid *
                             fSOM$scaled.scale[parNames] +
                             fSOM$scaled.center[parNames]))
        } else {
          codes <- fSOM$map$codes
        }
      } else { # if the user decides to use kmeans
        ## Code to generate kmeans centroids
        set.seed(2020)
        fkMeans <- stats::kmeans(x = fSOM$data[,parNames],
                                 centers = FJ_PAR_XDIM*FJ_PAR_YDIM,
                                 iter.max = 100)
        ## Code to gate kmeans results
        #retrieve codes
        if(FJ_PAR_SCALE) {
          codes <- t(apply(fkMeans$centers,
                           1,
                           function(centroid)
                             centroid *
                             fSOM$scaled.scale[parNames] +
                             fSOM$scaled.center[parNames]))
        } else {
          codes <- fkMeans$centers
        }
      }
      ls_fSOM <- list(codes)
      names(ls_fSOM) <- rownames(flowCore::pData(gs[[fSample]]))
      #create FlowSet with FlowSOM centroids
      fS_SOM <- lapply(ls_fSOM,
                       function(sample)
                         flowCore::flowFrame(sample)) %>%
        flowCore::flowSet()
      flowCore::parameters(fS_SOM[[1]]) <- flowCore::parameters(flowWorkspace::gh_pop_get_data(gs[[fSample]],
                                                                                               y = "root")[,parIndices])
      #create GatingSet with FlowSOM centroids
      suppressMessages(gs_SOM <- GatingSet(fS_SOM))
      flowCore::pData(gs_SOM) <- flowCore::pData(gs[[fSample]])
      #get gates and apply to cluster centroids
      #if(noChildMode) {
      #  gates <- basename(names_gates_of_int)
      #} else {
      gates <- basename(flowWorkspace::gs_get_pop_paths(gs[[fSample]]))[
        lapply(strsplit(x = dirname(flowWorkspace::gs_get_pop_paths(gs[[fSample]])),
                        split = "/"),
               function(nodes)
                 tail(nodes, n = 1) == basename(pops_to_SOM[[pop_to_SOM]])) %>%
          unlist(.) %>%
          which(.)]
      #}
      for(gate in gates) {
        suppressMessages(flowWorkspace::gs_pop_add(gs_SOM,
                                                   flowWorkspace::gh_pop_get_gate(gs[[fSample]],
                                                                                  paste0(pops_to_SOM[[pop_to_SOM]],
                                                                                         "/", gate))))
      }
      tryCatch({suppressMessages(flowWorkspace::recompute(gs_SOM))},
               error = function(e){
                 stop(paste0("It looks like the channel \"",
                             strsplit(x = e$message, 
                                      split = "\n  ",
                                      fixed = TRUE)[[1]][2] %>%
                               strsplit(x = ., 
                                        split = " not found",
                                        fixed = TRUE) %>% 
                               .[[1]] %>%
                               .[1],
                             "\" has not been selected when the plugin was called although it was used down the gating hiearchy. \nPlease, make sure all flow channels used in the gating tree are selected when calling the plugin."), 
                      call. = FALSE)
               })
      ## Code to update assignment of cell identity according to DAFi results
      SOM_labels <- vector(mode = "list",
                           length = length(gates))
      names(SOM_labels) <- gates
      for(gate in gates){
        SOM_labels[[gate]] <- rep(FALSE,
                                  FJ_PAR_XDIM*FJ_PAR_YDIM)
      }
      for(gate in gates) {
        SOM_labels[[gate]][flowWorkspace::gh_pop_get_indices(gs_SOM[[1]],
                                                             gate)] <- TRUE
      }
      cell_DAFi_label <- vector(mode = "list",
                                length = length(gates))
      names(cell_DAFi_label) <- gates
      for(gate in gates) {
        if(FJ_PAR_SOM){
          cell_DAFi_label[[gate]] <- SOM_labels[[gate]][fSOM$map$mapping[,1]]
        } else {
          cell_DAFi_label[[gate]] <- SOM_labels[[gate]][fkMeans$cluster]
        }
      }
      all_cells_DAFi_label <- vector(mode = "list",
                                     length = length(gates))
      names(all_cells_DAFi_label) <- gates
      for(gate in gates) {
        all_cells_DAFi_label[[gate]] <- rep(FALSE,
                                            length(flowWorkspace::gh_pop_get_indices(gs[[fSample]],
                                                                                     y = pops_to_SOM[pop_to_SOM] %>% names(.))))
        all_cells_DAFi_label[[gate]][
          flowWorkspace::gh_pop_get_indices(gs[[fSample]],
                                            y = pops_to_SOM[pop_to_SOM] %>% names(.))] <- cell_DAFi_label[[gate]]
        all_cells_DAFi_label[[gate]] <- list(all_cells_DAFi_label[[gate]])
        names(all_cells_DAFi_label[[gate]]) <- sampleNames(gs[[fSample]])
      }
      for(gate in gates) {
        flowWorkspace::gs_pop_add(gs[[fSample]],
                                  all_cells_DAFi_label[[gate]],
                                  parent = pops_to_SOM[pop_to_SOM] %>%
                                    names(.),
                                  name = paste0("DAFi_", gate) ) %>%
          gsub(pattern = "^/",
               replacement = "",
               x = .)
      }
      suppressMessages(flowWorkspace::recompute(gs[[fSample]]))
    } else { # close if statement of minimum population size
      #if(noChildMode) {
      # gates <- basename(names_gates_of_int)
      #} else {
      gates <- basename(flowWorkspace::gs_get_pop_paths(gs[[fSample]]))[
        lapply(strsplit(x = dirname(flowWorkspace::gs_get_pop_paths(gs[[fSample]])),
                        split = "/"),
               function(nodes)
                 tail(nodes, n = 1) == basename(pops_to_SOM[[pop_to_SOM]])) %>%
          unlist(.) %>%
          which(.)]
      #}
      for(gate in gates) {
        flowWorkspace::gs_pop_add(gs[[fSample]],
                                  flowWorkspace::gh_pop_get_gate(gs[[fSample]],
                                                                 paste0(pops_to_SOM[[pop_to_SOM]],
                                                                        "/", gate)),
                                  parent = pops_to_SOM[pop_to_SOM] %>% names(.),
                                  name = paste0("DAFi_",
                                                gate) ) %>%
          gsub(pattern = "^/",
               replacement = "",
               x = .)
      }
      suppressMessages(flowWorkspace::recompute(gs[[fSample]]))
    }
  }
}

# BUG: when running the plugin on a population down the tree from a previously DAFi-ed pop
# flowWorkspace does not import the derived paramenters (used in FlowJo to gate DAFi) and 
# descendant gates
# Failed solution: saving the GatingSet with DAFi gates to load in a second run. The DAFi
# gates are saved and loaded correctly, but the descendant gates are missing!

#save_gs(gs,
#       path = gsFileName)

DAFi_nodes <- flowWorkspace::gs_get_pop_paths(gs)[grep(pattern = "DAFi_",
                                                       x = basename(flowWorkspace::gs_get_pop_paths(gs)),
                                                       fixed = TRUE)]
DAFi_nodes <- DAFi_nodes[grep(pattern = popOfInt,
                              x = DAFi_nodes,
                              fixed = TRUE)]
all_cell_DAFi_label <- foreach::foreach(DAFi_node = DAFi_nodes) %do% {
  flowWorkspace::gh_pop_get_indices(gs[[1]],
                                    DAFi_node)
}
names(all_cell_DAFi_label) <- DAFi_nodes
#if(noChildMode) {
# EventNumberDP <- read.csv(file = parentFilePath,
#                           check.names=FALSE)$EventNumberDP
#} else {
EventNumberDP <- read.csv(file = "FJ_DATA_FILE_PATH",
                          check.names=FALSE)$EventNumberDP
#}
FJ_event_DAFi_label <- foreach::foreach(DAFi_node = DAFi_nodes) %do% {
  all_cell_DAFi_label[[DAFi_node]][EventNumberDP]
}
names(FJ_event_DAFi_label) <- DAFi_nodes

## Extract DAFi clustering to pass back to FlowJo (or SeqGeq)
labels.ls <- foreach::foreach(DAFi_node = DAFi_nodes) %do% {
  ## Please note "FJ_event_DAFi_label" here stemming from DAFi
  ## FlowJo, let's do labels as 100, 200, 300, all with a tiny bit of noise (to make FlowJo cluster those better)
  label <- as.matrix(FJ_event_DAFi_label[[DAFi_node]] %>%
                       as.integer(.))
}
names(labels.ls) <- DAFi_nodes

labels <- matrix(unlist(labels.ls,
                        use.names = FALSE),
                 ncol = length(labels.ls),
                 byrow = FALSE)
colnames(labels) <- foreach::foreach(DAFi_node = DAFi_nodes,
                                     .final = unlist) %do% {
                                       DAFi_node %>%
                                         strsplit(x = .,
                                                  split = paste0(popOfInt),
                                                  fixed = TRUE) %>%
                                         .[[1]] %>%
                                         tail(.,1) %>%
                                         gsub(pattern = "^/",
                                              replacement = "",
                                              x = .) %>%
                                         gsub(pattern = "/",
                                              replacement = "_",
                                              x = .,
                                              fixed = TRUE) %>%
                                         gsub(pattern = ",",
                                              replacement = ".",
                                              x = .,
                                              fixed = TRUE)
                                     }

#sanity check
apply(labels,
      2,
      function(pop)
        mean(pop > 0))
#write results
write.csv(labels, file="FJ_CSV_OUPUT_FILE", row.names=FALSE, quote=TRUE)
write.csv(parNames, paste0("FJ_CSV_OUPUT_FILE", ".pars.csv"), row.names=FALSE)

all.labels.ls <- foreach::foreach(DAFi_node = DAFi_nodes) %do% {
  all.label <- as.matrix(all_cell_DAFi_label[[DAFi_node]] %>%
                           as.integer(.))
}
names(all.labels.ls) <- DAFi_nodes

all.labels <- matrix(unlist(all.labels.ls,
                            use.names = FALSE),
                     ncol = length(all.labels.ls),
                     byrow = FALSE)
colnames(all.labels) <- foreach::foreach(DAFi_node = DAFi_nodes,
                                         .final = unlist) %do% {
                                           DAFi_node %>%
                                             strsplit(x = .,
                                                      split = paste0(popOfInt),
                                                      fixed = TRUE) %>%
                                             .[[1]] %>%
                                             tail(.,1) %>%
                                             gsub(pattern = "^/",
                                                  replacement = "",
                                                  x = .) %>%
                                             gsub(pattern = "/",
                                                  replacement = "_",
                                                  x = .,
                                                  fixed = TRUE) %>%
                                             gsub(pattern = ",",
                                                  replacement = ".",
                                                  x = .,
                                                  fixed = TRUE)
                                         }
#2nd sanity check
apply(all.labels,
      2,
      function(pop)
        mean(pop > 0))

flowEnv <- new.env()

for(pop in colnames(all.labels)) {
  mat <- matrix(c(0.5, 1.5),
                ncol = 1,
                dimnames = list(c("min", "max"),
                                pop))
  rg <- rectangleGate(filterId = pop,
                      .gate = mat)
  flowEnv[[as.character(pop)]] <- rg
}

outputFile <- paste0("FJ_DATA_FILE_PATH",
                     ".gating-ml2.xml")

addObjectToGatingML <- function(gatingMLNode, x, flowEnv, addParent = NULL, forceGateId = NULL) {#from https://rdrr.io/bioc/flowUtils/src/R/writeGatingML.R 
  if(is(x, "character")) object = flowEnv[[x]]
  else object = x
  switch(class(object),
         "rectangleGate" = addRectangleGateNode(gatingMLNode, x, flowEnv, addParent, forceGateId),
         "unitytransform" = NA,
         {
           errMessage <- paste("Class \'", class(object), "\' is not supported in Gating-ML 2.0 output.", sep="")
           if(is(object, "singleParameterTransform"))
             errMessage <- paste(errMessage, " Only Gating-ML 2.0 compatible transformations are supported by Gating-ML 2.0 output. Transformation \'", 
                                 object@transformationId, "\' is not among those and cannot be included. Therefore, any gate referencing this transformation would be referencing a non-existent transformation in the Gating-ML output. Please correct the gates and transformations in your environment and try again.", sep="")
           if(is(object, "filter"))
             errMessage <- paste(errMessage, " Only Gating-ML 2.0 compatible gates are supported by Gating-ML 2.0 output. Filter \'", 
                                 object@filterId, "\' is not among those and cannot be included. Please remove this filter and any references to it from the environment and try again.", sep="")
           stop(errMessage, call. = FALSE)    
         }
  )
}

# Add rectangle gate x to the Gating-ML node
addRectangleGateNode <- function(gatingMLNode, x, flowEnv, addParent, forceGateId) {#from https://rdrr.io/bioc/flowUtils/src/R/writeGatingML.R 
  gate = objectNameToObject(x, flowEnv)
  if(!is(gate, "rectangleGate")) stop(paste("Unexpected object insted of a rectangleGate - ", class(gate))) 
  addDebugMessage(paste("Working on rectangleGate ", gate@filterId, sep=""), flowEnv)
  
  myID = getObjectId(gate, forceGateId, flowEnv)
  if(isIdWrittenToXMLAlready(myID, flowEnv)) return(FALSE) 
  attrs = c("gating:id" = myID)
  if (!is.null(addParent)) attrs = c(attrs, "gating:parent_id" = filterIdtoXMLId(addParent, flowEnv))
  
  gatingMLNode$addNode("gating:RectangleGate", attrs = attrs, close = FALSE)
  addDimensions(gatingMLNode, x, flowEnv)
  gatingMLNode$closeTag() # </gating:RectangleGate>
}

addDebugMessage <- function(msg, flowEnv) {#from https://rdrr.io/bioc/flowUtils/src/R/writeGatingML.R 
  flowEnv[['.debugMessages']] = c(flowEnv[['.debugMessages']], msg)
}

doubleCheckExistanceOfParameter <- function(par, flowEnv) {#from https://rdrr.io/bioc/flowUtils/src/R/writeGatingML.R 
  if(is(par, "transform")) 
  {
    if(!is.null(par@transformationId) && par@transformationId != "" && !exists(par@transformationId, envir=flowEnv, inherits=FALSE)) 
    {
      flowEnv[[par@transformationId]] <- par
      flowEnv[['.addedObjects']][[par@transformationId]] <- par@transformationId
      addReferencedObjectsToEnv(par@transformationId, flowEnv)
    }    
  }
}

objectNameToObject <- function(x, flowEnv) { #from https://rdrr.io/bioc/flowUtils/src/R/writeGatingML.R 
  if(is(x, "character")) flowEnv[[x]]
  else x
}

addReferencedObjectsToEnv <- function(x, flowEnv) { #from https://rdrr.io/bioc/flowUtils/src/R/writeGatingML.R 
  object = objectNameToObject(x, flowEnv)
  for(par in object@parameters) doubleCheckExistanceOfParameter(par, flowEnv)
}

createTransformIdentifier <- function(trans) {#from https://rdrr.io/bioc/flowUtils/src/R/writeGatingML.R 
  name <- class(trans)
  for (slotName in slotNames(trans))
  {
    if(slotName != ".Data" && slotName != "parameters" && slotName != "transformationId")
    {
      slotValue = slot(trans, slotName)
      if(is(slotValue, "numeric") || is(slotValue, "character"))
      {
        name <- paste(name, slotName, slot(trans, slotName), sep = "_")
      }
    }
  }
  name
}

shouldTransformationBeSkipped <- function(x, flowEnv) { #from https://rdrr.io/bioc/flowUtils/src/R/writeGatingML.R 
  trEnv = flowEnv[['.singleParTransforms']]
  trans = flowEnv[[x]]
  if(!is.null(trEnv) && !is.null(trans) && is(trans, "singleParameterTransform"))
  {
    key = createTransformIdentifier(trans)
    if (!is.null(trEnv[[key]])){
      if (x == trEnv[[key]]) FALSE
      else TRUE
    } else FALSE
  } else FALSE
}

addDimensions <- function(gatingMLNode, x, flowEnv, quadGateDividerIdBasedName = NULL)
{
  gate = objectNameToObject(x, flowEnv)
  for (i in 1:length(gate@parameters))
  {
    attrs = c()
    parameter = gate@parameters[[i]]
    
    if (is(gate, "rectangleGate"))
    {
      min = gate@min[[i]]
      max = gate@max[[i]]
      if(min != -Inf) attrs = c(attrs, "gating:min" = min)
      if(max != Inf) attrs = c(attrs, "gating:max" = max)
    }
    
    if(is(parameter, "transformReference")) parameter = resolveTransformationReference(parameter)
    if(is(parameter, "unitytransform")) attrs = c(attrs, "gating:compensation-ref" = "uncompensated")
    else if(is(parameter, "singleParameterTransform"))
    {
      attrs = c(attrs, "gating:transformation-ref" = filterIdtoXMLId(parameter@transformationId, flowEnv))
      parameter = parameter@parameters
      if(is(parameter, "transformReference")) parameter = resolveTransformationReference(parameter)
      
      if(is(parameter, "unitytransform")) attrs = c(attrs, "gating:compensation-ref" = "uncompensated")
      else if(is(parameter, "compensatedParameter")) attrs = addCompensationRef(attrs, parameter, flowEnv)
      else if(is(parameter, "ratiotGml2") || is(parameter, "ratio")) attrs = addCompensationRef(attrs, parameter@numerator, flowEnv)
      else stop(paste("Unexpected parameter class ", class(parameter), ", compound transformations are not supported in Gating-ML 2.0.", sep=""))
    } 
    else if(is(parameter, "compensatedParameter")) attrs = addCompensationRef(attrs, parameter, flowEnv)
    else if(is(parameter, "ratiotGml2") || is(parameter, "ratio")) attrs = addCompensationRef(attrs, parameter@numerator, flowEnv)
    else stop(paste("Unexpected parameter class", class(parameter), "- not supported in Gating-ML 2.0 output)."))
    
    if(is(gate, "quadGate")) 
    {
      attrs = c(attrs, "gating:id" = paste(quadGateDividerIdBasedName, ".D", i, sep = ""))
      gatingMLNode$addNode("gating:divider", attrs = attrs, close = FALSE)
    }
    else gatingMLNode$addNode("gating:dimension", attrs = attrs, close = FALSE)
    
    addDimensionContents(gatingMLNode, parameter, flowEnv)
    if (is(gate, "quadGate")) gatingMLNode$addNode("gating:value", as.character(gate@boundary[i]))
    gatingMLNode$closeTag() # </gating:dimension> or </gating:divider>
  }
}

# Add the contents of a Gating-ML dimension to a Gating-ML node
addDimensionContents <- function(gatingMLNode, parameter, flowEnv)
{
  newDimension = FALSE
  if(is(parameter, "compensatedParameter")) 
  {
    if (parameter@spillRefId == "SpillFromFCS") 
      attrs = c("data-type:name" = parameter@parameters)
    else 
      attrs = c("data-type:name" = parameter@transformationId)
  }
  else if(is(parameter, "unitytransform")) attrs = c("data-type:name" = parameter@parameters)
  else if(is(parameter, "character")) attrs = c("data-type:name" = parameter)
  else if(is(parameter, "ratiotGml2") || is(parameter, "ratio")) {
    attrs = c("data-type:transformation-ref" = parameter@transformationId)
    newDimension = TRUE
  }
  else stop(paste("Unrecognized parameter type, class ", class(parameter), ". Note that compound transformations are not supported in Gating-ML 2.0.", sep=""))
  
  if(newDimension)
    gatingMLNode$addNode("data-type:new-dimension", attrs = attrs)
  else
    gatingMLNode$addNode("data-type:fcs-dimension", attrs = attrs)
}


modified.write.gatingML <- function(flowEnv, file = NULL){
  
  #THIS FUNCTION HAS BEEN MODIFIED BY A CLUELESS PERSON! DONT TRUST IT TOO MUCH! SOURCE:https://rdrr.io/bioc/flowUtils/src/R/writeGatingML.R
  
  if(!is.null(file) && !is(file, "character")) 
    stop("A file has to be either NULL or a character string.", call. = FALSE)
  if(is.null(flowEnv) || !is.environment(flowEnv))
    stop("A flowEnv environment with objects to be saved is requred.", call. = FALSE)
  if(!is.null(file) && substr(file, nchar(file) - 3, nchar(file)) != ".xml")
    file <- paste(file, "xml", sep=".")
  
  flowEnv[['.debugMessages']] = c()
  
  namespaces <- c(
    "gating" = "http://www.isac-net.org/std/Gating-ML/v2.0/gating", 
    "xsi" = "http://www.w3.org/2001/XMLSchema-instance", 
    "transforms" = "http://www.isac-net.org/std/Gating-ML/v2.0/transformations", 
    "data-type" = "http://www.isac-net.org/std/Gating-ML/v2.0/datatypes")
  
  gatingMLNode = suppressWarnings(xmlTree("gating:Gating-ML", namespaces = namespaces, 
                                          attrs = c("xsi:schemaLocation" = "http://www.isac-net.org/std/Gating-ML/v2.0/gating http://flowcyt.sourceforge.net/gating/2.0/xsd/Gating-ML.v2.0.xsd http://www.isac-net.org/std/Gating-ML/v2.0/transformations http://flowcyt.sourceforge.net/gating/2.0/xsd/Transformations.v2.0.xsd http://www.isac-net.org/std/Gating-ML/v2.0/datatypes http://flowcyt.sourceforge.net/gating/2.0/xsd/DataTypes.v2.0.xsd")))
  
  ##### THE FOLLOWING SEVERAL LINES ARE NOT COMMENTED OUT IN THE ORIGINAL
  #gatingMLNode$addNode("data-type:custom_info", close = FALSE)
  #gatingMLNode$addNode("info", "Gating-ML 2.0 export generated by R/flowUtils/flowCore")
  #gatingMLNode$addNode("R-version", sessionInfo()$R.version$version.string)
  #gatingMLNode$addNode("flowCore-version", as.character(packageVersion("flowCore")))
  #gatingMLNode$addNode("flowUtils-version", as.character(packageVersion("flowUtils")))
  #gatingMLNode$addNode("XML-version", as.character(packageVersion("XML")))
  #gatingMLNode$closeTag()
  
  flowEnv[['.objectIDsWrittenToXMLOutput']] = list() # Use this list to collect XML Ids
  
  flowEnv[['.addedObjects']] = list() # List of object identifiers of objects that we have to temporarily add to flowEnv
  for (x in ls(flowEnv)) addReferencedObjectsToEnv(x, flowEnv) 
  
  for (x in ls(flowEnv)) if(is(flowEnv[[x]], "transform"))
    if(!shouldTransformationBeSkipped(x, flowEnv)) addObjectToGatingML(gatingMLNode, x, flowEnv)
  for (x in ls(flowEnv)) if(!is(flowEnv[[x]], "transform")) addObjectToGatingML(gatingMLNode, x, flowEnv)
  
  if(!is.null(file)) sink(file = file)
  cat(saveXML(gatingMLNode$value(), encoding = "UTF-8"))
  if(!is.null(file)) sink()
  
  rm(list = as.character(flowEnv[['.addedObjects']]), envir = flowEnv)
  rm('.addedObjects', envir = flowEnv)
  
  rm('.objectIDsWrittenToXMLOutput', envir = flowEnv) 
  
}

getObjectId <- function(object, forceGateId, flowEnv) { #from: https://rdrr.io/bioc/flowUtils/src/R/writeGatingML.R
  if (is(object, "filter")) {
    if (is.null(forceGateId)) myID = filterIdtoXMLId(object@filterId, flowEnv)
    else myID = filterIdtoXMLId(forceGateId, flowEnv)    
  } else if (is(object, "transform")) {
    if (is.null(forceGateId)) myID = filterIdtoXMLId(object@transformationId, flowEnv)
    else myID = filterIdtoXMLId(forceGateId, flowEnv)
  } else if (is(object, "compensation")) {
    if (is.null(forceGateId)) myID = filterIdtoXMLId(object@compensationId, flowEnv)
    else myID = filterIdtoXMLId(forceGateId, flowEnv)
  }
  
  else stop(paste("Unexpected object to get id from, class", class(object)))
  myID
}

filterIdtoXMLId <- function(x, flowEnv) { #from: https://rdrr.io/bioc/flowUtils/src/R/writeGatingML.R
  if(!(is.character(x))) stop(paste("Object of class", class(x), "cannot be converted to an XML identifier."))
  if(length(x) <= 0) stop(paste("An empty string cannot be converted to an XML identifier."))
  
  # First, if it is a singleParameterTransform then check for a representative and use it instead eventually
  trEnv = flowEnv[['.singleParTransforms']]
  trans = flowEnv[[x]]
  if(!is.null(trEnv) && !is.null(trans) && is(trans, "singleParameterTransform"))
  {
    key = createTransformIdentifier(trans)
    if (!is.null(trEnv[[key]])) x = trEnv[[key]]        
  }
  
  # Now make it a safe XML identifier
  # 1) Put an underscore prefix if it starts with a number 
  if(substr(x, 1, 1) >= "0" && substr(x, 1, 1) <= "9") x = paste("_", x, sep="")
  # 2) Replace 'strange characters with '.'
  for(i in 1:nchar(x)) {
    if(!isNCNameChar(substr(x, i, i))) x <- paste(substr(x, 0, i - 1), '.', substr(x, i + 1, nchar(x)), sep= "")
  }
  x
}

isNCNameChar <- function(char) { #from: https://rdrr.io/bioc/flowUtils/src/R/writeGatingML.R
  # Based on the ASCII table and XML NCName syntax
  asciiValue = as.numeric(charToRaw(char))
  ##### PLEASE READ NOTE IN THE NEXT LINE
  if(asciiValue < 32) return(FALSE) ##### ORIGINAL VALUE IN SOURCE WAS 45 #####
  ##### PLEASE READ NOTE IN THE PREVIOUS LINE
  if(asciiValue == 47) return(FALSE)
  ##### PLEASE READ NOTE IN THE NEXT LINES
  #  if(asciiValue >= 58 && asciiValue <= 64) return(FALSE) ##### ORIGINAL NOT COMMENTED OUT #####
  #  if(asciiValue >= 91 && asciiValue <= 94) return(FALSE) ##### ORIGINAL NOT COMMENTED OUT #####
  #  if(asciiValue == 96) return(FALSE) ##### ORIGINAL NOT COMMENTED OUT #####
  #  if(asciiValue >= 123) return(FALSE) ##### ORIGINAL NOT COMMENTED OUT #####
  if(asciiValue == 127) return(FALSE) ##### THIS ONE DID NOT EXIST IN ORIGINAL SINCE COVERED IN PREVIOUS LINE
  TRUE    
}

isIdWrittenToXMLAlready <- function(id, flowEnv) { #from: https://rdrr.io/bioc/flowUtils/src/R/writeGatingML.R
  idsList = flowEnv[['.objectIDsWrittenToXMLOutput']]
  if (is.null(idsList[[id]])) {
    idsList[[id]] = TRUE
    flowEnv[['.objectIDsWrittenToXMLOutput']] = idsList 
    FALSE
  } else {
    addDebugMessage(paste("ID", id, "should be in the Gating-ML file already."), flowEnv)
    TRUE
  }
}

modified.write.gatingML(flowEnv, outputFile)

# R seems to be saving the .RData when exiting, so let's clean up to at least make that tiny (i.e., empty environment)
rm(list=ls())
