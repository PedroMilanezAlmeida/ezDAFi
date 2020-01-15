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

## This will load required packages and, if not found, install them without updating old Bioc pckgs
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

sessionInfo()

populationName <- "FJ_POPULATION_NAME"
minPopSize <- FJ_PAR_MINPOPSIZE

## Code to read gates from wsp file
popOfInt <- populationName
popOfInt

#find and load wsp file
wspNames <- dirname(path = "FJ_DATA_FILE_PATH") %>%
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
                            x = basename("FJ_DATA_FILE_PATH"))) %>%
  unlist(.)
nameSearchRes <- names(nameSearch)[nameSearch %>%
                                     names(.) %>% 
                                     nchar(.) %>% 
                                     which.max(.)]
sampleFCS <- paste0(nameSearchRes,
                    ".fcs")

gsFileName <- paste0(dirname("FJ_DATA_FILE_PATH"),
                     "/",
                     basename(wspNames),
                     ".",
                     sampleFCS,
                     ".gs")

if(file.exists(gsFileName)){
  gs <- load_gs(gsFileName)
} else {
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
  }

orig.parNames <- getData(gs[[1]]) %>%
  parameters(.) %>%
  pData(.) %>%
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
#if doing recursive analysis, run whole DAFi process for each
#non-terminal gate, adding the results to GatingSet as boolean filter
#importantly, we can still run children only analysis despite these changes, see below

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
names_gates_non_term_to_SOM <- list(names_gates_non_term)

names(names_gates_non_term_to_SOM) <-  unlist(names_gates_non_term_to_SOM) %>%
  gsub(pattern = popOfInt_full_path,
       replacement = "",
       x = names_gates_non_term_to_SOM) %>%
  strsplit(.,
           split = "/") %>%
  lapply(.,
         function(pop)
           pop[-1]  %>%
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
                fixed = TRUE) %>%
           gsub(pattern = " ",
                replacement = ".",
                x = .,
                fixed = TRUE) %>%
           gsub(pattern = "+",
                replacement = "pos",
                x = .,
                fixed = TRUE) %>%
           gsub(pattern = "-",
                replacement = "neg",
                x = .,
                fixed = TRUE) %>%
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
  unlist
if(FJ_PAR_CHILDREN){
  pops_to_SOM <- names_gates_to_SOM[tree_pos_gate_to_SOM == min(tree_pos_gate_to_SOM)]
} else {
  pops_to_SOM <- names_gates_to_SOM[order(tree_pos_gate_to_SOM)] #order is very important to ensure hierarchy of gates
  }
#actual DAFi
for(pop_to_SOM in seq_along(pops_to_SOM)){
  print(pops_to_SOM[pop_to_SOM] %>% names(.))
  for(fSample in seq_along(gs)) {
    if(dim(getData(gs[[fSample]],
                   pops_to_SOM[pop_to_SOM] %>% names(.))[,parIndices])[1] > minPopSize) { # in case a subpop is smaller than min #events, SOM is not applied
      ## Code to read the GatingSet data from each population that will be analyzed with DAFi
      if (nchar("FJ_PAR_APPLY_ON_PREV") > 5) { ## Expected either "None" or a valid file path
        load("FJ_PAR_APPLY_ON_PREV")
        fSOM <- NewData(fSOM, getData(gs[[fSample]],
                                      pops_to_SOM[pop_to_SOM] %>% names(.))[,parIndices]);
        } else {
          fSOM <- ReadInput(getData(gs[[fSample]],
                                    pops_to_SOM[pop_to_SOM] %>% names(.))[,parIndices],
                            compensate = FALSE,
                            transform = FALSE,
                            scale = FJ_PAR_SCALE,
                            silent = TRUE)
        }
        ## Code to generate SOM centroids
        set.seed(2020)
        fSOM <- BuildSOM(fSOM,
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
        ls_fSOM <- list(codes)
        names(ls_fSOM) <- rownames(pData(gs[[fSample]]))
        #create FlowSet with FlowSOM centroids
        fS_SOM <- lapply(ls_fSOM,
                         function(sample)
                           flowCore::flowFrame(sample)) %>%
          flowSet()
        parameters(fS_SOM[[1]]) <- parameters(getData(gs[[fSample]],
                                                      y = "root")[,parIndices])
        #create GatingSet with FlowSOM centroids
        suppressMessages(gs_SOM <- GatingSet(fS_SOM))
        pData(gs_SOM) <- pData(gs[[fSample]])
        #get gates and apply to cluster centroids
        gates <- basename(getNodes(gs[[fSample]]))[
          lapply(strsplit(x = dirname(getNodes(gs[[fSample]])),
                          split = "/"),
                 function(nodes)
                   tail(nodes, n = 1) == basename(pops_to_SOM[[pop_to_SOM]])) %>%
            unlist %>%
            which]
        for(gate in gates) {
          suppressMessages(add(gs_SOM,
                               getGate(gs[[fSample]],
                                       paste0(pops_to_SOM[[pop_to_SOM]],
                                              "/", gate))))
        }
        suppressMessages(recompute(gs_SOM))
        ## Code to update assignment of cell identity according to DAFi results
        SOM_labels <- vector(mode = "list",
                             length = length(gates))
        names(SOM_labels) <- gates
        for(gate in gates){
          SOM_labels[[gate]] <- rep(FALSE,
                                    FJ_PAR_XDIM*FJ_PAR_YDIM)
        }
        for(gate in gates) {
          SOM_labels[[gate]][getIndices(gs_SOM[[1]],
                                        gate)] <- TRUE
        }
        cell_DAFi_label <- vector(mode = "list",
                                  length = length(gates))
        names(cell_DAFi_label) <- gates
        for(gate in gates) {
          cell_DAFi_label[[gate]] <- SOM_labels[[gate]][fSOM$map$mapping[,1]]
        }
        all_cells_DAFi_label <- vector(mode = "list",
                                       length = length(gates))
        names(all_cells_DAFi_label) <- gates
        for(gate in gates) {
          all_cells_DAFi_label[[gate]] <- rep(FALSE,
                                              length(getIndices(gs[[fSample]],
                                                                y = pops_to_SOM[pop_to_SOM] %>% names(.))))
          all_cells_DAFi_label[[gate]][
            getIndices(gs[[fSample]],
                       y = pops_to_SOM[pop_to_SOM] %>% names(.))] <- cell_DAFi_label[[gate]]
          all_cells_DAFi_label[[gate]] <- list(all_cells_DAFi_label[[gate]])
          names(all_cells_DAFi_label[[gate]]) <- sampleNames(gs[[fSample]])
        }
        for(gate in gates) {
          add(gs[[fSample]],
              all_cells_DAFi_label[[gate]],
              parent = pops_to_SOM[pop_to_SOM] %>% names(.),
              name = paste0("DAFi_", gate) %>%
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
                     fixed = TRUE) %>%
                gsub(pattern = " ",
                     replacement = ".",
                     x = .,
                     fixed = TRUE) %>%
                gsub(pattern = "+",
                     replacement = "pos",
                     x = .,
                     fixed = TRUE) %>%
                gsub(pattern = "-",
                     replacement = "neg",
                     x = .,
                     fixed = TRUE))
        }
        suppressMessages(recompute(gs[[fSample]]))
      } else {
        gates <- basename(getNodes(gs[[fSample]]))[
          lapply(strsplit(x = dirname(getNodes(gs[[fSample]])),
                          split = "/"),
                 function(nodes)
                   tail(nodes, n = 1) == basename(pops_to_SOM[[pop_to_SOM]])) %>%
            unlist %>%
            which]
        for(gate in gates) {
          add(gs[[fSample]],
              getGate(gs[[fSample]],
                      paste0(pops_to_SOM[[pop_to_SOM]],
                             "/", gate)),
              parent = pops_to_SOM[pop_to_SOM] %>% names(.),
              name = paste0("DAFi_",
                            gate) %>%
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
                     fixed = TRUE) %>%
                gsub(pattern = " ",
                     replacement = ".",
                     x = .,
                     fixed = TRUE) %>%
                gsub(pattern = "+",
                     replacement = "pos",
                     x = .,
                     fixed = TRUE) %>%
                gsub(pattern = "-",
                     replacement = "neg",
                     x = .,
                     fixed = TRUE))
        }
        suppressMessages(recompute(gs[[fSample]]))
      }
    }
  }

# BUG: when running the plugin on a population down the tree from a previously DAFi-ed pop
# flowWorkspace does not import the derived paramenters (used in FlowJo to gate DAFi) and 
# descendant gates
# Failed solution: saving the GatingSet with DAFi gates to load in a second run. The DAFi
# gates are saved and loaded correctly, but the descendant gates are missing!

save_gs(gs,
        path = gsFileName)

DAFi_nodes <- getNodes(gs)[grep(pattern = "DAFi_",
                                x = basename(getNodes(gs)),
                                fixed = TRUE)]
DAFi_nodes <- DAFi_nodes[grep(pattern = popOfInt,
                              x = DAFi_nodes,
                              fixed = TRUE)]
all_cell_DAFi_label <- foreach(DAFi_node = DAFi_nodes) %do% {
  getIndices(gs[[1]],
             DAFi_node)
  }
names(all_cell_DAFi_label) <- DAFi_nodes
EventNumberDP <- read.csv(file = "FJ_DATA_FILE_PATH",
                          check.names=FALSE)$EventNumberDP
FJ_event_DAFi_label <- foreach(DAFi_node = DAFi_nodes) %do% {
  all_cell_DAFi_label[[DAFi_node]][EventNumberDP]
  }
names(FJ_event_DAFi_label) <- DAFi_nodes

## Extract DAFi clustering to pass back to FlowJo (or SeqGeq)
labels.ls <- foreach(DAFi_node = DAFi_nodes) %do% {
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
colnames(labels) <- foreach(DAFi_node = DAFi_nodes,
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
                                     fixed = TRUE) %>%
                                gsub(pattern = " ",
                                     replacement = ".",
                                     x = .,
                                     fixed = TRUE) %>%
                                gsub(pattern = "+",
                                     replacement = "pos",
                                     x = .,
                                     fixed = TRUE) %>%
                                gsub(pattern = "-",
                                     replacement = "neg",
                                     x = .,
                                     fixed = TRUE)
                              }

#sanity check
apply(labels,
      2,
      function(pop)
        mean(pop > 0))
#write results
write.csv(labels, file="FJ_CSV_OUPUT_FILE", row.names=FALSE, quote=FALSE)
write.csv(parNames, paste0("FJ_CSV_OUPUT_FILE", ".pars.csv"), row.names=FALSE)

all.labels.ls <- foreach(DAFi_node = DAFi_nodes) %do% {
  all.label <- as.matrix(all_cell_DAFi_label[[DAFi_node]] %>%
                           as.integer(.))
  }
names(all.labels.ls) <- DAFi_nodes

all.labels <- matrix(unlist(all.labels.ls,
                            use.names = FALSE),
                     ncol = length(all.labels.ls),
                     byrow = FALSE)
colnames(all.labels) <- foreach(DAFi_node = DAFi_nodes,
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
                                         fixed = TRUE) %>%
                                    gsub(pattern = " ",
                                         replacement = ".",
                                         x = .,
                                         fixed = TRUE) %>%
                                    gsub(pattern = "+",
                                         replacement = "pos",
                                         x = .,
                                         fixed = TRUE) %>%
                                    gsub(pattern = "-",
                                         replacement = "neg",
                                         x = .,
                                         fixed = TRUE)
                                 }
#2nd sanity check
apply(all.labels,
      2,
      function(pop)
        mean(pop > 0))

ls_ML <- list(all.labels)
names(ls_ML) <- "ls_ML"
#create FlowSet with DAFi results
fS_ML <- lapply(ls_ML,
                function(sample)
                  flowCore::flowFrame(sample)) %>%
  flowSet()

#create GatingSet with DAFi results
gs_ML <- GatingSet(fS_ML)

for(pop in colnames(all.labels)) {
  mat <- matrix(c(0.5, 1.5),
                ncol = 1,
                dimnames = list(c("min", "max"),
                                pop))
  rg <- rectangleGate(filterId = pop,
                      .gate = mat)
  add(gs_ML[["ls_ML"]],
      rg,
      parent = "root",
      name = pop)
  }
suppressMessages(recompute(gs_ML[["ls_ML"]]))

#3rd sanity check
foreach(pop = colnames(all.labels)) %do% {
  getIndices(gs_ML[["ls_ML"]],
             pop) %>%
    mean(.)
  } %>% unlist(.)

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
write.gatingML(flowEnv, outputFile)

# R seems to be saving the .RData when exiting, so let's clean up to at least make that tiny (i.e., empty environment)
rm(list=ls())
