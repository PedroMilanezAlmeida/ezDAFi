#######################################################################
# Copyright (c) 2020 Pedro Milanez-Almeida, Ph.D., NIAID/NIH
#
# Parts of this code were modified from the source of the packages used here.
# Credits for these parts go to the authors of the packages.
# See list of all packages used below.
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
tryCatch(suppressMessages(library("pheatmap")),
         error = function(e){
           install.packages(pkgs =  "pheatmap",
                            repos = 'http://cran.us.r-project.org')
           suppressMessages(library("pheatmap"))
         })
tryCatch(suppressMessages(library("gridExtra")),
         error = function(e){
           install.packages(pkgs =  "gridExtra",
                            repos = 'http://cran.us.r-project.org')
           suppressMessages(library("gridExtra"))
         })
tryCatch(suppressMessages(library("Rcpp")),
         error = function(e){
           install.packages(pkgs =  "Rcpp",
                            repos = 'http://cran.us.r-project.org')
           suppressMessages(library("Rcpp"))
         })
tryCatch(suppressMessages(library("glue")),
         error = function(e){
           install.packages(pkgs =  "glue",
                            repos = 'http://cran.us.r-project.org')
           suppressMessages(library("glue"))
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
tryCatch(suppressMessages(library("ggcyto")),
         error = function(e){
           if (!requireNamespace("BiocManager",
                                 quietly = TRUE))
             install.packages("BiocManager",
                              repos = 'http://cran.us.r-project.org')
           BiocManager::install("ggcyto",
                                update = FALSE,
                                ask = FALSE)
           suppressMessages(library("ggcyto"))
         })
elbow_finder <- function(y_values) { #https://stackoverflow.com/a/42810075
  # Max values to create line
  x_values <- seq_along(y_values)
  max_x_x <- max(x_values)
  max_x_y <- y_values[which.max(x_values)]
  max_y_y <- max(y_values)
  max_y_x <- x_values[which.max(y_values)]
  max_df <- data.frame(x = c(max_y_x, max_x_x), y = c(max_y_y, max_x_y))
  
  # Creating straight line between the max values
  fit <- lm(max_df$y ~ max_df$x)
  
  # Distance from point to line
  distances <- c()
  for(i in 1:length(x_values)) {
    distances <- c(distances, abs(coef(fit)[2]*x_values[i] - y_values[i] + coef(fit)[1]) / sqrt(coef(fit)[2]^2 + 1^2))
  }
  
  # Max distance point
  x_max_dist <- x_values[which.max(distances)]
  y_max_dist <- y_values[which.max(distances)]
  
  return(c(x_max_dist, y_max_dist))
}

hierarc.str <- function(DAFi_gate_name, n){
  i <- n
  pop_pars_v <- vector("character",
                       n)
  pop_pars_v[i] <- DAFi_gate_name
  while(i > 1) {
    pop_pars_v[i-1] <- strsplit(x =  pop_pars_v[i], 
                                split = "_DAFi_",
                                fixed = TRUE) %>%
      .[[1]] %>%
      tail(.,
           1) %>%
      paste0("_DAFi_",
             .) %>%
      nchar(.) %>%
      `-`(nchar(pop_pars_v[i])) %>%
      abs() %>%
      substr(x = pop_pars_v[i],
             start = 1,
             stop = .)
    i <- i - 1
  }
  return(pop_pars_v %>%
           unique)
}

sessionInfo()

# create R objects with FJ options
fj_data_file_path <- "FJ_DATA_FILE_PATH"
fj_data_file_path
batch_mode <- FJ_BATCH_MODE
batch_mode
#fj_par_apply_on_prev <- "FJ_PAR_APPLY_ON_PREV"
#fj_par_apply_on_prev
popOfInt <- "FJ_POPULATION_NAME"
popOfInt
minPopSize <- FJ_PAR_MINPOPSIZE
minPopSize
wspDir <- "FJ_PARM_WSPDIR"
wspDir
wspName <- "FJ_PARM_WSPNAME"
wspName
#parNames <- c(FJ_PARAMS_LIST)
#parNames
#fj_par_scale <- FJ_PAR_SCALE
fj_par_scale <- TRUE
fj_par_scale
fj_par_som <- FJ_PAR_SOM
fj_par_som
fj_par_xdim <- FJ_PAR_XDIM
fj_par_xdim
fj_par_ydim <- FJ_PAR_YDIM
fj_par_ydim
fj_csv_ouput_file <- "FJ_CSV_OUPUT_FILE"
fj_csv_ouput_file
fj_output_folder <- "FJ_OUTPUT_FOLDER"
fj_output_folder
fj_par_children <- FJ_PAR_CHILDREN
fj_sample_node_name <- "FJ_SAMPLE_NODE_NAME"
fj_population_name <- "FJ_POPULATION_NAME"
fj_transform <- FJ_TRANSFORM
fj_millis_time <- "FJ_MILLIS_TIME"
plotDir <- paste0(dirname(fj_data_file_path),
                  "/plots")
statsDir <- paste0(dirname(fj_data_file_path),
                   "/stats")
min.nPar <- FJ_MIN_N_PAR
#if(min.nPar == 1) {
#  min.nPar <- 2
#}
#min.nPar <- ifelse(min.nPar < 3,
#                   yes = 3,
#                  no = min.nPar)
#max.nPar <- FJ_MAX_N_PAR
#if(max.nPar < min.nPar) {
# max.nPar <- min.nPar
#}

# avoid issue with large numbers of centroids and small minPopSize
if(minPopSize < fj_par_xdim * fj_par_ydim) {
  minPopSize <- (fj_par_xdim * fj_par_ydim) + 1
}

## Code to read gates from wsp file
#find and load wsp file
wspName <- paste0(wspDir, 
                  "/",
                  wspName)
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
                            x = basename(fj_data_file_path),
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

sampleFCS_path <- sampleFCS_paths[basename(sampleFCS_paths) == sampleFCS]
sampleFCS_path

sampleID_doc <- which(sampleFCS_paths == sampleFCS_path)
sampleID_doc

# the following is meant to add support for acs files on windows
# TODO: test on Mac!
nchar_wspDir <- nchar(wspDir)
wspDir_last4 <- substr(wspDir, 
                       nchar_wspDir - 4 + 1,
                       nchar_wspDir)
if(wspDir_last4 != ".acs" & Sys.info()["sysname"] == "Windows") {
  if(!batch_mode){
    sampleFCS_path <- substring(sampleFCS_path, 2)
  } else {
    sampleFCS_paths <- substring(sampleFCS_paths, 2)
  }
}

if(wspDir_last4 == ".acs"){
  if(!batch_mode){
    sampleFCS_path <- paste0(wspDir,
                             "/",
                             sampleFCS_path)
  } else {
    sampleFCS_paths <- paste0(wspDir,
                              "/",
                              sampleFCS_paths)
  }
}

#parse wsp and fcs files into a GatingSet object
if(!batch_mode) {
  pathFCS <- tryCatch({
    data.frame(sampleID = CytoML::fj_ws_get_samples(ws)$sampleID[sampleID_doc],
               file = sampleFCS_path)
  },
  error = function(e) {
    data.frame(sampleID = CytoML::fj_ws_get_samples(ws,
                                                    sampNloc = "sampleNode")$sampleID[sampleID_doc],
               file = sampleFCS_path)
  })
} else {
  pathFCS <- tryCatch({
    data.frame(sampleID = CytoML::fj_ws_get_samples(ws)$sampleID,
               file = sampleFCS_paths)
  },
  error = function(e){
    data.frame(sampleID = CytoML::fj_ws_get_samples(ws,
                                                    sampNloc = "sampleNode")$sampleID,
               file = sampleFCS_paths)
  })
  
}
pathFCS$sampleID <- as.numeric(pathFCS$sampleID)
pathFCS

if(!batch_mode) {
  gs <- tryCatch({
    CytoML::flowjo_to_gatingset(ws,
                                name = 1,
                                path = pathFCS,
                                isNcdf = FALSE,
                                transform = fj_transform)
  },
  error = function(e){
    CytoML::flowjo_to_gatingset(ws,
                                name = 1,
                                path = pathFCS,
                                isNcdf = FALSE,
                                transform = fj_transform,
                                sampNloc = "sampleNode")
  })
} else {
  gs <- tryCatch({
    CytoML::flowjo_to_gatingset(ws,
                                name = 1,
                                path = pathFCS,
                                isNcdf = TRUE,
                                transform = fj_transform)
  },
  error = function(e){
    CytoML::flowjo_to_gatingset(ws,
                                name = 1,
                                path = pathFCS,
                                isNcdf = TRUE,
                                transform = fj_transform,
                                sampNloc = "sampleNode")
  })
}

### THIS FOLLOWING CODE IS SHUT OFF
### IT INTENDS TO ALLOW THE USER TO RUN DAFi ON PREVIOUSLY DAFi-ed POPS
### ALTHOUGH IT IS POSSIBLE TO RECOVER DAFi GATES FROM DERIVED PARAMETERS CSV FILES,
### IT IS NOT CURRENTLY POSSIBLE TO PARSE THE GATING TREE DOWN THE DAFi GATE
### :'(((
if(FALSE){
  #### flowworkspace won't load gates based on derived parameters
  ### therefore trying to do it manually here
  ## find derived parameter files
  der.par.paths <- XML::xpathApply(ws@doc,
                                   file.path("/Workspace/SampleList/Sample/DerivedParameters","DerivedParameter"),
                                   function(x)
                                     XML::xmlGetAttr(x, "importFile") %>%
                                     gsub(pattern = "%20", replacement = " ", x = .) %>%
                                     gsub(pattern = "file:", replacement = "", x = .)) %>%
    unlist %>%
    unique %>%
    grep(pattern = paste0(gsub(x = basename(as.character(pathFCS$file)),
                               pattern = ".fcs$",
                               replacement = "",
                               fixed = FALSE),
                          ".EPA.2.csv.EPA.csv"),
         fixed = TRUE,
         value = TRUE,
         x = .)
  der.par.paths
  
  # read DAFi derived parameter csv file and get DAFi gate names
  der.par.gate.names <- sapply(seq_along(der.par.paths),
                               function(der.par.path)
                                 read.csv(file = der.par.path, 
                                          header = FALSE, 
                                          stringsAsFactors = FALSE, 
                                          nrows = 1)[1,])
  der.par.gate.names
  # read DAFi derived parameter csv file and get DAFi gates as logical gates
  der.par.gates <- lapply(seq_along(der.par.gate.names),
                          function(der.par.gate.idx) {
                            colClasses <- rep("NULL", length(der.par.gate.idx))
                            colClasses[der.par.gate.idx] <- "numeric"
                            der.par.gates <-
                              read.csv(file = der.par.paths, 
                                       header = FALSE, 
                                       stringsAsFactors = FALSE,
                                       skip = 1,
                                       colClasses = colClasses)[,1]
                            der.par.gates <- 
                              ifelse(der.par.gates > 5e4,
                                     yes = TRUE,
                                     no = FALSE)
                            return(der.par.gates)
                          })
  names(der.par.gates) <- der.par.gate.names
  der.par.gates
  # find names of population DAFi was called on
  der.par.first.pop <- strsplit(x = names(der.par.gates),
                                split = "_DAFi_",
                                fixed = TRUE) %>%
    lapply(., `[[`, 1)
  names(der.par.first.pop) <- names(der.par.gates)
  der.par.first.pop
  # add DAFi logical gates from derived param file to GatingStrategy
  for(der.par.gate in names(der.par.gates)) {
    ls.der.par.gate <- list(der.par.gates[[der.par.gate]])
    names(ls.der.par.gate) <- sampleNames(gs)
    flowWorkspace::gs_pop_add(gs = gs,
                              gate = ls.der.par.gate,
                              parent = der.par.first.pop[[der.par.gate]],
                              name = der.par.gate)
  }
  
  
  flowWorkspace::recompute(gs)
  
}

flowCore::fsApply(flowWorkspace::gs_pop_get_data(gs), 
                  print)

pData.asDF <- flowCore::parameters(gh_pop_get_data(gs[[1]])) %>%
  flowCore::pData()

if(min.nPar >
   pData.asDF %>%
   dim(.) %>%
   .[1]) {
  min.nPar <-
    pData.asDF %>%
    dim(.) %>%
    .[1]
}

## In CSV files, the parameter names are often like FJComp-xxx while in parNames we may be getting Comp-dsfdsdxxx
#FJCompToComp <- function(char_vec) {
# if (any(grepl("FJComp", char_vec))) {
#   ## If it looks like there is FJComp-xxx in parNames but no FJComp-xxx in the column names of the FCS file, then
#   ## rename FJComp-xxx to Comp-xxx in the parNames and we will be looking for those instead.
#   new_char_vec <- gsub("^\\FJComp-", "Comp-", char_vec)
#   return(new_char_vec)
# } else {
#   return(char_vec)
# }
#}
#changeFJSpecialChar <- function(char_vec, cor_char_vec) {
# new_char_vec <- unlist(lapply(char_vec, function(name) {
#   if (name %in% cor_char_vec | name == "EventNumberDP") {
#     name
#   } else {
#     # Let's try [] to <>
#     name2 <- gsub("[", "<", name, fixed=TRUE)
#     name2 <- gsub("]", ">", name2, fixed=TRUE)
#     if (name2 %in% cor_char_vec) {
#       name2 # Worked, return it
#     } else {
#       # Previous fix did not do it, _ => / on the original names
#       name2 <- gsub("_", "/", name, fixed=TRUE)
#       if (name2 %in% cor_char_vec) {
#         name2 # Worked, return it
#       } else {
#         # That did not work either, let's try both [] => on top of the previous fix (_ => /)
#         name3 <- gsub("[", "<", name2, fixed=TRUE)
#         name3 <- gsub("]", ">", name3, fixed=TRUE)
#         if (name3 %in% cor_char_vec) {
#           name3 # Worked, finally, return it
#         } else {
#           ## Maybe we read a wrong dataset?
#           cat(paste("The input FCS file does not contain the provided input parameter, missing", name, "\n"))
#           "MISSINGPARAMETER"
#         }
#       }
#     }
#   }
# }))
# return(new_char_vec)
#}

# if the fcs was exported from FlowJo, it usually starts with "FJComp-"
# in such cases, there is no need to change parNames since it orig.parNames already
# has "FJComp-" in the begining of parameters names
#if(!grepl(pattern = "^FJComp-",
#         orig.parNames,
#         fixed = FALSE) %>%
#  any) {
# parNames <- FJCompToComp(parNames)
#}
#parNames <- changeFJSpecialChar(parNames, orig.parNames)

#if(grepl(pattern = "time", 
#        x = parNames, 
#        ignore.case = TRUE, 
#        fixed = FALSE) %>%
#  any(.)){
# stop("Please remove time as parameter for  clustering.")
#}

#if(grepl(pattern = "FSC|SSC", 
#        x = parNames, 
#        ignore.case = FALSE, 
#        fixed = FALSE) %>%
#  any(.) &
#  !fj_par_scale){
# stop("\n  It seems that FSC and/or SSC were included as clustering parameter, but with no data scaling.\n  FSC/SSC are handled differently than fluorochrome data in FlowJo.\n Please select scaling to make sure all data used in clustering is on the same scale.")
#}

#parIndices <- match(parNames, orig.parNames)

#if (length(parNames) == 0 || length(parIndices) == 0){
# stop("Something seems wrong, it's like the input FCS file does not contain the provided input parameters.", call.=FALSE)
#}

eventsCount <- flowWorkspace::gh_pop_get_data(gs[[1]]) %>%
  dim %>%
  .[1]

#if (length(parNames) == 0){
# stop("Some input parameters need to be selected!", call.=FALSE)
#}
#if (length(parNames) == 0 || "MISSINGPARAMETER" %in% parNames){
# stop("The input file is missing some of the specified parameters.", call.=FALSE)
#}
if (eventsCount == 0){
  stop("R failed to read the input file.", call.=FALSE)
}

#define gates of the selected samples that will be used here
if(batch_mode){
  gates_of_sel_sample <- flowWorkspace::gh_get_pop_paths(gs[[CytoML::fj_ws_get_samples(ws)$sampleID[sampleID_doc]]])
} else {
  gates_of_sel_sample <- flowWorkspace::gh_get_pop_paths(gs[[1]])
}
popOfInt_full_path <- gates_of_sel_sample[
  basename(gates_of_sel_sample) %in%
    popOfInt]

#get info about gating hierarchy for each pop of interest
names_gates_SOM <- foreach::foreach(pop = seq_along(basename(popOfInt_full_path))) %do% {
  strsplit(x = gates_of_sel_sample[
    grepl(pattern = paste0("/",
                           basename(popOfInt_full_path)[pop],
                           "/"),
          x = gates_of_sel_sample,
          fixed = TRUE)],
    split = paste0(gates_of_sel_sample[
      grepl(pattern = paste0("/",
                             basename(popOfInt_full_path)[pop],
                             "$"),
            x = gates_of_sel_sample,
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

#if doing recursive analysis, run whole DAFi process for each
#non-terminal gate, adding the results to GatingSet as boolean filter
#importantly, we can still run children only analysis despite these changes, see below

#find all gates down the gating hierarchy starting from the selected pop
names_gates_of_int <- foreach::foreach(pop = seq_along(basename(popOfInt_full_path)),
                                       .final = unlist) %do% {
                                         gates_of_sel_sample[grepl(pattern = paste0("/",
                                                                                    basename(popOfInt_full_path)[pop],
                                                                                    "/"),
                                                                   x = gates_of_sel_sample,
                                                                   fixed = TRUE)]
                                       }

#find non-terminal gates down the gating hierarchy, which will all be used in clustering
names_gates_non_term <- unlist(names_gates_of_int,
                               use.names = FALSE)[
                                 lapply(unlist(names_gates_of_int,
                                               use.names = FALSE),
                                        function(gate_of_int)
                                          grepl(pattern = paste0("/",
                                                                 basename(gate_of_int),
                                                                 "/"),
                                                x = unlist(names_gates_of_int, use.names = FALSE),
                                                fixed = TRUE) %>%
                                          any) %>%
                                   unlist
                                 ]
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
if(fj_par_children &
   !batch_mode){
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
#actual DAFi
for(pop_to_SOM in seq_along(pops_to_SOM)){
  print(pops_to_SOM[pop_to_SOM] %>%
          names(.))
  for(fSample in seq_along(gs)) {
    if(dim(flowWorkspace::gh_pop_get_data(gs[[fSample]],
                                          pops_to_SOM[pop_to_SOM] %>%
                                          names(.)))[1] > minPopSize) {#[,parIndices])[1] > minPopSize) { # in case a subpop is smaller than min #events, SOM is not applied
      ## Code to read the GatingSet data from each population that will be analyzed with DAFi
      #if (nchar(fj_par_apply_on_prev) > 5) { ## Expected either "None" or a valid file path
      #  load(fj_par_apply_on_prev)
      # fSOM <- FlowSOM::NewData(fSOM, 
      #                          flowWorkspace::gh_pop_get_data(gs[[fSample]],
      #                                                               pops_to_SOM[pop_to_SOM] %>%
      #                                                                 names(.))[,parIndices]);
      #} else {
      pop.exprs <- flowWorkspace::gh_pop_get_data(
        gs[[fSample]],
        pops_to_SOM[[pop_to_SOM]]) %>%
        flowCore::exprs()
      #pop.pData <- flowWorkspace::gh_pop_get_data(
      #  gs[[fSample]],
      #  pops_to_SOM[[pop_to_SOM]]) %>%
      #  parameters() %>%
      #  pData()
      colnames(pop.exprs) <- ifelse(
        is.na(
          pData.asDF$desc),
        yes = pData.asDF$name,
        no = pData.asDF$desc
      )
      gates <- basename(
        flowWorkspace::gh_pop_get_children(
          gs[[fSample]], 
          pops_to_SOM[[pop_to_SOM]])
      )
      for(gate in gates) {
        gate_par <- flowWorkspace::gh_pop_get_gate(
          gs[[fSample]],
          paste0(pops_to_SOM[[pop_to_SOM]],
                 "/",
                 gate))@parameters %>% 
          names
        gate_par <- colnames(pop.exprs)[pData.asDF$name %in% gate_par]
        in.gate <- flowWorkspace::gh_pop_get_indices(
          gs[[fSample]],
          paste0(pops_to_SOM[[pop_to_SOM]],
                 "/",
                 gate))[
                   flowWorkspace::gh_pop_get_indices(
                     gs[[fSample]],
                     pops_to_SOM[[pop_to_SOM]])
                   ]
        if(sum(in.gate) > 10 &
           sum(!in.gate) > 10) {
          markers.t <- apply(pop.exprs,
                             2, 
                             function(marker) 
                               t.test(marker[in.gate],
                                      marker[!in.gate],
                                      var.equal = FALSE)$statistic)%>%
            abs() %>%
            sort(decreasing = TRUE)
          hist.t <- hist(markers.t,
                         breaks = "FD",
                         plot = FALSE)
          #elbow <- elbow_finder(markers.t)[1]
          hist.t.threshold <- hist.t$breaks[which(hist.t$counts == 0)[1] + 1]
          #hist.n <- sum(markers.t > hist.t.threshold)
          #keep.marker <- min(elbow, 
          #                   hist.n)
          #keep.marker <- markers.t[1:keep.marker] %>%
          #  names
          if(min.nPar != 1) {
            top.nPar <- markers.t[1:min.nPar] %>%
              names
            keep.marker <- c(gate_par, top.nPar) %>%
              unique() %>%
              .[1:min.nPar]
          } else {
            #hist.n <- sum(markers.t > hist.t.threshold)
            #top.nPar <- markers.t[1:hist.n] %>%
            #  names
            elbow <- elbow_finder(markers.t)[1]
            top.nPar <- markers.t[1:elbow] %>%
              names
            keep.marker <- c(gate_par, 
                             top.nPar) %>%
              unique()
            if(length(keep.marker) == 1){
              keep.marker <- c(keep.marker, 
                               names(markers.t)[2])
            }
          }
          #keep.marker <- max(min.nPar,
          #                   elbow_finder(markers.t)[1])
          #keep.marker <- min(keep.marker, max.nPar)
          #keep.marker <- markers.t[1:keep.marker] %>%
          #  names
          keep.marker <- colnames(pop.exprs) %in% 
            keep.marker
          print("gate:")
          print(gate)
          print("markers t-stat:")
          print(markers.t)
          #          plot(markers.t)
          print("hist: ")
          print(markers.t[markers.t > hist.t.threshold])
          print("elbow: ")
          print(markers.t[1:elbow])
        } else { #else call: if there are too few cells in traditional gate
          keep.marker <- colnames(pop.exprs) %in% 
            gate_par
          print("gate:")
          print(gate)
          print("markers t-stat: not enough cells to generate t-statistic")
        }
        print("used markers: ")
        print(colnames(pop.exprs)[keep.marker])
        #pca with markers only
        #rm(pop.exprs)
        #pop.exprs.scale <- flowWorkspace::gh_pop_get_data(
        # gs[[fSample]],
        # pops_to_SOM[pop_to_SOM] %>%
        #   names()) %>%
        # flowCore::exprs() %>%
        # .[,keep.marker] %>%
        # scale(center = TRUE, 
        #       scale = TRUE)
        #pop.pca <- prcomp(x = flowWorkspace::gh_pop_get_data(
        # gs[[fSample]],
        # pops_to_SOM[pop_to_SOM] %>%
        #   names()) %>%
        #   flowCore::exprs() %>%
        #   .[,keep.marker],
        # center = TRUE,
        # scale. = TRUE)
        # print(gate)
        #        plot(summary(pop.pca)$importance[2,],
        #            main = gate)
        #colnames(pop.pca$x) <- paste0("PC", 
        #                             seq(dim(pop.pca$x)[2]))
        #ls_pop.pca <- list(pop.pca$x)
        #names(ls_pop.pca) <- rownames(flowCore::pData(gs[[fSample]]))
        #create FlowSet with FlowSOM centroids
        #fS_pop.pca <- lapply(ls_pop.pca,
        #                    function(sample)
        #                      flowCore::flowFrame(sample)) %>%
        # flowCore::flowSet()
        #fSOM <- FlowSOM::ReadInput(fS_pop.pca,
        #                          compensate = FALSE,
        #                          transform = FALSE,
        #                          scale = FALSE,
        #                          silent = TRUE)
        #### SOM/kmeans ####
        fSOM <- FlowSOM::ReadInput(
          flowWorkspace::gh_pop_get_data(gs[[fSample]],
                                         pops_to_SOM[pop_to_SOM] %>%
                                           names(.)) %>%
            #.[,parNames] %>%
            .[,keep.marker],
          compensate = FALSE,
          transform = FALSE,
          scale = fj_par_scale,
          silent = TRUE)
        if(fj_par_som){ #if user decides to use self-organizing maps
          # Code to generate SOM centroids
          set.seed(2020)
          fSOM <- FlowSOM::BuildSOM(fSOM,
                                    colsToUse = NULL,
                                    silent = TRUE,
                                    xdim = fj_par_xdim,
                                    ydim = fj_par_ydim)
          # Code to gate flowSOM results
          # retrieve codes
          if(fj_par_scale) {
            codes <- t(apply(fSOM$map$codes,
                             1,
                             function(centroid)
                               centroid *
                               fSOM$scaled.scale +
                               fSOM$scaled.center))
          } else {
            codes <- fSOM$map$codes
          }
        } else { # if the user decides to use kmeans
          # Code to generate kmeans centroids
          set.seed(2020)
          fkMeans <- stats::kmeans(x = fSOM$data,#[,parNames],
                                   centers = fj_par_xdim*fj_par_ydim,
                                   iter.max = 100)
          if(fkMeans$ifault == 4){ # https://stackoverflow.com/a/30055776
            fkMeans <- stats::kmeans(x = fSOM$data,
                                     centers = fkMeans$centers,
                                     iter.max = 100,
                                     algorithm = "MacQueen")}
          # Code to gate kmeans results
          # retrieve codes
          if(fj_par_scale) {
            codes <- t(apply(fkMeans$centers,
                             1,
                             function(centroid)
                               centroid *
                               fSOM$scaled.scale +
                               fSOM$scaled.center))
          } else {
            #codes.pca <- fkMeans$centers
            codes <- fkMeans$centers
          }
        }
        #codes <- t(t(codes.pca %*% 
        #              t(pop.pca$rotation)) * 
        #            pop.pca$scale + 
        #            pop.pca$center)
        #print(dim(codes))
        #codes <- codes.pca %*%
        #diag(pop.pca$d) %*%
        #t(pop.pca$v) %>%
        #apply(.,
        #  1,
        #  function(parameter)
        #    parameter *
        #    attr(pop.exprs.scale, "scaled:scale") +
        #   attr(pop.exprs.scale, "scaled:center")) %>%
        #t
        #### graph-based clustering ####
        # most of this section is from
        #http://jef.works/blog/2017/09/13/graph-based-community-detection-for-clustering-analysis/
        # find nearest neighbors
        #k <- 10
        #nn.idx <- RANN::nn2(codes,
        #                  k = k)$nn.idx
        # following code is from
        #https://rdrr.io/github/CamaraLab/STvEA/src/R/seurat_anchor_correction.R
        # build adjacency matrix
        #j <- as.numeric(x = t(x = nn.idx))
        #i <- ((1:length(x = j)) - 1) %/% k + 1
        #nn.mat <- Matrix::sparseMatrix(i = i,
        #                             j = j, 
        #                             x = 1,
        #                             dims = c(nrow(codes),
        #                                      nrow(codes)))
        #rownames(nn.mat) <- colnames(nn.mat) <- seq_len(nrow(codes))
        #source: https://rpubs.com/nurakawa/spectral-clustering
        #dg <- Matrix::colSums(nn.mat) # degrees of vertices
        #graph_laplacian <- diag(dg) - nn.mat
        #ei <- eigen(graph_laplacian,
        #           symmetric = TRUE)
        #ei.vec <- ei$vectors[,(dim(nn.mat)[1] - min.nPar):(dim(nn.mat)[1] - 1)]
        #spec.clust <- kmeans(x = ei.vec,
        #                    centers = 20,
        #                    iter.max = 100)
        #spec.codes <- apply(codes,
        #                   2,
        #                   function(marker)
        #                     sapply(seq_len(20),
        #                            function(cluster)
        #                              mean(marker[spec.clust$cluster == cluster])))
        #codes <- spec.codes
        # build and simplify graphs (remove self-loops).
        #kM.g <- igraph::graph.adjacency(nn.mat, 
         #                              mode = "undirected")
        #kM.g <- igraph::simplify(kM.g)
        #Finally, build communities with short random walks in the graph and update the identity of each single cell based on nearest meta-cell and the results of meta-clustering.
        #kM.km <- igraph::cluster_walktrap(kM.g)
        #kM.com <- kM.km$membership
        #names(kM.com) <- kM.km$names
        #print(table(kM.com))
        #V(kM.g)$color <- heat.colors(length(kM.km))[kM.com[names(V(kM.g))]]
        #codes <- apply(codes,
        #               2,
        #               function(marker)
        #                 sapply(seq_len(length(kM.km)),
        #                        function(cluster)
        #                          mean(marker[kM.com == cluster])))
        #### gate centroids ####
        ls_fSOM <- list(codes)
        names(ls_fSOM) <- rownames(flowCore::pData(gs[[fSample]]))
        #create FlowSet with FlowSOM centroids
        fS_SOM <- lapply(ls_fSOM,
                         function(sample)
                           flowCore::flowFrame(sample)) %>%
          flowCore::flowSet()
        flowCore::parameters(fS_SOM[[1]]) <- flowCore::parameters(
          flowWorkspace::gh_pop_get_data(
            gs[[fSample]],
            y = "root")[,keep.marker])#[,parNames][,keep.marker])
        #create GatingSet with FlowSOM centroids
        suppressMessages(gs_SOM <- GatingSet(fS_SOM))
        flowCore::pData(gs_SOM) <- flowCore::pData(gs[[fSample]])
        #get gates and apply to cluster centroids
        suppressMessages(
          flowWorkspace::gs_pop_add(
            gs_SOM,
            flowWorkspace::gh_pop_get_gate(
              gs[[fSample]],
              paste0(pops_to_SOM[[pop_to_SOM]],
                     "/", gate))))
        tryCatch({
          suppressMessages(flowWorkspace::recompute(gs_SOM))
        },
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
        #### gate cells based on centroid gating ####
        ## Code to update assignment of cell identity according to DAFi results
        #graph_labels <- rep(FALSE,
        #                   length(kM.km))
        #graph_labels[flowWorkspace::gh_pop_get_indices(gs_SOM[[1]],
        #                                              gate)] <- TRUE
        #
        #SOM_labels <- graph_labels[kM.com]
        SOM_labels <- rep(FALSE,
                          dim(codes)[1])
        SOM_labels[
          flowWorkspace::gh_pop_get_indices(gs_SOM[[1]],
                                            gate)
          ] <- TRUE
        #spec_labels <- rep(FALSE,
        #                  dim(spec.clust$centers)[1])
        #spec_labels[flowWorkspace::gh_pop_get_indices(gs_SOM[[1]],
        #                                             gate)] <- TRUE
        #SOM_labels <- spec_labels[spec.clust$cluster]
        if(fj_par_som){
          cell_DAFi_label <- SOM_labels[fSOM$map$mapping[,1]]
        } else {
          cell_DAFi_label <- SOM_labels[fkMeans$cluster]
        }
        all_cells_DAFi_label <- rep(FALSE,
                                    length(
                                      flowWorkspace::gh_pop_get_indices(
                                        gs[[fSample]],
                                        y = pops_to_SOM[pop_to_SOM] %>%
                                          names(.))))
        all_cells_DAFi_label[
          flowWorkspace::gh_pop_get_indices(gs[[fSample]],
                                            y = pops_to_SOM[pop_to_SOM] %>%
                                              names(.))] <- cell_DAFi_label
        all_cells_DAFi_label <- list(all_cells_DAFi_label)
        names(all_cells_DAFi_label) <- sampleNames(gs[[fSample]])
        flowWorkspace::gs_pop_add(gs[[fSample]],
                                  all_cells_DAFi_label,
                                  parent = pops_to_SOM[pop_to_SOM] %>%
                                    names(.),
                                  name = paste0("DAFi_", gate) ) %>%
          gsub(pattern = "^/",
               replacement = "",
               x = .)
        suppressMessages(flowWorkspace::recompute(gs[[fSample]]))
      }
    } else {
      gates <- basename(
        flowWorkspace::gh_pop_get_children(
          gs[[fSample]], 
          pops_to_SOM[[pop_to_SOM]]))
      for(gate in gates) {
        flowWorkspace::gs_pop_add(
          gs[[fSample]],
          flowWorkspace::gh_pop_get_gate(
            gs[[fSample]],
            paste0(pops_to_SOM[[pop_to_SOM]],
                   "/", 
                   gate)),
          parent = pops_to_SOM[pop_to_SOM] %>% 
            names(.),
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

if(batch_mode){
  post_DAFi_gates <- flowWorkspace::gh_get_pop_paths(
    gs[[CytoML::fj_ws_get_samples(ws)$sampleID[sampleID_doc]]])
} else {
  post_DAFi_gates <- flowWorkspace::gh_get_pop_paths(gs[[1]])
}

# Close the flowjo workspace connection
CytoML::flowjo_ws_close(ws)

DAFi_nodes <- post_DAFi_gates[
  grep(pattern = "DAFi_",
       x = basename(post_DAFi_gates),
       fixed = TRUE)
  ]
DAFi_nodes <- DAFi_nodes[grep(pattern = popOfInt,
                              x = DAFi_nodes,
                              fixed = TRUE)]

tree_pos_DAFi_gate_to_SOM <- strsplit(x = DAFi_nodes,
                                      split = "/",
                                      fixed = TRUE) %>%
  lapply(length) %>%
  unlist(.)
DAFi_nodes <- DAFi_nodes[order(tree_pos_DAFi_gate_to_SOM)] #order is very important to ensure hierarchy of gates

DAFi_leaf_nodes <- post_DAFi_gates[
  grep(pattern = "DAFi_",
       x = post_DAFi_gates,
       fixed = TRUE)
  ]
DAFi_leaf_nodes <- DAFi_leaf_nodes[grep(pattern = popOfInt,
                                        x = DAFi_leaf_nodes,
                                        fixed = TRUE)]

if(batch_mode){
  modified.autoplot.GatingSet <- function(object, gate, x = NULL,  y = "SSC-A", bins = 30, axis_inverse_trans = TRUE, stats = "percent", ...){
    if(missing(gate))
      stop("Must specifiy 'gate'!")
    if(is.null(x)){
      #determine dimensions from gate
      g <- gh_pop_get_gate(object[[1]], gate[1])
      params <- parameters(g)
      nDims <- length(params)
      if(nDims == 1){
        x <- params
        y <- flowWorkspace:::fix_y_axis(gs = object, x = x, y = y)
      }else{
        x <- params[1]
        y <- params[2]
      }
    }
    
    mapping <- aes_q(x = as.symbol(x), y = as.symbol(y))
    
    p <- ggcyto(object, mapping, ...) + geom_hex(bins = bins) + geom_gate(gate)
    if(stats != "none") {
      p <- p + geom_stats(type = stats)
    }
    p <- p + ggcyto_par_set(limits = "instrument")
    if(axis_inverse_trans)
      p <- p + axis_x_inverse_trans() + axis_y_inverse_trans()
    p
    
  }
  
  modified.autoplot.GatingHierarchy <- function(object, gate, y = "SSC-A", bool=FALSE
                                                , arrange.main = sampleNames(object), arrange=TRUE, merge=TRUE
                                                , projections = list()
                                                , strip.text = c("parent", "gate")
                                                , path = "auto"
                                                , ...){
    strip.text <- match.arg(strip.text)
    if(missing(gate)){
      gate <- gs_get_pop_paths(object, path = path)
      gate <- setdiff(gate,"root")
    }else if (is.numeric(gate)){
      gate <- gs_get_pop_paths(object, path = path)[gate]
    }
    
    #match given axis to channel names
    fr <- gh_pop_get_data(object, use.exprs = FALSE)
    projections <- lapply(projections, function(thisPrj){
      sapply(thisPrj, function(thisAxis)getChannelMarker(fr, thisAxis)[["name"]])
    })
    
    
    plotList <- flowWorkspace:::.mergeGates(object, gate, bool, merge, projections = projections)
    Objs <- lapply(plotList,function(plotObjs){
      
      if(is.list(plotObjs)){
        gate <- plotObjs[["popIds"]]
        parent <- plotObjs[["parentId"]]
        myPrj <- projections[[as.character(gate[1])]]
        
      }else{
        gate <- plotObjs
        parent <- gs_pop_get_parent(object, gate, path = path)
        myPrj <- projections[[as.character(gate)]]
      }
      
      
      if(is.null(myPrj)){
        p <- modified.autoplot.GatingSet(object, gate, y = y, ...)
      }else{
        p <- modified.autoplot.GatingSet(object, gate, x = myPrj[["x"]], y = myPrj[["y"]], ...)
      }
      
      p <- p + guides(fill=FALSE) + labs(title = NULL)
      myTheme <- theme(axis.title = element_text(color = gray(0.3), size = 8)
                       , axis.text = element_text(color = gray(0.3), size = 6)
                       , strip.text = element_text(size = 10)
                       , plot.margin = unit(c(0,0,0,0), "cm")
                       , panel.spacing = unit(0, "cm")
      )
      p <- p + myTheme
      
      #rename sample name with parent or current pop name in order to display it in strip
      
      if(strip.text == "parent"){
        popName <- parent
      }else{
        popName <- paste(gate, collapse = "|")
      }
      attr(p$data, "strip.text") <- popName
      
      p
      
    })
    
    if(arrange){
      #convert it to a special class to dispatch the dedicated print method
      Objs <- as(Objs, "ggcyto_GatingLayout")
      Objs@arrange.main <- arrange.main
    }
    
    Objs
    
  }
  
  # create subfolder to plot results
  if(!dir.exists(plotDir)) {
    dir.create(plotDir)
  }
  # create and save plots
  for(fSample in seq_along(gs)) { # for each sample
    for(DAFi_leaf_node in DAFi_leaf_nodes){ # for each terminal DAFi node
      n.up.gates <- (nchar(as.character(DAFi_leaf_node))) - # get the number of gates up the tree for each terminal DAFi node (important later to determine size of PNG file)
        (nchar(gsub(pattern = "/",
                    replacement = "",
                    x =  DAFi_leaf_node,
                    fixed = TRUE)))
      png(paste0(plotDir,
                 "/backgating_",
                 basename(DAFi_leaf_node),
                 "_for_",
                 sampleNames(gs[[fSample]]),
                 ".png"),
          width = 4 * n.up.gates %>%
            sqrt(.) %>% 
            floor(.) * 300,
          height = 3 * n.up.gates %>%
            sqrt(.) %>% 
            ceiling(.) * 300,
          res = 300)
      print(
        modified.autoplot.GatingHierarchy(gs[[fSample]],
                                          bins = 256,
                                          stats = "none",
                                          strip.text = "parent",
                                          arrange.main = paste0("Sample: ", 
                                                                sampleNames(gs[[fSample]]),
                                                                " - Gate: ",
                                                                basename(DAFi_leaf_node))) +
          theme_light() +
          geom_overlay(DAFi_leaf_node, 
                       size = 0.1, 
                       alpha = 1)
      )
      dev.off()
    }
  }
  
  save_gs(gs,
          path = paste0(wspName,
                        ".R.gs"),
          cdf = "move")
  fileConn <- file(paste0(wspName,
                          ".R"))
  writeLines(c("library(foreach)",
               "library(gridExtra)",
               "library(magrittr)",
               "library(XML)",
               "library(FlowSOM)",
               "library(flowWorkspace)",
               "library(CytoML)",
               "library(flowUtils)",
               "library(flowCore)",
               "library(ggcyto)",
               "",
               '"============================="',
               '"Open folder with plots:"',
               paste0("system2(command =",
                      '"open"',
                      ", ",
                      "args = ",
                      "\"",
                      paste0(plotDir),
                      "\"",
                      "%>% normalizePath() %>% shQuote()",
                      ")"),
               '"============================="',
               "",
               '"============================="',
               '"Load GatingSet with all gates, FCS files and DAFi results:"',
               paste0("gs <- load_gs(",
                      "\"",
                      paste0(wspName,
                             ".R.gs"),
                      "\"",
                      ")"),
               '"============================="',
               "",
               '"============================="',
               '"Use the loaded packages to explore your data!"',
               '"For example:"',
               "ggcyto::autoplot(gs[[1]], bins = 128)",
               '"============================="'),
             fileConn)
  close(fileConn)
  system2(command = "open",
          args = paste0(wspName,
                        ".R") %>%
            normalizePath(.) %>%
            shQuote(.))
} else {
  all_cell_DAFi_label <- foreach::foreach(DAFi_node = DAFi_nodes) %do% {
    flowWorkspace::gh_pop_get_indices(gs[[1]],
                                      DAFi_node)
  }
  names(all_cell_DAFi_label) <- DAFi_nodes
  
  EventNumberDP <- read.csv(file = fj_data_file_path,
                            check.names=FALSE)$EventNumberDP
  
  FJ_event_DAFi_label <- foreach::foreach(DAFi_node = DAFi_nodes) %do% {
    all_cell_DAFi_label[[DAFi_node]][EventNumberDP]
  }
  names(FJ_event_DAFi_label) <- DAFi_nodes
  
  ## Extract DAFi clustering to pass back to FlowJo (or SeqGeq)
  labels.ls <- foreach::foreach(DAFi_node = DAFi_nodes) %do% {
    ## Please note "FJ_event_DAFi_label" here stemming from DAFi
    ## FlowJo, let's do labels as 100, 200, 300, all with a tiny bit of noise (to make FlowJo cluster those better)
    label <- as.matrix(as.integer(FJ_event_DAFi_label[[DAFi_node]]) * 1e5 + 
                         rnorm(n = length(FJ_event_DAFi_label[[DAFi_node]]),
                               mean = 0,
                               sd = 1000))
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
                                                    split = popOfInt,
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
                                           trimws(.,
                                                  which = "right") %>%
                                           paste0(popOfInt,
                                                  "_",
                                                  .) %>%
                                           gsub(pattern = ",",
                                                replacement = ".",
                                                x = .,
                                                fixed = TRUE)
                                       }
  
  #sanity check
  print(
    apply(labels,
          2,
          function(pop)
            mean(pop > 5e4))
  )
  #write derived parameters
  write.csv(labels, 
            file = fj_csv_ouput_file, 
            row.names = FALSE, 
            quote = TRUE)
  
  #get stats
  pop.stats.count <- gh_pop_get_stats(gs[[1]],
                                      type =  "count")
  pop.stats.percent <- gh_pop_get_stats(gs[[1]],
                                        type =  "percent")
  pop.stats.percent$percent <- pop.stats.percent$percent * 100
  
  pop.stats.count.DAFi <- pop.stats.count[pop.stats.count$pop %in% 
                                            DAFi_nodes,]$count
  names(pop.stats.count.DAFi) <- pop.stats.count[pop.stats.count$pop %in% 
                                                   DAFi_nodes,]$pop
  pop.stats.count.DAFi <- data.frame(pop.stats.count.DAFi) %>%
    t
  rownames(pop.stats.count.DAFi) <- sampleFCS
  
  pop.stats.percent.DAFi <- pop.stats.percent[pop.stats.percent$pop %in% 
                                                DAFi_nodes,]$percent
  names(pop.stats.percent.DAFi) <- pop.stats.percent[pop.stats.percent$pop %in% 
                                                       DAFi_nodes,]$pop
  pop.stats.percent.DAFi <- data.frame(pop.stats.percent.DAFi) %>%
    t
  rownames(pop.stats.percent.DAFi) <- sampleFCS
  
  pop.stats.count.trad <- pop.stats.count[
    match(gsub(pattern = "DAFi_", 
               replacement = "",
               x = DAFi_nodes,
               fixed = TRUE),
          pop.stats.count$pop),
    ]$count
  names(pop.stats.count.trad) <- pop.stats.count[
    match(gsub(pattern = "DAFi_", 
               replacement = "",
               x = DAFi_nodes,
               fixed = TRUE),
          pop.stats.count$pop),
    ]$pop
  pop.stats.count.trad <- data.frame(pop.stats.count.trad) %>%
    t
  rownames(pop.stats.count.trad) <- sampleFCS
  
  pop.stats.percent.trad <- pop.stats.percent[
    match(gsub(pattern = "DAFi_", 
               replacement = "",
               x = DAFi_nodes,
               fixed = TRUE),
          pop.stats.count$pop)
    ,]$percent
  names(pop.stats.percent.trad) <- pop.stats.percent[
    match(gsub(pattern = "DAFi_", 
               replacement = "",
               x = DAFi_nodes,
               fixed = TRUE),
          pop.stats.count$pop)
    ,]$pop
  pop.stats.percent.trad <- data.frame(pop.stats.percent.trad) %>%
    t
  rownames(pop.stats.percent.trad) <- sampleFCS
  #write stats
  if(!dir.exists(statsDir)) {
    dir.create(statsDir)
  }
  DAFi.count.file <- paste0(statsDir,
                            "/",
                            "DAFi_count_",
                            popOfInt,
                            ".csv")
  DAFi.percent.file <- paste0(statsDir,
                              "/",
                              "DAFi_percent_",
                              popOfInt,
                              ".csv")
  trad.count.file <- paste0(statsDir,
                            "/",
                            "trad_count_",
                            popOfInt,
                            ".csv")
  trad.percent.file <- paste0(statsDir,
                              "/",
                              "trad_percent_",
                              popOfInt,
                              ".csv")
  if(!DAFi.count.file %>%
     file.exists()) {
    write.table(x = c("sample",
                      colnames(pop.stats.count.DAFi)) %>%
                  t,
                append = FALSE,
                sep = ",",
                file = DAFi.count.file,
                row.names = FALSE, 
                quote = TRUE,
                col.names = FALSE)
  } else if(
    !((suppressMessages(
      read.csv(file = DAFi.count.file,
               header = FALSE,
               nrows = 1)) %>%
      unlist(use.names = FALSE) %>%
      as.character() ==
      c("sample",
        colnames(pop.stats.count.DAFi))) %>%
      all)
  ) {
    stop("Either the gates of \"",
         popOfInt,
         "\" changed since ezDAFi's last run\nor batch analysis was called on samples with different gating strategies.\nPlease delete stats before rerunning and make sure the same gating\nstrategy is applied to all samples.", 
         call.=FALSE)
  }
  if(!(sampleFCS %in%
       read.csv(file = DAFi.count.file,
                header = TRUE,
                colClasses = c("character",
                               rep("NULL",
                                   length(DAFi_nodes))))[,1])) {
    write.table(x = pop.stats.count.DAFi,
                append = TRUE,
                sep = ",",
                file = DAFi.count.file,
                row.names = TRUE, 
                quote = TRUE,
                col.names = FALSE)
  } else {
    sampleFCS.pos <- which(
      read.csv(file = DAFi.count.file,
               header = FALSE,
               colClasses = c("character",
                              rep("NULL",
                                  length(DAFi_nodes))))[,1] ==
        sampleFCS
    )
    DAFi.count.df <- read.csv(file = DAFi.count.file,
                              header = FALSE,
                              skip = 1)
    colnames(DAFi.count.df) <- c("sample",
                                 colnames(pop.stats.count.DAFi))
    DAFi.count.df[(sampleFCS.pos - 1),] <- c(sampleFCS,
                                             pop.stats.count.DAFi[1,])
    write.table(x = DAFi.count.df,
                append = FALSE,
                sep = ",",
                file = DAFi.count.file,
                row.names = FALSE, 
                quote = TRUE,
                col.names = TRUE)
  }
  if(!DAFi.percent.file %>%
     file.exists()) {
    write.table(x = c("sample", 
                      colnames(pop.stats.percent.DAFi)) %>%
                  t,
                append = FALSE,
                sep = ",",
                file = DAFi.percent.file,
                row.names = FALSE, 
                quote = TRUE,
                col.names = FALSE)
  } else if(
    !((suppressMessages(
      read.csv(file = DAFi.percent.file,
               header = FALSE,
               nrows = 1)
    ) %>%
    unlist(use.names = FALSE) %>%
    as.character() ==
    c("sample",
      colnames(pop.stats.percent.DAFi))) %>%
    all)
  ) {
    stop("Either the gates of \"",
         popOfInt,
         "\" changed since ezDAFi's last run\nor batch analysis was called on samples with different gating strategies.\nPlease delete stats before rerunning and make sure the same gating\nstrategy is applied to all samples.", 
         call.=FALSE)
  }
  if(!(sampleFCS %in%
       read.csv(file = DAFi.percent.file,
                header = TRUE,
                colClasses = c("character",
                               rep("NULL",
                                   length(DAFi_nodes))))[,1])) {
    write.table(x = pop.stats.percent.DAFi,
                append = TRUE,
                sep = ",",
                file = DAFi.percent.file,
                row.names = TRUE, 
                quote = TRUE,
                col.names = FALSE)
  } else {
    sampleFCS.pos <- which(
      read.csv(file = DAFi.percent.file,
               header = FALSE,
               colClasses = c("character",
                              rep("NULL",
                                  length(DAFi_nodes))))[,1] ==
        sampleFCS
    )
    DAFi.percent.df <- read.csv(file = DAFi.percent.file,
                                header = FALSE,
                                skip = 1)
    colnames(DAFi.percent.df) <- c("sample",
                                   colnames(pop.stats.percent.DAFi))
    DAFi.percent.df[(sampleFCS.pos - 1),] <- c(sampleFCS,
                                               pop.stats.percent.DAFi[1,])
    write.table(x = DAFi.percent.df,
                append = FALSE,
                sep = ",",
                file = DAFi.percent.file,
                row.names = FALSE, 
                quote = TRUE,
                col.names = TRUE)
  }
  if(!trad.count.file %>%
     file.exists()) {
    write.table(x = c("sample",
                      colnames(pop.stats.count.trad)) %>%
                  t,
                append = FALSE,
                sep = ",",
                file = trad.count.file,
                row.names = FALSE, 
                quote = TRUE,
                col.names = FALSE)
  } else if(
    !((suppressMessages(
      read.csv(file = trad.count.file,
               header = FALSE,
               nrows = 1)) %>%
      unlist(use.names = FALSE) %>%
      as.character() ==
      c("sample",
        colnames(pop.stats.count.trad))) %>%
      all)
  ) {
    stop("Either the gates of \"",
         popOfInt,
         "\" changed since ezDAFi's last run\nor batch analysis was called on samples with different gating strategies.\nPlease delete stats before rerunning and make sure the same gating\nstrategy is applied to all samples.", 
         call.=FALSE)
  }
  if(!(sampleFCS %in%
       read.csv(file = trad.count.file,
                header = TRUE,
                colClasses = c("character",
                               rep("NULL",
                                   length(DAFi_nodes))))[,1])) {
    write.table(x = pop.stats.count.trad,
                append = TRUE,
                sep = ",",
                file = trad.count.file,
                row.names = TRUE, 
                quote = TRUE,
                col.names = FALSE)
  } else {
    sampleFCS.pos <- which(
      read.csv(file = trad.count.file,
               header = FALSE,
               colClasses = c("character",
                              rep("NULL",
                                  length(DAFi_nodes))))[,1] ==
        sampleFCS
    )
    trad.count.df <- read.csv(file = trad.count.file,
                              header = FALSE,
                              skip = 1)
    colnames(trad.count.df) <- c("sample",
                                 colnames(pop.stats.count.trad))
    trad.count.df[(sampleFCS.pos - 1),] <- c(sampleFCS,
                                             pop.stats.count.trad[1,])
    write.table(x = trad.count.df,
                append = FALSE,
                sep = ",",
                file = trad.count.file,
                row.names = FALSE, 
                quote = TRUE,
                col.names = TRUE)
  }
  if(!trad.percent.file %>%
     file.exists()) {
    write.table(x = c("sample",
                      colnames(pop.stats.percent.trad)) %>%
                  t,
                append = FALSE,
                sep = ",",
                file = trad.percent.file,
                row.names = FALSE, 
                quote = TRUE,
                col.names = FALSE)
  } else if(
    !((suppressMessages(
      read.csv(file = trad.percent.file,
               header = FALSE,
               nrows = 1)) %>%
      unlist(use.names = FALSE) %>%
      as.character() ==
      c("sample",
        colnames(pop.stats.percent.trad))) %>%
      all)
  ) {
    stop("Either the gates of \"",
         popOfInt,
         "\" changed since ezDAFi's last run\nor batch analysis was called on samples with different gating strategies.\nPlease delete stats before rerunning and make sure the same gating\nstrategy is applied to all samples.", 
         call.=FALSE)
  }
  if(!(sampleFCS %in%
       read.csv(file = trad.percent.file,
                header = TRUE,
                colClasses = c("character",
                               rep("NULL",
                                   length(DAFi_nodes))))[,1])) {
    write.table(x = pop.stats.percent.trad,
                append = TRUE,
                sep = ",",
                file = trad.percent.file,
                row.names = TRUE, 
                quote = TRUE,
                col.names = FALSE)
  } else {
    sampleFCS.pos <- which(
      read.csv(file = trad.percent.file,
               header = FALSE,
               colClasses = c("character",
                              rep("NULL",
                                  length(DAFi_nodes))))[,1] ==
        sampleFCS
    )
    trad.percent.df <- read.csv(file = trad.percent.file,
                                header = FALSE,
                                skip = 1)
    colnames(trad.percent.df) <- c("sample",
                                   colnames(pop.stats.percent.trad))
    trad.percent.df[(sampleFCS.pos - 1),] <- c(sampleFCS,
                                               pop.stats.percent.trad[1,])
    write.table(x = trad.percent.df,
                append = FALSE,
                sep = ",",
                file = trad.percent.file,
                row.names = FALSE, 
                quote = TRUE,
                col.names = TRUE)
  }
  
  # create heatmaps with median expression of all selected paramaters on DAFi vs traditional gates
  # as well as pseudocolor for the bidimensional gate
  if(!dir.exists(plotDir)){
    dir.create(plotDir)
  }
  for(DAFi_node in DAFi_nodes) {
    nonDAFi_node <- gsub(pattern = "DAFi_", 
                         replacement = "",
                         x = DAFi_node,
                         fixed = TRUE)
    gate_par <- flowWorkspace::gh_pop_get_gate(
      gs[[1]],
      nonDAFi_node)@parameters %>% 
      names
    filtLs <- filterList(gs_pop_get_gate(gs[[1]],
                                         nonDAFi_node))
    n.events.DAFi <- (gh_pop_get_indices(gs[[1]],
                                         DAFi_node) %>%
                        sum)
    n.events.trad <- (gh_pop_get_indices(gs[[1]],
                                         nonDAFi_node) %>%
                        sum)
    tryCatch({
      if(length(gate_par) == 2) {
        pop.exprs <- rbind(flowWorkspace::gh_pop_get_data(gs[[1]],
                                                          DAFi_node) %>%
                             exprs() %>%
                             .[,gate_par],
                           flowWorkspace::gh_pop_get_data(gs[[1]],
                                                          nonDAFi_node) %>%
                             exprs() %>%
                             .[,gate_par]) %>%
          as.data.frame()
        pop.exprs$gate.type <- c(rep(paste0("DAFi :: ",
                                            n.events.DAFi,
                                            " events"),
                                     n.events.DAFi),
                                 rep(paste0("manual :: ",
                                            n.events.trad,
                                            " events"),
                                     n.events.trad))
        xlim.outliers <- (abs(flowWorkspace::gh_pop_get_data(gs[[1]],
                                                             nonDAFi_node %>%
                                                               dirname()) %>%
                                exprs() %>%
                                .[,gate_par[1]] - median(
                                  flowWorkspace::gh_pop_get_data(gs[[1]],
                                                                 nonDAFi_node %>%
                                                                   dirname()) %>%
                                    exprs() %>%
                                    .[,gate_par[1]]
                                )) / mad(
                                  flowWorkspace::gh_pop_get_data(gs[[1]],
                                                                 nonDAFi_node %>%
                                                                   dirname()) %>%
                                    exprs() %>%
                                    .[,gate_par[1]]
                                ))
        xlim.outliers <- xlim.outliers > quantile(xlim.outliers, prob = 0.999)
        ylim.outliers <- (abs(flowWorkspace::gh_pop_get_data(gs[[1]],
                                                             nonDAFi_node %>%
                                                               dirname()) %>%
                                exprs() %>%
                                .[,gate_par[2]] - median(
                                  flowWorkspace::gh_pop_get_data(gs[[1]],
                                                                 nonDAFi_node %>%
                                                                   dirname()) %>%
                                    exprs() %>%
                                    .[,gate_par[2]]
                                )) / mad(
                                  flowWorkspace::gh_pop_get_data(gs[[1]],
                                                                 nonDAFi_node %>%
                                                                   dirname()) %>%
                                    exprs() %>%
                                    .[,gate_par[2]]
                                ))
        ylim.outliers <- ylim.outliers > quantile(ylim.outliers, prob = 0.999)
        xlim.exprs <- flowWorkspace::gh_pop_get_data(gs[[1]],
                                                     nonDAFi_node %>%
                                                       dirname()) %>%
          exprs() %>%
          .[!xlim.outliers,gate_par[1]] %>%
          range()
        ylim.exprs <- flowWorkspace::gh_pop_get_data(gs[[1]],
                                                     nonDAFi_node %>%
                                                       dirname()) %>%
          exprs() %>%
          .[!ylim.outliers,gate_par[2]] %>%
          range()
        plot.cells <- suppressMessages(
          ggplot(pop.exprs, 
                 aes(x = !!sym(gate_par[1]),
                     y = !!sym(gate_par[2]))) + 
            theme_bw() +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5)) +
            coord_cartesian(xlim = xlim.exprs,
                            ylim = ylim.exprs) +
            geom_polygon(data = filtLs, 
                         fill = NA, 
                         col = "black") +
            xlab(ifelse(is.na(pData.asDF[pData.asDF$name %in% 
                                           gate_par[1],]$desc),
                        yes = pData.asDF[pData.asDF$name %in% 
                                           gate_par[1],]$name,
                        no = pData.asDF[pData.asDF$name %in% 
                                          gate_par[1],]$desc)) +
            ylab(ifelse(is.na(pData.asDF[pData.asDF$name %in% 
                                           gate_par[2],]$desc),
                        yes = pData.asDF[pData.asDF$name %in% 
                                           gate_par[2],]$name,
                        no = pData.asDF[pData.asDF$name %in% 
                                          gate_par[2],]$desc)) +
            facet_wrap("gate.type") +
            ggtitle(paste0(nonDAFi_node %>%
                             basename(),
                           "\n",
                           sampleFCS))
        )
        if(!dim(pop.exprs)[1] < 100) {
          plot.cells <- plot.cells +
            geom_hex(binwidth = c((xlim.exprs[2] - xlim.exprs[1])/256,
                                  (ylim.exprs[2] - ylim.exprs[1])/256),
                     aes(fill = stat(log(count)))
            ) +
            scale_fill_viridis_c(option = "inferno")
        } else {
          plot.cells <- plot.cells +
            geom_point()
        }
        ggsave(plot = plot.cells,
               filename = paste0(plotDir,
                                 "/PSEUDOCOLOR.",
                                 gsub(pattern = "/",
                                      replacement = "_", 
                                      x = DAFi_node,
                                      fixed = TRUE),
                                 "_",
                                 sampleFCS,
                                 ".pdf"),
               width = 5,
               height = 3)
      }
      if(length(gate_par) == 1) {
        filtLs <- fortify(filtLs)
        filtLs <- unlist(filtLs[,1], use.names = F)
        filtLs <- data.frame(x1 = filtLs[1],
                             xend = filtLs[2],
                             y1 = 0.5,
                             yend = 0.5)
        #        filtLs.v1 <- data.frame(xintercept = filtLs$x1)
        pop.exprs <- c(flowWorkspace::gh_pop_get_data(gs[[1]],
                                                      DAFi_node) %>%
                         exprs() %>%
                         .[,gate_par[1]],
                       flowWorkspace::gh_pop_get_data(gs[[1]],
                                                      nonDAFi_node) %>%
                         exprs() %>%
                         .[,gate_par[1]]) %>%
          data.frame()
        colnames(pop.exprs) <- gate_par[1]
        xlim.outliers <- (abs(flowWorkspace::gh_pop_get_data(gs[[1]],
                                                             nonDAFi_node %>%
                                                               dirname()) %>%
                                exprs() %>%
                                .[,gate_par[1]] - median(
                                  flowWorkspace::gh_pop_get_data(gs[[1]],
                                                                 nonDAFi_node %>%
                                                                   dirname()) %>%
                                    exprs() %>%
                                    .[,gate_par[1]]
                                )) > (6  * mad(
                                  flowWorkspace::gh_pop_get_data(gs[[1]],
                                                                 nonDAFi_node %>%
                                                                   dirname()) %>%
                                    exprs() %>%
                                    .[,gate_par[1]]
                                ))) %>%
          which
        xlim.exprs <- flowWorkspace::gh_pop_get_data(gs[[1]],
                                                     nonDAFi_node %>%
                                                       dirname()) %>%
          exprs() %>%
          .[-xlim.outliers,gate_par[1]] %>%
          range()
        pop.exprs$gate.type <- c(rep(paste0("DAFi :: ",
                                            n.events.DAFi,
                                            " events"),
                                     n.events.DAFi),
                                 rep(paste0("manual :: ",
                                            n.events.trad,
                                            " events"),
                                     n.events.trad))
        plot.cells <- suppressMessages(
          ggplot(pop.exprs, 
                 aes(x = !!sym(gate_par[1]))) + 
            theme_bw() +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5)) +
            coord_cartesian(xlim = xlim.exprs) +
            geom_density(fill = "black",
                         aes(y = ..scaled..)) +
            geom_segment(aes(x = x1, 
                             y = y1, 
                             xend = xend, 
                             yend = yend,
                             color = "red"),
                         data = filtLs) +
            geom_vline(xintercept = c(filtLs$x1,
                                      filtLs$x2),
                       color = "red") +
            xlab(ifelse(is.na(pData.asDF[pData.asDF$name %in% 
                                           gate_par[1],]$desc),
                        yes = pData.asDF[pData.asDF$name %in% 
                                           gate_par[1],]$name,
                        no = pData.asDF[pData.asDF$name %in% 
                                          gate_par[1],]$desc)) +
            facet_wrap("gate.type") +
            ggtitle(paste0(nonDAFi_node %>%
                             basename(),
                           "\n",
                           sampleFCS))
        )
        ggsave(plot = plot.cells,
               filename = paste0(plotDir,
                                 "/HISTOGRAM.",
                                 gsub(pattern = "/",
                                      replacement = "_", 
                                      x = DAFi_node,
                                      fixed = TRUE),
                                 "_",
                                 sampleFCS,
                                 ".pdf"),
               width = 5,
               height = 3)
      }
    },
    error = function(e)
      print(paste0("Single cell plot failed for: ",
                   DAFi_node)))
    # get median
    mark.exprs <- cbind(
      flowWorkspace::gh_pop_get_data(gs[[1]],
                                     DAFi_node) %>% 
        flowCore::exprs() %>%
        apply(2,
              median)
      , 
      flowWorkspace::gh_pop_get_data(gs[[1]],
                                     nonDAFi_node) %>%
        flowCore::exprs() %>%
        apply(2,
              median))
    mark.exprs <- mark.exprs[!grepl(pattern = "time", 
                                    x = rownames(mark.exprs), 
                                    ignore.case = TRUE, 
                                    fixed = FALSE),]
    mark.exprs <- mark.exprs[!grepl(pattern = "FSC|SSC", 
                                    x = rownames(mark.exprs), 
                                    ignore.case = FALSE, 
                                    fixed = FALSE),]
    rownames(mark.exprs) <- pData.asDF$desc[pData.asDF$name %in%
                                              rownames(mark.exprs)]
    tryCatch({
      mark.exprs %>%
        .[order(abs(.[,1] - .[,2]),
                decreasing = TRUE),] %>%
        pheatmap(mat = .,
                 cluster_cols = FALSE,
                 cluster_rows = FALSE, 
                 scale = "none",
                 #               color = hcl.colors(256, 
                 #                                  palette = "RdYlBu",
                 #                                 alpha = NULL, 
                 #                                 rev = FALSE, 
                 #                                 fixup = TRUE),
                 labels_col = c("DAFi", "traditional"),
                 main = nonDAFi_node %>%
                   basename(),
                 filename = paste0(plotDir,
                                   "/HEATMAP.",
                                   gsub(pattern = "/",
                                        replacement = "_", 
                                        x = DAFi_node,
                                        fixed = TRUE),
                                   sampleFCS,
                                   ".pdf"),
                 width = 3,
                 height = ifelse(0.25 * dim(.)[1] < 4,
                                 yes = 4,
                                 no = 0.25 * dim(.)[1]))},
      error = function(e)
        print(paste0("Heatmap failed for: ",
                     DAFi_node)))
  }
  
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
                                                        split = popOfInt,
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
                                               trimws(.,
                                                      which = "right") %>%
                                               paste0(popOfInt,
                                                      "_",
                                                      .) %>%
                                               gsub(pattern = ",",
                                                    replacement = ".",
                                                    x = .,
                                                    fixed = TRUE)
                                           }
  #2nd sanity check
  print(
    apply(all.labels,
          2,
          function(pop)
            mean(pop > 0))
  )
  
  flowEnv <- new.env()
  
  for(pop in seq_along(colnames(all.labels))) {
    if(!(strsplit(x =  colnames(all.labels)[pop], 
                  split = "_DAFi_",
                  fixed = TRUE) %>%
         .[[1]] %>% 
         length()) > 2){
      mat <- matrix(c(5e4, 5e5),
                    ncol = 1,
                    dimnames = list(c("min", "max"),
                                    colnames(all.labels)[pop]))
      rg <- rectangleGate(filterId = colnames(all.labels)[pop],
                          .gate = mat)
      # test if DAFi is not direct child of pop of interest
      # if it is, add gate directly to flowEnv
      # if it is a grandchild, get name of DAFi parent and add it to flowEnv hierarchically
      flowEnv[[as.character(colnames(all.labels)[pop])]] <- rg
    } else { # from: https://rdrr.io/github/RGLab/CytoML/src/R/gate-methods.R
      DAFi_gates_v <- hierarc.str(DAFi_gate_name = colnames(all.labels)[pop],
                                  n = 2)
      rgs <- vector("list",
                    length = 2)
      names(rgs) <- DAFi_gates_v
      for(DAFi_gate in DAFi_gates_v) {
        rgs[[as.character(DAFi_gate)]] <- 
          rectangleGate(filterId = DAFi_gate,
                        .gate = matrix(c(5e4, 5e5),
                                       ncol = 1,
                                       dimnames = list(c("min", "max"),
                                                       DAFi_gate)))
        
      }
      rgs <- rev(rgs) # parent gate must come later
      flowEnv[[as.character(colnames(all.labels)[pop])]] <-
        new("subsetFilter",
            filterId = colnames(all.labels)[pop], 
            filters = rgs)
      
    }
  }
  
  outputFile <- paste0(fj_output_folder,
                       "/",
                       basename(fj_data_file_path),
                       ".gating-ml2.xml")
  
  #############################################
  ## Code related to writing Gating-ML files ##
  ##########################################################
  ## Copied and modified from #############################
  ## https://rdrr.io/bioc/flowUtils/src/R/writeGatingML.R ##
  ##########################################################
  
  # Write objects in the flowEnv environment to an Gating-ML 2.0 XML file.
  # If file is NULL then output is written to standard output.
  modified.write.gatingML <- function(flowEnv, file = NULL)
  {
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
    
    somethingUseful = FALSE
    for (x in ls(flowEnv)) {
      object = objectNameToObject(x, flowEnv)
      if(is(object, "parameterFilter") || is(object, "singleParameterTransform") || is(object, "setOperationFilter"))
      {
        somethingUseful = TRUE
        break
      }
    }
    if(!somethingUseful) warning("Nothing useful seems to be present in the environment; the output Gating-ML file may not be very useful.", call. = FALSE)
    
    # Go over everything and temporarily add transformations and argument gates to flowEnv
    # if they are not saved in flowEnv directly, but they are being used in other objects
    flowEnv[['.addedObjects']] = list() # List of object identifiers of objects that we have to temporarily add to flowEnv
    for (x in ls(flowEnv)) addReferencedObjectsToEnv(x, flowEnv) 
    
    flowEnv[['.singleParTransforms']] = new.env() # Use this env to collect transformations
    for (x in ls(flowEnv)) if(is(flowEnv[[x]], "singleParameterTransform")) collectTransform(x, flowEnv)
    
    # Transforms go first unless they can be skipped all together
    for (x in ls(flowEnv)) if(is(flowEnv[[x]], "transform"))
      if(!shouldTransformationBeSkipped(x, flowEnv)) addObjectToGatingML(gatingMLNode, x, flowEnv)
    for (x in ls(flowEnv)) if(!is(flowEnv[[x]], "transform")) addObjectToGatingML(gatingMLNode, x, flowEnv)
    
    if(!is.null(file)) sink(file = file)
    cat(saveXML(gatingMLNode$value(), encoding = "UTF-8"))
    if(!is.null(file)) sink()
    
    rm(list = ls(flowEnv[['.singleParTransforms']], all.names = TRUE), envir = flowEnv[['.singleParTransforms']])
    rm('.singleParTransforms', envir = flowEnv)
    
    rm(list = as.character(flowEnv[['.addedObjects']]), envir = flowEnv)
    rm('.addedObjects', envir = flowEnv)
    
    rm('.objectIDsWrittenToXMLOutput', envir = flowEnv) 
  }
  
  # Add the object x to the Gating-ML node
  addObjectToGatingML <- function(gatingMLNode, x, flowEnv, addParent = NULL, forceGateId = NULL)
  {
    if(is(x, "character")) object = flowEnv[[x]]
    else object = x
    switch(class(object),
           "rectangleGate" = addRectangleGateNode(gatingMLNode, x, flowEnv, addParent, forceGateId),
           "polygonGate" = addPolygonGateNode(gatingMLNode, x, flowEnv, addParent, forceGateId),
           "ellipsoidGate" = addEllipsoidGateNode(gatingMLNode, x, flowEnv, addParent, forceGateId),
           "quadGate" = addQuadGateNode(gatingMLNode, x, flowEnv, addParent, forceGateId),
           "intersectFilter" = addBooleanAndGateNode(gatingMLNode, x, flowEnv, addParent, forceGateId),
           "unionFilter" = addBooleanOrGateNode(gatingMLNode, x, flowEnv, addParent, forceGateId),
           "complementFilter" = addBooleanNotGateNode(gatingMLNode, x, flowEnv, addParent, forceGateId),
           "subsetFilter" = addGateWithParent(gatingMLNode, x, flowEnv),
           "compensation" = addCompensation(gatingMLNode, x, flowEnv),
           "asinhtGml2" = addAsinhtGml2(gatingMLNode, x, flowEnv),
           "hyperlogtGml2" = addHyperlogtGml2(gatingMLNode, x, flowEnv),
           "lintGml2" = addLintGml2(gatingMLNode, x, flowEnv),
           "logtGml2" = addLogtGml2(gatingMLNode, x, flowEnv),
           "logicletGml2" = addLogicletGml2(gatingMLNode, x, flowEnv),
           "ratiotGml2" = addRatiotGml2(gatingMLNode, x, flowEnv),
           "ratio" = addRatioGml1.5(gatingMLNode, x, flowEnv),
           "asinht" = addAsinhtGml1.5(gatingMLNode, x, flowEnv),
           "compensatedParameter" = NA,
           "unitytransform" = NA,
           "numeric" = NA,
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
  addRectangleGateNode <- function(gatingMLNode, x, flowEnv, addParent, forceGateId)
  {
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
  
  # Add polygon gate x to the Gating-ML node
  addPolygonGateNode <- function(gatingMLNode, x, flowEnv, addParent, forceGateId)
  {
    gate = objectNameToObject(x, flowEnv)
    if(!is(gate, "polygonGate")) stop(paste("Unexpected object insted of a polygonGate - ", class(gate))) 
    addDebugMessage(paste("Working on polygonGate ", gate@filterId, sep=""), flowEnv)
    
    myID = getObjectId(gate, forceGateId, flowEnv)
    if(isIdWrittenToXMLAlready(myID, flowEnv)) return(FALSE) 
    attrs = c("gating:id" = myID)
    if (!is.null(addParent)) attrs = c(attrs, "gating:parent_id" = filterIdtoXMLId(addParent, flowEnv))    
    
    gatingMLNode$addNode("gating:PolygonGate", attrs = attrs, close = FALSE)
    addDimensions(gatingMLNode, x, flowEnv)
    for (i in 1:length(gate@boundaries[,1]))
    {
      gatingMLNode$addNode("gating:vertex", close = FALSE)
      # attrs = c("data-type:value" = gate@boundaries[i,1])
      attrs = c("data-type:value" = as.numeric(gate@boundaries[i,1]))
      gatingMLNode$addNode("gating:coordinate", attrs = attrs)
      # attrs = c("data-type:value" = gate@boundaries[i,2])
      attrs = c("data-type:value" = as.numeric(gate@boundaries[i,2]))
      gatingMLNode$addNode("gating:coordinate", attrs = attrs)
      gatingMLNode$closeTag() # </gating:vertex>
    }
    gatingMLNode$closeTag() # </gating:PolygonGate>
  }
  
  # Add ellipse gate x to the Gating-ML node
  addEllipsoidGateNode <- function(gatingMLNode, x, flowEnv, addParent, forceGateId)
  {
    gate = objectNameToObject(x, flowEnv)
    if(!is(gate, "ellipsoidGate")) stop(paste("Unexpected object insted of an ellipsoidGate - ", class(gate))) 
    addDebugMessage(paste("Working on ellipsoidGate ", gate@filterId, sep=""), flowEnv)
    
    myID = getObjectId(gate, forceGateId, flowEnv)
    if(isIdWrittenToXMLAlready(myID, flowEnv)) return(FALSE) 
    attrs = c("gating:id" = myID)
    if (!is.null(addParent)) attrs = c(attrs, "gating:parent_id" = filterIdtoXMLId(addParent, flowEnv))    
    
    gatingMLNode$addNode("gating:EllipsoidGate", attrs = attrs, close = FALSE)
    addDimensions(gatingMLNode, x, flowEnv)
    
    gatingMLNode$addNode("gating:mean", close = FALSE)
    for (i in 1:length(gate@mean))
    {
      attrs = c("data-type:value" = as.numeric(gate@mean[i]))
      gatingMLNode$addNode("gating:coordinate", attrs = attrs)
    }
    gatingMLNode$closeTag() # </gating:mean>
    
    gatingMLNode$addNode("gating:covarianceMatrix", close = FALSE)
    for (row in 1:length(gate@cov[,1]))
    {
      gatingMLNode$addNode("gating:row", close = FALSE)
      for (column in 1:length(gate@cov[1,]))
      {
        attrs = c("data-type:value" = gate@cov[row,column])
        gatingMLNode$addNode("gating:entry", attrs = attrs)
      }
      gatingMLNode$closeTag() # </gating:row>
    }
    gatingMLNode$closeTag() # </gating:covarianceMatrix>
    
    attrs = c("data-type:value" = gate@distance ^ 2)
    gatingMLNode$addNode("gating:distanceSquare", attrs = attrs)
    
    gatingMLNode$closeTag() # </gating:EllipsoidGate>
  }
  
  # Add a Boolean AND gate x to the Gating-ML node
  addBooleanAndGateNode <- function(gatingMLNode, x, flowEnv, addParent, forceGateId)
  {
    gate = objectNameToObject(x, flowEnv)
    if(!is(gate, "intersectFilter")) stop(paste("Unexpected object insted of an intersectFilter - ", class(gate))) 
    addDebugMessage(paste("Working on intersectFilter ", gate@filterId, sep=""), flowEnv)
    
    myID = getObjectId(gate, forceGateId, flowEnv)
    if(isIdWrittenToXMLAlready(myID, flowEnv)) return(FALSE) 
    attrs = c("gating:id" = myID)
    if (!is.null(addParent)) attrs = c(attrs, "gating:parent_id" = filterIdtoXMLId(addParent, flowEnv))    
    
    gatingMLNode$addNode("gating:BooleanGate", attrs = attrs, close = FALSE)
    gatingMLNode$addNode("gating:and", close = FALSE)
    if(length(gate@filters) == 0) 
      stop("Boolean AND gates (intersectFilter) have to reference some arguments.", call. = FALSE)
    for (i in 1:length(gate@filters))
    {
      attrs = c("gating:ref" = filterIdtoXMLId(gate@filters[[i]]@filterId, flowEnv))
      gatingMLNode$addNode("gating:gateReference", attrs = attrs)
    }
    if(length(gate@filters)  == 1) 
    {
      # If there was just one referenced filter than we add it twice
      # since and/or gates require at least two arguments in Gating-ML 2.0
      gatingMLNode$addNode("gating:gateReference", attrs = attrs)
    } 
    gatingMLNode$closeTag() # </gating:and>
    gatingMLNode$closeTag() # </gating:BooleanGate>    
  }
  
  # Add a Boolean OR gate x to the Gating-ML node
  addBooleanOrGateNode <- function(gatingMLNode, x, flowEnv, addParent, forceGateId)
  {
    gate = objectNameToObject(x, flowEnv)
    if(!is(gate, "unionFilter")) stop(paste("Unexpected object insted of a unionFilter - ", class(gate))) 
    addDebugMessage(paste("Working on unionFilter ", gate@filterId, sep=""), flowEnv)
    
    myID = getObjectId(gate, forceGateId, flowEnv)
    if(isIdWrittenToXMLAlready(myID, flowEnv)) return(FALSE) 
    attrs = c("gating:id" = myID)
    if (!is.null(addParent)) attrs = c(attrs, "gating:parent_id" = filterIdtoXMLId(addParent, flowEnv))    
    
    gatingMLNode$addNode("gating:BooleanGate", attrs = attrs, close = FALSE)
    gatingMLNode$addNode("gating:or", close = FALSE)
    if(length(gate@filters) == 0) 
      stop("Boolean OR gates (unionFilter) have to reference some arguments.", call. = FALSE)
    for (i in 1:length(gate@filters))
    {
      attrs = c("gating:ref" = filterIdtoXMLId(gate@filters[[i]]@filterId, flowEnv))
      gatingMLNode$addNode("gating:gateReference", attrs = attrs)
    }
    if(length(gate@filters)  == 1) 
    {
      # If there was just one referenced filter than we add it twice
      # since and/or gates require at least two arguments in Gating-ML 2.0
      gatingMLNode$addNode("gating:gateReference", attrs = attrs)
    } 
    gatingMLNode$closeTag() # </gating:or>
    gatingMLNode$closeTag() # </gating:BooleanGate>    
  }
  
  # Add a Boolean NOT gate x to the Gating-ML node
  addBooleanNotGateNode <- function(gatingMLNode, x, flowEnv, addParent, forceGateId)
  {
    gate = objectNameToObject(x, flowEnv)
    if(!is(gate, "complementFilter")) stop(paste("Unexpected object insted of a complementFilter - ", class(gate))) 
    addDebugMessage(paste("Working on complementFilter ", gate@filterId, sep=""), flowEnv)
    
    myID = getObjectId(gate, forceGateId, flowEnv)
    if(isIdWrittenToXMLAlready(myID, flowEnv)) return(FALSE) 
    attrs = c("gating:id" = myID)
    if (!is.null(addParent)) attrs = c(attrs, "gating:parent_id" = filterIdtoXMLId(addParent, flowEnv))    
    
    gatingMLNode$addNode("gating:BooleanGate", attrs = attrs, close = FALSE)
    gatingMLNode$addNode("gating:not", close = FALSE)
    if(length(gate@filters)  == 1) 
    {
      attrs = c("gating:ref" = filterIdtoXMLId(gate@filters[[1]]@filterId, flowEnv))
      gatingMLNode$addNode("gating:gateReference", attrs = attrs)
    } else stop("Boolean NOT gates (complementFilter) have to reference exactly one argument.", call. = FALSE)
    gatingMLNode$closeTag() # </gating:not>
    gatingMLNode$closeTag() # </gating:BooleanGate>    
  }
  
  # Add a Quadrant gate x to the Gating-ML node
  addQuadGateNode <- function(gatingMLNode, x, flowEnv, addParent, forceGateId)
  {
    gate = objectNameToObject(x, flowEnv)
    if(!is(gate, "quadGate")) stop(paste("Unexpected object insted of a quadGate - ", class(gate))) 
    addDebugMessage(paste("Working on quadGate ", gate@filterId, sep=""), flowEnv)
    
    myID = getObjectId(gate, forceGateId, flowEnv)
    if(isIdWrittenToXMLAlready(myID, flowEnv)) return(FALSE) 
    attrs = c("gating:id" = myID)
    if (!is.null(addParent)) attrs = c(attrs, "gating:parent_id" = filterIdtoXMLId(addParent, flowEnv))    
    
    gatingMLNode$addNode("gating:QuadrantGate", attrs = attrs, close = FALSE)
    addDimensions(gatingMLNode, x, flowEnv, myID)
    
    attrs = c("gating:id" = paste(myID, ".PP", sep = ""))
    gatingMLNode$addNode("gating:Quadrant", attrs = attrs, close = FALSE)
    attrs = c("gating:divider_ref" = paste(myID, ".D1", sep = ""))
    attrs = c(attrs, "gating:location" = as.character(gate@boundary[1] + 1))
    gatingMLNode$addNode("gating:position", attrs = attrs)
    attrs = c("gating:divider_ref" = paste(myID, ".D2", sep = ""))
    attrs = c(attrs, "gating:location" = as.character(gate@boundary[2] + 1))
    gatingMLNode$addNode("gating:position", attrs = attrs)
    gatingMLNode$closeTag() # </gating:Quadrant>
    
    attrs = c("gating:id" = paste(myID, ".PN", sep = ""))
    gatingMLNode$addNode("gating:Quadrant", attrs = attrs, close = FALSE)
    attrs = c("gating:divider_ref" = paste(myID, ".D1", sep = ""))
    attrs = c(attrs, "gating:location" = as.character(gate@boundary[1] + 1))
    gatingMLNode$addNode("gating:position", attrs = attrs)
    attrs = c("gating:divider_ref" = paste(myID, ".D2", sep = ""))
    attrs = c(attrs, "gating:location" = as.character(gate@boundary[2] - 1))
    gatingMLNode$addNode("gating:position", attrs = attrs)
    gatingMLNode$closeTag() # </gating:Quadrant>
    
    attrs = c("gating:id" = paste(myID, ".NP", sep = ""))
    gatingMLNode$addNode("gating:Quadrant", attrs = attrs, close = FALSE)
    attrs = c("gating:divider_ref" = paste(myID, ".D1", sep = ""))
    attrs = c(attrs, "gating:location" = as.character(gate@boundary[1] - 1))
    gatingMLNode$addNode("gating:position", attrs = attrs)
    attrs = c("gating:divider_ref" = paste(myID, ".D2", sep = ""))
    attrs = c(attrs, "gating:location" = as.character(gate@boundary[2] + 1))
    gatingMLNode$addNode("gating:position", attrs = attrs)
    gatingMLNode$closeTag() # </gating:Quadrant>
    
    attrs = c("gating:id" = paste(myID, ".NN", sep = ""))
    gatingMLNode$addNode("gating:Quadrant", attrs = attrs, close = FALSE)
    attrs = c("gating:divider_ref" = paste(myID, ".D1", sep = ""))
    attrs = c(attrs, "gating:location" = as.character(gate@boundary[1] - 1))
    gatingMLNode$addNode("gating:position", attrs = attrs)
    attrs = c("gating:divider_ref" = paste(myID, ".D2", sep = ""))
    attrs = c(attrs, "gating:location" = as.character(gate@boundary[2] - 1))
    gatingMLNode$addNode("gating:position", attrs = attrs)
    gatingMLNode$closeTag() # </gating:Quadrant>
    
    gatingMLNode$closeTag() # </gating:QuadrantGate>
  }
  
  # Add a subsetFilter gate named x to the the Gating-ML node
  addGateWithParent <- function(gatingMLNode, x, flowEnv)
  {
    addDebugMessage(paste("Working on ", x, sep=""), flowEnv)
    gate = objectNameToObject(x, flowEnv)
    if (!is(gate, "subsetFilter")) stop(paste("Expected a subsetFilter to add a gate with a parent id, but found an object of class", class(gate)))
    if (length(gate@filters) == 2){
      newX = gate@filters[[1]]
      parent = gate@filters[[2]]
      if (is(parent, 'filterReference')) parentName = parent@name
      else parentName = parent@filterId
      addObjectToGatingML(gatingMLNode, newX, flowEnv, parentName, gate@filterId)    
    }
    else stop(paste("Unexpected length of filters for class", class(gate)))
  }
  
  # Add a compensation named x to the the Gating-ML node
  addCompensation <- function(gatingMLNode, x, flowEnv)
  {
    myComp = objectNameToObject(x, flowEnv)
    if(!is(myComp, "compensation")) stop(paste("Unexpected object insted of a compensation - ", class(myComp))) 
    addDebugMessage(paste("Working on compensation ", myComp@compensationId, sep=""), flowEnv)
    
    myID = getObjectId(myComp, NULL, flowEnv)
    if(isIdWrittenToXMLAlready(myID, flowEnv)) return(FALSE) 
    
    detectors <- colnames(myComp@spillover)
    if (is.null(detectors)) 
    {
      stop(paste("Cannot export a spillover matrix without column names (", myComp@compensationId, ").", sep=""))
      return
    }
    
    fluorochromes <- rownames(myComp@spillover)
    if(is.null(fluorochromes))
    {
      if(nrow(myComp@spillover) != ncol(myComp@spillover)) 
      {
        stop(paste("Cannot export a non-sqaure spillover (spectrum) matrix without row names (", myComp@compensationId, ").", sep=""))
        return
      }
      else
      {
        fluorochromes <- detectors
      }
    }
    
    attrs = c("transforms:id" = myID)
    gatingMLNode$addNode("transforms:spectrumMatrix", attrs = attrs, close = FALSE)
    
    gatingMLNode$addNode("transforms:fluorochromes", close = FALSE)
    for (fname in fluorochromes) 
    {
      attrs = c("data-type:name" = fname)
      gatingMLNode$addNode("data-type:fcs-dimension", attrs = attrs)
    }
    gatingMLNode$closeTag() # </transforms:fluorochromes>
    
    gatingMLNode$addNode("transforms:detectors", close = FALSE)
    for (dname in detectors) 
    {
      attrs = c("data-type:name" = dname)
      gatingMLNode$addNode("data-type:fcs-dimension", attrs = attrs)
    }
    gatingMLNode$closeTag() # </transforms:detectors>
    
    for (rowNo in 1:nrow(myComp@spillover))
    {
      gatingMLNode$addNode("transforms:spectrum", close = FALSE)
      for (colNo in 1:ncol(myComp@spillover)) 
      {
        # attrs = c("transforms:value" = myComp@spillover[rowNo,colNo])
        attrs = c("transforms:value" = as.vector(myComp@spillover[rowNo,colNo]))
        gatingMLNode$addNode("transforms:coefficient", attrs = attrs)
      }
      gatingMLNode$closeTag() # </transforms:spectrum>
    }
    
    gatingMLNode$closeTag() # </transforms:spectrumMatrix>
  }
  
  # Add an asinhtGml2 transformation named x to the the Gating-ML node
  addAsinhtGml2 <- function(gatingMLNode, x, flowEnv)
  {
    myTrans = objectNameToObject(x, flowEnv)
    if(!is(myTrans, "asinhtGml2")) stop(paste("Unexpected object insted of asinhtGml2 - ", class(myTrans))) 
    addDebugMessage(paste("Working on asinhtGml2 ", myTrans@transformationId, sep=""), flowEnv)
    
    myID = getObjectId(myTrans, NULL, flowEnv)
    if(isIdWrittenToXMLAlready(myID, flowEnv)) return(FALSE) 
    
    attrs = c("transforms:id" = myID)
    if (is.finite(myTrans@boundMin)) attrs = append(attrs, c("transforms:boundMin" = myTrans@boundMin))
    if (is.finite(myTrans@boundMax)) attrs = append(attrs, c("transforms:boundMax" = myTrans@boundMax))
    gatingMLNode$addNode("transforms:transformation", attrs = attrs, close = FALSE)
    attrs = c("transforms:T" = myTrans@T, "transforms:M" = myTrans@M, "transforms:A" = myTrans@A)
    gatingMLNode$addNode("transforms:fasinh", attrs = attrs)
    gatingMLNode$closeTag() # </transforms:transformation>    
  }
  
  # Add an asinht transformation named x to the the Gating-ML node.
  # Encode asinht from Gating-ML 1.5 compatible parameterization using Gating-ML 2.0
  # compatible parameterization as follows:
  #
  # asinht (ASinH from Gating-ML 1.5) is defined as 
  # f(x) = asinh(a*x)*b
  # asinhtGml2 (fasinh from Gating-ML 2.0) is defined as:
  # f(x) = (asinh(x*sinh(M*log(10))/T) + A*log(10)) / ((M+A)*log(10))
  # Therefore, we will encode asinht as asinhtGml2 by stating
  # A = 0
  # M = 1 / (b * log(10))
  # T = (sinh(1/b)) / a
  # which will give us exactly the right transformation in the Gating-ML 2.0 
  # compatible parameterization. Btw. log is natural logarithm, i.e., based e
  addAsinhtGml1.5 <- function(gatingMLNode, x, flowEnv)
  {
    myTrans = objectNameToObject(x, flowEnv)
    if(!is(myTrans, "asinht")) stop(paste("Unexpected object insted of asinht - ", class(myTrans))) 
    addDebugMessage(paste("Working on asinht ", myTrans@transformationId, sep=""), flowEnv)
    
    myID = getObjectId(myTrans, NULL, flowEnv)
    if(isIdWrittenToXMLAlready(myID, flowEnv)) return(FALSE) 
    
    attrs = c("transforms:id" = myID)
    gatingMLNode$addNode("transforms:transformation", attrs = attrs, close = FALSE)
    
    A = 0
    M = 1 / (myTrans@b * log(10))
    T = (sinh(1/myTrans@b)) / myTrans@a
    attrs = c("transforms:T" = T, "transforms:M" = M, "transforms:A" = A)
    
    gatingMLNode$addNode("transforms:fasinh", attrs = attrs)
    gatingMLNode$closeTag() # </transforms:transformation>    
  }
  
  # Add a hyperlogtGml2 transformation named x to the the Gating-ML node
  addHyperlogtGml2 <- function(gatingMLNode, x, flowEnv)
  {
    myTrans = objectNameToObject(x, flowEnv)
    if(!is(myTrans, "hyperlogtGml2")) stop(paste("Unexpected object insted of hyperlogtGml2 - ", class(myTrans))) 
    addDebugMessage(paste("Working on hyperlogtGml2 ", myTrans@transformationId, sep=""), flowEnv)
    
    myID = getObjectId(myTrans, NULL, flowEnv)
    if(isIdWrittenToXMLAlready(myID, flowEnv)) return(FALSE) 
    
    attrs = c("transforms:id" = myID)
    if (is.finite(myTrans@boundMin)) attrs = append(attrs, c("transforms:boundMin" = myTrans@boundMin))
    if (is.finite(myTrans@boundMax)) attrs = append(attrs, c("transforms:boundMax" = myTrans@boundMax))
    gatingMLNode$addNode("transforms:transformation", attrs = attrs, close = FALSE)
    attrs = c("transforms:T" = myTrans@T, "transforms:M" = myTrans@M, "transforms:W" = myTrans@W, "transforms:A" = myTrans@A)
    gatingMLNode$addNode("transforms:hyperlog", attrs = attrs)
    gatingMLNode$closeTag() # </transforms:transformation>    
  }
  
  # Add a logicletGml2 transformation named x to the the Gating-ML node
  addLogicletGml2 <- function(gatingMLNode, x, flowEnv)
  {
    myTrans = objectNameToObject(x, flowEnv)
    if(!is(myTrans, "logicletGml2")) stop(paste("Unexpected object insted of logicletGml2 - ", class(myTrans))) 
    addDebugMessage(paste("Working on logicletGml2 ", myTrans@transformationId, sep=""), flowEnv)
    
    myID = getObjectId(myTrans, NULL, flowEnv)
    if(isIdWrittenToXMLAlready(myID, flowEnv)) return(FALSE) 
    
    attrs = c("transforms:id" = myID)
    if (is.finite(myTrans@boundMin)) attrs = append(attrs, c("transforms:boundMin" = myTrans@boundMin))
    if (is.finite(myTrans@boundMax)) attrs = append(attrs, c("transforms:boundMax" = myTrans@boundMax))
    gatingMLNode$addNode("transforms:transformation", attrs = attrs, close = FALSE)
    attrs = c("transforms:T" = myTrans@T, "transforms:M" = myTrans@M, "transforms:W" = myTrans@W, "transforms:A" = myTrans@A)
    gatingMLNode$addNode("transforms:logicle", attrs = attrs)
    gatingMLNode$closeTag() # </transforms:transformation>    
  }
  
  # Add a lintGml2 transformation named x to the the Gating-ML node
  addLintGml2 <- function(gatingMLNode, x, flowEnv)
  {
    myTrans = objectNameToObject(x, flowEnv)
    if(!is(myTrans, "lintGml2")) stop(paste("Unexpected object insted of lintGml2 - ", class(myTrans))) 
    addDebugMessage(paste("Working on lintGml2 ", myTrans@transformationId, sep=""), flowEnv)
    
    myID = getObjectId(myTrans, NULL, flowEnv)
    if(isIdWrittenToXMLAlready(myID, flowEnv)) return(FALSE) 
    
    attrs = c("transforms:id" = myID)
    if (is.finite(myTrans@boundMin)) attrs = append(attrs, c("transforms:boundMin" = myTrans@boundMin))
    if (is.finite(myTrans@boundMax)) attrs = append(attrs, c("transforms:boundMax" = myTrans@boundMax))
    gatingMLNode$addNode("transforms:transformation", attrs = attrs, close = FALSE)
    attrs = c("transforms:T" = myTrans@T, "transforms:A" = myTrans@A)
    gatingMLNode$addNode("transforms:flin", attrs = attrs)
    gatingMLNode$closeTag() # </transforms:transformation>    
  }
  
  # Add a logtGml2 transformation named x to the the Gating-ML node
  addLogtGml2 <- function(gatingMLNode, x, flowEnv)
  {
    myTrans = objectNameToObject(x, flowEnv)
    if(!is(myTrans, "logtGml2")) stop(paste("Unexpected object insted of logtGml2 - ", class(myTrans))) 
    addDebugMessage(paste("Working on logtGml2 ", myTrans@transformationId, sep=""), flowEnv)
    
    myID = getObjectId(myTrans, NULL, flowEnv)
    if(isIdWrittenToXMLAlready(myID, flowEnv)) return(FALSE) 
    
    attrs = c("transforms:id" = myID)
    if (is.finite(myTrans@boundMin)) attrs = append(attrs, c("transforms:boundMin" = myTrans@boundMin))
    if (is.finite(myTrans@boundMax)) attrs = append(attrs, c("transforms:boundMax" = myTrans@boundMax))
    gatingMLNode$addNode("transforms:transformation", attrs = attrs, close = FALSE)
    attrs = c("transforms:T" = myTrans@T, "transforms:M" = myTrans@M)
    gatingMLNode$addNode("transforms:flog", attrs = attrs)
    gatingMLNode$closeTag() # </transforms:transformation>    
  }
  
  # Add a ratiotGml2 transformation named x to the the Gating-ML node
  addRatiotGml2 <- function(gatingMLNode, x, flowEnv)
  {
    myTrans = objectNameToObject(x, flowEnv)
    if(!is(myTrans, "ratiotGml2")) stop(paste("Unexpected object insted of ratiotGml2 - ", class(myTrans))) 
    addDebugMessage(paste("Working on ratiotGml2 ", myTrans@transformationId, sep=""), flowEnv)
    
    myID = getObjectId(myTrans, NULL, flowEnv)
    if(isIdWrittenToXMLAlready(myID, flowEnv)) return(FALSE) 
    
    attrs = c("transforms:id" = myID)
    if (is.finite(myTrans@boundMin)) attrs = append(attrs, c("transforms:boundMin" = myTrans@boundMin))
    if (is.finite(myTrans@boundMax)) attrs = append(attrs, c("transforms:boundMax" = myTrans@boundMax))
    gatingMLNode$addNode("transforms:transformation", attrs = attrs, close = FALSE)
    attrs = c("transforms:A" = myTrans@pA, "transforms:B" = myTrans@pB, "transforms:C" = myTrans@pC)
    gatingMLNode$addNode("transforms:fratio", attrs = attrs, close = FALSE)
    addDimensionContents(gatingMLNode, myTrans@numerator, flowEnv)
    addDimensionContents(gatingMLNode, myTrans@denominator, flowEnv)
    gatingMLNode$closeTag() # </transforms:fratio>
    gatingMLNode$closeTag() # </transforms:transformation>    
  }
  
  # Add a ratio transformation (from Gating-ML 1.5) named x 
  # to the the Gating-ML node. This will be translated to how "fratio" of Gating-ML 2.0
  # (When we set A = 1, B = 0, C = 0 then ratio of Gating-ML 1.5 == fratio of Gating-ML 2.0)
  addRatioGml1.5 <- function(gatingMLNode, x, flowEnv)
  {
    myTrans = objectNameToObject(x, flowEnv)
    if(!is(myTrans, "ratio")) stop(paste("Unexpected object insted of ratio - ", class(myTrans))) 
    addDebugMessage(paste("Working on ratio ", myTrans@transformationId, sep=""), flowEnv)
    
    myID = getObjectId(myTrans, NULL, flowEnv)
    if(isIdWrittenToXMLAlready(myID, flowEnv)) return(FALSE) 
    
    attrs = c("transforms:id" = myID)
    gatingMLNode$addNode("transforms:transformation", attrs = attrs, close = FALSE)
    attrs = c("transforms:A" = "1", "transforms:B" = "0", "transforms:C" = "0")
    gatingMLNode$addNode("transforms:fratio", attrs = attrs, close = FALSE)
    addDimensionContents(gatingMLNode, myTrans@numerator, flowEnv)
    addDimensionContents(gatingMLNode, myTrans@denominator, flowEnv)
    gatingMLNode$closeTag() # </transforms:fratio>
    gatingMLNode$closeTag() # </transforms:transformation>    
  }
  
  
  # Add a Gating-ML dimension to a Gating-ML node
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
  
  # This converts the identifier to an XML safe identifier and also,
  # if it is a singleParameterTransform and we have a different 
  # 'representative' transform for those (saved in flowEnv[['.singleParTransforms']])
  # then the identifier of the representative is used instead.
  filterIdtoXMLId <- function(x, flowEnv)
  {
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
  
  # Return true if you are sure that the character is safe to be placed in
  # an XML identifier.
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
  
  # Returns TRUE if and only if x is a singleParameterTransform
  # and there is another equivalent singleParameterTransform
  # in flowEnv that is the chosen representative among all
  # equivalent transforms. This is used to merge transforms
  # for Gating-ML 2.0 output since in Gating-ML 2.0, the same
  # transformation is applicable to many FCS parameters. For us,
  # the transformation with the shortest identifier is the chosen
  # representative. This function requires the flowEnv[['.singleParTransforms']]
  # to be set by calling the collectTransform function on all available
  # transforms before shouldTransformationBeSkipped can be used.
  shouldTransformationBeSkipped <- function(x, flowEnv)
  {
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
  
  # Resolve transformation reference, return the transformation that the
  # reference is pointing to.
  resolveTransformationReference <- function(trRef)
  {
    if(!is(trRef, "transformReference")) 
      stop(paste("Cannot call resolveTransformationReference on", class(trRef)))
    if(exists(trRef@transformationId, envir=trRef@searchEnv, inherits=FALSE))
      trRef@searchEnv[[trRef@transformationId]]
    else
      stop(paste("Cannot find", trRef@transformationId, "in the environment."))
  }
  
  # This will create an identifier of a singleParameterTransform that
  # is based on the class and slot values, such as T, M, W, A, etc. as applicable
  # for the various single parameter transformations. We will use this to 
  # merge "the same transformations" applied to different FCS parameter into a single
  # transformation in the Gating-ML 2.0 output.
  createTransformIdentifier <- function(trans)
  {
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
  
  # The flowEnv[['.singleParTransforms']] environment will serve as a hashmap
  # with keys based on values returned by createTransformIdentifier and
  # values being the shortest transformationId value of all the transformations
  # matching that key. That way, we can merge all these transformations into
  # a single one in Gating-ML.
  collectTransform <- function(x, flowEnv)
  {
    trEnv = flowEnv[['.singleParTransforms']]
    trans = flowEnv[[x]]
    key = createTransformIdentifier(trans)
    if (is.null(trEnv[[key]]) || length(trEnv[[key]]) > trans@transformationId) trEnv[[key]] = trans@transformationId   
  }
  
  # Add a debug message to out list of debug messages in flowEnv[['.debugMessages']]
  addDebugMessage <- function(msg, flowEnv)
  {
    flowEnv[['.debugMessages']] = c(flowEnv[['.debugMessages']], msg)
  }
  
  # Return TRUE of the provided id has been checked (and supposedly written)
  # before. Otherwise, add the id to the list in flowEnv[['.objectIDsWrittenToXMLOutput']]
  # and retusn FALSE. This function is used to prevent writing multiple objects
  # with the same ID to the Gating-ML output in case a gate or transformation
  # with the same ID is stored several times in the flowEnv.
  isIdWrittenToXMLAlready <- function(id, flowEnv)
  {
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
  
  # Add an appropriate gating:compensation-ref attribute to the passed attrs
  addCompensationRef <- function(attrs, parameter, flowEnv)
  {
    if(is(parameter, "unitytransform")) attrs = c(attrs, "gating:compensation-ref" = "uncompensated")
    else if(is(parameter, "compensatedParameter")) 
    {
      if (parameter@spillRefId != "SpillFromFCS")
        attrs = c(attrs, "gating:compensation-ref" = filterIdtoXMLId(parameter@spillRefId, flowEnv))
      else 
        attrs = c(attrs, "gating:compensation-ref" = "FCS")
    }
    else stop(paste("Unexpected parameter class", class(parameter)))
    
    attrs
  }
  
  # Add to attrs the gating:min and/or gating:max attributes 
  # based on dimension number i of a rectangle gate gate.
  addRectGateMinMax <- function(attrs, gate, i)
  {
    if (is(gate, "rectangleGate"))
    {
      min = gate@min[[i]]
      max = gate@max[[i]]
      if(min != -Inf) attrs = c(attrs, "gating:min" = min)
      if(max != Inf) attrs = c(attrs, "gating:max" = max)
    } else stop(paste("Unexpected gate class", class(gate), "- expected a rectangleGate."))
    
    attrs
  }
  
  # Get the XML compliant identifier of an object. This only works for object of type
  # "filter", "transform" or "compensation". The filterIdtoXMLId function is incorporated,
  # which includes the use of representative singleParameterTransforms instead of a different
  # transform whenever it is applied to a different FCS parameter.
  getObjectId <- function(object, forceGateId, flowEnv)
  {
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
  
  # If x is character then return flowEnv[[x]], otherwise return x
  objectNameToObject <- function(x, flowEnv) 
  {
    if(is(x, "character")) flowEnv[[x]]
    else x
  }
  
  # Check object named x in flowEnv and make sure
  # flowEnv contains objects referenced from x, such as parameter
  # transformations used in x. If objects are missing then
  # add them to flowEnv and keep track of what has been
  # added in the flowEnv[['.addedObjects']] list so that it can be
  # removed at the end of the write.gatingML function. 
  addReferencedObjectsToEnv <- function(x, flowEnv) 
  {
    object = objectNameToObject(x, flowEnv)
    if(is(object, "parameterFilter")) 
      for(par in object@parameters) doubleCheckExistanceOfParameter(par, flowEnv)
    else if (is(object, "singleParameterTransform")) 
      doubleCheckExistanceOfParameter(object@parameters, flowEnv)
    else if (is(object, "setOperationFilter"))
      for(filt in object@filters) doubleCheckExistanceOfFilter(filt, flowEnv)
    
  }
  
  # If par is a transform then check whether it exists in the flowEnv environment, 
  # and if it doesn't then add it there and make a note of it in flowEnv[['.addedObjects']]
  doubleCheckExistanceOfParameter <- function(par, flowEnv)
  {
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
  
  # If filt is a concreteFilter then check whether it exists in the flowEnv environment, 
  # and if it doesn't then add it there and make a note of it in flowEnv[['.addedObjects']]
  doubleCheckExistanceOfFilter <- function(filt, flowEnv)
  {
    if(is(filt, "concreteFilter")) 
    {
      if(!is.null(filt@filterId) && filt@filterId != "" && !exists(filt@filterId, envir=flowEnv, inherits=FALSE)) 
      {
        flowEnv[[filt@filterId]] <- filt
        flowEnv[['.addedObjects']][[filt@filterId]] <- filt@filterId
        addReferencedObjectsToEnv(filt@filterId, flowEnv)
      }    
    }
  }
  
  modified.write.gatingML(flowEnv, outputFile)
}

# R seems to be saving the .RData when exiting, so let's clean up to at least make that tiny (i.e., empty environment)
rm(list=ls())