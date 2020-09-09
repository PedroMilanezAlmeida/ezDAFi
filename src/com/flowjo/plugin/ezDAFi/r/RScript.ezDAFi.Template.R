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

#test whether R version older than 4.0.2
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

if(Rver.maj < 4){
  stop(paste0("The plugin cannot run with R versions older than 4.0.2. ",
              "Your version is: ",
              paste0(version$major, ".", version$minor),
              ". Please, update R and try again."))
} else if(Rver.maj == 4 &
          Rver.min.1 < 0){
  stop(paste0("The plugin cannot run with R versions older than 4.0.2. ",
              "Your version is: ",
              paste0(version$major, ".", version$minor),
              ". Please, update R and try again."))
} else if(Rver.maj == 4 &
          Rver.min.1 == 0 &
          Rver.min.2 < 2) {
  stop(paste0("The plugin cannot run with R versions older than 4.0.2. ",
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
  stop(paste0("The plugin cannot run with Bioconductor releases older than 3.11. ",
              "Your version is: ",
              BiocManager::version(),
              ". Please, update Bioconductor (visit https://www.bioconductor.org/install/) and try again."))
} else if(Bioc.ver.maj == 3 & 
          Bioc.ver.min < 11){
  stop("The plugin cannot run with Bioconductor releases older than 3.11. ",
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
tryCatch(suppressMessages(library("pls")),
         error = function(e){
           install.packages(pkgs =  "pls",
                            repos = 'http://cran.us.r-project.org')
           suppressMessages(library("pls"))
         })
tryCatch(suppressMessages(library("kernlab")),
         error = function(e){
           install.packages(pkgs =  "kernlab",
                            repos = 'http://cran.us.r-project.org')
           suppressMessages(library("kernlab"))
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

hierarc.str <- function(ezDAFi_gate_name, n){
  i <- n
  pop_pars_v <- vector("character",
                       n)
  pop_pars_v[i] <- ezDAFi_gate_name
  while(i > 1) {
    pop_pars_v[i-1] <- strsplit(x =  pop_pars_v[i], 
                                split = "_ezDAFi_",
                                fixed = TRUE) %>%
      .[[1]] %>%
      tail(.,
           1) %>%
      paste0("_ezDAFi_",
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
popOfInt <- "FJ_POPULATION_NAME"
popOfInt
minPopSize <- 50
minPopSize
wspDir <- "FJ_PARM_WSPDIR"
wspDir
wspName <- "FJ_PARM_WSPNAME"
wspName
sampleURI <- "FJ_PARM_SAMPLE_URI"
sampleURI
fj_par_scale <- TRUE
fj_par_scale
fj_par_som <- FJ_PAR_SOM
#fj_par_som <- !fj_par_som
fj_par_som
fj_par_plsda <- FJ_PAR_PLSDA
fj_par_plsda
#fj_par_xdim <- FJ_PAR_XDIM
#fj_par_xdim
fj_csv_ouput_file <- "FJ_CSV_OUPUT_FILE"
fj_csv_ouput_file
fj_output_folder <- "FJ_OUTPUT_FOLDER"
fj_output_folder
fj_par_children <- FJ_PAR_CHILDREN
fj_sample_node_name <- "FJ_SAMPLE_NODE_NAME"
fj_sample_node_name
fj_population_name <- "FJ_POPULATION_NAME"
fj_population_name
fj_millis_time <- "FJ_MILLIS_TIME"
fj_millis_time
fj_par_meta <- FJ_PAR_META
fj_par_meta
plotDir <- paste0(wspDir,
                  "/",
                  wspName,
                  "_ezDAFi_plots")
plotDir
statsDir <- paste0(wspDir,
                   "/",
                   wspName,
                   "_ezDAFi_stats")
statsDir
max.nPar <- FJ_MAX_N_PAR
fj_plot_stats <- FJ_PLOT_STATS

## Code to read gates from wsp file
#load wsp file
wspName <- paste0(wspDir, 
                  "/",
                  wspName)
wspName
ws <- CytoML::open_flowjo_xml(wspName,
                              sample_names_from = "sampleNode")

cs <- load_cytoset_from_fcs(
  files = normalizePath(sampleURI),
  #path = normalizePath(dirname(sampleURI)),
  pattern = NULL,
  phenoData = NULL,
  #descriptions,
  #name.keyword,
  transformation = "linearize",
  which.lines = NULL,
  alter.names = FALSE,
  column.pattern = NULL,
  invert.pattern = FALSE,
  decades = 0,
  is_h5 = TRUE,
  min.limit = NULL,
  truncate_max_range = TRUE,
  dataset = NULL,
  emptyValue = TRUE,
  num_threads = 1,
  ignore.text.offset = FALSE,
  sep = "\t",
  as.is = TRUE,
  #name,
  h5_dir = tempdir(),
  file_col_name = NULL#,
  #  ...
)


#if(!batch_mode) {
gs <- CytoML::flowjo_to_gatingset(ws,
                                  name = 1,
                                  subset = fj_sample_node_name,
                                  extend_val = -Inf,
                                  cytoset = cs,
                                  additional.sampleID = TRUE
)

# make sure only the intended FCS file is in the GatingSet
gs <- gs[flowWorkspace::keyword(gs,
                                "FILENAME")$FILENAME ==
           sampleURI]

flowCore::fsApply(flowWorkspace::gs_pop_get_data(gs), 
                  print)

pData.asDF <- flowCore::parameters(gh_pop_get_data(gs[[1]])) %>%
  flowCore::pData()

eventsCount <- flowWorkspace::gh_pop_get_data(gs[[1]]) %>%
  dim %>%
  .[1]

if (eventsCount == 0){
  stop("R failed to read the input file.", 
       call.=FALSE)
}

#define gates of the selected samples that will be used here
gates_of_sel_sample <- flowWorkspace::gh_get_pop_paths(gs[[1]])
if(popOfInt == "__pluginCalledOnRoot__") {
  popOfInt_full_path <- gates_of_sel_sample[1]
} else {
  popOfInt_full_path <- gates_of_sel_sample[
    basename(gates_of_sel_sample) %in%
      popOfInt]
}

if(length(popOfInt_full_path) > 1){
  stop("It looks like there is another population with the same name in this FlowJo workspace. Please change one of the names and try again.", 
       call. = FALSE)
}


#get info about gating hierarchy for each pop of interest

if(popOfInt == "__pluginCalledOnRoot__") {
  names_gates_SOM <- sub(pattern = "/", 
                         replacement = "", 
                         x = gates_of_sel_sample[-1],
                         fixed = TRUE) %>%
    list(root = .)
} else {
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
      unlist(use.names = FALSE)
  }
}
names(names_gates_SOM) <- basename(popOfInt_full_path)
names_gates_SOM

# check if pop the plugin was called on has any child gate
if(length(names_gates_SOM) == 1 &
   is.null(names_gates_SOM[[1]])) {
  stop("It looks like the selected population has no children for ezDAFi to refine. ezDAFi requires the selected population to have at least one child gate.",
       call. = FALSE)
}

# TODO: CHANGE CODE TO BE ABLE TO HANDLE WHEN PLUGIN IS CALLED ON ROOT
#if(popOfInt == "root"){
 # stop("It looks like the plugin was either called on a gate with a name that ends in '.fcs' (which is not supported, please change the gate name if that is the case) or the plugin was called on the sample directly, not on a gate thereof (the plugin cannot handle the that yet, please select a gate and rerun ezDAFi).",
  #     call. = FALSE)
#}

# for recursive analysis, run whole ezDAFi process for each
# non-terminal gate, adding the results to GatingSet as boolean filter

# find all gates down the gating hierarchy starting from the selected pop
if(popOfInt == "__pluginCalledOnRoot__") {
  names_gates_of_int <- gates_of_sel_sample[-1]
} else {
  names_gates_of_int <- foreach::foreach(pop = seq_along(basename(popOfInt_full_path)),
                                         .final = unlist) %do% {
                                           gates_of_sel_sample[grepl(pattern = paste0("/",
                                                                                      basename(popOfInt_full_path)[pop],
                                                                                      "/"),
                                                                     x = gates_of_sel_sample,
                                                                     fixed = TRUE)]
                                         }
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
#change names of non-terminal gates to reflect the fact they will be ezDAFi-refined
names_gates_non_term_to_SOM <- as.list(names_gates_non_term)
if(popOfInt == "__pluginCalledOnRoot__") {
  names(names_gates_non_term_to_SOM) <-  unlist(names_gates_non_term_to_SOM,
                                                use.names = FALSE) %>%
    strsplit(.,
             split = "/") %>%
    lapply(.,
           function(pop)
             pop[-1]  %>%
             gsub(pattern = "^/",
                  replacement = "",
                  x = .) %>%
             paste0("ezDAFi_",
                    .) %>%
             paste0(.,
                    collapse = "/")) %>%
    unlist(.)
} else {
  names(names_gates_non_term_to_SOM) <-  unlist(names_gates_non_term_to_SOM,
                                                use.names = FALSE) %>%
    sub(pattern = popOfInt_full_path,
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
             paste0("ezDAFi_",
                    .) %>%
             paste0(.,
                    collapse = "/") %>%
             paste0(popOfInt_full_path,
                    "/",
                    .)) %>%
    unlist(.)
}
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
pops_to_SOM <- names_gates_to_SOM[order(tree_pos_gate_to_SOM)] #order is very important to ensure hierarchy of gates

#drop empty gates
pops_to_SOM <- pops_to_SOM[
  lapply(pops_to_SOM,
         function(pop_to_SOM)
           !identical(pop_to_SOM, character(0))) %>%
    unlist(., use.names = FALSE)]
#number of events in each nonezDAFi gate
sapply(pops_to_SOM,
       function(pop_to_SOM)
         dim(flowWorkspace::gh_pop_get_data(gs[[1]],
                                            pop_to_SOM))[1])

#actual ezDAFi
for(pop_to_SOM in seq_along(pops_to_SOM)){
  print("clustering pop:")
  print(pops_to_SOM[pop_to_SOM] %>%
          names(.))
  for(fSample in seq_along(gs)) {
    if(dim(flowWorkspace::gh_pop_get_data(gs[[fSample]],
                                          pops_to_SOM[pop_to_SOM] %>%
                                          names(.)))[1] > minPopSize) {#[,parIndices])[1] > minPopSize) { # in case a subpop is smaller than min #events, SOM is not applied
      ## Code to read the GatingSet data from each population that will be analyzed
      ## with ezDAFi
      gates <- basename(
        flowWorkspace::gh_pop_get_children(
          gs[[fSample]], 
          pops_to_SOM[[pop_to_SOM]])
      )
      for(gate in gates) {
        pop.exprs <- flowWorkspace::gh_pop_get_data(
          gs[[fSample]],
          pops_to_SOM[[pop_to_SOM]]) %>%
          flowCore::exprs()
        colnames(pop.exprs) <- ifelse(
          is.na(pData.asDF$desc),
          yes = pData.asDF$name,
          no = pData.asDF$desc
        )
        irrel.par <- grepl(pattern = "^FSC|^SSC|^Time", 
                           x = colnames(pop.exprs), 
                           ignore.case = FALSE, 
                           fixed = FALSE)
        irrel.par.expr <- pop.exprs[,irrel.par]
        if(class(irrel.par.expr)[1] != "matrix"){
          irrel.par.expr <- as.matrix(irrel.par.expr)
          colnames(irrel.par.expr) <- colnames(pop.exprs)[irrel.par]
        }
        pop.exprs <- pop.exprs[,!irrel.par]
        if(max.nPar >
           dim(pop.exprs)[2]) {
          max.nPar <-
            dim(pop.exprs)[2]
        }
        # we need the gating parameters since if they're not present
        # we cannot gate the centroids
        gate_par <- flowWorkspace::gh_pop_get_gate(
          gs[[fSample]],
          paste0(pops_to_SOM[[pop_to_SOM]],
                 "/",
                 gate))@parameters %>% 
          names
        if(max.nPar !=
           dim(pop.exprs)[2]) {
          ezDAFi.nPar <- max.nPar + length(gate_par)
        } else {
          ezDAFi.nPar <- max.nPar
        }
        gate_par_asName <- gate_par
        gate_par <- ifelse(is.na(pData.asDF$desc[pData.asDF$name %in% gate_par]),
                           yes = pData.asDF$name[pData.asDF$name %in% gate_par],
                           no = pData.asDF$desc[pData.asDF$name %in% gate_par])
        if((!gate_par %in%
            colnames(pop.exprs)) %>%
           any) {
          pop.exprs <- cbind(pop.exprs,
                             irrel.par.expr[,colnames(irrel.par.expr) %in%
                                              gate_par])
          colnames(pop.exprs)[colnames(pop.exprs) == ""] <- 
            colnames(irrel.par.expr)[colnames(irrel.par.expr) %in%
                                       gate_par]
        }
        # get cells within gate of interest to derive t-stat
        in.gate <- flowWorkspace::gh_pop_get_indices(
          gs[[fSample]],
          paste0(pops_to_SOM[[pop_to_SOM]],
                 "/",
                 gate))[
                   flowWorkspace::gh_pop_get_indices(
                     gs[[fSample]],
                     pops_to_SOM[[pop_to_SOM]])
                 ]
        if(sum(in.gate) > 4 &
           sum(!in.gate) > 4) {
          # rank markers by t-stat absolute value
          markers.t <- apply(pop.exprs,
                             2, 
                             function(marker) 
                               t.test(marker[in.gate],
                                      marker[!in.gate],
                                      var.equal = FALSE)$statistic) %>%
            abs() %>%
            sort(decreasing = TRUE)
          keep.marker <- ezDAFi.nPar
          top.nPar <- markers.t[1:keep.marker] %>%
            names
          keep.marker <- c(gate_par, #gating parameters must be included otherwise centroids cannot be gated
                           top.nPar) %>%
            unique() %>%
            .[1:keep.marker]
          if(length(keep.marker) == 1){
            keep.marker <- c(keep.marker, 
                             names(markers.t)[2])
          }
          keep.marker <- colnames(pop.exprs) %in% 
            keep.marker
          print("gate:")
          print(gate)
          print("markers t-stat:")
          print(markers.t)
        } else { #else call: if there are too few cells in traditional gate
          print("gate:")
          print(gate)
          print("Not enough cells to find most informative hidden dimensions.")
          print("ezDAFi will simply apply the manual gate for this gate.")
          flowWorkspace::gs_pop_add(
            gs[[fSample]],
            flowWorkspace::gh_pop_get_gate(
              gs[[fSample]],
              paste0(pops_to_SOM[[pop_to_SOM]],
                     "/", 
                     gate)),
            parent = pops_to_SOM[pop_to_SOM] %>% 
              names(.),
            name = paste0("ezDAFi_",
                          gate) %>%
              gsub(pattern = "^/",
                   replacement = "",
                   x = .)) 
          suppressMessages(flowWorkspace::recompute(gs[[fSample]]))
          next
        }
        print("used markers: ")
        print(colnames(pop.exprs)[keep.marker])
        keep.marker <- ifelse(pData.asDF$desc %in% colnames(pop.exprs)[keep.marker],
                              yes = pData.asDF$desc %in% colnames(pop.exprs)[keep.marker],
                              no = pData.asDF$name %in% colnames(pop.exprs)[keep.marker])
        #### number of centroids ####
        # if the number of cells is larger than 1k, 100 centroids are used;
        # if the number of cells is between 1000 and 50, the number of centroids
        # is approximately 1 centroid for every 10 cells
        if(sum(flowWorkspace::gh_pop_get_indices(gs[[fSample]],
                                                 pops_to_SOM[pop_to_SOM] %>%
                                                 names(.))) > 1000
        ) { 
          fj_par_xdim <- 10 # 100 centroids showed consistent results at high speed
        } else { # for low number of cells, around 1 centroid every 10 cells.
          # minPopSize is always 50
          fj_par_xdim <- sum(
            flowWorkspace::gh_pop_get_indices(gs[[fSample]],
                                              pops_to_SOM[pop_to_SOM] %>%
                                                names(.))
          ) %>% 
            "/"(10) %>%
            sqrt %>% # number of centroids = (fj_par_xdim) ^ 2
            floor() # avoid decimals
        }
        print("number of centroids:")
        print(fj_par_xdim ^ 2)
        
        #### PLS ####
        # Only run PLS-DA if target pop (Y == TRUE) is larger than 10!
        # Y (in.gate) needs to be updated according to results of ezDAFi-ing
        # the parent population;
        # this will be done here by simply applying the manual gate to
        # the ezDAFi-ed parent as an approximation
        ezDAFi.in.gate <- flowWorkspace::gh_pop_get_indices(
          gs[[fSample]],
          paste0(pops_to_SOM[[pop_to_SOM]],
                 "/",
                 gate))[
                   flowWorkspace::gh_pop_get_indices(
                     gs[[fSample]],
                     pops_to_SOM[pop_to_SOM] %>%
                       names(.))
                 ]
        if(fj_par_plsda &
           sum(ezDAFi.in.gate) > 10) {
          X.df <- data.frame(X = scale(flowWorkspace::gh_pop_get_data(gs[[fSample]],
                                                                      pops_to_SOM[pop_to_SOM] %>%
                                                                        names(.)) %>%
                                         .[,keep.marker] %>%
                                         flowCore::exprs(),
                                       center = TRUE,
                                       scale = TRUE) %>%
                               I(),
                             Y = ((ezDAFi.in.gate) * 1 ) %>%
                               I())
          PLS <- tryCatch({
            pls::cppls(Y ~ X,
                       ncomp = sum(keep.marker),
                       data = X.df,
                       center = TRUE)
          },
          error = function(e) {
            stop("PLS-DA cannot easily handle highly correlated features. Please reduce the number of hidden dimensions to about 3 or turn PLS-DA off.",
                 call. = FALSE)
          }
          )
          ls_pop.PLS <- PLS$scores %>%
            matrix(ncol = sum(keep.marker), dimnames = list(rownames(PLS$scores),
                                                            colnames(PLS$scores))) %>%
            list()
          names(ls_pop.PLS) <- rownames(flowCore::pData(gs[[fSample]]))
          fS_pop.PLS <- lapply(ls_pop.PLS,
                               function(sample)
                                 flowCore::flowFrame(sample)) %>%
            flowCore::flowSet()
          fSOM <- FlowSOM::ReadInput(fS_pop.PLS,
                                     compensate = FALSE,
                                     transform = FALSE,
                                     scale = FALSE,
                                     silent = TRUE)
        } else {
          if(fj_par_plsda) {
            print("Too few cells in target population to run PLS-DA. Clustering will be run on original space instead of on PLS latent variables.")
          }
          fSOM <- FlowSOM::ReadInput(
            flowWorkspace::gh_pop_get_data(gs[[fSample]],
                                           pops_to_SOM[pop_to_SOM] %>%
                                             names(.)) %>%
              .[,keep.marker],
            compensate = FALSE,
            transform = FALSE,
            scale = TRUE,
            silent = TRUE)
        }
        if(fj_par_som){
          #### SOM/kmeans clustering ####
          # Code to generate SOM centroids
          set.seed(2020)
          fSOM <- FlowSOM::BuildSOM(fSOM,
                                    colsToUse = NULL,
                                    silent = TRUE,
                                    xdim = fj_par_xdim,
                                    ydim = fj_par_xdim)
          # Code to gate flowSOM results
          # retrieve codes
          if(!fj_par_plsda) {
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
          fkMeans <- stats::kmeans(x = fSOM$data,
                                   centers = fj_par_xdim * fj_par_xdim,
                                   iter.max = 100)
          if(fkMeans$ifault == 4) { # https://stackoverflow.com/a/30055776
            fkMeans <- stats::kmeans(x = fSOM$data,
                                     centers = fkMeans$centers,
                                     iter.max = 100,
                                     algorithm = "MacQueen")
          }
          # Code to gate kmeans results
          # retrieve codes
          if(!fj_par_plsda) {
            codes <- t(apply(fkMeans$centers,
                             1,
                             function(centroid)
                               centroid *
                               fSOM$scaled.scale +
                               fSOM$scaled.center))
          } else {
            codes <- fkMeans$centers
          }
        }
        if(fj_par_plsda) {
          codes <- codes %*%
            t(PLS$loadings) %>%
            apply(.,
                  1,
                  function(parameter)
                    parameter *
                    attr(X.df$X,
                         "scaled:scale") +
                    attr(X.df$X,
                         "scaled:center")) %>%
            t()
          colnames(codes) <- pData.asDF$name[keep.marker]
        }
        #### meta cluster centroids ####
        if(fj_par_meta &
           dim(codes)[1] > 10) { # minPopSize that can trigger spectral meta-clustering = 160 cells
          codes <- scale(codes,#[,gate_par_asName],
                         center = TRUE,
                         scale = TRUE)
          ##############################
          #####
          ##### The next several lines are adapted from https://github.com/SofieVG/FlowSOM/blob/master/R/4_metaClustering.R.
          ##### Credit to the author, SofieVG.
          #####
          ##############################
          meta <- suppressMessages(
            ConsensusClusterPlus::ConsensusClusterPlus(
            d = t(codes),
            maxK = (dim(codes)[1] / 10) %>%
              ceiling(),
            reps = 10, 
            pItem = 0.9, 
            pFeature = 1, 
            title = tempdir(),
            plot = "pdf",
            verbose = FALSE,
            clusterAlg = "hc",
            distance = "euclidean",
            seed = 2020)
            )[[((dim(codes)[1] / 10) %>%
                 ceiling())]]$consensusClass
          # get centroids of metaclusters
          meta.codes <- sapply(
            (dim(codes)[1] / 10) %>%
              ceiling() %>%
              seq(),
            function(metacl)
              if(sum(meta == metacl) > 1){
                if(dim(codes)[2] != 1) {
                  colMeans(codes[meta == metacl,])
                } else {
                  mean(codes[meta == metacl,])
                }
              } else {
                codes[meta == metacl,]
              }) %>%
            t()
          codes <- apply(meta.codes,
                         1,
                         function(metacl)
                           metacl *
                           attr(codes,
                                "scaled:scale") +
                           attr(codes,
                                "scaled:center")) %>%
            t()
          #if(dim(codes)[1] == 1) {
          #  codes <- matrix(codes,
          #                  ncol = 1,
          #                  dimnames = list(NULL,
          #                                  gate_par_asName))
          #}
        }
        #### gate centroids ####
        ls_fSOM <- list(codes)
        names(ls_fSOM) <- rownames(flowCore::pData(gs[[fSample]]))
        #create FlowSet with FlowSOM centroids
        fS_SOM <- lapply(ls_fSOM,
                         function(sample)
                           flowCore::flowFrame(sample)) %>%
          flowCore::flowSet()
        #if(fj_par_meta){
        #  flowCore::parameters(fS_SOM[[1]]) <- flowCore::parameters(
        #    flowWorkspace::gh_pop_get_data(
        #      gs[[fSample]],
        #      y = "root")[,gate_par_asName])
        #}
        #else {
          flowCore::parameters(fS_SOM[[1]]) <- flowCore::parameters(
            flowWorkspace::gh_pop_get_data(
              gs[[fSample]],
              y = "root")[,keep.marker])
          #}
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
        SOM_labels <- rep(FALSE,
                          dim(codes)[1])
        SOM_labels[
          flowWorkspace::gh_pop_get_indices(gs_SOM[[1]],
                                            gate)
        ] <- TRUE
        if(fj_par_meta){
          SOM_labels <- SOM_labels[meta]
        }
        if(fj_par_som){
          cell_ezDAFi_label <- SOM_labels[fSOM$map$mapping[,1]]
        } else {
          cell_ezDAFi_label <- SOM_labels[fkMeans$cluster]
        }
        all_cells_ezDAFi_label <- rep(FALSE,
                                      length(
                                        flowWorkspace::gh_pop_get_indices(
                                          gs[[fSample]],
                                          y = pops_to_SOM[pop_to_SOM] %>%
                                            names(.)))
        )
        all_cells_ezDAFi_label[
          flowWorkspace::gh_pop_get_indices(gs[[fSample]],
                                            y = pops_to_SOM[pop_to_SOM] %>%
                                              names(.))
        ] <- cell_ezDAFi_label
        all_cells_ezDAFi_label <- list(all_cells_ezDAFi_label)
        names(all_cells_ezDAFi_label) <- sampleNames(gs[[fSample]])
        flowWorkspace::gs_pop_add(gs[[fSample]],
                                  all_cells_ezDAFi_label,
                                  parent = pops_to_SOM[pop_to_SOM] %>%
                                    names(.),
                                  name = paste0("ezDAFi_", gate) %>%
                                    gsub(pattern = "^/",
                                         replacement = "",
                                         x = .))
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
          name = paste0("ezDAFi_",
                        gate) %>%
            gsub(pattern = "^/",
                 replacement = "",
                 x = .))
      }
      suppressMessages(flowWorkspace::recompute(gs[[fSample]]))
    }
  }
}

post_ezDAFi_gates <- flowWorkspace::gh_get_pop_paths(gs[[1]])

ezDAFi_nodes <- post_ezDAFi_gates[
  grep(pattern = "ezDAFi_",
       x = basename(post_ezDAFi_gates),
       fixed = TRUE)
]
if(popOfInt != "__pluginCalledOnRoot__") {
  ezDAFi_nodes <- ezDAFi_nodes[grep(pattern = paste0("/",
                                                     popOfInt,
                                                     "/"),
                                    x = ezDAFi_nodes,
                                    fixed = TRUE)]
}
tree_pos_ezDAFi_gate_to_SOM <- strsplit(x = ezDAFi_nodes,
                                        split = "/",
                                        fixed = TRUE) %>%
  lapply(length) %>%
  unlist(.)
ezDAFi_nodes <- ezDAFi_nodes[order(tree_pos_ezDAFi_gate_to_SOM)] #order is very important to ensure hierarchy of gates

nonezDAFi_nodes <- gsub(pattern = "ezDAFi_", 
                        replacement = "",
                        x = ezDAFi_nodes,
                        fixed = TRUE)

# get logical gate for each ezDAFi node
all_cell_ezDAFi_label <- foreach::foreach(ezDAFi_node = ezDAFi_nodes) %do% {
  flowWorkspace::gh_pop_get_indices(gs[[1]],
                                    ezDAFi_node)
}
names(all_cell_ezDAFi_label) <- ezDAFi_nodes
# same for nonezDAFi nodes
all_cell_nonezDAFi_label <- foreach::foreach(nonezDAFi_node = nonezDAFi_nodes) %do% {
  flowWorkspace::gh_pop_get_indices(gs[[1]],
                                    nonezDAFi_node)
}
names(all_cell_nonezDAFi_label) <- nonezDAFi_nodes

# retrieve flowjo cell index
EventNumberDP <- read.csv(file = fj_data_file_path,
                          check.names=FALSE)$EventNumberDP

# given flowjo cell index, get whether or not cell is in ezDAFi node
FJ_event_ezDAFi_label <- foreach::foreach(ezDAFi_node = ezDAFi_nodes) %do% {
  all_cell_ezDAFi_label[[ezDAFi_node]][EventNumberDP]
}
names(FJ_event_ezDAFi_label) <- ezDAFi_nodes
# same for nonezDAFi nodes
FJ_event_nonezDAFi_label <- foreach::foreach(nonezDAFi_node = nonezDAFi_nodes) %do% {
  all_cell_nonezDAFi_label[[nonezDAFi_node]][EventNumberDP]
}
names(FJ_event_nonezDAFi_label) <- nonezDAFi_nodes

# extract ezDAFi gating results to pass back to FlowJo
ezDAFi_labels.ls <- foreach::foreach(ezDAFi_node = ezDAFi_nodes) %do% {
  ezDAFi_label <- as.matrix(as.integer(FJ_event_ezDAFi_label[[ezDAFi_node]]) * 1e5 + 
                              rnorm(n = length(FJ_event_ezDAFi_label[[ezDAFi_node]]),
                                    mean = 0,
                                    sd = 1000))
}
names(ezDAFi_labels.ls) <- ezDAFi_nodes
# same for nonezDAFi nodes
nonezDAFi_labels.ls <- foreach::foreach(nonezDAFi_node = nonezDAFi_nodes) %do% {
  nonezDAFi_label <- as.matrix(as.integer(FJ_event_nonezDAFi_label[[nonezDAFi_node]]) * 1e5 + 
                                 rnorm(n = length(FJ_event_nonezDAFi_label[[nonezDAFi_node]]),
                                       mean = 0,
                                       sd = 1000))
}
names(nonezDAFi_labels.ls) <- nonezDAFi_nodes

# get results in right format for CSV (results are only saved in the end of the script!)
ezDAFi_labels <- matrix(
  unlist(ezDAFi_labels.ls,
         use.names = FALSE),
  ncol = length(ezDAFi_labels.ls),
  byrow = FALSE)
if(popOfInt == "__pluginCalledOnRoot__") {
  colnames_ezDAFi_labels <- foreach::foreach(ezDAFi_node = ezDAFi_nodes,
                                             .final = unlist) %do% {
                                               ezDAFi_node %>%
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
                                                 paste0("root",
                                                        "_",
                                                        .) %>%
                                                 gsub(pattern = ",",
                                                      replacement = ".",
                                                      x = .,
                                                      fixed = TRUE)
                                             }
} else {
  colnames_ezDAFi_labels <- foreach::foreach(ezDAFi_node = ezDAFi_nodes,
                                             .final = unlist) %do% {
                                               ezDAFi_node %>%
                                                 strsplit(x = .,
                                                          split = paste0("/",
                                                                         popOfInt,
                                                                         "/"),
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
}
colnames(ezDAFi_labels) <- colnames_ezDAFi_labels
# same for nonezDAFi nodes
nonezDAFi_labels <- matrix(
  unlist(nonezDAFi_labels.ls,
         use.names = FALSE),
  ncol = length(nonezDAFi_labels.ls),
  byrow = FALSE)
if(popOfInt == "__pluginCalledOnRoot__") {
  colnames_nonezDAFi_labels <- foreach::foreach(nonezDAFi_node = nonezDAFi_nodes,
                                                .final = unlist) %do% {
                                                  nonezDAFi_node %>%
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
                                                    paste0("root",
                                                           "_",
                                                           .) %>%
                                                    gsub(pattern = ",",
                                                         replacement = ".",
                                                         x = .,
                                                         fixed = TRUE)
                                                }
} else {
  colnames_nonezDAFi_labels <- foreach::foreach(nonezDAFi_node = nonezDAFi_nodes,
                                                .final = unlist) %do% {
                                                  nonezDAFi_node %>%
                                                    strsplit(x = .,
                                                             split = paste0("/",
                                                                            popOfInt,
                                                                            "/"),
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
}
colnames(nonezDAFi_labels) <- colnames_nonezDAFi_labels
#sanity check
print(
  apply(ezDAFi_labels,
        2,
        function(pop)
          mean(pop > 5e4))
)
if(fj_plot_stats){
  #get stats
  pop.stats.count <- gh_pop_get_stats(gs[[1]],
                                      type =  "count")
  pop.stats.percent <- gh_pop_get_stats(gs[[1]],
                                        type =  "percent")
  pop.stats.percent$percent <- pop.stats.percent$percent * 100
  
  pop.stats.count.ezDAFi <- pop.stats.count[pop.stats.count$pop %in% 
                                              ezDAFi_nodes,]$count
  names(pop.stats.count.ezDAFi) <- pop.stats.count[pop.stats.count$pop %in% 
                                                     ezDAFi_nodes,]$pop
  pop.stats.count.ezDAFi <- data.frame(pop.stats.count.ezDAFi) %>%
    t
  rownames(pop.stats.count.ezDAFi) <- basename(sampleURI)
  
  pop.stats.percent.ezDAFi <- pop.stats.percent[pop.stats.percent$pop %in% 
                                                  ezDAFi_nodes,]$percent
  names(pop.stats.percent.ezDAFi) <- pop.stats.percent[pop.stats.percent$pop %in% 
                                                         ezDAFi_nodes,]$pop
  pop.stats.percent.ezDAFi <- data.frame(pop.stats.percent.ezDAFi) %>%
    t
  rownames(pop.stats.percent.ezDAFi) <- basename(sampleURI)
  
  pop.stats.count.trad <- pop.stats.count[
    match(gsub(pattern = "ezDAFi_", 
               replacement = "",
               x = ezDAFi_nodes,
               fixed = TRUE),
          pop.stats.count$pop),
  ]$count
  names(pop.stats.count.trad) <- pop.stats.count[
    match(gsub(pattern = "ezDAFi_", 
               replacement = "",
               x = ezDAFi_nodes,
               fixed = TRUE),
          pop.stats.count$pop),
  ]$pop
  pop.stats.count.trad <- data.frame(pop.stats.count.trad) %>%
    t
  rownames(pop.stats.count.trad) <- basename(sampleURI)
  
  pop.stats.percent.trad <- pop.stats.percent[
    match(gsub(pattern = "ezDAFi_", 
               replacement = "",
               x = ezDAFi_nodes,
               fixed = TRUE),
          pop.stats.count$pop)
    ,]$percent
  names(pop.stats.percent.trad) <- pop.stats.percent[
    match(gsub(pattern = "ezDAFi_", 
               replacement = "",
               x = ezDAFi_nodes,
               fixed = TRUE),
          pop.stats.count$pop)
    ,]$pop
  pop.stats.percent.trad <- data.frame(pop.stats.percent.trad) %>%
    t
  rownames(pop.stats.percent.trad) <- basename(sampleURI)
  #write stats
  if(!dir.exists(statsDir)) {
    dir.create(statsDir)
  }
  ezDAFi.count.file <- paste0(statsDir,
                              "/",
                              "ezDAFi_count_",
                              ifelse(popOfInt == "__pluginCalledOnRoot__",
                                     yes = "root",
                                     no = popOfInt),
                              ".csv")
  ezDAFi.percent.file <- paste0(statsDir,
                                "/",
                                "ezDAFi_percent_",
                                ifelse(popOfInt == "__pluginCalledOnRoot__",
                                       yes = "root",
                                       no = popOfInt),
                                ".csv")
  trad.count.file <- paste0(statsDir,
                            "/",
                            "manual_count_",
                            ifelse(popOfInt == "__pluginCalledOnRoot__",
                                   yes = "root",
                                   no = popOfInt),
                            ".csv")
  trad.percent.file <- paste0(statsDir,
                              "/",
                              "manual_percent_",
                              ifelse(popOfInt == "__pluginCalledOnRoot__",
                                     yes = "root",
                                     no = popOfInt),
                              ".csv")
  if(!ezDAFi.count.file %>%
     file.exists()) {
    write.table(x = c("sample",
                      colnames(pop.stats.count.ezDAFi)) %>%
                  t,
                append = FALSE,
                sep = ",",
                file = ezDAFi.count.file,
                row.names = FALSE, 
                quote = TRUE,
                col.names = FALSE)
  } else if(
    !((suppressMessages(
      read.csv(file = ezDAFi.count.file,
               header = FALSE,
               nrows = 1)) %>%
      unlist(use.names = FALSE) %>%
      as.character() ==
      c("sample",
        colnames(pop.stats.count.ezDAFi))) %>%
      all)
  ) {
    stop("Either the gates of \"",
         ifelse(popOfInt == "__pluginCalledOnRoot__",
                yes = "root",
                no = popOfInt),
         "\" changed since ezDAFi's last run\nor batch analysis was called on samples with different gating strategies.\nPlease delete stats before rerunning and make sure the same gating\nstrategy is applied to all samples.", 
         call.=FALSE)
  }
  if(!(basename(sampleURI) %in%
       read.csv(file = ezDAFi.count.file,
                header = TRUE,
                colClasses = c("character",
                               rep("NULL",
                                   length(ezDAFi_nodes))))[,1])) {
    write.table(x = pop.stats.count.ezDAFi,
                append = TRUE,
                sep = ",",
                file = ezDAFi.count.file,
                row.names = TRUE, 
                quote = TRUE,
                col.names = FALSE)
  } else {
    sampleFCS.pos <- which(
      read.csv(file = ezDAFi.count.file,
               header = FALSE,
               colClasses = c("character",
                              rep("NULL",
                                  length(ezDAFi_nodes))))[,1] ==
        basename(sampleURI)
    )
    ezDAFi.count.df <- read.csv(file = ezDAFi.count.file,
                                header = FALSE,
                                skip = 1)
    colnames(ezDAFi.count.df) <- c("sample",
                                   colnames(pop.stats.count.ezDAFi))
    ezDAFi.count.df[(sampleFCS.pos - 1),] <- c(basename(sampleURI),
                                               pop.stats.count.ezDAFi[1,])
    write.table(x = ezDAFi.count.df,
                append = FALSE,
                sep = ",",
                file = ezDAFi.count.file,
                row.names = FALSE, 
                quote = TRUE,
                col.names = TRUE)
  }
  if(!ezDAFi.percent.file %>%
     file.exists()) {
    write.table(x = c("sample", 
                      colnames(pop.stats.percent.ezDAFi)) %>%
                  t,
                append = FALSE,
                sep = ",",
                file = ezDAFi.percent.file,
                row.names = FALSE, 
                quote = TRUE,
                col.names = FALSE)
  } else if(
    !((suppressMessages(
      read.csv(file = ezDAFi.percent.file,
               header = FALSE,
               nrows = 1)
    ) %>%
    unlist(use.names = FALSE) %>%
    as.character() ==
    c("sample",
      colnames(pop.stats.percent.ezDAFi))) %>%
    all)
  ) {
    stop("Either the gates of \"",
         ifelse(popOfInt == "__pluginCalledOnRoot__",
                yes = "root",
                no = popOfInt),
         "\" changed since ezDAFi's last run\nor batch analysis was called on samples with different gating strategies.\nPlease delete stats before rerunning and make sure the same gating\nstrategy is applied to all samples.", 
         call.=FALSE)
  }
  if(!(basename(sampleURI) %in%
       read.csv(file = ezDAFi.percent.file,
                header = TRUE,
                colClasses = c("character",
                               rep("NULL",
                                   length(ezDAFi_nodes))))[,1])) {
    write.table(x = pop.stats.percent.ezDAFi,
                append = TRUE,
                sep = ",",
                file = ezDAFi.percent.file,
                row.names = TRUE, 
                quote = TRUE,
                col.names = FALSE)
  } else {
    sampleFCS.pos <- which(
      read.csv(file = ezDAFi.percent.file,
               header = FALSE,
               colClasses = c("character",
                              rep("NULL",
                                  length(ezDAFi_nodes))))[,1] ==
        basename(sampleURI)
    )
    ezDAFi.percent.df <- read.csv(file = ezDAFi.percent.file,
                                  header = FALSE,
                                  skip = 1)
    colnames(ezDAFi.percent.df) <- c("sample",
                                     colnames(pop.stats.percent.ezDAFi))
    ezDAFi.percent.df[(sampleFCS.pos - 1),] <- c(basename(sampleURI),
                                                 pop.stats.percent.ezDAFi[1,])
    write.table(x = ezDAFi.percent.df,
                append = FALSE,
                sep = ",",
                file = ezDAFi.percent.file,
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
         ifelse(popOfInt == "__pluginCalledOnRoot__",
                yes = "root",
                no = popOfInt),
         "\" changed since ezDAFi's last run\nor batch analysis was called on samples with different gating strategies.\nPlease delete stats before rerunning and make sure the same gating\nstrategy is applied to all samples.", 
         call.=FALSE)
  }
  if(!(basename(sampleURI) %in%
       read.csv(file = trad.count.file,
                header = TRUE,
                colClasses = c("character",
                               rep("NULL",
                                   length(ezDAFi_nodes))))[,1])) {
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
                                  length(ezDAFi_nodes))))[,1] ==
        basename(sampleURI)
    )
    trad.count.df <- read.csv(file = trad.count.file,
                              header = FALSE,
                              skip = 1)
    colnames(trad.count.df) <- c("sample",
                                 colnames(pop.stats.count.trad))
    trad.count.df[(sampleFCS.pos - 1),] <- c(basename(sampleURI),
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
         ifelse(popOfInt == "__pluginCalledOnRoot__",
                yes = "root",
                no = popOfInt),
         "\" changed since ezDAFi's last run\nor batch analysis was called on samples with different gating strategies.\nPlease delete stats before rerunning and make sure the same gating\nstrategy is applied to all samples.", 
         call.=FALSE)
  }
  if(!(basename(sampleURI) %in%
       read.csv(file = trad.percent.file,
                header = TRUE,
                colClasses = c("character",
                               rep("NULL",
                                   length(ezDAFi_nodes))))[,1])) {
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
                                  length(ezDAFi_nodes))))[,1] ==
        basename(sampleURI)
    )
    trad.percent.df <- read.csv(file = trad.percent.file,
                                header = FALSE,
                                skip = 1)
    colnames(trad.percent.df) <- c("sample",
                                   colnames(pop.stats.percent.trad))
    trad.percent.df[(sampleFCS.pos - 1),] <- c(basename(sampleURI),
                                               pop.stats.percent.trad[1,])
    write.table(x = trad.percent.df,
                append = FALSE,
                sep = ",",
                file = trad.percent.file,
                row.names = FALSE, 
                quote = TRUE,
                col.names = TRUE)
  }
  
  # create heatmaps with median expression of all selected paramaters on ezDAFi vs traditional gates
  # as well as pseudocolor for the bidimensional gate
  if(!dir.exists(plotDir)){
    dir.create(plotDir)
  }
  ############################################################################
  ## Code heavily inspired in ggcyto, credit to the authors of that package ##
  ############################################################################
  for(ezDAFi_node in ezDAFi_nodes) {
    nonezDAFi_node <- gsub(pattern = "ezDAFi_", 
                           replacement = "",
                           x = ezDAFi_node,
                           fixed = TRUE)
    gate_par <- flowWorkspace::gh_pop_get_gate(
      gs[[1]],
      nonezDAFi_node)@parameters %>% 
      names
    filtLs <- filterList(gs_pop_get_gate(gs[[1]],
                                         nonezDAFi_node))
    n.events.ezDAFi <- (gh_pop_get_indices(gs[[1]],
                                           ezDAFi_node) %>%
                          sum)
    n.events.trad <- (gh_pop_get_indices(gs[[1]],
                                         nonezDAFi_node) %>%
                        sum)
    tryCatch({
      if(length(gate_par) == 2) {
        pop.exprs <- rbind(flowWorkspace::gh_pop_get_data(gs[[1]],
                                                          ezDAFi_node) %>%
                             flowCore::exprs() %>%
                             .[,gate_par],
                           flowWorkspace::gh_pop_get_data(gs[[1]],
                                                          nonezDAFi_node) %>%
                             flowCore::exprs() %>%
                             .[,gate_par]) %>%
          as.data.frame()
        pop.exprs$gate.type <- c(rep(paste0("ezDAFi :: ",
                                            n.events.ezDAFi,
                                            " events"),
                                     n.events.ezDAFi),
                                 rep(paste0("manual :: ",
                                            n.events.trad,
                                            " events"),
                                     n.events.trad))
        xlim.outliers <- (abs(flowWorkspace::gh_pop_get_data(gs[[1]],
                                                             nonezDAFi_node %>%
                                                               dirname()) %>%
                                flowCore::exprs() %>%
                                .[,gate_par[1]] - median(
                                  flowWorkspace::gh_pop_get_data(gs[[1]],
                                                                 nonezDAFi_node %>%
                                                                   dirname()) %>%
                                    flowCore::exprs() %>%
                                    .[,gate_par[1]]
                                )) / mad(
                                  flowWorkspace::gh_pop_get_data(gs[[1]],
                                                                 nonezDAFi_node %>%
                                                                   dirname()) %>%
                                    flowCore::exprs() %>%
                                    .[,gate_par[1]]
                                ))
        xlim.outliers <- xlim.outliers > quantile(xlim.outliers, prob = 0.999)
        ylim.outliers <- (abs(flowWorkspace::gh_pop_get_data(gs[[1]],
                                                             nonezDAFi_node %>%
                                                               dirname()) %>%
                                flowCore::exprs() %>%
                                .[,gate_par[2]] - median(
                                  flowWorkspace::gh_pop_get_data(gs[[1]],
                                                                 nonezDAFi_node %>%
                                                                   dirname()) %>%
                                    flowCore::exprs() %>%
                                    .[,gate_par[2]]
                                )) / mad(
                                  flowWorkspace::gh_pop_get_data(gs[[1]],
                                                                 nonezDAFi_node %>%
                                                                   dirname()) %>%
                                    flowCore::exprs() %>%
                                    .[,gate_par[2]]
                                ))
        ylim.outliers <- ylim.outliers > quantile(ylim.outliers, prob = 0.999)
        xlim.exprs <- flowWorkspace::gh_pop_get_data(gs[[1]],
                                                     nonezDAFi_node %>%
                                                       dirname()) %>%
          flowCore::exprs() %>%
          .[!xlim.outliers,gate_par[1]] %>%
          range()
        ylim.exprs <- flowWorkspace::gh_pop_get_data(gs[[1]],
                                                     nonezDAFi_node %>%
                                                       dirname()) %>%
          flowCore::exprs() %>%
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
            ggtitle(paste0(nonezDAFi_node %>%
                             basename(),
                           "\n",
                           basename(sampleURI)))
        )
        if(!dim(pop.exprs)[1] < 100) {
          plot.cells <- plot.cells +
            geom_hex(
              binwidth = c((xlim.exprs[2] - xlim.exprs[1])/256,
                           (ylim.exprs[2] - ylim.exprs[1])/256),
              mapping = aes(fill = stat(log(count)))
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
                                      x = ezDAFi_node,
                                      fixed = TRUE),
                                 "_",
                                 basename(sampleURI),
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
        pop.exprs <- c(flowWorkspace::gh_pop_get_data(gs[[1]],
                                                      ezDAFi_node) %>%
                         flowCore::exprs() %>%
                         .[,gate_par[1]],
                       flowWorkspace::gh_pop_get_data(gs[[1]],
                                                      nonezDAFi_node) %>%
                         flowCore::exprs() %>%
                         .[,gate_par[1]]) %>%
          data.frame()
        colnames(pop.exprs) <- gate_par[1]
        xlim.outliers <- (abs(flowWorkspace::gh_pop_get_data(gs[[1]],
                                                             nonezDAFi_node %>%
                                                               dirname()) %>%
                                flowCore::exprs() %>%
                                .[,gate_par[1]] - median(
                                  flowWorkspace::gh_pop_get_data(gs[[1]],
                                                                 nonezDAFi_node %>%
                                                                   dirname()) %>%
                                    flowCore::exprs() %>%
                                    .[,gate_par[1]]
                                )) / mad(
                                  flowWorkspace::gh_pop_get_data(gs[[1]],
                                                                 nonezDAFi_node %>%
                                                                   dirname()) %>%
                                    flowCore::exprs() %>%
                                    .[,gate_par[1]]
                                ))
        xlim.outliers <- xlim.outliers > quantile(xlim.outliers, prob = 0.999)
        xlim.exprs <- flowWorkspace::gh_pop_get_data(gs[[1]],
                                                     nonezDAFi_node %>%
                                                       dirname()) %>%
          flowCore::exprs() %>%
          .[-xlim.outliers,gate_par[1]] %>%
          range()
        pop.exprs$gate.type <- c(rep(paste0("ezDAFi :: ",
                                            n.events.ezDAFi,
                                            " events"),
                                     n.events.ezDAFi),
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
            xlab(ifelse(is.na(pData.asDF[pData.asDF$name %in% 
                                           gate_par[1],]$desc),
                        yes = pData.asDF[pData.asDF$name %in% 
                                           gate_par[1],]$name,
                        no = pData.asDF[pData.asDF$name %in% 
                                          gate_par[1],]$desc)) +
            facet_wrap("gate.type") +
            ggtitle(paste0(nonezDAFi_node %>%
                             basename(),
                           "\n",
                           basename(sampleURI)))
        )
        if(!filtLs$x1 < -1e100) {
          plot.cells <- plot.cells +
            geom_vline(xintercept = filtLs$x1,
                       color = "red")
        }
        if(!filtLs$xend > 1e100) {
          plot.cells <- plot.cells +
            geom_vline(xintercept = filtLs$xend,
                       color = "red")
        }
        ggsave(plot = plot.cells,
               filename = paste0(plotDir,
                                 "/HISTOGRAM.",
                                 gsub(pattern = "/",
                                      replacement = "_", 
                                      x = ezDAFi_node,
                                      fixed = TRUE),
                                 "_",
                                 basename(sampleURI),
                                 ".pdf"),
               width = 5,
               height = 3)
      }
    },
    error = function(e)
      print(paste0("Single cell plot failed for: ",
                   ezDAFi_node)))
    # get median
    mark.exprs <- cbind(
      flowWorkspace::gh_pop_get_data(gs[[1]],
                                     ezDAFi_node) %>% 
        flowCore::exprs() %>%
        apply(2,
              median)
      , 
      flowWorkspace::gh_pop_get_data(gs[[1]],
                                     nonezDAFi_node) %>%
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
                 labels_col = c("ezDAFi", "manual"),
                 main = nonezDAFi_node %>%
                   basename(),
                 filename = paste0(plotDir,
                                   "/HEATMAP.",
                                   gsub(pattern = "/",
                                        replacement = "_", 
                                        x = ezDAFi_node,
                                        fixed = TRUE),
                                   "_",
                                   basename(sampleURI),
                                   ".pdf"),
                 width = 3,
                 height = ifelse(0.25 * dim(.)[1] < 4,
                                 yes = 4,
                                 no = 0.25 * dim(.)[1]),
                 angle_col = 45)},
      error = function(e)
        print(paste0("Heatmap failed for: ",
                     ezDAFi_node)))
  }
}

all.labels.ls <- foreach::foreach(ezDAFi_node = ezDAFi_nodes) %do% {
  all.label <- as.matrix(all_cell_ezDAFi_label[[ezDAFi_node]] %>%
                           as.integer(.))
}
names(all.labels.ls) <- ezDAFi_nodes

all.labels <- matrix(unlist(all.labels.ls,
                            use.names = FALSE),
                     ncol = length(all.labels.ls),
                     byrow = FALSE)
colnames(all.labels) <- colnames(ezDAFi_labels)
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
                split = "_ezDAFi_",
                fixed = TRUE) %>%
       .[[1]] %>% 
       length()) > 2){
    mat <- matrix(c(5e4, 5e5),
                  ncol = 1,
                  dimnames = list(c("min", "max"),
                                  colnames(all.labels)[pop]))
    rg <- rectangleGate(filterId = colnames(all.labels)[pop],
                        .gate = mat)
    # test if ezDAFi is not direct child of pop of interest
    # if it is, add gate directly to flowEnv
    # if it is a grandchild, get name of ezDAFi parent and add it to flowEnv hierarchically
    flowEnv[[as.character(colnames(all.labels)[pop])]] <- rg
  } else { # from: https://rdrr.io/github/RGLab/CytoML/src/R/gate-methods.R
    ezDAFi_gates_v <- hierarc.str(ezDAFi_gate_name = colnames(all.labels)[pop],
                                  n = 2)
    rgs <- vector("list",
                  length = 2)
    names(rgs) <- ezDAFi_gates_v
    for(ezDAFi_gate in ezDAFi_gates_v) {
      rgs[[as.character(ezDAFi_gate)]] <- 
        rectangleGate(filterId = ezDAFi_gate,
                      .gate = matrix(c(5e4, 5e5),
                                     ncol = 1,
                                     dimnames = list(c("min", "max"),
                                                     ezDAFi_gate)))
      
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

##########################################################
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

#write ezDAFi gates
modified.write.gatingML(flowEnv, outputFile)
#write derived parameters
write.csv(ezDAFi_labels, 
          file = fj_csv_ouput_file, 
          row.names = FALSE, 
          quote = TRUE)
write.csv(nonezDAFi_labels, 
          file = sub(pattern = ".csv.ezDAFi.csv$",
                     replacement = ".csv.nonezDAFi.csv",
                     x = fj_csv_ouput_file,
                     fixed = FALSE), 
          row.names = FALSE, 
          quote = TRUE)
#}