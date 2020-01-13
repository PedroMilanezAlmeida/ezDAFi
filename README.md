# ez-DAFi
Have you ever wanted to bridge automated clustering techniques and 
human-driven gating strategies? Now you can, with the new DAFi
plugin for FlowJo!

DAFi stands for **D**irected **A**utomated **F**iltering and
**I**dentification of cell populations in polychromatic
flow cytometry data.

The major feature of DAFi is its ability to control unsupervised
clustering tools for the identification of previously known
populations of interest in high-dimensional flow cytometry
datasets.

There are several advantages to this approach, including a 
gating strategy that follows a much more natural distribution of
the data! For more reasons why you should be using DAFi,
please see: [https://doi.org/10.1002/cyto.a.23371]( https://doi.org/10.1002/cyto.a.23371). 

Under the hood, the FlowJo plugin extracts your manual gating
strategy to set the boundaries of regions of interest in your
parameter hyperspace. Then, it gates on learned cluster centroids
located in the regions of interest and finds all cells whose
nearest neighbors are centroids of interest. Finally, the plugin
automatically gates each cell according to their updated
identities, returning them to your FlowJo workspace.

Give it try! Just download the __DAFi.jar__ file above (or find it
on [FlowJo's Plugin page](https://www.flowjo.com/exchange/#/)) and
copy the file to your FlowJo plugins folder!
  
  