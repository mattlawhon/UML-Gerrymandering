library(redist)
library(dplyr)
library(sf)

if (! file.exists("data")) {
  dir.create("data")
}

extract_graph_and_data <- function(dataset, dataset_name){
  map <- redist_map(dataset, ndists = 2, pop_tol = 0.1)
  adj <- map$adj
  
  lines <- vector(mode="character", length=10)
  for (i in seq_along(adj)){
    lines[i] <- paste(c(i - 1, unlist(adj[i])), collapse = " ")
  }
  
  graph <- file(paste0("data/", dataset_name, ".adjlist"))
  writeLines(lines, graph)
  close(graph)
  
  df = st_drop_geometry(dataset)
  write.csv(df, paste0("data/", dataset_name, ".csv"), row.names = FALSE)
}

data(iowa, fl25, fl70, fl250)
extract_graph_and_data(iowa, "iowa")
extract_graph_and_data(fl25, "fl25")
extract_graph_and_data(fl70, "fl70")
extract_graph_and_data(fl250, "fl250")

