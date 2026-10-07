global.libraries <- c("ggtext", "viridis", "viridisLite", "tikzDevice", "scales", "ggmap", "rgeos", "gridExtra", "lfe",
                      "Matrix", "rgdal", "sp", "readstata13", "forcats", "stringr", "dplyr", "purrr", "readr", "tidyr",
                      "tibble", "ggplot2", "tidyverse", "magrittr", "foreign", "matrixStats", "httr", "jsonlite", "modelr",
                      "Formula", "assertthat", "cellranger", "pillar", "backports", "lattice", "glue", "gridtext", "rvest",
                      "colorspace", "sandwich", "plyr", "pkgconfig", "broom", "haven", "xtable", "jpeg", "generics",
                      "ellipsis", "withr", "cli", "crayon", "readxl", "fs", "fansi", "xml2", "tools", "hms", "RgoogleMaps",
                      "lifecycle", "munsell", "reprex", "compiler", "rlang", "rstudioapi", "rjson", "filehash", "bitops",
                      "gtable", "DBI", "R6", "zoo", "lubridate", "utf8", "stringi", "parallel", "Rcpp", "vctrs", "png",
                      "dbplyr", "tidyselect")
pkgTest <- function(x) {
  if(!require(x, character.only = TRUE)) {
    install.packages(x, dep = TRUE)
  }
  return("OK")
}
results <- sapply(as.list(global.libraries), pkgTest)
rm(list = c("pkgTest", "global.libraries", "results"))
restartSession()

replicationfolder <- "/users/jeremymagruder/Dropbox/Irrigation_Rwanda/Data/analysis_master/20220408replication/"
setwd(paste0(replicationfolder, ""))
source(paste0(replicationfolder, "01construct.R"))
source(paste0(replicationfolder, "02analysis.R"))