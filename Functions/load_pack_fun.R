# installs/calls necessary packages ---------------------------------------
load_pack_fun <- function(){
  packages <- c("openxlsx",
                "ggplot2",
                "tidymv",
                "car",
                "ggfortify",
                "gridExtra",
                "grid",
                "ggpubr",
                "openair", 
                "cvTools",
                "extrafont",
                "remotes",
                "R2jags",
                "dplyr",
                "broom.mixed",
                "boot",
                "ggmcmc",
                "scales",
                "postpack",
                "MCMCvis",
                "HDInterval",
                "stringr",
                "lubridate")# keep
  
  if (!require(install.load)) {
    install.packages("install.load")
  }
  
  install.load::install_load(packages)
}