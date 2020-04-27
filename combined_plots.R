options(Ncpus = parallel::detectCores() - 2)

# Libraries ---------------------------------------------------------------

library(cowplot)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(here)
library(httr)
library(purrr)
library(readr)
library(scales)
library(tidyr)
library(vroom)



# Data preparation --------------------------------------------------------

cases_deaths = "deaths" #cases deaths

source(here::here("R/download_or_load_JH_API.R"))
source(here::here("R/data-download.R"))
source(here::here("R/data-preparation.R"))

data_download()



source("R/LATAM_plot1.R")
p1_to_save
source("R/LATAM_plot2.R")
# p2_to_save


combined_plot = 
  cowplot::plot_grid(
    p1_to_save,
    p2_to_save,
    nrow = 2,
    labels = c("A)", "B)"),
    rel_heights = c(1, 1)
  )

combined_plot
ggsave("outputs/plots/LATAM_combined_plot.png", combined_plot, width = 19.2, height = 10.8, dpi = 300)

# xxx = 1.2
# c(16 * xxx, 9 * xxx)
