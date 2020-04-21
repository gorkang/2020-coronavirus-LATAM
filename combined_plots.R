options(Ncpus = parallel::detectCores() - 2)

# Libraries ---------------------------------------------------------------

library(cowplot)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(httr)
library(purrr)
library(readr)
library(scales)
# library(shiny)
# library(shinyjs)
# library(shinythemes)
library(tidyr)
library(vroom)



# Data preparation --------------------------------------------------------

cases_deaths = "deaths" #cases deaths

source(here::here("R/download_or_load.R"))
source(here::here("R/download_or_load_JH_API.R"))

source(here::here("R/fetch_worldometers_safely.R"))
source(here::here("R/data-download.R"))
source(here::here("R/data-preparation.R"))

data_download()



source("R/LATAM_plot1.R")
source("R/LATAM_plot2.R")


# title = paste0("Coronavirus confirmed ", cases_deaths , if (relative == TRUE) " / million people"),
# subtitle = paste0("Starting at ",  "10" ," or more accumulated ", cases_deaths),

# title <- cowplot::ggdraw() + 
#   draw_label(
#     "Coronavirus confirmed deaths",
#     fontface = 'bold',
#     x = 0,
#     hjust = 0
#   ) +
#   theme(
#     # add margin on the left of the drawing canvas, so title is aligned with left edge of first plot
#     plot.margin = margin(0, 0, 0, 7)
#   )

combined_plot = 
  cowplot::plot_grid(
    # title,
    p1_to_save,
    p2_to_save,
    nrow = 2,
    labels = c("A)", "B)"),
    rel_heights = c(1, 1)
    # nrow = 3,
    # labels = c("", "A)", "B)"),
    # rel_heights = c(0.1, 1, 1)
    
  )

combined_plot
ggsave("outputs/plots/LATAM_combined_plot.png", combined_plot, width = 19.2, height = 10.8, dpi = 300)

# xxx = 1.2
# c(16 * xxx, 9 * xxx)
