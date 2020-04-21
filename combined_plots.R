library(cowplot)
options(Ncpus = parallel::detectCores() - 2)

# set.seed(seed = 3)
source("NOT_GITHUB/DEV/LATAM/LATAM_plot1.R")
source("NOT_GITHUB/DEV/LATAM/LATAM_plot2.R")


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
ggsave("NOT_GITHUB/DEV/LATAM/LATAM_combined_plot.png", combined_plot, width = 19.2, height = 10.8, dpi = 300)

xxx = 1.2
c(16 * xxx, 9 * xxx)
