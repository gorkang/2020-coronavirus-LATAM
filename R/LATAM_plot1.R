library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(purrr)
source("R/data-preparation.R")

dta_raw = read_csv(here::here("outputs/raw_data.csv"),
                   col_types =
                     cols(
                       country = col_character(),
                       time = col_date(format = ""),
                       cases_sum = col_double(),
                       cases_diff = col_double(),
                       deaths_sum = col_double(),
                       deaths_diff = col_double(),
                       source = col_character()
                     )) %>% 
  mutate(country = 
           case_when(
             country == "USA" ~ "US",
             TRUE ~ country
           ))


DF_population_countries = read_csv(here::here("data/population_countries.csv"),
                                   col_types =
                                     cols(
                                       country = col_character(),
                                       population = col_double()
                                     )) %>%
  mutate(country =
           case_when(
             country == "Korea, South" ~ "Korea (South)",
             TRUE ~ country
           )) %>%
  mutate(country =
           case_when(
             country == "Korea, South" ~ "Korea (South)",
             TRUE ~ country
           )) %>% 
  mutate(continent_name = 
           case_when(
             country %in% c("Mexico", "Dominican Republic", "Panama", "Honduras", "Cuba", "Costa Rica", "Guatemala", "El Salvador") ~ "Latin America",
             country %in% c("Russia", "Kazakhstan", "Azerbaijan", "Armenia") ~ "Asia",
             continent_name == "South America" ~ "Latin America",
             TRUE ~ continent_name
           )) %>% 
  mutate(country = 
           case_when(
             country == "USA" ~ "US",
             TRUE ~ country
           ))



# Variables ---------------------------------------------------------------

# Data preparation

min_n = 10
INPUT_min_n = min_n 
VAR_min_n = min_n

cases_deaths = "deaths"
INPUT_cases_deaths = cases_deaths
INPUT_continent_name_input = c("Asia", "Europe", "North America", "Latin America")


countries_plot = dta_raw %>% 
  left_join(DF_population_countries %>% select(country, continent_name)) %>%
  filter(continent_name %in% INPUT_continent_name_input) %>% 
  drop_na(continent_name) %>% 
  filter(deaths_sum > 1) %>% 
  distinct(country) %>% 
  pull(country)

INPUT_countries_plot = countries_plot



# Plot specific

smooth = TRUE
log_scale = TRUE


relative = FALSE
INPUT_relative = relative


accumulated_daily_pct = "accumulated"
INPUT_accumulated_daily_pct = accumulated_daily_pct

growth = 30
VAR_growth = growth

highlight = " "
INPUT_highlight = highlight
VAR_highlight = highlight



# Data preparation --------------------------------------------------------

dta_temp = data_preparation(
  data_source = "OWID",
  cases_deaths = INPUT_cases_deaths,
  countries_plot = INPUT_countries_plot,
  min_n = INPUT_min_n,
  relative = INPUT_relative
)  %>%

  # If repeated values the same day, keep higher
  group_by(country, time) %>% 
  distinct(KEY = paste0(country, time, value), .keep_all = TRUE) %>% 
  select(-KEY) %>% 
  ungroup() %>% 
  
  # re-adjust after filtering
  group_by(country) %>%
  mutate(days_after_100 = as.numeric(0:(length(country) - 1))) %>% 
  
  mutate(days_after_100 = round(days_after_100, 0)) %>% 
  
  group_by(country) %>%
  
  # Get rid of the latest data if it either 0 or negative
  filter( !(days_after_100 == max(days_after_100, na.rm = TRUE) & diff <= 0)) %>% 
  filter(source == "OWID") %>% 
  
  
  # Create name_end labels
  mutate(
    name_end =
      case_when(
        # days_after_100 == max(days_after_100, na.rm = TRUE) & time == max(time, na.rm = TRUE) ~ paste0(as.character(country), ": ", format(value, big.mark=","), " - ", days_after_100, " days"),
        days_after_100 == max(days_after_100, na.rm = TRUE) & time == max(time, na.rm = TRUE) ~ paste0(as.character(country)),
        # what == "lockdown" ~ "*",
        TRUE ~ "")) %>% 
  mutate(highlight = country)



# Highlight
if (any(' ' != VAR_highlight)) {
  
  # Create colors diccionary
  DF_colors_temp =
    dta_temp %>% 
    filter(country %in% VAR_highlight) %>% 
    distinct(country)
  
  # If the selected country does not have observations over the threshold
  if (nrow(DF_colors_temp) > 0) { 
    DF_colors = DF_colors_temp %>% 
      bind_cols(highlight = hue_pal(l = 50)(nrow(.)))
  } else {
    DF_colors = DF_colors_temp %>% 
      mutate(highlight = "")
  }
  
  
  final_df = dta_temp %>% 
    left_join(DF_colors, by = "country") %>% 
    mutate(highlight = 
             case_when(
               is.na(highlight) ~ "grey",
               TRUE ~ highlight
             ))
} else {
  final_df = dta_temp %>% mutate(highlight = country)
}



# Show accumulated or daily plot
  DF_plot = final_df

# Define which countries get a smooth (have enough points)
VALUE_span = 3
counts_filter = DF_plot %>% count(country) %>% filter(n > VALUE_span)




# Growth line -------------------------------------------------------------



# LIMITS of DATA
  MAX_y = max(final_df$value, na.rm = TRUE) * 1.1

# To avoid error
if (is.infinite(max(final_df$days_after_100, na.rm = TRUE))) {
  max_finaldf_days_after_100 = 10 
} else {
  max_finaldf_days_after_100 = max(final_df$days_after_100, na.rm = TRUE)
}

# If we use 1.1 * to avoid overlaping y axis goes up a lot
line_factor = 1

  growth_line = tibble(
    value = cumprod(c(VAR_min_n, rep((100 + VAR_growth) / 100, line_factor * max_finaldf_days_after_100))),
    days_after_100 = 0:(line_factor * max_finaldf_days_after_100)) %>% 
    filter(value <= MAX_y)







# Text annotation Latam ---------------------------------------------------

  reference_continent = final_df %>% filter(days_after_100 == max(days_after_100)) %>% arrange(desc(days_after_100)) %>% head(1) %>% pull("continent_name")
  reference_country = final_df %>% filter(days_after_100 == max(days_after_100)) %>% arrange(desc(days_after_100)) %>% head(1) %>% pull("country")
  list_continents_plot = final_df %>% ungroup() %>%  distinct(continent_name) %>% pull(continent_name)
  list_remaining_continents_plot = list_continents_plot[!list_continents_plot %in% reference_continent]
  
  if (!is_empty(list_remaining_continents_plot)) {
    
    ann_position = 
      1:nrow(final_df %>% filter(continent_name %in% list_remaining_continents_plot) %>% ungroup() %>% distinct(continent_name)) %>%
      map(~ max(
        final_df %>% 
          filter(continent_name == list_remaining_continents_plot[.x]) %>% 
          pull(days_after_100))) %>% unlist()
    
    label_headstart = paste0("[", reference_country, "'s headstart]")
    
    
  } else {
    # If only one continent remaining
    list_remaining_continents_plot = reference_continent
    ann_position = Inf
    label_headstart = ""
  }
  
  
  ann_text <-
    data.frame(
      days_after_100 = (ann_position + max(final_df$days_after_100))/2, # 39 Italy, # 55 China
      value = 10,
      lab = "text",
      country = NA_character_,
      continent_name = factor(
        list_remaining_continents_plot, 
        levels = list_remaining_continents_plot
      )
    )


# Draw plot ---------------------------------------------

p_temp = ggplot(data = DF_plot, 
                aes(x = days_after_100, y = value, group = as.factor(country), color = continent_name)) +
  
  # Trend line
  geom_line(data = growth_line, aes(days_after_100, value), linetype = "dotted", inherit.aes = FALSE, alpha = 1, size = .9) +
  
  # Country points (last one bigger)
  geom_point(aes(size = .005 + as.integer(final_df$name_end != "" & final_df$name_end != "*") - .5), alpha = .1) +
  
  scale_x_continuous(breaks = seq(0, max(final_df$days_after_100, na.rm = TRUE) + 1, 10)) +
  labs(
    x = paste0("Days after ",  VAR_min_n ," accumulated ", cases_deaths),
    y = paste0("Confirmed ", accumulated_daily_pct, " ", cases_deaths, " (log)",  if (relative == TRUE) " / million people")
  ) +
  theme_minimal(base_size = 12) +
  
  theme(legend.position = "none",
        panel.grid.minor = element_line(size = 0.25),
        panel.spacing = unit(1, "lines")
  )

# Smooth or not
if(smooth == FALSE) {
  p_temp = p_temp + geom_line(alpha = .4) 
} else {
  p_temp = p_temp +  
    
    geom_line(alpha = .4) +
    
    geom_smooth(data = DF_plot %>% filter(country %in% counts_filter$country), 
                aes(x = days_after_100, y = value, group = as.factor(continent_name), color = continent_name),
                method = "loess", span = 1.5, se = FALSE, size = 1, alpha = .9, na.rm = TRUE)
}

# If any value is not " ", scale_color_identity
if (any(' ' != VAR_highlight)) { p_temp =  p_temp + scale_color_identity() }


# LIMITS
if (accumulated_daily_pct == "daily") {
  MAX_y = max(final_df$diff, na.rm = TRUE, na.rm = TRUE) * 1.1
  MIN_y = min(final_df$diff, na.rm = TRUE, na.rm = TRUE) * 0.1 # In Log scale can't use 0
} else if (accumulated_daily_pct == "%") {
  MAX_y = max(final_df$diff_pct, na.rm = TRUE, na.rm = TRUE) * 100
  MIN_y = min(final_df$diff_pct, na.rm = TRUE, na.rm = TRUE) * 1 # In Log scale can't use 0
} else {
  MAX_y = max(final_df$value, na.rm = TRUE, na.rm = TRUE) * 1.1
  MIN_y = min(final_df$value, na.rm = TRUE, na.rm = TRUE) * 0.95
}

if (MIN_y == 0) MIN_y = 1
# message("MIN_y: ", MIN_y, " MAX_y: ", MAX_y)

# Scale, log or not
if (log_scale == TRUE) {
  p_temp = p_temp +
    scale_y_log10(breaks = scales::log_breaks(n = 10), labels = function(x) format(x, big.mark = ",", scientific = FALSE), limits = c(MIN_y, MAX_y)) 
} else {
  p_temp = p_temp +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = function(x) format(x, big.mark = ",", scientific = FALSE), limits = c(MIN_y, MAX_y)) +
    labs(y = paste0("Confirmed ", accumulated_daily_pct, " ", cases_deaths))
}


# Annotation trend line
  which_position = 22
  x_axis = max(growth_line$days_after_100[which_position], na.rm = TRUE) - 3.5
  y_axis = max(growth_line$value[which_position], na.rm = TRUE)  # MAX 

p1_to_save <- p_temp + 
  annotate(geom = "text",
           size = 2.8,
           x = x_axis, 
           y = y_axis, 
           angle = 71,
           alpha = .8,
           label = paste0(VAR_growth, "% growth")) +
  
  # Country label
  ggrepel::geom_label_repel(aes(label = name_end), show.legend = FALSE, segment.color = "grey", segment.size  = .15, alpha = .75, size = 3, seed = 30) +
  geom_rect(data = data.frame(continent_name = list_remaining_continents_plot), #c("Europe", "North America", "Latin America")),
            aes(
                xmin = ann_position,
                xmax = Inf,
                ymin = 0,
                ymax = Inf),
            alpha = 0.2,
            fill = "grey",
            inherit.aes = FALSE) +
  geom_text(data = ann_text, label = "[China's headstart]", size = 3, alpha = .8, color = "black") +    
  facet_grid( ~ continent_name) +#, scales = "free_x"
  theme(aspect.ratio = 1) + 
  scale_colour_manual(drop = TRUE,
                      limits = levels(DF_plot$continent_name), 
                      values = c('#cb4d42','#377eb8','#4daf4a','#984ea3','#ff7f00','#3B7080'))


p1_to_save 

# ggsave("NOT_GITHUB/DEV/LATAM/LATAM_plot1.png", p1_to_save, width = 16, height = 9, dpi = 300)

