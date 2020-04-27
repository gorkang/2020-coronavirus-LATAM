# library(tidyverse)
# source("R/data-preparation.R")

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
  
  min_n = 1
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
  
  
  accumulated_daily_pct = "daily"
  INPUT_accumulated_daily_pct = accumulated_daily_pct
  
  growth = 30
  VAR_growth = growth

  highlight = " "
  INPUT_highlight = highlight
  VAR_highlight = highlight



# Data preparation --------------------------------------------------------

  dta_temp = data_preparation(
    data_source = "JHU",
    cases_deaths = INPUT_cases_deaths,
    countries_plot = INPUT_countries_plot,
    min_n = INPUT_min_n,
    relative = INPUT_relative
  ) %>% 
    
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
    filter(source == "JHU") %>% 
    
    
    # Create name_end labels
    mutate(
      name_end =
        case_when(
          days_after_100 == max(days_after_100, na.rm = TRUE) & time == max(time, na.rm = TRUE) ~ paste0(as.character(country)),
          TRUE ~ "")) %>% 
    mutate(highlight = country)
  


 # Show accumulated or daily plot

      DF_plot = dta_temp %>% 
        rename(value_temp = value,
               value = diff) %>% 
        ungroup()  %>% 
        mutate(value_old = value) %>%
        group_by(country) %>% 
        
        # Rolling mean of last 7 days --------------
        mutate(value = map_dbl(1:n(), ~ mean(value[(max(.x - 7, 1)):.x], na.rm = FALSE))) %>%
        filter(value_temp >= 10)
      
    
  # Define which countries get a smooth (have enough points)
  VALUE_span = 3
  counts_filter = DF_plot %>% count(country) %>% filter(n > VALUE_span)
    


    # Draw plot ---------------------------------------------
    
    p_temp = ggplot(data = DF_plot, 
                    aes(x = value_temp, y = value, group = as.factor(country), color = continent_name)) +

        # Trend line
        geom_abline(intercept = -1, slope = 1, linetype = "dashed", alpha = .8, size = .5) +

        # Country points (last one bigger)
        geom_point(aes(size = .005 + as.integer(DF_plot$name_end != "" & DF_plot$name_end != "*") - .5), alpha = .1) +
        
        scale_x_log10(breaks = scales::log_breaks(n = 7), labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +

        
        labs(
          x = "Confirmed acumulated deaths (log)",
          y = paste0("Weekly average for confirmed ", accumulated_daily_pct, " ", cases_deaths, " (log)",  if (relative == TRUE) " / million people")
          ) +
        theme_minimal(base_size = 12) +
        
        theme(legend.position = "none",
              panel.grid.minor = element_line(size = 0.25),
              panel.spacing = unit(1, "lines")
              )
    
    # Smooth 
      p_temp = p_temp +  
        
        geom_line(alpha = .4) +
      
        geom_smooth(data = DF_plot %>% filter(country %in% counts_filter$country), 
                    aes(x = value_temp, y = value, group = as.factor(continent_name), color = continent_name),
                    method = "loess", span = 1.5, se = FALSE, size = 1, alpha = .9, na.rm = TRUE)


    
    # LIMITS
      MAX_y = max(dta_temp$diff, na.rm = TRUE, na.rm = TRUE) * 1.1
      MIN_y = min(dta_temp$diff, na.rm = TRUE, na.rm = TRUE) * 0.1 # In Log scale can't use 0
      if (MIN_y <= 0) MIN_y = 1
    
    if (MIN_y == 0) MIN_y = 1

      
    # Scale, log
    p_temp = p_temp +
      scale_y_log10(breaks = scales::log_breaks(n = 10), labels = function(x) format(x, big.mark = ",", scientific = FALSE), limits = c(MIN_y, MAX_y)) 
    
    p2_to_save <- p_temp + 
        
        # Country label
        ggrepel::geom_label_repel(aes(label = name_end), show.legend = FALSE, segment.color = "grey", segment.size  = .15, alpha = .75, size = 3) +
        facet_grid( ~ continent_name) +#, scales = "free_x"
        theme(aspect.ratio = 1) +
        scale_colour_manual(drop = TRUE,
                            limits = levels(DF_plot$continent_name), 
                            values = c('#cb4d42','#377eb8','#4daf4a','#984ea3','#ff7f00','#3B7080'))
    
    
    p2_to_save 
    
    # ggsave("NOT_GITHUB/DEV/LATAM/LATAM_plot2.png", p2_to_save, width = 16, height = 9, dpi = 300)
    