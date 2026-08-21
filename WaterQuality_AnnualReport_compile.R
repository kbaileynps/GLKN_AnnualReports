# This is the file that runs all the data wrangling for the
# WaterQuality_AnnualReport.Rmd. If the Rmd does not knit, this is
# where the error will come from.

# Note: Run the wqp_retrival.R document in the GLKN Visualizer project to
# get data for this report. 

# Libraries ----
library(readr) # tidyverse data import
library(dplyr) # data wrangling
library(ggplot2) # plotting
# library(knitr) # for kable
# library(kableExtra) # custom kable features
library(leaflet) # for mapping
library(gt) # for table
library(tidyr) # for pivoting

# Start of Source Code ----
## Loading data ----

## water quality data (from wqp_retrival.r in Data Visualizer)
wqp_data1 <- read_csv(paste0("./data/wqp_glkn_",
                             format(Sys.Date(), "%Y"),
                             ".csv"))
# or 
# wqp_data1 <- read_csv(paste0("./data/wqp_glkn_2026.csv"))

# filtering based on current sampling year 
wqp_data2 <- wqp_data1 |> 
  filter(any(year == 2024),
         .by = MonitoringLocationName)
  # filter(any(year == format(Sys.Date(), "%Y")),
         # .by = MonitoringLocationIdentifier)

## Data wrangling ----
wqp_data <- wqp_data2 |> 
  # renaming columns for ease
  dplyr::rename(start_date = ActivityStartDate,
                end_date = ActivityEndDate,
                depth = ActivityDepthHeightMeasure.MeasureValue,
                depth_unit = ActivityDepthHeightMeasure.MeasureUnitCode,
                value = ResultMeasureValue,
                value_unit = ResultMeasure.MeasureUnitCode,
                lat = LatitudeMeasure,
                lon = LongitudeMeasure) |>
  # adding month and period (for historic vs current)
  dplyr::mutate(month_name = lubridate::month(end_date,
                                              label = TRUE,
                                              abbr = FALSE),
                period = case_when(year == 2024 ~ "Current",
                                   year < 2024 ~ "Historic",
                                   TRUE ~ NA))
                # period = case_when(year == format(Sys.Date(), "%Y") ~ "Current",
                #                    year < format(Sys.Date(), "%Y") ~ "Historic"))
  
## park types 
park_type <- c("ISRO" = "lake",
               "PIRO" = "lake",
               "SACN" = "river",
               "SLBE" = "lake",
               "VOYA" = "lake")

# Parameters based on park type:
river_param <- c("Phosphorus", "Nitrate + Nitrite",
                 "Chlorophyll a, free of pheophytin",
                 "Total suspended solids", "Calcium",
                 "Chloride") 

lake_param <- c("Water level", "Dissolved oxygen (DO)",
                "Phosphorus", "Depth, Secchi disk depth",
                "Calcium")

## Data prep functions ----
### map data 
prep_map <- function(df){
  df |> 
    # selecting lat/lon
    select(MonitoringLocationName,
           lat,
           lon) |>
    # getting distinct values 
    distinct()
}

### boxplot data 
prep_boxplot <- function(df, park_code, type){
  
  # setting type 
  type <- type[[park_code]]
  
  # filtering based on type
  parameters <- if(identical(type, "river")){
    df |> 
      dplyr::filter(CharacteristicName %in% river_param)
  } else{
    df |> 
      dplyr::filter(CharacteristicName %in% lake_param)
  }
}

### table data 
prep_table <- function(df, park_code, type){
  # setting type 
  type <- type[[park_code]]
  
  # filtering based on type
  parameters <- if(identical(type, "river")){
    df |> 
      dplyr::filter(CharacteristicName %in% river_param)|> 
      filter(!is.na(value)) |> 
      summarise(med = median(value, na.rm = TRUE),
                stan_dev = sd(value, na.rm = TRUE),
                min_val = min(value, na.rm = TRUE),
                max_val = max(value, na.rm = TRUE),
                .by = c("CharacteristicName",
                        "period")) |> 
      select(CharacteristicName,
             med,
             stan_dev,
             min_val,
             max_val,
             period) |>  
      pivot_wider(names_from = period,
                  values_from = c(med, 
                                  stan_dev,
                                  min_val,
                                  max_val))
  } else{
    df |> 
      dplyr::filter(CharacteristicName %in% lake_param)|> 
      filter(!is.na(value)) |> 
      summarise(med = median(value, na.rm = TRUE),
                stan_dev = sd(value, na.rm = TRUE),
                min_val = min(value, na.rm = TRUE),
                max_val = max(value, na.rm = TRUE),
                .by = c("CharacteristicName",
                        "period")) |> 
      select(CharacteristicName,
             med,
             stan_dev,
             min_val,
             max_val,
             period) |>  
      pivot_wider(names_from = period,
                  values_from = c(med, 
                                  stan_dev,
                                  min_val,
                                  max_val))
  }
}

# ### timeseries data
# prep_timeseries <- function(df, park){
#   if(park_type == "river"){
#     df |>
#       # filtering parameters
#       filter(CharaeristName %in% river_param) |>
#       # filtering depth
#       dplyr::filter(depth >= -2 | is.na(depth)) |>
#       # summarizing based on number of values
#       dplyr::summarise(value = case_when(n() == 1 ~ value[1], # needed with duplicate values
#                                          n() == 2 ~ mean(value, na.rm = TRUE),
#                                          n() >= 3 ~ median(value, na.rm = TRUE)),
#                        .by = c(Park,
#                                MonitoringLocationName,
#                                CharacteristicName,
#                                end_date,
#                                AxisName,
#                                lat,
#                                lon,
#                                value_unit,
#                                PickListName,
#                                AxisName,
#                                LowerPoint,
#                                UpperPoint,
#                                ResultDetectionConditionText)) |>
#       dplyr::arrange(Park,
#                      MonitoringLocationName,
#                      end_date,
#                      CharacteristicName)
# 
#   } else{
#     df |>
#       # filtering park
#       filter(ParkCode == park) |>
#       # filtering parameters
#       filter(CharaeristName %in% lake_param) |>
#       # filtering depth
#       dplyr::filter(depth >= -2 | is.na(depth)) |>
#       # summarizing based on number of values
#       dplyr::summarise(value = case_when(n() == 1 ~ value[1], # needed with duplicate values
#                                          n() == 2 ~ mean(value, na.rm = TRUE),
#                                          n() >= 3 ~ median(value, na.rm = TRUE)),
#                        .by = c(Park,
#                                MonitoringLocationName,
#                                CharacteristicName,
#                                end_date,
#                                AxisName,
#                                lat,
#                                lon,
#                                value_unit,
#                                PickListName,
#                                AxisName,
#                                LowerPoint,
#                                UpperPoint,
#                                ResultDetectionConditionText)) |>
#       dplyr::arrange(Park,
#                      MonitoringLocationName,
#                      end_date,
#                      CharacteristicName)
#   }
# }
# 
# ### depth profile data
# prep_depthprofile <- function(df, park){
#   if(park_type == "river"){
#     return(NULL) # no depth data 
#   } else {
#     df |> 
#       # filtering depth parameters
#       dplyr::filter(CharacteristicName %in% c("Dissolved oxygen (DO)",
#                                               "pH",
#                                               "Specific conductance",
#                                               "Temperature, water")) |> 
#       dplyr::arrange(MonitoringLocationName,
#                      end_date,
#                      depth)
#   }
# }





## Plot functions ----

### map 
plot_map <- function(df){
  leaflet(df) |> 
    addTiles() |> 
    addCircleMarkers(lng = ~lon,
                     lat = ~lat,
                     radius = 5,
                     color = "blue",
                     popup = ~MonitoringLocationName)
}

### box plot 
plot_boxplot <- function(df){
  ggplot() + 
    geom_boxplot(data = df,
                 aes(x = period,
                     y = value,
                     fill = period)) + 
    facet_wrap(~AxisName,
               scales = "free",
               ncol = 3) +
    labs(fill = "Period",
         x = "Period",
         y = "Value")+
    theme_bw()
}

### table 
create_table <- function(df){
  df |> 
    gt() |> 
    tab_spanner(label = html("<strong>Historic</strong>"),
                columns = c(min_val_Historic,
                            med_Historic,
                            max_val_Historic,
                            stan_dev_Historic)) |> 
    tab_spanner(label = html("<strong>Present</strong>"),
                columns = c(min_val_Current,
                            med_Current,
                            max_val_Current,
                            stan_dev_Current)) |> 
    cols_label(CharacteristicName = html("<strong>Parameter</strong>"),
               min_val_Historic = html("<strong>Minimum</strong>"),
               med_Historic = html("<strong>Median</strong>"),
               max_val_Historic = html("<strong>Maximum</strong>"),
               stan_dev_Historic = html("<strong>SD</strong>"),
               min_val_Current = html("<strong>Minimum</strong>"),
               med_Current = html("<strong>Median</strong>"),
               max_val_Current = html("<strong>Maximum</strong>"),
               stan_dev_Current = html("<strong>SD</strong>"))
}

# ### timeseries
# plot_timeseries <- function(df, park_type){
#   if(park_type == "river"){
#     ggplot(data = df,
#            aes(x = date,
#                y = value)) + 
#       geom_line() +
#       geom_point() +
#       theme_minimal()
#   } else{
#     ggplot(data = df,
#          aes(x = date,
#              y = value)) + 
#     geom_line() +
#     geom_point() +
#     theme_minimal()
#   }
# }
# 
# ### depth profile
# plot_depthprofile <- function(df){
#   ggplot(data = df,
#          aes(x = value,
#              y = depth)) +
#     geom_path() +
#     geom_point() +
#     theme_minimal()
# }



