# Libraries ----
library(readr) # tidyverse data import
library(dplyr) # data wrangling
library(ggplot2) # plotting
library(knitr) # for kable
library(kableExtra) # custom kable features

# Loading data ----
# water quality portal data
wqp_data1 <- read_csv("./data/wqp_glkn.csv")

# filtering >2m depth data
wqp_data <- wqp_data1 |> 
  dplyr::mutate(start_date = as.Date(ActivityStartDate),
                end_date = as.Date(ActivityEndDate),
                month_name = lubridate::month(end_date,
                                              label = TRUE,
                                              abbr = FALSE)) |>
  dplyr::rename(depth = ActivityDepthHeightMeasure.MeasureValue,
                depth_unit = ActivityDepthHeightMeasure.MeasureUnitCode,
                value = ResultMeasureValue,
                value_unit = ResultMeasure.MeasureUnitCode,
                lat = LatitudeMeasure,
                lon = LongitudeMeasure) |> 
  dplyr::select(-ActivityStartDate,
                -ActivityEndDate)

## park types 
park_type <- c("APIS" = "lake",
               "INDU" = "lake",
               "ISRO" = "lake",
               "PIRO" = "lake",
               "SACN" = "river",
               "SLBE" = "lake",
               "VOYA" = "lake")

## Data prep functions ----
### timeseries data
prep_timeseries <- function(df, park){
  if(park_type == "river"){
    df |> 
      # filtering parameters 
      filter(!CharaeristName %in% c("Depth, bottom",
                                    "Depth, Secchi disk depth",
                                    "Dissolved oxygen (DO)",
                                    "Water Level")) |> 
      dplyr::filter(depth >= -2 | is.na(depth)) |> 
      dplyr::summarise(value = case_when(n() == 1 ~ value[1], # needed with duplicate values 
                                         n() == 2 ~ mean(value, na.rm = TRUE),
                                         n() >= 3 ~ median(value, na.rm = TRUE)),
                       .by = c(Park,
                               MonitoringLocationName,
                               CharacteristicName,
                               end_date,
                               AxisName,
                               lat,
                               lon,
                               value_unit,
                               PickListName,
                               AxisName,
                               LowerPoint,
                               UpperPoint,
                               ResultDetectionConditionText)) |>
      dplyr::arrange(Park,
                     MonitoringLocationName,
                     end_date,
                     CharacteristicName)
      
  } else{
    df |> 
      # filtering parameters 
      filter(!CharaeristName %in% c("Depth, bottom",
                                    "Depth, Secchi disk depth",
                                    "Dissolved oxygen (DO)",
                                    "Total suspended solids")) |> 
      dplyr::filter(depth >= -2 | is.na(depth)) |> 
      dplyr::summarise(value = case_when(n() == 1 ~ value[1], # needed with duplicate values 
                                         n() == 2 ~ mean(value, na.rm = TRUE),
                                         n() >= 3 ~ median(value, na.rm = TRUE)),
                       .by = c(Park,
                               MonitoringLocationName,
                               CharacteristicName,
                               end_date,
                               AxisName,
                               lat,
                               lon,
                               value_unit,
                               PickListName,
                               AxisName,
                               LowerPoint,
                               UpperPoint,
                               ResultDetectionConditionText)) |>
      dplyr::arrange(Park,
                     MonitoringLocationName,
                     end_date,
                     CharacteristicName)
  }
}

### depth profile data
prep_depthprofile <- function(df, park_type){
  if(park_type != "lake"){
    return(NULL) # no depth data 
  } else {
    df |> 
      dplyr::filter(CharacteristicName %in% c("Dissolved oxygen saturation",
                                              "pH",
                                              "Specific conductance",
                                              "Temperature, water")) |> 
      dplyr::arrange(MonitoringLocationName,
                     end_date,
                     depth)
  }
}

### boxplot data 
prep_boxplot <- function(df){
  df
}

### table data 
prep_table <- function(df){
  df 
}

## Plot functions ----

### timeseries
plot_timeseries <- function(df, park_type){
  if(park_type == "river"){
    ggplot(data = df,
           aes(x = date,
               y = value)) + 
      geom_line() +
      geom_point() +
      theme_minimal()
  } else{
    ggplot(data = df,
         aes(x = date,
             y = value)) + 
    geom_line() +
    geom_point() +
    theme_minimal()
  }
}

### depth profile
plot_depthprofile <- function(df){
  ggplot(data = df,
         aes(x = value,
             y = depth)) +
    geom_path() +
    geom_point() +
    theme_minimal()
}

### box plot 
plot_boxplot <- function(df){
  ggplot(data = df,
         aes(x = date,
             y = value))
}

### table 
create_table <- function(df){
  DT::datatable(exceedance_values,
                extension = c("Buttons",
                              "KeyTable"),
                options = list(dom = "Bfrtip",
                               autoWidth = F,
                               buttons = c("copy",
                                           "csv",
                                           "excel"),
                               keys = TRUE),
                class = "stripe hover order-column cell-border compact",
                rownames = FALSE,
                filter = "top")
}