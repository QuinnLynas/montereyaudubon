#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#install.packages("shinythemes")
library(shiny)
library(ggplot2)
library(dplyr)

data <- bird_data |>
  select(date, count, common_name, atmp_deg_c, wdir_deg_t, wspd_mph, comp_dir, pres_h_pa, atmp_pres_bin, wtmp_deg_c, count2, day.time, b_windscale)

plotdata <- plots

# Define UI 
ui <- fluidPage(
  title = "Interactive ggplot",
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dateRange", "Select Date Range:",
                     start = min(data$date), end = max(data$date)),
      selectInput("species", "Select Species:",
                  choices = unique(data$common_name), multiple = TRUE, selected = unique(data$common_name)),
      selectInput("plot", "Select Plot:",
                  choices = c("Polar Degrees", "Polar Compass Point", "Polar Compass Bar", "Histogram", "Heatmap", "Time Series", "Time Series Line"), multiple = FALSE, selected = c("Polar Degrees"))
    ),
    mainPanel(
      plotOutput("plot"),
      textOutput("plot_description")
    )
  )
)

# Define server logic 
server <- function(input, output) {
  filteredData <- reactive({
    data |>
      filter(date >= input$dateRange[1], date <= input$dateRange[2]) |>
      filter(common_name %in% input$species)
             
  })
  
  output$plot_description <- renderText({
    req(input$dateRange[1], input$dateRange[2], input$species, input$plot)
    
    start_date <-input$dateRange[1]
    end_date <- input$dateRange[2]
    species <- input$species
    plot_type = input$plot
    
    if(input$plot == "Polar Degrees"){
      glue::glue("Displaying a {plot_type} graph. Note that the further out a point is from the center,
                 the higher the count is for an observation. Additionally, the color of each point represents the wind speed in miles per hour for each observation. Consider looking at specific species (i.e. select Pacific Loon only)
                 so that you can see relationships within each species, or selecting a specific date range (choosing a specific year is a good idea). If you want to look at differences between species,
                  consider looking at the heatmap, histogram, or time series visualizations.")
    }else if(input$plot == "Polar Compass Point"){
      glue::glue("Displaying a {plot_type} graph. Note that this graph is similar to the polar degrees graph, the only difference is that wind directions 
                 are binned into compass directions. Additionally, atmospheric pressure is mapped to the color instead of wind speed. The further a point is from the center, the higher the count of selected bird species 
                 is for that observation. It is recommended that you select one species, and one year to see any meaningful relationship. If you want to look at differences between species,
                  consider looking at the heatmap, histogram, or time series visualizations.")
    }else if(input$plot == "Polar Compass Bar"){
      glue::glue("Displaying a {plot_type} graph. Note that this graph is similar to the polar compass point graph, the difference is that this plot uses bars instead of points to quantify bird count. 
                  Additionally, atmospheric pressure is mapped to the color instead of wind speed. The further a bar is from the center, the higher the count of selected bird species 
                 is for that observation. It is recommended that you select one species, and one year to see any meaningful relationship. If you want to look at differences between species,
                  consider looking at the heatmap, histogram, or time series visualizations.")
    }else if(input$plot == "Histogram"){
      glue::glue("Displaying a {plot_type} graph. This graph shows what air temperatures have the most amount of birds observed. We can see there a differences between groups 
                 particularly between Pacific Loon and the other species. It is recommended to play around with the dates to see if the relationship between count and temperature changes through different 
                 years.")
    }else if(input$plot == "Heatmap"){
      glue::glue("Displaying a {plot_type} graph. This graph shows how water temperature and count concentration are related. 
                 You can see differences between species, as well as differences within species. It is recommended to play around with the dates to see how water temperature and count 
                 change year by year. ")
    }else if(input$plot == "Time Series"){
      glue::glue("Displaying a {plot_type} graph. This graph looks at the change through time for count and wind speed. In order to get any 
                 valuable information, it is recommended to look at small windows of time. 1-3 days is a good window to start with. Anything more than a week makes any relationship hard to see.")
    }else if(input$plot == "Time Series Line"){
      glue::glue("Displaying a {plot_type} graph. This graph looks at the change through time for count and wind speed. In order to get any 
                 valuable information, it is recommended to look at small windows of time, particularly 1 day at a time. Anything more will make it difficult to see any trends.")
    }
  })
  
  output$plot <- renderPlot({
    if(input$plot == "Polar Degrees"){
      filteredData() |>
        filter(!is.na(wdir_deg_t)) |>
        ggplot() +
        aes(x = wdir_deg_t, y = count, colour = wspd_mph, shape = common_name) +
        geom_jitter(position = position_jitter(height = 0.3, width = 0.2)) +
        scale_color_viridis_c(option = "viridis", direction = 1) +
        theme_minimal() + 
        coord_polar() + 
        scale_y_continuous(trans = "sqrt") + 
        scale_x_continuous(n.breaks = 12, limits = c(0, 360)) + 
        labs(x = "Wind Direction", y = "Count", color = "Wind Speed MPH", title = "Wind direction and Bird count", shape = "Species") +
        theme(legend.position = "right")

    }else if(input$plot == "Polar Compass Point") {
      
      filteredData() |>
        filter(!is.na(comp_dir)) |>
        ggplot() +
        aes(x = comp_dir, y = count, colour = pres_h_pa, shape = common_name) +
        geom_jitter(position = position_jitter(height = 0.3, width = 0.2)) +
        scale_color_viridis_c(option = "viridis", direction = 1) +
        theme_classic() + 
        scale_y_continuous(trans = "sqrt") +
        coord_polar(start = -0.18) + 
        guides(color = guide_colorbar(order = 0), shape = guide_legend(order = 1)) +
        labs(x = "Wind Direction", y = "Count", color = "Atmosperic Pressure", title = "Wind direction and Bird count", shape = "Species") + 
        theme(legend.position = "right") 
      
    }else if(input$plot == "Polar Compass Bar") {
      
      filteredData() |> 
        filter(!is.na(comp_dir)) |>
        filter(!is.na(atmp_pres_bin)) |>
        ggplot() +
        aes(x = comp_dir, y = count, fill = atmp_pres_bin) +
        geom_col() +
        scale_fill_viridis_d(option = "viridis", direction = 1) +
        theme_classic() + 
        coord_polar(start = -0.18) + 
        scale_y_continuous() + 
        labs(x = "Wind Direction", y = "Count", fill = "Atmosperic Pressure", title = "Wind direction and Bird count") + 
        theme(legend.position = "right") 
      
    }else if(input$plot == "Histogram") {
      
      filteredData() |>
        filter(!is.na(atmp_deg_c)) |>
        ggplot() + 
        geom_col(aes(x = cut( (atmp_deg_c * 9 / 5 + 32), breaks = 9, labels = c("45-47", "47-49", "49-51", "51-53", "53-55", "55-57", "57-59", "59-61", "61-63")), y = count, fill = common_name), position = "stack") + 
        scale_fill_viridis_d() + 
        theme_classic() + 
        labs(x = "Air Temperature in Degrees Fahrenheit", y = "Count", fill = "Species", title = "Count and Air Temperature for each species") + 
        theme(legend.position = "top")
      
    }else if(input$plot == "Heatmap") {
      
      filteredData() |>
        filter(wtmp_deg_c < 998) |>
        ggplot(aes(x = factor(cut((wtmp_deg_c * 9 / 5 + 32), , labels = c("52-54", "54-56", "56-58", "58-60", "60-62"), breaks = 5)),
                   y = common_name, fill = count2)) + 
        geom_tile() + 
        scale_fill_viridis_c(begin = 0.2, trans = "log") + 
        theme_classic() + 
        labs(y = "", 
             x = "Water Temperature in Fahrenheit", 
             fill = "Scaled Count",
             title = "Count Concentration for each species at a specific water temperature")
    }else if(input$plot == "Time Series") {
      plot1 <- filteredData() |>
        ggplot() +
        geom_smooth(aes(x = day.time, y = count, color = common_name), alpha = 0) +
        scale_y_log10(sec.axis = sec_axis(~sqrt(.)/2, name = "Wind Speed")) +
        geom_smooth(aes(x = day.time, y = 2 * (wspd_mph)^2, color = "Wind speed"), linetype = 2, alpha = 0) +
        theme_minimal() + 
        labs(x = "Date and Time",
             y = "Bird Count",
             color = "") + 
        theme(legend.title = element_blank()) +
        scale_color_viridis_d()

      plot1
    }else if(input$plot == "Time Series Line"){
      filteredData() |>
        ggplot() + 
        geom_line(aes(x = day.time, y = count, color = common_name)) + 
        scale_y_log10(sec.axis = sec_axis(~sqrt(.)/2 , name = "Wind Speed")) + 
        geom_line(aes(x = day.time, y =  2 * wspd_mph^2, color = "Wind speed"), linetype = 2) +
        theme_minimal() + 
        scale_color_viridis_d(end = 0.8) + 
        labs(x = "Day and Time", y = "Count", color = "")
    }
  })

}



# Run the application 
shinyApp(ui = ui, server = server)
