library("shiny")
library("ggplot2")
library("dplyr")
library("tidyr")
library("plotly")
#wrangle data
forest_data <- read.csv("data/WBI_Forest_Area_Cleaned.csv", stringsAsFactors = FALSE)
forest_table <- filter(forest_data, Series.Code == "AG.LND.AGRI.ZS" | Series.Code == "AG.LND.FRST.ZS") %>% 
  gather(key = year, value = value, YR1992 : YR2016) %>% 
  mutate(year = substr(year, 3, 6)) %>% 
  select(Series.Name, Country.Name, year, value) %>% 
  group_by(Series.Name) 

#create shiny function
shinyServer(function(input, output) {
    #create output by rendering data table based on input
    output$table <- renderDataTable({
      data <- forest_table %>%
        filter(Country.Name == (input$country1))

      data <- data %>%
        filter(Series.Name == input$category1)

      data
      })
    #create output by rendering plot based on input
    output$plot <- renderPlotly({
      new_forest_table <- forest_table %>% 
        filter(Country.Name == input$country2 & Series.Name == input$category2) 
      ggplot(data = new_forest_table) +
        geom_point(mapping = aes(x = year, y = value), color = "blue", na.rm = T) +
                   labs(x = "Time (years)", y = input$category2, title = paste0(input$category2, " in ", input$country2))
    }
        )
    #create text for table by rendering text based on input
     output$table_message <- renderText({
       paste0("This table shows the data regarding ", input$category1, " in countries around the world in ",
                             input$country1, ". This information shows the comparison between agricultural land and forested land and how
                             these values have changed in countries over the span of 1992 to 2016.")
     })
     #create text for plot by rendering text based on input
    output$graph_message <- renderText({
    message_str2 <- paste0("This scatter plot shows the data regarding ", input$category2, " in countries around the world in ",
                            input$country2, ". This information shows the change in agricultural land or forested land in countries
                            over the span of 1992 to 2016.")
    })
     
}    )

