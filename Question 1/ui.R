library("shiny")
library("dplyr")
library("plotly")
library("tidyr")
#wrangle data
forest_data <- read.csv("data/WBI_Forest_Area_Cleaned.csv", stringsAsFactors = FALSE)
forest_table <- filter(forest_data, Series.Code == "AG.LND.AGRI.ZS" | Series.Code == "AG.LND.FRST.ZS") %>% 
  gather(key = year, value = value, YR1992 : YR2016) %>% 
  mutate(year = substr(year, 3, 6)) %>% 
  select(Series.Name, Country.Name, year, value) %>% 
  group_by(Series.Name)

#create page
shinyUI(navbarPage(
  #application title
  "Deforestation",
  tabPanel(
    "Table", 
    sidebarLayout(
      #sidebar with two widgets. Select input for country.
      sidebarPanel(
        #selectInput for choosing country.
        selectInput("country1",
                     "Select a country:",
                      c(unique(as.character(forest_table$Country.Name)))
                     ),
        #radioButton for category
        radioButtons("category1",
                    "Select category of interest:",
                    choices = c(
                      "Agricultural land (% of land area)", "Forest area (% of land area)")
                    )
      ),
      #create mainPanel with data table and caption
      mainPanel(
        dataTableOutput(outputId = "table"),
        textOutput(outputId = "table_message")
        )
      )
   ),
  
 #create another sidebar.
 tabPanel(
       "Plot",
       sidebarLayout(
         #sidebar with two widgets. Select input for country.
         sidebarPanel(
           #selectInput for country.
           selectInput("country2",
                       "Select a country:",
                       c(unique(as.character(forest_table$Country.Name)))
           ),
           #radioButton for category
           radioButtons("category2",
                        "Select category of interest:",
                        choices = c("Agricultural land (% of land area)", "Forest area (% of land area)")
           )
         ),
         #create mainPanel with plot and caption
         mainPanel(
           plotlyOutput("plot"),
           textOutput(outputId = "graph_message")
     
      )
   )
 )
)
)

