#Author: Mauricio Moreno, Middlebury 2023

library(tidyverse)
library(ggplot2)
library(gganimate)
library(gapminder)
library(gifski)
library(leaflet)
library(geojsonio)
library(rjson)
#library(shiny)
#library(plotly)
#library(ggiraph)
library(ggthemes)
#library(htmltools)
library(directlabels)
library(data.table)

#load in birth data
world.data1 <- fread("children-born-per-woman.csv")

#remove unnecessary columns
world.data <- world.data1[,-c(2)]
names(world.data)[1] <- "Country"
names(world.data)[2] <- "Year"
names(world.data)[3] <- "Births"

#Let's read in our geojson file
world.borders <- geojson_read("countries.geojson",
                              what = "sp")

names(world.borders)[1] <- "Country"

world.borders$Country <- str_replace_all(world.borders$Country,
                                         "United States of America",
                                         "United States") 

#merging border and birth data
require(sp)
joined.data1 <- merge(world.borders, world.data, by = "Country", duplicateGeoms = TRUE)

world.births <- world.data

#Filtering only for years that are available in other data set
world.births <- world.births[world.births$Year %in% c("1952", "1957", "1962",
                                                      "1967", "1972", "1977",
                                                      "1982", "1987", "1992",
                                                      "1997", "2002", "2007")]

#let's bring in the gapminder world pop data
world.pop <- gapminder

#dropping unneeded column
world.pop <- select(world.pop, -2)

#rename column to prep for merging
names(world.pop)[2] <- "Year"
names(world.pop)[1] <- "Country"

#merging birth data with pop data
world.pop.birth <- merge(x = world.pop, y = world.births, by = c("Country", "Year"), all.x = TRUE, no.dups = TRUE)

server <- function(input, output) {
  
  output$Plot1 <- renderPlot({
    
    req(input$countries)
    
    chart = ggplot(world.data %>% filter(Country == input$countries), aes(x = Year, y = Births, color=Country)) +
      #geom_point(size=2) +
      geom_line(alpha=0.8) +
      geom_dl(aes(label = Country), 
              method = list(dl.trans(x = x + 0.1), "last.points", cex = 0.7)) +
      geom_dl(aes(label = Country), 
              method = list(dl.trans(x = x - 0.1), "first.points", cex = 0.7)) +
      theme_solarized_2(light=F)+
      labs(title = "Changes in Average Gross Births per Female",
           caption = "Source: The UN | Gap Minder, 2021") +
      theme(text=element_text(colour="#EEEEEE"),
            title=element_text(colour="#EEEEEE",size=8,face = "bold"),
            plot.title=element_text(hjust=0.5),
            axis.title.x = element_blank(),
            panel.grid.minor.x=element_blank(),
            legend.background = element_blank(),
            legend.key= element_blank(),
            legend.position=c(0.10, 0.10),# legend at top-left, inside the plot
            plot.margin = unit(c(0.5,1.3,0.5,0.5), "cm"))# +
      #transition_reveal(Year)+
      #view_follow(fixed_y=T)
    #anim_save("output.gif", animate(chart, height=420, width=700, fps = 15, duration = 3, res=100, rewind=F, renderer = gifski_renderer(loop = FALSE)))
    #list(src = "output.gif", contentType = "image/gif")
    
    chart
    
  }#, deleteFile = TRUE
  )
  
  output$heatmap <- renderLeaflet({
    #birth.year.data <- world.data %>%
    #  filter(Year == input$year)
    require(sp)
    joined.data <- subset(joined.data1, Year == input$year)
    
    require(sp)
    #joined.data <- merge(world.borders, birth.year.data, by = "Country", all.x = FALSE)
    
    #class(joined.data)
    
    bins1 <- c(0, 1, 2, 3, 4, 5, 6, 7, Inf)
    colors1 <- colorBin(palette = "YlOrRd",
                        bins = bins1,
                        domain = joined.data$Births)
    
    part1 <- paste("Females in ", 
                    joined.data$Country)
    part2 <- paste(" averaged ",
                    joined.data$Births,
                    " births ")
    part3 <- paste("in ",
                    joined.data$Year,
                    ".")
    
    country_popup <- paste(part1, 
                           part2, 
                           part3,
                           sep="\n")
                
    joined.data %>%
      leaflet() %>%
      addTiles() %>%
      addPolygons(fillColor = colors1(joined.data$Births),
                  layerId = joined.data$Births,
                  #popup = country_popup,
                  label = country_popup,
                  labelOptions = labelOptions(
                                  style = list("font-weight" = "normal", padding = "3px 8px"),
                                  textsize = "15px",
                                  direction = "auto"),
                  weight = 2,
                  color = "white",
                  dashArray = "3",
                  opacity = 1,
                  fillOpacity = .7) %>%
      setView(0, 40, 1.49) %>%
      addLegend(pal = colors1,
                values = joined.data$Births)
  
      }
  )
  
  output$bubbleplot <- renderImage({
    bubbles = ggplot(world.pop.birth %>% 
                       filter(world.pop.birth$Country %in% c(input$country)), 
                     aes(gdpPercap, lifeExp, size = Births, color = Country)) +
      geom_point() +
      scale_x_log10() +
      theme_bw() +
      # gganimate specific bits:
      labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'Life Expectancy') +
      transition_time(Year) +
      ease_aes('linear') 
    
    anim_save("bubbles.gif", animate(bubbles, fps = 10, duration = 5, res=100, rewind=F,
                                     renderer = gifski_renderer(loop = FALSE)))
    
    list(src = "bubbles.gif", contentType = "image/gif")
    
    #animate(p, fps=8, renderer = gifski_renderer(loop = FALSE))
    
  }, deleteFile=TRUE
  
  )
  
  #create popups with mouse
observeEvent(input$heatmap_shape_click, { # update the location selectInput on map clicks
  p <- input$heatmap_shape_click
})
    


output$textIntro<-renderUI(HTML(paste("This web app is my independent and capstone project 
from my Topics in Reproductive Medicine (Biol 450) senior seminar course with Professor Catherine 
Combelles. We were tasked with creating projects of personal interest which convey information 
learned this term. In this application, I am visualizing birth rates data from gapminder along 
with fertility rate data from the World Bank. I have coded for three different ways to look this 
data, all of which can be found under the different tabs for this application. Please feel free 
to explore and tinker about!")))

output$textByCountry<-renderUI(HTML(paste("For this line chart, World Bank data was taken for 
world birth counts per female by country. I created a multiple-select input such that the user 
may select to visulaize multiple countries' trends over time. One interesting trend that I saw 
when selecting for different country inputs was that for countries with heavy war involvements, 
like the US, we see drops in birth rates during wars and spikes subsequently after their end!")))

output$textBubble<-renderUI(HTML(paste("For this map, I used the same data set but now kept additional 
information such as life expectancy and GDP per capita. Using this data, I wanted to visualize 
any relationships between changes in life expectancy and GDP per capita with birth rate changes 
over time for a country. Some trends that I observed was a general trend in life expectancy increasing 
regardless of the country, and a downward trend in birth rates (visualized by decreasing 
bubble radius). My main take-away from this chart is that greater life expectancies and higher 
GDP per capita correspond with lower birth rates.")))

output$textWorld<-renderUI(HTML(paste("This chart is a simpler heat map which demonstrates macroscopic 
global trends for birth rates for a given year. As a whole, we see that as time has continued on, 
birth rates at a global scale have begun to dwindle down.")))

}

ui <- fluidPage(
  
  mainPanel(navlistPanel(
                tabPanel("About the App",
                         br(),
                         htmlOutput("textIntro")),
                tabPanel("Birth Rates by Country", 
                         br(),
                         htmlOutput("textByCountry"),
                         selectInput(inputId = "countries",
                                     label = h3("Select a Single or Multiple Countries"),
                                     choices = unique(world.data$Country),
                                     multiple = TRUE),
                         imageOutput("Plot1", width = "175%")),
                tabPanel("Birth Rates vs Life Expectancy vs GDP/capita",
                         br(),
                         htmlOutput("textBubble"),
                         selectInput("country",
                                     label = h3 ("Select One Country"),
                                     choices = unique(world.pop.birth$Country)),
                         imageOutput("bubbleplot")),
                tabPanel("Global Heatmap",
                         br(),
                         htmlOutput("textWorld"),
                         sliderInput(inputId = "year", 
                                     label = h3("Select a Year"),
                                     min = 1800, max = 2019, step = 5, value = 2019,
                                     animate = FALSE, sep = "", width = 700),
                         leafletOutput("heatmap", width = "150%"))
  
)))


shinyApp(ui = ui, server = server)

