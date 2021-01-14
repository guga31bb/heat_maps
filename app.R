source('functions.R')

# if we make any figures
shinyOptions(cache = diskCache("./cache"), size = 50e6)

#####################################################################################
######## Pull in the necessary data #################################################
#####################################################################################


souce_url <- "https://raw.githubusercontent.com/ArrowheadAnalytics/next-gen-scrapy-2.0/master/pass_and_game_data.csv"

df <- read_csv(
  souce_url
  ) %>%
  # na.omit() %>%
  dplyr::rename(x_coord = x, y_coord = y) %>%
  filter(!is.na(x_coord), !is.na(y_coord), !is.na(name)) %>%
  mutate(name = if_else(name == "Cameron Newton", "Cam Newton", name)) %>%
  group_by(name) %>%
  mutate(n = n()) %>%
  filter(n > 100) %>%
  ungroup() %>%
  arrange(name)

#####################################################################################
######## App                        #################################################
#####################################################################################

ui <- fluidPage(
  
  theme = shinythemes::shinytheme("cerulean"),
  #shinythemes::themeSelector(),
  
  mainPanel(

    titlePanel("NFL QB Passing Heat Maps"),
    
    fluidRow(
      column(3, align = "center",
             h3("Credits", align = "center"),
             
             p("-- Data: Collected by Ethan Douglas (program originally created by Sarah Mallepalle), sourced from NGS"),
             p("-- Code heavily borrowed from: Thomas Mock"),
             p("-- Website: Ben Baldwin")
             ),
      column(3, align="center", 
             selectInput("name1",
                         "Player 1:",
                         c(unique(as.character(df$name))), selected = "Russell Wilson"),
             
             sliderInput("range1", "Seasons:",
                         min = 2017, max = 2020,
                         value = c(2017,2020), sep=""),
             
             sliderInput("weeks1", "Weeks:",
                         min = 1, max = 21,
                         value = c(1,21), sep=""),
             actionButton("update", "Update", width = '100%')
             
      ),
      column(3, align="center", 
             selectInput("name2",
                         "Player 2:",
                         c(unique(as.character(df$name))), selected = "Drew Brees"),
             
             sliderInput("range2", "Seasons:",
                         min = 2017, max = 2020,
                         value = c(2017,2020), sep=""),
             
             sliderInput("weeks2", "Weeks:",
                         min = 1, max = 21,
                         value = c(1,21), sep="")

      )
    ),
    
    fluidRow(
      column(12, align="center",
             {plotOutput(outputId = "plot1", width = "100%", height = "600px") %>%
                 shinycssloaders::withSpinner(type = 6,# types see https://projects.lukehaas.me/css-loaders/
                                              color = "#414141",
                                              color.background = "#FFFFFF")
             }
      )
    ), 
    
    
    fluidRow(
      column(12, align="center",
             {plotOutput(outputId = "plot2", width = "100%", height = "700px") %>%
                 shinycssloaders::withSpinner(type = 6,# types see https://projects.lukehaas.me/css-loaders/
                                              color = "#414141",
                                              color.background = "#FFFFFF")
             }
      )
    )
    
    
  )
)


server <- function(input, output, session) {
  
  # testing
# input <- NULL
# input$name1 <- "Russell Wilson"
# input$name2 <- "Drew Brees"
# input$range1 <- c(2017, 2019)
# input$range2 <- c(2017, 2019)
# input$weeks1 <- c(1, 17)
# input$weeks2 <- c(1, 17)

  #data
  fullInput <- eventReactive(
    input$update, {

      bind_rows(
        df %>%
          filter(
            name == input$name1,
            week >= input$weeks1[1] & week <= input$weeks1[2],
            season >= input$range1[1] & season <= input$range1[2]
          ) %>%
          mutate(sample = 1),
        df %>%
          filter(
            name == input$name2,
            week >= input$weeks2[1] & week <= input$weeks2[2],
            season >= input$range2[1] & season <= input$range2[2]
          ) %>%
          mutate(sample = 2)
      ) %>%
        mutate(
          name = if_else(
            input$name1 == input$name2 & sample == 2,
            glue::glue("{input$name2} 2"),
            name
          ),
          name = factor(name, levels = 
                    c(
                      dplyr::first(name), 
                      dplyr::last(name)
                      )
          )) %>%
        select(name, sample, x_coord, y_coord)
      
    }, ignoreNULL = FALSE
  )
  
  
  #data
  comparisonInput <- eventReactive(
    input$update, {
      
      qb_density_compare(fullInput(), n = 50)
      
    }, ignoreNULL = FALSE
  )
  
  

  output$plot1 <- renderCachedPlot(
    expr = maps(fullInput(), input), 
    res = 150,
    cacheKeyExpr = { list(fullInput()) }
  )
  
  
  output$plot2 <- renderCachedPlot(
    expr = compare(comparisonInput()), 
    res = 150,
    cacheKeyExpr = { list(fullInput()) }
  )
  
  
}


# Create Shiny app ----

shinyApp(ui, server)

