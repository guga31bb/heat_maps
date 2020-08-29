source('functions.R')

# if we make any figures
shinyOptions(cache = diskCache("./cache"), size = 50e6)

#####################################################################################
######## Pull in the necessary data #################################################
#####################################################################################

souce_url <- "https://raw.githubusercontent.com/ArrowheadAnalytics/next-gen-scrapy-2.0/master/pass_and_game_data.csv"

df <- read_csv(souce_url) %>%
  na.omit() %>%
  select(-X1) %>%
  group_by(name) %>%
  mutate(n = n()) %>%
  filter(n > 100) %>%
  ungroup()

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
                         min = 2017, max = 2019,
                         value = c(2017,2019), sep=""),
             
             sliderInput("weeks1", "Weeks:",
                         min = 1, max = 17,
                         value = c(1,17), sep=""),
             actionButton("update", "Update", width = '100%')
             
      ),
      column(3, align="center", 
             selectInput("name2",
                         "Player 2:",
                         c(unique(as.character(df$name))), selected = "Drew Brees"),
             
             sliderInput("range2", "Seasons:",
                         min = 2017, max = 2019,
                         value = c(2017,2019), sep=""),
             
             sliderInput("weeks2", "Weeks:",
                         min = 1, max = 17,
                         value = c(1,17), sep="")

      )
    ),
    
    fluidRow(
      column(12, align="center",
             {plotOutput(outputId = "plot1", height = 'auto') %>%
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
# input$name1 <- "Matthew Ryan"
# input$name2 <- "Aaron Rodgers"
# input$range1 <- c(2019, 2019)
# input$range2 <- c(2019, 2019)
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
          ),
        df %>%
          filter(
            name == input$name2,
            week >= input$weeks2[1] & week <= input$weeks2[2],
            season >= input$range2[1] & season <= input$range2[2]
          )
      ) %>%
        mutate(name = factor(name, levels = c(input$name1, input$name2))) %>%
        select(name, x_coord, y_coord)
      
    }, ignoreNULL = FALSE
  )
  
  
  # comparison plot
  output$plot1 <- renderPlot({
    maps(fullInput())
  }, height = function() {
    (9/16) * session$clientData$output_plot1_width
  })
  
  
}


# Create Shiny app ----

shinyApp(ui, server)

