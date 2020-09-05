source('functions.R')

# if we make any figures
shinyOptions(cache = diskCache("./cache"), size = 50e6)

#####################################################################################
######## Pull in the necessary data #################################################
#####################################################################################


spec_csv(souce_url)
  

souce_url <- "https://raw.githubusercontent.com/ArrowheadAnalytics/next-gen-scrapy-2.0/master/pass_and_game_data.csv"

df <- read_csv(
  souce_url,
  col_types = cols(
    game_id = col_double(),
    name = col_character(),
    pass_type = col_character(),
    team = col_character(),
    week = col_character(),
    x_coord = col_double(),
    y_coord = col_double(),
    type = col_character(),
    home_team = col_character(),
    away_team = col_character(),
    season = col_double(),
    game_url = col_character(),
    home_score = col_double(),
    away_score = col_double()
  )
) %>%
  na.omit() %>%
  select(-X1) %>%
  mutate(
    week = case_when(
      week == "wild-card" ~ "18",
      week == "divisional" ~ "19",
      week == "conference" ~ "20",
      week == "super-bowl" ~ "21",
      TRUE ~ week
    ),
    week = as.integer(week)
  ) %>%
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
                         min = 2017, max = 2019,
                         value = c(2017,2019), sep=""),
             
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
                         min = 2017, max = 2019,
                         value = c(2017,2019), sep=""),
             
             sliderInput("weeks2", "Weeks:",
                         min = 1, max = 21,
                         value = c(1,21), sep="")

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
        select(name, x_coord, y_coord)
      
    }, ignoreNULL = FALSE
  )
  
  
  # comparison plot
  output$plot1 <- renderPlot({
    maps(fullInput(), input)
  }, height = function() {
    (9/16) * session$clientData$output_plot1_width
  })
  
  
}


# Create Shiny app ----

shinyApp(ui, server)

