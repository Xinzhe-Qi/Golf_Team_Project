# Define UI

ui <- fluidPage(
  tags$head(
    tags$link(rel = "shortcut icon", type = "image/png", href = "tartan.png")),
  navbarPage(
    title = tags$a(href = "https://athletics.cmu.edu/sports/mgolf/index",
                   tags$div(img(src="tartan.png", height = '33px', width = '33px'), 
                            "Golf Capstone")),
    theme = shinytheme('flatly'),
    tabPanel("Team", fluid = TRUE, icon = icon("users"),
             titlePanel("Team Performance"),
             
             fluidRow(
               column(4, 
                      wellPanel(checkboxGroupButtons(inputId = "GenderFinder", 
                                                     label = "Select Team(s):", 
                                                     choices = c("Men" = "Male", 
                                                                 "Women" = "Female"), 
                                                     justified = TRUE, status = "primary",
                                                     selected = "Male",
                                                     checkIcon = list(yes = icon("ok", 
                                                                                 lib = "glyphicon"))),
                                pickerInput(inputId = "CourseFinder",
                                            label = "Select Course(s):",
                                            choices = unique(GolfData$Course),
                                            selected = "Longue Vue Club",
                                            options = list(`actions-box` = TRUE, 
                                                           style = "btn-primary"),
                                            multiple = T),
                                
                                awesomeCheckboxGroup(inputId = "RoundFinder", 
                                                     label = "Select Round Type(s):", 
                                                     choices = unique(GolfData$Round), 
                                                     selected = "Qualifying",
                                                     status = "primary"),
                                
                                awesomeRadio(inputId = "MetricFinder", 
                                             label = "Select Performance Metric:", 
                                             choices = c("Scoring Average", "Par Scoring",
                                                         "Putting Distribution", "Fairways/Greens in Regulation",
                                                         "Strokes Gained Putting"),
                                             selected = "Scoring Average", 
                                             status = "primary")
                      )
               ),
               
               column(8, wellPanel(div(DT::dataTableOutput('Team_metric'), 
                                       style = "font-size:120%"))),
               
               column(8, plotOutput(outputId = "distPlot"))
               
             ),
             
             
             fluidRow(
               column(12, DT::dataTableOutput('Team_rawdata'))
             )
    ),
    
    tabPanel("Individual", fluid = TRUE, icon = icon("user-alt"),
             titlePanel("Player Performance"),
             
             fluidRow(
               column(4,
                      wellPanel(pickerInput(inputId = "PlayerFinder",
                                            label = "Select Player:",
                                            choices = unique(GolfData$Name)[mixedorder(unique(GolfData$Name))],
                                            selected = "Player 1",
                                            options = list(`actions-box` = TRUE, 
                                                           style = "btn-primary")))),
               column(8, plotOutput(outputId = "IndPlot"))),
             
             fluidRow(
               column(12, wellPanel(div(DT::dataTableOutput('Ind_metric'), 
                                        style = "font-size:120%")))
             )
             
    ),
    
    tabPanel("Developers", fluid = TRUE, icon = icon("laptop-code"),
             p(a("Xinzhe Qi", href = "mailto:xqi@andrew.cmu.edu"), style = "font-size:25px"),
             p("email: xqi@andrew.cmu.edu", style = "font-size:20px"),
             p(a("Yedin Lui", href = "mailto:yclui@andrew.cmu.edu"), style = "font-size:25px"),
             p("email: yclui@andrew.cmu.edu", style = "font-size:20px"),
             p(a("Marc Edwards", href = "mailto:mpedward@andrew.cmu.edu"), style = "font-size:25px"),
             p("email: mpedward@andrew.cmu.edu", style = "font-size:20px"))
  )
)
