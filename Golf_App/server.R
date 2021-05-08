# Define server

server <- function(input, output) {
  Team <- reactive({
    req(input$GenderFinder)
    req(input$CourseFinder)
    req(input$RoundFinder)
    filter(GolfData, Gender %in% input$GenderFinder) %>% 
      filter(Course %in% input$CourseFinder) %>% 
      filter(Round %in% input$RoundFinder)
  })
  
  output$distPlot <- renderPlot({
    if (input$MetricFinder == "Scoring Average") {
      ggplot(subset(Team(), Hole == "Total"), 
             aes(x = reorder(Name, -Score, FUN = median), 
                 y = Score)) +
        geom_boxplot(fill = "#BD2031") +
        coord_flip() +
        labs(
          x = "Player",
          y = "Score",
          title = "Scoring Average by Player") +
        theme(legend.position = "none") +
        theme_bw()} 
    
    else if (input$MetricFinder == "Par Scoring") {
      ggplot(subset(Team(), Par == 3 | Par == 4 | Par == 5), 
             aes(x = as.factor(Score),
                 fill = ifelse(Score == Par, "Highlighed", "Normal"))) +
        scale_fill_manual(values=c("#BD2031", "#808080")) +
        geom_bar(aes(y = (..count..)/sum(..count..))) +
        scale_y_continuous(labels=scales::percent) +
        facet_wrap(~ Par) +
        theme_bw() +
        labs(
          x = "Score",
          y = "Count",
          title = "Par Scoring") +
        theme(legend.position = "none")}
    
    else if (input$MetricFinder == "Putting Distribution") {
      ggplot(subset(Team(), Par == 3 | Par == 4 | Par == 5), aes(x = Putts)) +
        geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#BD2031") +
        geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),2)),
                      y = ((..count..)/sum(..count..)/2)), stat="count", 
                  color = "black", size = 6 ) + 
        scale_y_continuous(labels=scales::percent) +
        theme(legend.position = "none") +
        labs(
          x = "Number of Putts",
          title = "Putting Distribution") +
        theme_bw()}
    
    else if (input$MetricFinder == "Fairways/Greens in Regulation") {
      f <- subset(Team(), Par == 3 | Par == 4 | Par == 5) %>%
        group_by(FIR) %>%
        summarise(counts = n()) %>%
        mutate(percentages = round(counts/sum(counts), 4))
      g <- subset(Team(), Par == 3 | Par == 4 | Par == 5) %>%
        group_by(GIR) %>%
        summarise(counts = n()) %>%
        mutate(percentages = round(counts/sum(counts), 4))
      
      fir <- ggplot(f, aes(x = factor(1), y = counts, fill = FIR)) +
        geom_col() +
        geom_text(aes(label = scales::percent(percentages)), position = position_stack(vjust = 0.5), col = "white", size = 6) +
        coord_polar(theta = "y") +
        scale_fill_manual(values=c("#BD2031", "#808080", "lightgray")) +
        theme_void() +
        theme(legend.position="bottom") + 
        theme(legend.title = element_blank()) +
        labs(title = "Fairways in Regulation") +
        theme(plot.title = element_text(hjust = 0.5)) + 
        scale_y_continuous(labels = scales::percent)
      
      gir <- ggplot(g, aes(x = factor(1), y = counts, fill = GIR)) +
        geom_col() +
        geom_text(aes(label = scales::percent(percentages)), position = position_stack(vjust = 0.5), col = "white", size = 6) +
        coord_polar(theta = "y") +
        scale_fill_manual(values=c("#BD2031", "#808080", "lightgray")) +
        theme_void() +
        theme(legend.position="bottom") + 
        theme(legend.title = element_blank()) +
        labs(title = "Greens in Regulation") +
        theme(plot.title = element_text(hjust = 0.5))
      
      grid.arrange(fir, gir, nrow = 1) }
    
    else if (input$MetricFinder == "Strokes Gained Putting") {
      Team() %>% 
        filter(., PuttLength <= 150) %>% 
        group_by(PuttLength) %>% 
        summarize(., strokesgained = mean(strokesgained)) %>% 
        ggplot(., aes(x = PuttLength, y = strokesgained)) +
        geom_point() +
        geom_smooth(method = lm, se = TRUE, col = "#BD2031") +
        labs(x = "Length of Putt",
             y = "Strokes Gained",
             title = "Strokes Gained: Putting") +
        theme_bw()
    }
  })
  
  Player <- reactive({
    subset(GolfData, 
           Name == paste("Player", as.numeric(gsub("Player ", "", input$PlayerFinder))))
    
  })
  
  Player_metric <- reactive({
    subset(GolfData, 
           Name == paste("Player", as.numeric(gsub("Player ", "", input$PlayerFinder)))) %>% 
      summarize(., "Scoring Average" = mean(Score[Hole == "Total"]),
                "Par 3" = mean(Score[Par == 3]),
                "Par 4" = mean(Score[Par == 4]),
                "Par 5" = mean(Score[Par == 5]),
                "Average Putts" = mean(Putts[Par == 3 | Par == 4 | Par == 5]),
                "FIR" = mean(FIR[Par == 3 | Par == 4 | Par == 5] == "Hit")*100,
                "GIR" = mean(GIR[Par == 3 | Par == 4 | Par == 5] == "Hit")*100,
                "SG:P" = mean(strokesgained[Par == 3 | Par == 4 | Par == 5])) %>% 
      mutate_if(is.numeric, round, 2)
  })
  
  output$IndPlot <- renderPlot({
    ggplot(subset(Player(), Hole == "Total"), 
           aes(x = reorder(Course, -Score, FUN = median), 
               y = Score)) +
      geom_boxplot(fill = "#BD2031") +
      coord_flip() +
      theme_bw() +
      labs(
        x = "Courses",
        y = "Score",
        title = paste("Scoring Distribution of", input$PlayerFinder)) +
      theme(legend.position = "none")
  })
  
  # show raw data
  # observeEvent(input$RoundFinder, {
  #   print(paste0("You have chosen: ", input$RoundFinder))
  # })
  
  Team_metrics <- reactive({
    if(input$MetricFinder == "Scoring Average") {
      gainputt <- GolfData %>% 
        filter(Gender %in% input$GenderFinder) %>% 
        filter(Course %in% input$CourseFinder) %>% 
        filter(Round %in% input$RoundFinder) %>%
        summarize(., "Scoring Average" = mean(Score[Hole == "Total"])) %>% 
        mutate_if(is.numeric, round, 2) }
    else if (input$MetricFinder == "Par Scoring") {
      gainputt <- GolfData %>% 
        filter(Gender %in% input$GenderFinder) %>% 
        filter(Course %in% input$CourseFinder) %>% 
        filter(Round %in% input$RoundFinder) %>%
        summarize(., "Par 3" = mean(Score[Par == 3]),
                  "Par 4" = mean(Score[Par == 4]),
                  "Par 5" = mean(Score[Par == 5])) %>% 
        mutate_if(is.numeric, round, 4) }
    else if (input$MetricFinder == "Putting Distribution") {
      gainputt <- GolfData %>% 
        filter(Gender %in% input$GenderFinder) %>% 
        filter(Course %in% input$CourseFinder) %>% 
        filter(Round %in% input$RoundFinder) %>%
        summarize(., "Average Putts" = mean(Putts[Par == 3 | Par == 4 | Par == 5])) %>% 
        mutate_if(is.numeric, round, 4) }
    else if (input$MetricFinder == "Fairways/Greens in Regulation") {
      gainputt <- GolfData %>% 
        filter(Gender %in% input$GenderFinder) %>% 
        filter(Course %in% input$CourseFinder) %>% 
        filter(Round %in% input$RoundFinder) %>%
        summarize(., "FIR %" = mean(FIR[Par == 3 | Par == 4 | Par == 5] == "Hit")*100,
                  "GIR %" = mean(GIR[Par == 3 | Par == 4 | Par == 5] == "Hit")*100) %>% 
        mutate_if(is.numeric, round, 4) }
    else if (input$MetricFinder == "Strokes Gained Putting") {
      gainputt <- GolfData %>% 
        filter(Gender %in% input$GenderFinder) %>% 
        filter(Course %in% input$CourseFinder) %>% 
        filter(Round %in% input$RoundFinder) %>%
        summarize(., "Strokes Gained" = mean(strokesgained)) %>% 
        mutate_if(is.numeric, round, 4) 
    }
  })
  
  Team_rawdata <- reactive({
    gainputt <- GolfData %>%
      filter(Gender %in% input$GenderFinder) %>%
      filter(Course %in% input$CourseFinder) %>%
      filter(Round %in% input$RoundFinder) %>% 
      filter(Par %in% c(3, 4, 5)) 
    gainputt = gainputt[,-c(12)]
    names(gainputt)[12] <- "Strokes.Gained"
    names(gainputt)[13] <- "Putt.Length"
    gainputt
  })
  
  
  output$Team_metric <- DT::renderDataTable(Team_metrics(), style = "bootstrap",
                                            options = list(autoWidth = TRUE, dom = 't', 
                                                           ordering = FALSE, 
                                                           columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                                            rownames = FALSE) 
  output$Team_rawdata <- DT::renderDataTable(Team_rawdata(), style = "bootstrap", 
                                             rownames = FALSE,
                                             options = list(pageLength = 10, autoWidth = TRUE, 
                                                            columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  output$Ind_metric <- DT::renderDataTable(Player_metric(), style = "bootstrap",
                                           options = list(autoWidth = TRUE, dom = 't', ordering = FALSE,
                                                          columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                                           rownames = FALSE)
}