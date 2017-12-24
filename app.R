#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plyr)
library(ggplot2)
growth <- read.csv("./data/fe9_growth.csv", stringsAsFactors = FALSE)
base <- read.csv("./data/fe9_base.csv", stringsAsFactors = FALSE)
prom <- read.csv("./data/fe9_promotions.csv", stringsAsFactors = FALSE)
max <- read.csv("./data/fe9_max.csv", stringsAsFactors = FALSE)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Fire Emblem: Path Radiance"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       #simulation tab
       conditionalPanel(condition="input.conditionedPanels == 'Simulate'",       
                        h4(p(strong("Level-up Simulator"))),
                        selectInput("char", label = h4("Character"),
                                    choices = base$Name,
                                    selected = "Ike"),
                        actionButton("action", "Simulate")
       ),    
       
       #comparison tab
       conditionalPanel(condition="input.conditionedPanels == 'Compare'", 
                        h4(p(strong("Compare Characters"))),
                        selectInput("char1", label = h4("Character 2"),
                                    choices = base$Name,
                                    selected = "Ike"),
                        selectInput("char2", label = h4("Character 1"),
                                    choices = base$Name,
                                    selected = "Titania"),
                        selectInput("lab", label = h4("Type of Stats"),
                                    choices = list("Growth Rates" = "growth",
                                                   "Max Stats" = "max")),
                        actionButton("action2", "Compare")
                        ),
       
       #scatterplot tab
       conditionalPanel(condition="input.conditionedPanels == 'Scatterplot'", 
                        h4(p(strong("Compare Stats"))),
                        selectInput("stat1", label = h4("Stat 1"),
                                    choices = list("HP" = "HP", "Strength" = "Str",
                                                   "Magic" = "Mag", "Skill" = "Skill",
                                                   "Speed" = "Spd", "Luck" = "Luck",
                                                   "Defense" = "Def", "Resisitance" = "Res"),
                                    selected = "HP"),
                        selectInput("stat2", label = h4("Stat 2"),
                                    choices = list("HP" = "HP", "Strength" = "Str",
                                                   "Magic" = "Mag", "Skill" = "Skill",
                                                   "Speed" = "Spd", "Luck" = "Luck",
                                                   "Defense" = "Def", "Resisitance" = "Res"
                                                   ),
                                    selected = "Str"),
                        selectInput("val", label = h4("Type of Stats"),
                                    choices = list("Growth Rates" = "growth",
                                                   "Max Stats" = "max")),
                        actionButton("action3", "Scatterplot")
       )
      ),
      
     mainPanel(
       tabsetPanel(
         tabPanel("Simulate",
             tableOutput("char")                      
         ),
         tabPanel("Compare", h4("Comparison:"),
                  tableOutput("comp")
         ), 
         tabPanel("Scatterplot",
                  plotOutput("scatterplot")                 
         ),
         id = "conditionedPanels"                
       )
     )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # change values in columns to integer for calculation purposes
  growth[, c(3:10)] <- sapply(growth[, c(3:10)], as.integer)
  base[, c(4:12)] <- sapply(base[, c(4:12)], as.integer)
  max[, c(3:10)] <- sapply(max[, c(3:10)], as.integer)
  prom[, c(4:11)] <- sapply(prom[, c(4:11)], as.integer)
   
  #function to simulate final stats
  simulate <- function(name) {
    bases <- base[base$Name == name, ]
    character <- growth[which(growth$Name == name), ]
    
    if (bases$Prom == TRUE) {
      sums <- 40 - bases$Level
    }
    else {
      sums <- 20 - bases$Level
    }
    
    simu <- droplevels(data.frame(matrix(ncol = 8, nrow = sums)))
    for (i in c(1:sums)) {
      simu[i,] <- sample(1:100, 8, replace = TRUE)
    }
    names(simu) <- c("HP", "Str", "Mag", "Skill", "Sp", "Lck", "Def", "Res")
    hp <- nrow(simu[simu$HP<= character$HP, ])
    str <- nrow(simu[simu$Str<= character$Str, ])
    mag <- nrow(simu[simu$Mag<= character$Mag, ])
    skill <- nrow(simu[simu$Skill<= character$Skill, ])
    sp <- nrow(simu[simu$Sp<= character$Spd, ])
    lck <- nrow(simu[simu$Lck<= character$Luck, ])
    def <- nrow(simu[simu$Def<= character$Def, ])
    res <- nrow(simu[simu$Res<= character$Res, ])
    
    final_stats <- data.frame(matrix(ncol = 9, nrow = 1))
    
    if (bases$Prom == TRUE) {
      promo <- prom[prom$Class == bases$Class, ]
      hp <- hp + promo$HP
      str <- str + promo$Str
      mag <- mag + promo$Mag
      skill <- skill + promo$Skill
      sp <- sp + promo$Spd
      lck <- lck + promo$Luck
      def <- def + promo$Def
      res <- res + promo$Res
      char_max <- max[max$Class == promo$Promotion, ]
    }
    
    else {
      char_max <- max[max$Class == bases$Class, ]
    }
    
    # Adding the randomly-generated values for stat boost to his current
    # stats at Level 2 (based off the picture above)
    final_stats[1, 1] <- bases$Name
    final_stats[1, 2] <- min(hp + bases$HP, char_max$HP)
    final_stats[1, 3] <- min(str + bases$Str, char_max$Str)
    final_stats[1, 4] <- min(mag + bases$Mag, char_max$Mag)
    final_stats[1, 5] <- min(skill + bases$Skill, char_max$Skill)
    final_stats[1, 6] <- min(sp + bases$Spd, char_max$Spd)
    final_stats[1, 7] <- min(lck + bases$Luck, char_max$Luck)
    final_stats[1, 8] <- min(def + bases$Def, char_max$Def)
    final_stats[1, 9] <- min(res + bases$Res, char_max$Res)
    
    names(final_stats) <- c("Name", "HP", "Str", "Mag", "Skill",
                            "Sp", "Lck", "Def", "Res")
    
    return(final_stats)
  }
  
  # function to compare characters
  char_comp <- function(char1, char2, table) {
    if (char1 == char2) {
      stop("Must compare different characters")
    }
    if (isTRUE(all.equal(table, growth))) {
      return(growth[growth$Name == char1 | growth$Name == char2, 2:10])
    }
    if (isTRUE(all.equal(table, max))) {
      tab1 <- max_char(char1)
      tab2 <- max_char(char2)
      full_tab <- rbind(tab1, tab2)
      colnames(full_tab)[1] <- "Name"
      return(full_tab)
    }
  }
  
  # function to merge character name to associated max stats
  max_char <- function(p1) {
    baseline <- base[base$Name == p1, ]
    if (baseline$Prom == TRUE) {
      classline <- prom[prom$Class == baseline$Class, ]$Promotion
      merged <- merge(baseline$Name, max[max$Class == classline, 2:10])
    }
    else {
      merged <- merge(base[base$Name == p1, ]$Name,
                      max[max$Class == baseline$Class, 2:10])
    }
    return(merged)
  }
  
  # function to grab table from input
  frame <- function(type) {
    switch(type,
           "max" = max,
           "growth" = growth)
  }
  
  # function to plot out data for scatterplot
  plotting <- function(dataset, stat1, stat2) {
    if (isTRUE(all.equal(dataset, max))) {
      var <- "Class"
      name <- "Maximum Stats by Class"
    }
    if (isTRUE(all.equal(dataset, growth))) {
      var <- "Name"
      name <- "Growth Rates by Character"
    }
    ggplot(dataset, aes_string(grep(stat1, names(dataset), value = TRUE),
                               grep(stat2, names(dataset), value = TRUE))) +
      geom_jitter(aes_string(color = grep(var, names(dataset), value = TRUE))) +
      ggtitle(paste("Scatterplot of", name))
  }
  
  # outputing simulation
  data <- reactive({
    input$action
    isolate({
      simulate(input$char)
    })
  })
   output$char <- renderTable({
     v <- data()
     v
    }
   )
   
   # outputting comparison of characters
   data2 <- reactive({
     input$action2
     isolate({
       char_comp(input$char1, input$char2, frame(input$lab))
     })
   })
   output$comp <- renderTable({
     u <- data2()
     u
   }
   )
   
   
   # outputing scatterplot
   data3 <- reactive({
     input$action3
     isolate({
       plotting(frame(input$val), input$stat1, input$stat2)
     })
   })
   output$scatterplot <- renderPlot({
      w <- data3()
      w
     }
   )
}

# Run the application 
shinyApp(ui = ui, server = server)

