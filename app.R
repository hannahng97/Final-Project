library(tidyverse)
library(tidytext)
library(wordcloud)
library(reshape2)
library(knitr)
library(shiny)
library(shinydashboard)

# Data
regular <- read.csv('Regular Season Celtics vs Bucks.csv')

oct18 <- read.csv('Oct. 18 Home L 108-100.csv')
oct26 <- read.csv('Oct. 26 Away W 96-89.csv')
dec4 <- read.csv('Dec. 4 Home W 100-111.csv')
apr3 <- read.csv('Apr. 3 Away L 102-106.csv')
apr15 <- read.csv('Apr. 15 Home W 107-113 Playoff1.csv')
apr17 <- read.csv('Apr. 17 Home W 106-120 Playoff2.csv')
apr20 <- read.csv('Apr. 20 Away L 92-116 Playoff3.csv')
apr22 <- read.csv('Apr. 22 Away L 102-104 Playoff4.csv')
apr24 <- read.csv('Apr. 24 Home W 87-92 Playoff5.csv')
apr26 <- read.csv('Apr. 26 Away L 86-92 Playoff6.csv')

game6 <- read.csv('Playoff6 Real Time.csv')

al <- read.csv("Al Horford Real Time.csv")
jb <- read.csv("Jaylen Brown Real Time.csv")
mm <- read.csv("Marcus Morris Real Time.csv")
ms <- read.csv("Marcus Smart Real Time.csv")
tr <- read.csv("Terry Rozier Real Time.csv")
jt <- read.csv("Jayson Tatum Real Time.csv")

h1 <- read_csv("halftime1.csv")
q <- read_csv("3rdquarter1.csv")
final <- read_csv("final1.csv")

# ui
ui <- dashboardPage(
  dashboardHeader(title = "Celtics Game 6"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Celtics: Home vs Away", tabName = "barplot"),
      menuItem("Real Time Point Difference", tabName = "time_series"),
      menuItem("Four Factors", tabName = "time_series_average"),
      menuItem("Player Statistics", tabName = "time_series_players",
                menuSubItem("Field Goals", tabName = "time_series_players1"),
                menuSubItem("3 Pointers", tabName = "time_series_players2")),
      menuItem("Twitter", tabName = "wordclouds",
               menuSubItem("Positive vs Negative", tabName = "sideplot"),
               menuSubItem("Wordcloud", tabName = "cloud"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "barplot",
              fluidRow(
                box(selectInput("slot_score",
                                "Result:",
                                choices = list("Wins", "Losses")), 
                    plotOutput("plot1"), width = 12)
              )
      ),
      
      # Second tab content
      tabItem(tabName = "time_series",
              fluidRow(
                box( 
                    plotOutput("plot2"), width = 12)
              )
      ),
      
      # Third tab content
      tabItem(tabName = "time_series_average",
              fluidRow(
                box(selectInput("slot_factor",
                                "Factor:",
                                choices = list("Field Goals", "3 Pointers", "Free Throws", "Rebounds", "Turnovers")), 
                    plotOutput("plot3"), width = 12)
              )
      ),
      
      # Fourth tab content
      tabItem(tabName = "time_series_players1",
              fluidRow(
                box(selectInput("slot_players1",
                                "Player:",
                                choices = list("Jaylen Brown", "Jayson Tatum", "Al Horford", "Marcus Smart", "Terry Rozier", "Marcus Morris", "Kyrie Irving")), 
                    plotOutput("plot4"), width = 12)
              )
      ),
      
      # Fifth tab content
      tabItem(tabName = "time_series_players2",
              fluidRow(
                box(selectInput("slot_players2",
                                "Player:",
                                choices = list("Jaylen Brown", "Jayson Tatum", "Al Horford", "Marcus Smart", "Terry Rozier", "Marcus Morris", "Kyrie Irving")), 
                    plotOutput("plot5"), width = 12)
              )
      ),
      
      # Sixth tab content
      tabItem(tabName = "sideplot",
              fluidRow(
                box(selectInput("slot_time1",
                                "Time:",
                                choices = list("Halftime", "3rd Quarter", "Final")), 
                    plotOutput("plot6"), width = 12)
              )
      ),
      
      # Seventh tab content
      tabItem(tabName = "cloud",
              fluidRow(
                box(selectInput("slot_time2",
                                "Time:",
                                choices = list("Halftime", "3rd Quarter", "Final")), 
                    plotOutput("plot7"), width = 12)
              )
      )
    )
  )
)

#Server
server <- function(input, output) { 
  
  output$plot1 <- renderPlot({
    
    if (input$slot_score == "Wins") {
      
      p <- barplot(c(4, 1),
                   names.arg = c("Home", "Away"),
                   main = "Home vs Away Wins",
                   xlab = "Location",
                   ylab = "Number of Wins",
                   col = "palegreen1")
      print(p)
    }
    
    if (input$slot_score == "Losses") {

      p <- barplot(c(1, 3),
                   names.arg = c("Home", "Away"),
                   main = "Home vs Away Losses",
                   xlab = "Location",
                   ylab = "Number of Losses",
                   col = "palegreen1")
      print(p)
    }
    
  })
  
  output$plot2 <- renderPlot({

     p <- ggplot(game6, aes(game6$MIN, y = value)) + 
       geom_line(aes(y = game6$PTS, col = "Celtics")) + 
       geom_line(aes(y = game6$BUCKS, col = "Bucks")) +
       scale_colour_manual("", 
                           values = c("Celtics"="green", "Bucks"="brown")) +
       labs(title = "Point Difference",
            y = "Points",
            x = "Minutes")
    print(p)
    
  })
  
  output$plot3 <- renderPlot({
    
    if (input$slot_factor == "Field Goals") {
      getFieldGoals <- function(date) {
        date[length(date$PLAYER), 5]
      }
      
      fg1 <- getFieldGoals(oct18)
      fg2 <- getFieldGoals(oct26)
      fg3 <- getFieldGoals(dec4)
      fg4 <- getFieldGoals(apr3)
      fg5 <- getFieldGoals(apr15)
      fg6 <- getFieldGoals(apr17)
      fg7 <- getFieldGoals(apr20)
      fg8 <- getFieldGoals(apr22)
      fg9 <- getFieldGoals(apr24)
      
      FG_average <- mean(c(fg1, fg2, fg3, fg4, fg5, fg6, fg7, fg8, fg9))
      
      p <- ggplot(game6, aes(game6$MIN, y = value)) + 
        geom_line(aes(y = game6$FG., col = "Real Time")) + 
        geom_line(aes(y = FG_average, col = "Historical")) +
        labs(title = "Field Goal Comparison",
             y = "Field Goal %",
             x = "Minutes")
      print(p)
    }
    
    if (input$slot_factor == "3 Pointers") {
      get3Pointers <- function(date) {
        date[length(date$PLAYER), 8]
      }
      
      p1 <- get3Pointers(oct18)
      p2 <- get3Pointers(oct26)
      p3 <- get3Pointers(dec4)
      p4 <- get3Pointers(apr3)
      p5 <- get3Pointers(apr15)
      p6 <- get3Pointers(apr17)
      p7 <- get3Pointers(apr20)
      p8 <- get3Pointers(apr22)
      p9 <- get3Pointers(apr24)
      
      P_average <- mean(c(p1, p2, p3, p4, p5, p6, p7, p8, p9))
      
      p <- ggplot(game6, aes(game6$MIN, y = value)) + 
        geom_line(aes(y = game6$X3P., col = "Real Time")) + 
        geom_line(aes(y = P_average, col = "Historical")) +
        labs(title = "3 Pointer Comparison",
             y = "3 Pointer %",
             x = "Minutes")
      print(p)
    }
    
    if (input$slot_factor == "Free Throws") {
      getFreeThrows <- function(date) {
        date[length(date$PLAYER), 11]
      }
      
      ft1 <- getFreeThrows(oct18)
      ft2 <- getFreeThrows(oct26)
      ft3 <- getFreeThrows(dec4)
      ft4 <- getFreeThrows(apr3)
      ft5 <- getFreeThrows(apr15)
      ft6 <- getFreeThrows(apr17)
      ft7 <- getFreeThrows(apr20)
      ft8 <- getFreeThrows(apr22)
      ft9 <- getFreeThrows(apr24)
      
      FT_average <- mean(c(ft1, ft2, ft3, ft4, ft5, ft6, ft7, ft8, ft9))
      
      p <- ggplot(game6, aes(game6$MIN, y = value)) + 
        geom_line(aes(y = game6$FT., col = "Real Time")) + 
        geom_line(aes(y = FT_average, col = "Historical")) +
        labs(title = "Free Throw Comparison",
             y = "Free Throw %",
             x = "Minutes")
      print(p)
    }
    
    if (input$slot_factor == "Rebounds") {
      getRebound <- function(date) {
        date[length(date$PLAYER), 14]
      }
      
      R1 <- getRebound(oct18)
      R2 <- getRebound(oct26)
      R3 <- getRebound(dec4)
      R4 <- getRebound(apr3)
      R5 <- getRebound(apr15)
      R6 <- getRebound(apr17)
      R7 <- getRebound(apr20)
      R8 <- getRebound(apr22)
      R9 <- getRebound(apr24)
      R10 <- getRebound(apr26)
      
      p <- barplot(c(R1, R2, R3, R4, R5, R6, R7, R8, R9, R10),
                   names.arg = c("oct18", "oct26", "dec4", "apr3", "apr15", "apr17", "apr20", "apr22", "apr24", "apr26"),
                   main = "Rebounds Historical vs Real",
                   xlab = "Date", cex.names = 0.7,
                   ylab = "Rebounds",
                   col = "skyblue1")
      print(p)
    }
    
    if (input$slot_factor == "Turnovers") {
      getTurnover <- function(date) {
        date[length(date$PLAYER), 16]
      }
      
      T1 <- getTurnover(oct18)
      T2 <- getTurnover(oct26)
      T3 <- getTurnover(dec4)
      T4 <- getTurnover(apr3)
      T5 <- getTurnover(apr15)
      T6 <- getTurnover(apr17)
      T7 <- getTurnover(apr20)
      T8 <- getTurnover(apr22)
      T9 <- getTurnover(apr24)
      T10 <- getTurnover(apr26)
      
      p <- barplot(c(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10),
                   names.arg = c("oct18", "oct26", "dec4", "apr3", "apr15", "apr17", "apr20", "apr22", "apr24", "apr26"),
                   main = "Turnovers Historical vs Real",
                   xlab = "Date", cex.names = .7,
                   ylab = "Turnovers",
                   col = "royalblue1")
      print(p)
    }
    
  })
  
  output$plot4 <- renderPlot({
    
    ## Jaylen Brown
    getjb <- function(date, column){
      index <- date$PLAYER %>% `==` ("Jaylen Brown") %>% which()
      date[index, column]
    }
    
    # Field Goals
    jbfg1 <- getjb(oct18, 5)
    jbfg2 <- getjb(oct26, 5)
    jbfg3 <- getjb(dec4, 5)
    jbfg4 <- getjb(apr3, 5)
    jbfg5 <- getjb(apr15, 5)
    jbfg6 <- getjb(apr17, 5)
    jbfg7 <- getjb(apr20, 5)
    jbfg8 <- getjb(apr22, 5)
    jbfg9 <- getjb(apr24, 5)
    
    jbfg_average <- mean(c(jbfg1, jbfg2, jbfg3, jbfg4, jbfg5, jbfg6, jbfg7, jbfg8, jbfg9), na.rm = TRUE)
    
    ## Jayson Tatum
    getjt <- function(date, column){
      index <- date$PLAYER %>% `==` ("Jayson Tatum") %>% which()
      date[index, column]
    }
    
    # Field Goals
    jtfg1 <- getjt(oct18, 5)
    jtfg2 <- getjt(oct26, 5)
    jtfg3 <- getjt(dec4, 5)
    jtfg4 <- getjt(apr3, 5)
    jtfg5 <- getjt(apr15, 5)
    jtfg6 <- getjt(apr17, 5)
    jtfg7 <- getjt(apr20, 5)
    jtfg8 <- getjt(apr22, 5)
    jtfg9 <- getjt(apr24, 5)
    
    jtfg_average <- mean(c(jtfg1, jtfg2, jtfg3, jtfg4, jtfg5, jtfg6, jtfg7, jtfg8, jtfg9), na.rm = TRUE)
    
    ## Al Horford
    getal <- function(date, column){
      index <- date$PLAYER %>% `==` ("Al Horford") %>% which()
      date[index, column]
    }
    
    # Field Goals
    alfg1 <- getal(oct18, 5)
    alfg2 <- getal(oct26, 5)
    alfg3 <- getal(dec4, 5)
    alfg4 <- getal(apr3, 5)
    alfg5 <- getal(apr15, 5)
    alfg6 <- getal(apr17, 5)
    alfg7 <- getal(apr20, 5)
    alfg8 <- getal(apr22, 5)
    alfg9 <- getal(apr24, 5)
    
    alfg_average <- mean(c(alfg1, alfg2, alfg3, alfg4, alfg5, alfg6, alfg7, alfg8, alfg9), na.rm = TRUE)
    
    ## Marcus Smart
    getms <- function(date, column){
      index <- date$PLAYER %>% `==` ("Marcus Smart") %>% which()
      date[index, column]
    }
    
    # Field Goals
    msfg1 <- getms(oct18, 5)
    msfg2 <- getms(oct26, 5)
    msfg3 <- getms(dec4, 5)
    msfg4 <- getms(apr3, 5)
    msfg5 <- getms(apr15, 5)
    msfg6 <- getms(apr17, 5)
    msfg7 <- getms(apr20, 5)
    msfg8 <- getms(apr22, 5)
    msfg9 <- getms(apr24, 5)
    
    msfg_average <- mean(c(msfg1, msfg2, msfg3, msfg4, msfg5, msfg6, msfg7, msfg8, msfg9), na.rm = TRUE)
    
    ## Terry Rozier
    gettr <- function(date, column){
      index <- date$PLAYER %>% `==` ("Terry Rozier") %>% which()
      date[index, column]
    }
    
    # Field Goals
    trfg1 <- gettr(oct18, 5)
    trfg2 <- gettr(oct26, 5)
    trfg3 <- gettr(dec4, 5)
    trfg4 <- gettr(apr3, 5)
    trfg5 <- gettr(apr15, 5)
    trfg6 <- gettr(apr17, 5)
    trfg7 <- gettr(apr20, 5)
    trfg8 <- gettr(apr22, 5)
    trfg9 <- gettr(apr24, 5)
    
    trfg_average <- mean(c(trfg1, trfg2, trfg3, trfg4, trfg5, trfg6, trfg7, trfg8, trfg9), na.rm = TRUE)
    
    ## Marcus Morris
    getmm <- function(date, column){
      index <- date$PLAYER %>% `==` ("Marcus Morris") %>% which()
      date[index, column]
    }
    
    # Field Goals
    mmfg1 <- getmm(oct18, 5)
    mmfg2 <- getmm(oct26, 5)
    mmfg3 <- getmm(dec4, 5)
    mmfg4 <- getmm(apr3, 5)
    mmfg5 <- getmm(apr15, 5)
    mmfg6 <- getmm(apr17, 5)
    mmfg7 <- getmm(apr20, 5)
    mmfg8 <- getmm(apr22, 5)
    mmfg9 <- getmm(apr24, 5)
    
    mmfg_average <- mean(c(mmfg1, mmfg2, mmfg3, mmfg4, mmfg5, mmfg6, mmfg7, mmfg8, mmfg9), na.rm = TRUE)
    
    
    if (input$slot_players1 == "Jaylen Brown") {
      
      p <- ggplot(jb, aes(jb$MIN, y = value)) + 
        geom_line(aes(y = jb$FG., col = "Real Time")) + 
        geom_line(aes(y = jbfg_average, col = "Historical")) +
        labs(title = "Jaylen Brown Field Goals Comparison",
             y = "Field Goal %",
             x = "Minutes")
      print(p)
    }
    
    if (input$slot_players1 == "Jayson Tatum") {

            p <- ggplot(jt, aes(jt$MIN, y = value)) + 
        geom_line(aes(y = jt$FG., col = "Real Time")) + 
        geom_line(aes(y = jtfg_average, col = "Historical")) +
        labs(title = "Jayson Tatum Field Goals Comparison",
             y = "Field Goal %",
             x = "Minutes")
      print(p)
    }
    
    if (input$slot_players1 == "Al Horford") {
      
      p <- ggplot(al, aes(al$MIN, y = value)) + 
        geom_line(aes(y = al$FG., col = "Real Time")) + 
        geom_line(aes(y = alfg_average, col = "Historical")) +
        labs(title = "Al Horford Field Goals Comparison",
             y = "Field Goal %",
             x = "Minutes")
      print(p)
    }
    
    if (input$slot_players1 == "Marcus Smart") {
      
      p <- ggplot(ms, aes(ms$MIN, y = value)) + 
        geom_line(aes(y = ms$FG., col = "Real Time")) + 
        geom_line(aes(y = msfg_average, col = "Historical")) +
        labs(title = "Marcus Smart Field Goals Comparison",
             y = "Field Goal %",
             x = "Minutes")
      print(p)
    }
    
    if (input$slot_players1 == "Terry Rozier") {
      
      p <- ggplot(tr, aes(tr$MIN, y = value)) + 
        geom_line(aes(y = tr$FG., col = "Real Time")) + 
        geom_line(aes(y = trfg_average, col = "Historical")) +
        labs(title = "Terry Rozier Field Goals Comparison",
             y = "Field Goal %",
             x = "Minutes")
      print(p)
    }
    
    if (input$slot_players1 == "Marcus Morris") {
      
      p <- ggplot(mm, aes(mm$MIN, y = value)) + 
        geom_line(aes(y = mm$FG., col = "Real Time")) + 
        geom_line(aes(y = mmfg_average, col = "Historical")) +
        labs(title = "Marcus Morris Field Goals Comparison",
             y = "Field Goal %",
             x = "Minutes")
      print(p)
    }
    
    if (input$slot_players1 == "Kyrie Irving") {
      getk <- function(date, column){
        index <- date$PLAYER %>% `==` ("Kyrie Irving") %>% which()
        date[index, column]
      }
      
      # Field Goals
      kfg1 <- getk(oct18, 5)
      kfg2 <- getk(oct26, 5)
      kfg3 <- getk(dec4, 5)
      kfg4 <- getk(apr3, 5)
      kfg5 <- getk(apr15, 5)
      kfg6 <- getk(apr17, 5)
      kfg7 <- getk(apr20, 5)
      kfg8 <- getk(apr22, 5)
      kfg9 <- getk(apr24, 5)
      
      kfg_average <- mean(c(kfg1, kfg2, kfg3, kfg4, kfg5, kfg6, kfg7, kfg8, kfg9), na.rm = TRUE)
      
      p <- barplot(c(jbfg_average, jtfg_average, alfg_average, msfg_average, mmfg_average, trfg_average, kfg_average),
                   names.arg = c("Jaylen Brown", "Jayson Tatum", "Al Horford", "Marcus Smart", "Marcus Morris", "Terry Rozier", "Kyrie Irving"),
                   main = "Kyrie Irving vs Others Field Goals",
                   xlab = "Player", cex.names = .7,
                   ylab = "Field Goal %",
                   col = "forestgreen")
      print(p)
    }
    
  })
  
  output$plot5 <- renderPlot({
    
    ## Jaylen Brown
    getjb <- function(date, column){
      index <- date$PLAYER %>% `==` ("Jaylen Brown") %>% which()
      date[index, column]
    }
    
    jbp1 <- getjb(oct18, 8)
    jbp2 <- getjb(oct26, 8)
    jbp3 <- getjb(dec4, 8)
    jbp4 <- getjb(apr3, 8)
    jbp5 <- getjb(apr15, 8)
    jbp6 <- getjb(apr17, 8)
    jbp7 <- getjb(apr20, 8)
    jbp8 <- getjb(apr22, 8)
    jbp9 <- getjb(apr24, 8)
    
    jbp_average <- mean(c(jbp1, jbp2, jbp3, jbp4, jbp5, jbp6, jbp7, jbp8, jbp9), na.rm = TRUE)
    
    ## Jayson Tatum
    getjt <- function(date, column){
      index <- date$PLAYER %>% `==` ("Jayson Tatum") %>% which()
      date[index, column]
    }
    
    jtp1 <- getjt(oct18, 8)
    jtp2 <- getjt(oct26, 8)
    jtp3 <- getjt(dec4, 8)
    jtp4 <- getjt(apr3, 8)
    jtp5 <- getjt(apr15, 8)
    jtp6 <- getjt(apr17, 8)
    jtp7 <- getjt(apr20, 8)
    jtp8 <- getjt(apr22, 8)
    jtp9 <- getjt(apr24, 8)
    
    jtp_average <- mean(c(jtp1, jtp2, jtp3, jtp4, jtp5, jtp6, jtp7, jtp8, jtp9), na.rm = TRUE)
    
    
    ## Al Horford
    getal <- function(date, column){
      index <- date$PLAYER %>% `==` ("Al Horford") %>% which()
      date[index, column]
    }
    
    alp1 <- getal(oct18, 8)
    alp2 <- getal(oct26, 8)
    alp3 <- getal(dec4, 8)
    alp4 <- getal(apr3, 8)
    alp5 <- getal(apr15, 8)
    alp6 <- getal(apr17, 8)
    alp7 <- getal(apr20, 8)
    alp8 <- getal(apr22, 8)
    alp9 <- getal(apr24, 8)
    
    alp_average <- mean(c(alp1, alp2, alp3, alp4, alp5, alp6, alp7, alp8, alp9), na.rm = TRUE)
    
    ## Marcus Smart
    getms <- function(date, column){
      index <- date$PLAYER %>% `==` ("Marcus Smart") %>% which()
      date[index, column]
    }
    
    msp1 <- getms(oct18, 8)
    msp2 <- getms(oct26, 8)
    msp3 <- getms(dec4, 8)
    msp4 <- getms(apr3, 8)
    msp5 <- getms(apr15, 8)
    msp6 <- getms(apr17, 8)
    msp7 <- getms(apr20, 8)
    msp8 <- getms(apr22, 8)
    msp9 <- getms(apr24, 8)
    
    msp_average <- mean(c(msp1, msp2, msp3, msp4, msp5, msp6, msp7, msp8, msp9), na.rm = TRUE)
    
    ## Terry Rozier
    gettr <- function(date, column){
      index <- date$PLAYER %>% `==` ("Terry Rozier") %>% which()
      date[index, column]
    }
    
    trp1 <- gettr(oct18, 8)
    trp2 <- gettr(oct26, 8)
    trp3 <- gettr(dec4, 8)
    trp4 <- gettr(apr3, 8)
    trp5 <- gettr(apr15, 8)
    trp6 <- gettr(apr17, 8)
    trp7 <- gettr(apr20, 8)
    trp8 <- gettr(apr22, 8)
    trp9 <- gettr(apr24, 8)
    
    trp_average <- mean(c(trp1, trp2, trp3, trp4, trp5, trp6, trp7, trp8, trp9), na.rm = TRUE)
    
    ## Marcus Morris
    getmm <- function(date, column){
      index <- date$PLAYER %>% `==` ("Marcus Morris") %>% which()
      date[index, column]
    }
    
    mmp1 <- getmm(oct18, 8)
    mmp2 <- getmm(oct26, 8)
    mmp3 <- getmm(dec4, 8)
    mmp4 <- getmm(apr3, 8)
    mmp5 <- getmm(apr15, 8)
    mmp6 <- getmm(apr17, 8)
    mmp7 <- getmm(apr20, 8)
    mmp8 <- getmm(apr22, 8)
    mmp9 <- getmm(apr24, 8)
    
    mmp_average <- mean(c(mmp1, mmp2, mmp3, mmp4, mmp5, mmp6, mmp7, mmp8, mmp9), na.rm = TRUE)
    
    
    if (input$slot_players2 == "Jaylen Brown") {
      
      p <- ggplot(jb, aes(jb$MIN, y = value)) + 
        geom_line(aes(y = jb$X3P., col = "Real Time")) + 
        geom_line(aes(y = jbp_average, col = "Historical")) +
        labs(title = "Jaylen Brown 3 Pointers Comparison",
             y = "3 Point %",
             x = "Minutes")
      print(p)
    }
    
    if (input$slot_players2 == "Jayson Tatum") {
      
      p <- ggplot(jt, aes(jt$MIN, y = value)) + 
        geom_line(aes(y = jt$X3P., col = "Real Time")) + 
        geom_line(aes(y = jtp_average, col = "Historical")) +
        labs(title = "Jayson Tatum 3 Pointers Comparison",
             y = "3 Point %",
             x = "Minutes")
      print(p)
    }
    
    if (input$slot_players2 == "Al Horford") {
      
      p <- ggplot(al, aes(al$MIN, y = value)) + 
        geom_line(aes(y = al$X3P., col = "Real Time")) + 
        geom_line(aes(y = alp_average, col = "Historical")) +
        labs(title = "Al Horford 3 Pointers Comparison",
             y = "3 Point %",
             x = "Minutes")
      print(p)
    }
    
    if (input$slot_players2 == "Marcus Smart") {
      
      p <- ggplot(ms, aes(ms$MIN, y = value)) + 
        geom_line(aes(y = ms$X3P., col = "Real Time")) + 
        geom_line(aes(y = msp_average, col = "Historical")) +
        labs(title = "Marcus Smart 3 Pointers Comparison",
             y = "3 Point %",
             x = "Minutes")
      print(p)
    }
    
    if (input$slot_players2 == "Terry Rozier") {
      
      p <- ggplot(tr, aes(tr$MIN, y = value)) + 
        geom_line(aes(y = tr$X3P., col = "Real Time")) + 
        geom_line(aes(y = trp_average, col = "Historical")) +
        labs(title = "Terry Rozier 3 Pointers Comparison",
             y = "3 Point %",
             x = "Minutes")
      print(p)
    }
    
    if (input$slot_players2 == "Marcus Morris") {
      
      p <- ggplot(mm, aes(mm$MIN, y = value)) + 
        geom_line(aes(y = mm$X3P., col = "Real Time")) + 
        geom_line(aes(y = mmp_average, col = "Historical")) +
        labs(title = "Marcus Morris 3 Pointers Comparison",
             y = "3 Point %",
             x = "Minutes")
      print(p)
    }
    
    if (input$slot_players2 == "Kyrie Irving") {
      getk <- function(date, column){
        index <- date$PLAYER %>% `==` ("Kyrie Irving") %>% which()
        date[index, column]
      }
      
      kp1 <- getk(oct18, 8)
      kp2 <- getk(oct26, 8)
      kp3 <- getk(dec4, 8)
      kp4 <- getk(apr3, 8)
      kp5 <- getk(apr15, 8)
      kp6 <- getk(apr17, 8)
      kp7 <- getk(apr20, 8)
      kp8 <- getk(apr22, 8)
      kp9 <- getk(apr24, 8)
      
      kp_average <- mean(c(kp1, kp2, kp3, kp4, kp5, kp6, kp7, kp8, kp9), na.rm = TRUE)
      
      p <- barplot(c(jbp_average, jtp_average, alp_average, msp_average, mmp_average, trp_average, kp_average),
                   names.arg = c("Jaylen Brown", "Jayson Tatum", "Al Horford", "Marcus Smart", "Marcus Morris", "Terry Rozier", "Kyrie Irving"),
                   main = "Kyrie Irving vs Others 3 Pointers",
                   xlab = "Player", cex.names = .7,
                   ylab = "3 Point %",
                   col = "aquamarine3")
      print(p)
    }
    
  })
  
  output$plot6 <- renderPlot({
    
    if (input$slot_time1 == "Halftime") {
      h1 <- read_csv("halftime1.csv")
      
      h1 <- h1 %>% select(text) %>% unnest_tokens(word, text)
      
      # positive vs. negative overall
      bing_word_counts <- h1 %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        ungroup()
      
      p <- bing_word_counts %>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Contribution to sentiment",
             x = NULL) +
        coord_flip() 
        
      print(p)
    }
    
    if (input$slot_time1 == "3rd Quarter") {
      q <- read_csv("3rdquarter1.csv")
      
      q <- q %>% select(text) %>% unnest_tokens(word, text)
      
      # positive vs. negative overall
      bing_word_counts <- q %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        ungroup()
      
      p <- bing_word_counts %>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Contribution to sentiment",
             x = NULL) +
        coord_flip()
      
      print(p)
    }
    
    if (input$slot_time1 == "Final") {
      final <- read_csv("final1.csv")
      
      final <- final %>% select(text) %>% unnest_tokens(word, text)
      
      # positive vs. negative overall
      bing_word_counts <- final %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        ungroup()
      
      p <- bing_word_counts %>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Contribution to sentiment",
             x = NULL) +
        coord_flip()
      
      print(p)
    }
    
  })
  
  output$plot7 <- renderPlot({
    
    if (input$slot_time2 == "Halftime") {
  
      h1 <- h1 %>% select(text) %>% unnest_tokens(word, text)
      
      p <- h1 %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("springgreen3", "aquamarine2"),
                         max.words = 100)
      
      print(p)
    }
    
    if (input$slot_time2 == "3rd Quarter") {
      
      q <- q %>% select(text) %>% unnest_tokens(word, text)
      
      p <- q %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("springgreen3", "aquamarine2"),
                         max.words = 100)
      
      print(p)
    }
    
    if (input$slot_time2 == "Final") {
      
      final <- final %>% select(text) %>% unnest_tokens(word, text)
      
      p <- final %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("springgreen3", "aquamarine2"),
                         max.words = 100)
      
      print(p)
    }
    
  })
  
}

shinyApp(ui, server)