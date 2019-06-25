library(shiny)
require(shinydashboard)
library(e1071)
library(mlr)
library(caret)
library(dplyr)
library(plotly)

############################################################# UI ###################################################################

header <- dashboardHeader(title = "Naive Bayes")  

sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "sidebarmenu",
        menuItem("Step 1 : Initialisation", tabName = "initialisation"),
        menuItem("Step 2 : Data Quality Config", tabName = "dataqualityconfig"),
        menuItem("Step 3 : Costs Config", tabName = "costsconfig"),
        menuItem("Step 4 : Results", tabName = "results"),
        menuItem("Website", icon = icon("send",lib='glyphicon'), 
                 href = "https://archive.ics.uci.edu/ml/datasets/Cervical+cancer+%28Risk+Factors%29")
    )
)

body <- dashboardBody(
    tabItems(
        
        #__________________________________________________ Initialisation __________________________________________________#
        
        tabItem(
            tabName = "initialisation",
            sidebarLayout(
                sidebarPanel(
                    h1("Initialisation"),
                    tags$hr(),
                    
                    fileInput("fileCSV", "CSV File",
                              multiple = FALSE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    box(width = 12,
                        title = "Parameters (CSV)",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed = TRUE,
                        column(6,
                               checkboxInput("header", "Header", TRUE),
                               radioButtons("sep", "Separator",
                                            choices = c("Comma" = ",",
                                                        "Semicolon" = ';',
                                                        "Tab" = "\t"),
                                            selected = ';')
                        ),
                        column(6,
                               radioButtons("quote", "Quote",
                                            choices = c(None = "",
                                                        "Double Quote" = '"',
                                                        "Single Quote" = "'"),
                                            selected = "")
                        )
                    ),
                    uiOutput("uploadbutton"),
                    tags$hr(),
                    uiOutput("demobutton"),
                    tags$hr(),
                    uiOutput("selectcolumn"),
                    uiOutput("step2button"),
                    tags$hr(),
                    uiOutput("clearall")
                ),
                mainPanel(
                    dataTableOutput("tabLoadedstep1")
                )
            )
        ),
        
        #__________________________________________________ DataQuality Config __________________________________________________#
        
        tabItem(
            tabName = "dataqualityconfig",
            sidebarLayout(
                sidebarPanel(
                    h1("Data Quality Config"),
                    tags$hr(),
                    h4("Choose any options you want"),
                    uiOutput("removeNAsbutton"),
                    tags$hr(),
                    uiOutput("step3button"),
                    tags$hr(),
                    uiOutput("clearallreturnstep2")
                ),
                mainPanel(
                    dataTableOutput("tabLoadedstep2")
                )
            )
        ),
        
        #____________________________________________________ Costs Config ____________________________________________________#
        
        tabItem(
            tabName = "costsconfig",
            sidebarLayout(
                sidebarPanel(
                    h1("Costs Config"),
                    tags$hr(),
                    uiOutput("cost1"),
                    uiOutput("cost2"),
                    tags$hr(),
                    uiOutput("costsTab"),
                    tags$hr(),
                    uiOutput("step4button"),
                    tags$hr(),
                    uiOutput("clearallreturnstep3")
                ),
                mainPanel(
                    dataTableOutput("tabLoadedstep3")
                )
            )
        ),
        
        #_______________________________________________________ Results _______________________________________________________#
        
        tabItem(
            tabName = "results",
            
            
            
            sidebarLayout(
                sidebarPanel(
                    h1("Results"),
                    tags$hr(),
                    uiOutput("accurancyvalue"),
                    tags$hr(),
                    fluidRow(
                        box( width = 12,
                            title = "Accuracy Histogram"
                            ,status = "primary"
                            ,solidHeader = TRUE 
                            ,collapsible = TRUE
                            ,collapsed = TRUE
                            ,plotlyOutput("accuracyCVbar")
                        )
                    ),
                    tags$hr(),
                    uiOutput("costresultsvalue"),
                    tags$hr(),
                    uiOutput("clearallreturnstep4")
                ),
                mainPanel(
                    dataTableOutput("tabLoadedstep4")
                )
            )
        )
    )
)
ui <- dashboardPage(title = 'Data Quality test - Week 3', header, sidebar, body, skin='blue')


########################################################### Server #################################################################

server <- function(input, output,session) {
    
    #__________________________________________________ Reactive values ____________________________________________________#
    
    v <- reactiveValues(data = NULL,
                        dataframestep1 = NULL,
                        dataframestep2 = NULL,
                        dataframestep3 = NULL,
                        dataframestep4 = NULL,
                        columnSelected = NULL, 
                        resultData = NULL, 
                        accuracy = NULL, 
                        accuracyTab = NULL)
    
    #__________________________________________________ Initialisation ______________________________________________________#
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    # Clear All button --------------------------------------------
    
    output$clearall <- renderUI({
        actionButton("clearall","Clear")
    })
    observeEvent(input$clearall,{
        v$data = NULL
        v$dataframestep1 = NULL
        v$dataframestep2 = NULL
        v$dataframestep3 = NULL
        v$dataframestep4 = NULL
        v$columnSelected = NULL
        v$resultData = NULL
        v$accuracy = NULL
        v$accuracyTab = NULL
    })
    
    # Upload button --------------------------------------------
    
    output$uploadbutton <- renderUI({
        actionButton("uploadbutton","Upload")
    })
    observeEvent(input$uploadbutton,{
        infile <- input$fileCSV
        if (is.null(infile)) return (NULL)
        v$dataframestep1 <- read.csv(infile$datapath,
                                     header = input$header, 
                                     sep = input$sep,
                                     quote = input$quote)
        
    })
    
    # Demo button --------------------------------------------
    
    output$demobutton <- renderUI({
        actionButton("demobutton","Upload a Demo")
    })
    observeEvent(input$demobutton,{
        v$dataframestep1 <- read.csv("risk_factors_cervical_cancer_Copie.csv",
                                     header = input$header, 
                                     sep = input$sep,
                                     quote = input$quote)
    })
    
    # Step 2 button --------------------------------------------
    
    output$step2button <- renderUI({
        if (is.null(v$dataframestep1)) return (NULL)
        actionButton("step2button","Go Step 2")
    })
    observeEvent(input$step2button,{
        v$dataframestep2 <- v$dataframestep1
        newtab <- switch(input$sidebarmenu,
                         "initialisation" = "dataqualityconfig",
                         "dataqualityconfig" = "initialisation"
        )
        updateTabItems(session,"sidebarmenu", newtab)
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Selections ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    # Selection of column --------------------------------------------
    
    output$selectcolumn <- renderUI({
        if (is.null(v$dataframestep1)) {
            return (h4("Please upload a file and then select a column"))
        }
        
        items=rev(names(v$dataframestep1))
        names(items)=items
        selectInput("selectcolumn", "Choose a column (try with \"Smokes\")",items)
    })
    observeEvent(input$selectcolumn,{
        v$columnSelected <- input$selectcolumn
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    # Table from CSV  --------------------------------------------
    
    output$tabLoadedstep1 <- renderDataTable(
        v$dataframestep1,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    #__________________________________________________ DataQuality Config __________________________________________________#
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    # Remove NAs button --------------------------------------------
    
    output$removeNAsbutton <- renderUI({
        if(is.null(v$dataframestep2)) return (NULL)
        actionButton("removeNAsbutton","Remove NAs")
    })
    observeEvent(input$removeNAsbutton,{
        vect <- 0
        bool <- 0
        for (i in row.names(v$dataframestep2)){
            bool = is.na(v$dataframestep2[i,])
            if(!TRUE %in% bool){
                vect[i] = i
            }
        }
        v$dataframestep2 <- v$dataframestep2[vect,]
    })
    
    # Step 3 button --------------------------------------------
    
    output$step3button <- renderUI({
        if (is.null(v$dataframestep2)) return (NULL)
        actionButton("step3button","Go Step 3")
    })
    observeEvent(input$step3button,{
        v$dataframestep3 <- v$dataframestep2
        newtab <- switch(input$sidebarmenu,
                         "dataqualityconfig" = "costsconfig",
                         "costsconfig" = "dataqualityconfig"
        )
        updateTabItems(session,"sidebarmenu", newtab)
    })
    
    # Clear All and return button --------------------------------------------
    
    output$clearallreturnstep2 <- renderUI({
        actionButton("clearallreturnstep2","Clear All and return Step 1")
    })
    observeEvent(input$clearallreturnstep2,{
        v$data = NULL
        v$dataframestep1 = NULL
        v$dataframestep2 = NULL
        v$dataframestep3 = NULL
        v$dataframestep4 = NULL
        v$columnSelected = NULL
        v$resultData = NULL
        v$accuracy = NULL
        v$accuracyTab = NULL
        newtab <- switch(input$sidebarmenu,
                         "dataqualityconfig" = "initialisation",
                         "initialisation" = "dataqualityconfig"
        )
        updateTabItems(session,"sidebarmenu", newtab)
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    # Table from CSV 2 --------------------------------------------
    
    output$tabLoadedstep2 <- renderDataTable(
        v$dataframestep2,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    #____________________________________________________ Costs Config ____________________________________________________#
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    # Step 4 button (Results) --------------------------------------------
    
    output$step4button <- renderUI({
        if (is.null(v$dataframestep3)) return (NULL)
        actionButton("step4button","Results")
    })
    observeEvent(input$step4button,{
        v$dataframestep4 <- v$dataframestep3
        df <- v$dataframestep4
        
        # Naive Bayes CrossValidation #
        
        v$data <- unique(sort(df[,input$selectcolumn]))
        v$columnSelected <- input$selectcolumn
        
        colNA <- is.na(df[,v$columnSelected])
        df_noNAs <<- df[!colNA,]
        
        moy <- 0
        nono <- 0
        yesno <- 0
        noyes <- 0
        yesyes <- 0
        
        for (i in 1:10) {
            
            training.samples <- df_noNAs[,v$columnSelected] %>% 
                caret::createDataPartition(p = 0.8, list = FALSE)
            train.data <- df_noNAs[training.samples, ]
            test.data <- df_noNAs[-training.samples, ]
            
            task <<- makeClassifTask(data = train.data, target = v$columnSelected)
            selected_model <<- makeLearner("classif.naiveBayes")
            
            NB_mlr <<- mlr::train(selected_model, task)
            
            predictions_mlr <<- as.data.frame(predict(NB_mlr, newdata = test.data[,!names(df_noNAs) %in% c(v$columnSelected)]))
            resultNaiveBayes <<- table(predictions_mlr[,1],test.data[,v$columnSelected])
            res <- as.data.frame(resultNaiveBayes)
            aux <- 0
            for(j in row.names(res)){
                if (as.integer(res[j,c("Var1")]) == as.integer(res[j,c("Var2")])) aux[j] = res[j,c("Freq")]
            }
            aux <- as.data.frame(aux)
            moy[i]<- sum(aux)/sum(res$Freq)*100
            
            if(is.na(res[1,c("Freq")])){
                nono[i] <- 0
            }
            else {
                nono[i] <- res[1,c("Freq")]
            }
            
            if(is.na(res[2,c("Freq")])){
                yesno[i] <- 0
            }
            else {
                yesno[i] <- res[2,c("Freq")]
            }
            
            if (is.na(res[3,c("Freq")])){
                noyes[i] <- 0
            }
            else {
                noyes[i] <- res[3,c("Freq")]
            }
            
            if(is.na(res[4,c("Freq")])){
                yesyes[i] <- 0
            }
            else {
                yesyes[i] <- res[4,c("Freq")]
            }
        }
        restab <- data.frame(
            Var1 = c(0,1,0,1),
            Var2 = c(0,0,1,1),
            Freq = c(mean(nono),mean(yesno),mean(noyes),mean(yesyes))
        )
        v$resultData = sum(restab$Freq * resultats$cost) * 5
        
        
        v$accuracy <<- mean(moy)
        v$accuracyTab <<- moy
        
        # Naive Bayes CrossValidation #
        
        newtab <- switch(input$sidebarmenu,
                         "costsconfig" = "results",
                         "results" = "costsconfig"
        )
        updateTabItems(session,"sidebarmenu", newtab)
    })
    
    # Clear All and return button --------------------------------------------
    
    output$clearallreturnstep3 <- renderUI({
        actionButton("clearallreturnstep3","Clear All and return Step 1")
    })
    observeEvent(input$clearallreturnstep3,{
        v$data = NULL
        v$dataframestep1 = NULL
        v$dataframestep2 = NULL
        v$dataframestep3 = NULL
        v$dataframestep4 = NULL
        v$columnSelected = NULL
        v$resultData = NULL
        v$accuracy = NULL
        v$accuracyTab = NULL
        newtab <- switch(input$sidebarmenu,
                         "costsconfig" = "initialisation",
                         "initialisation" = "costsconfig"
        )
        updateTabItems(session,"sidebarmenu", newtab)
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Selections ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    # Cost 1 --------------------------------------------
    output$cost1 <- renderUI({
        numericInput("cost1","Cost Yes -> No",value = 5)
    })
    
    # Cost 2 --------------------------------------------
    output$cost2 <- renderUI({
        numericInput("cost2","Cost No -> Yes",value = 10)
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    # Table from CSV 3 --------------------------------------------
    
    output$tabLoadedstep3 <- renderDataTable(
        v$dataframestep3,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    # Costs Tab --------------------------------------------------
    output$costsTab <- renderTable({
        resultats <<- data.frame(Prediction=c("No","Yes","No","Yes"),
                                 Reality=c("No","No","Yes","Yes"),
                                 cost=c(0,input$cost1,input$cost2,0))
    })
    
    #_______________________________________________________ Results _______________________________________________________#
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    # Clear All and return button --------------------------------------------
    
    output$clearallreturnstep4 <- renderUI({
        actionButton("clearallreturnstep4","Clear All and return Step 1")
    })
    observeEvent(input$clearallreturnstep4,{
        v$data = NULL
        v$dataframestep1 = NULL
        v$dataframestep2 = NULL
        v$dataframestep3 = NULL
        v$dataframestep4 = NULL
        v$columnSelected = NULL
        v$resultData = NULL
        v$accuracy = NULL
        v$accuracyTab = NULL
        newtab <- switch(input$sidebarmenu,
                         "results" = "initialisation",
                         "initialisation" = "results"
        )
        updateTabItems(session,"sidebarmenu", newtab)
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    # Table from CSV 4 --------------------------------------------
    
    output$tabLoadedstep4 <- renderDataTable(
        v$dataframestep4,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    # Accuracy CrossValidation ------------------------------------
    
    output$accurancyvalue <- renderValueBox({
        v$accuracy <- round(v$accuracy, digits = 2)
        valueBox(
            value = paste(v$accuracy,"%")
            ,paste('Accuracy :',v$accuracy,"%")
            ,icon = icon("stats",lib='glyphicon')
            ,color = "purple")
        
        
    })
    
    # Accuracy tab CrossValidation ------------------------------------
    
    output$accuracyCVtab <- renderTable ({
        if (!is.null(v$accuracy)) {
            v$accuracyTab
        }
    })
    
    # Accuracy BarChart CrossValidation ------------------------------------
    
    output$accuracyCVbar <- renderPlotly ({
        if (!is.null(v$accuracy)) {
            plot_ly(
                x = c(1:10),
                y = c(v$accuracyTab),
                name = "Bar Chart",
                type = "bar"
            )
        }
    })
    
    # Cost Results -------------------------------------
    
    output$costresultsvalue <- renderValueBox({
        valueBox(
            value = v$resultData
            ,paste('Cost :',v$resultData)
            ,icon = icon("menu-hamburger",lib='glyphicon')
            ,color = "green")
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
