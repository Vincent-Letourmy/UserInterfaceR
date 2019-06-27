library(shiny)
require(shinydashboard)
library(e1071)
library(mlr)
library(caret)
library(dplyr)
library(plotly)
library(rhandsontable)

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
        
        #__________________________________________________ Initialisation _______________________________________________________________________________________#
        
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
        
        #__________________________________________________ DataQuality Config _______________________________________________________________________________________#
        
        tabItem(
            tabName = "dataqualityconfig",
            sidebarLayout(
                sidebarPanel(
                    h1("Data Quality Config"),
                    tags$hr(),
                    h4("Do you want to remove columns with too many missing values ?"),
                    uiOutput("pourcentageSelection"),
                    uiOutput("removecolumnbutton"),
                    tags$hr(),
                    h4("Then, do you want to remove each row where there is at least one missing value ?"),
                    uiOutput("removeNAsbutton"),
                    tags$hr(),
                    uiOutput("step3button"),
                    tags$hr(),
                    uiOutput("clearallreturnstep2")
                ),
                mainPanel(
                    tabsetPanel(
                        id = "tabset",
                        tabPanel(
                            "Bar Chart",
                            value = "barchart",
                            h3("Pourcentage of missing values in each column"),
                            plotlyOutput("NAsBarChart")
                        ),
                        tabPanel(
                            "DataBase",
                            value = "database",
                            dataTableOutput("tabLoadedstep2")
                        )
                    )
                )
            )
        ),
        
        #____________________________________________________ Costs Config _________________________________________________________________________________________#
        
        tabItem(
            tabName = "costsconfig",
            sidebarLayout(
                sidebarPanel(
                    h1("Costs Config"),
                    tags$hr(),
                    helpText("Editable table"),
                    rHandsontableOutput("costsTab"),
                    actionButton("saveBtn","Save"),
                    downloadButton('downloadData', 'Download'),
                    tags$hr(),
                    uiOutput("foldselection"),
                    uiOutput("step4button"),
                    tags$hr(),
                    uiOutput("clearallreturnstep3")
                ),
                mainPanel(
                    dataTableOutput("tabLoadedstep3")
                )
            )
        ),
        
        #_______________________________________________________ Results ____________________________________________________________________________________________#
        
        tabItem(
            tabName = "results",
                fluidRow(
                    column(6,
                           uiOutput("clearallreturnstep4"),
                           h1("Results"),
                           tags$hr(),
                           uiOutput("accurancyvalue"),
                           tags$hr(),
                           uiOutput("boxBarChar"),
                           tags$hr(),
                           uiOutput("costresultsvalue"),
                           tags$hr()
                           ,
                           dataTableOutput("tabLoadedstep4")
                           ),
                    
                    column(6,
                           uiOutput("compare"),
                           h1("Previous results"),
                           tags$hr(),
                           uiOutput("accurancyvalueSaved"),
                           tags$hr(),
                           uiOutput("boxBarCharSaved"),
                           tags$hr(),
                           uiOutput("costresultsvalueSaved"),
                           tags$hr()
                           ,
                           dataTableOutput("tabLoadedstep4Saved")
                           )
                )
        )
    )
)
ui <- dashboardPage(title = 'Data Quality test - Week 3', header, sidebar, body, skin='blue')


########################################################### Server ##################################################################################################

server <- function(input, output,session) {
    
    #__________________________________________________ Reactive values _________________________________________________________________________________________#
    
    v <- reactiveValues(data = NULL,
                        dataframestep1 = NULL,
                        dataframestep2 = NULL,
                        dataframestep2Bis = NULL,
                        dataframestep3 = NULL,
                        dataframestep4 = NULL,
                        columnSelected = NULL,
                        
                        tabCosts = NULL,
                        resultData = NULL, 
                        accuracy = NULL, 
                        accuracyTab = NULL,
                        resNAsBarChart = NULL,
                        
                        compared = NULL,
                        dataframestep4Saved = NULL,
                        resultDataSaved = NULL, 
                        accuracySaved = NULL, 
                        accuracyTabSaved = NULL,
                        resNAsBarChartSaved = NULL)
    
    #__________________________________________________ Initialisation ___________________________________________________________________________________________#
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    # Clear All button --------------------------------------------
    
    output$clearall <- renderUI({
        actionButton("clearall","Clear")
    })
    observeEvent(input$clearall,{
        v$data = NULL
        v$dataframestep1 = NULL
        v$dataframestep2 = NULL
        v$dataframestep2Bis = NULL
        v$dataframestep3 = NULL
        v$dataframestep4 = NULL
        v$columnSelected = NULL
        
        v$tabCosts = NULL
        v$resultData = NULL
        v$accuracy = NULL
        v$accuracyTab = NULL
        v$resNAsBarChart = NULL
        
        v$compared = NULL
        v$dataframestep4Saved = NULL
        v$resultDataSaved = NULL
        v$accuracySaved = NULL
        v$accuracyTabSaved = NULL
        v$resNAsBarChartSaved = NULL
        
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
        v$dataframestep2Bis <- v$dataframestep1
        
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
    
    #__________________________________________________ DataQuality Config _______________________________________________________________________________________#
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
    
    # Remove All NAs button --------------------------------------------
    
    output$removeNAsbutton <- renderUI({
        if(is.null(v$dataframestep2)) return (NULL)
        actionButton("removeNAsbutton","Remove All NAs")
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
        v$dataframestep2 <- v$dataframestep2[-1,]
        updateTabsetPanel(session, "tabset",
                          selected = "database")
    })
    
    # Step 3 button --------------------------------------------
    
    output$step3button <- renderUI({
        if (is.null(v$dataframestep2)) return (NULL)
        actionButton("step3button","Go Step 3")
    })
    observeEvent(input$step3button,{
        v$dataframestep3 <- v$dataframestep2
        
        v$tabCosts <- data.frame(Prediction=c("No","Yes","No","Yes"),
                                 Reality=c("No","No","Yes","Yes"),
                                 cost=c(0,5,10,0))
        
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
        v$dataframestep2Bis = NULL
        v$dataframestep3 = NULL
        v$dataframestep4 = NULL
        v$columnSelected = NULL
        
        v$tabCosts = NULL
        v$resultData = NULL
        v$accuracy = NULL
        v$accuracyTab = NULL
        v$resNAsBarChart = NULL
        
        v$compared = NULL
        v$dataframestep4Saved = NULL
        v$resultDataSaved = NULL
        v$accuracySaved = NULL
        v$accuracyTabSaved = NULL
        v$resNAsBarChartSaved = NULL
        
        newtab <- switch(input$sidebarmenu,
                         "dataqualityconfig" = "initialisation",
                         "initialisation" = "dataqualityconfig"
        )
        updateTabItems(session,"sidebarmenu", newtab)
    })
    
    # Remove column with NAs according pourcent button --------------------------------------------
    
    output$removecolumnbutton <- renderUI({
        if(is.null(v$dataframestep2)) return (NULL)
        actionButton("removecolumnbutton","Remove column(s) according to your choice")
    })
    observeEvent(input$removecolumnbutton,{
        resColo <- 0
        for (i in names(v$resNAsBarChart)){
            if (v$resNAsBarChart[i] < input$pourcentageSelection) resColo[i] = i
        }
        resColo <- resColo[-1]
        v$dataframestep2 <- v$dataframestep2Bis[,resColo]
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Selections ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    output$pourcentageSelection <- renderUI(
        sliderInput("pourcentageSelection","Choose a pourcentage of NAs max", 0,100,50)
    )
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    # Table from CSV 2 --------------------------------------------
    
    output$tabLoadedstep2 <- renderDataTable(
        v$dataframestep2,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    # Bar chart of NAs pourcentage --------------------------------------------
    
    output$NAsBarChart <- renderPlotly({
            res <- 0
            for (i in names(v$dataframestep2)) {
                col <- v$dataframestep2[,i]
                res[i] = round(sum(is.na(col), na.rm = TRUE) / length(col) * 100,digits = 2)
            }
            v$resNAsBarChart <- res[-1]
            plot_ly(x = names(v$resNAsBarChart), y = v$resNAsBarChart, name = "Pourcentage of NAs in each column", type = "bar")
        
    }
    )
    
    #____________________________________________________ Costs Config _________________________________________________________________________________________#
    
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
        
        for (i in 1:input$foldselection) {
            
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
        v$resultData = sum(restab$Freq * v$tabCosts$cost) * 5 ##############################################################################################################################################
        
        
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
        v$dataframestep2Bis = NULL
        v$dataframestep3 = NULL
        v$dataframestep4 = NULL
        v$columnSelected = NULL
        
        v$tabCosts = NULL
        v$resultData = NULL
        v$accuracy = NULL
        v$accuracyTab = NULL
        v$resNAsBarChart = NULL
        
        v$compared = NULL
        v$dataframestep4Saved = NULL
        v$resultDataSaved = NULL
        v$accuracySaved = NULL
        v$accuracyTabSaved = NULL
        v$resNAsBarChartSaved = NULL
        
        newtab <- switch(input$sidebarmenu,
                         "costsconfig" = "initialisation",
                         "initialisation" = "costsconfig"
        )
        updateTabItems(session,"sidebarmenu", newtab)
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Selections ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    # Slide selection number of fold for crossValidation
    output$foldselection <- renderUI({
        sliderInput("foldselection","Number of fold for crossValidation", 1,50,30)
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    # Table from CSV 3 --------------------------------------------
    
    output$tabLoadedstep3 <- renderDataTable(
        v$dataframestep3,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    # Costs Tab --------------------------------------------------
    output$costsTab <- renderRHandsontable({
        rhandsontable(v$tabCosts)
    })
    observeEvent(input$saveBtn, {
        write.csv(hot_to_r(input$costsTab), file = "MyData.csv",row.names = FALSE)
        v$tabCosts <- as.data.frame(read.csv("MyData.csv"))
        
    })
    
    # DownLoad Button
    
    output$downloadData <- downloadHandler(
        
        # This function returns a string which tells the client
        # browser what name to use when saving the file.
        filename = function() {
            paste("MydataDownload", "csv", sep = ".")
        },
        
        # This function should write data to a file given to it by
        # the argument 'file'.
        content = function(file) {
            # Write to a file specified by the 'file' argument
            write.table(v$tabCosts, file, sep = ",",
                        row.names = FALSE)
        }
    )
    
    #_______________________________________________________ Results ____________________________________________________________________________________________#
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    # Clear All and return button --------------------------------------------
    
    output$clearallreturnstep4 <- renderUI({
        actionButton("clearallreturnstep4","Clear All and return Step 1")
    })
    observeEvent(input$clearallreturnstep4,{
        v$data = NULL
        v$dataframestep1 = NULL
        v$dataframestep2 = NULL
        v$dataframestep2Bis = NULL
        v$dataframestep3 = NULL
        v$dataframestep4 = NULL
        v$columnSelected = NULL
        
        v$tabCosts = NULL
        v$resultData = NULL
        v$accuracy = NULL
        v$accuracyTab = NULL
        v$resNAsBarChart = NULL
        
        v$compared = NULL
        v$dataframestep4Saved = NULL
        v$resultDataSaved = NULL
        v$accuracySaved = NULL
        v$accuracyTabSaved = NULL
        v$resNAsBarChartSaved = NULL
        
        newtab <- switch(input$sidebarmenu,
                         "results" = "initialisation",
                         "initialisation" = "results"
        )
        updateTabItems(session,"sidebarmenu", newtab)
    })
    
    # Compare with other inputs
    
    output$compare <- renderUI({
        actionButton("compare","Compare with other inputs")
    })
    observeEvent(input$compare,{
        
        v$resultDataSaved = v$resultData
        v$accuracySaved = v$accuracy
        v$accuracyTabSaved = v$accuracyTab
        v$resNAsBarChartSaved = v$resNAsBarChart
        v$dataframestep4Saved = v$dataframestep4
        v$compared = TRUE
        
        newtab <- switch(input$sidebarmenu,
                         "results" = "dataqualityconfig",
                         "dataqualityconfig" = "results"
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
        
        res <- v$accuracyTab
        mean <- mean(res)
        error <- qt(0.975,df=length(res)-1)*sd(res)/sqrt(length(res))
        
        left <- mean - error
        right <- mean + error
        
        v$accuracy <- round(v$accuracy, digits = 2)
        valueBox(
            value = paste("Accuracy : ",v$accuracy,"%")
            ,paste('Confidence Interval :',round(left,digits = 1),"%  /  ",round(right,digits = 1),"%")
            ,icon = icon("stats",lib='glyphicon')
            ,color = "purple")
        
        
    })
    
    # Accuracy BarChart CrossValidation ------------------------------------
    
    output$accuracyCVbar <- renderPlotly ({
        if (!is.null(v$accuracy)) {
            plot_ly(
                x = c(1:input$foldselection),
                y = c(v$accuracyTab),
                name = "Bar Chart",
                type = "bar"
            )
        }
    })
    
    output$boxBarChar <- renderUI({
        if (!is.null(v$accuracy)) {
        fluidRow(
            box( width = 12,
                 title = "Accuracy Bar Chart"
                 ,status = "primary"
                 ,solidHeader = TRUE 
                 ,collapsible = TRUE
                 ,collapsed = TRUE
                 ,plotlyOutput("accuracyCVbar")
            )
        )
        }
    })
    
    # Cost Results -------------------------------------
    
    output$costresultsvalue <- renderValueBox({
        result <- round(v$resultData, digits = 0)
        valueBox(
            value = paste("Cost : ",result)
            ,paste('Cost :',result)
            ,icon = icon("menu-hamburger",lib='glyphicon')
            ,color = "green")
    })
    
    ############################## SAVED #################################"
    
    # Accuracy CrossValidation SAVED ------------------------------------
    
    output$accurancyvalueSaved <- renderValueBox({
        if (!is.null(v$accuracyTabSaved)){
        res <- v$accuracyTabSaved
        mean <- mean(res)
        error <- qt(0.975,df=length(res)-1)*sd(res)/sqrt(length(res))
        
        left <- mean - error
        right <- mean + error
        
        v$accuracySaved <- round(v$accuracySaved, digits = 2)
        valueBox(
            value = paste("Accuracy : ",v$accuracySaved,"%")
            ,paste('Confidence Interval :',round(left,digits = 1),"%  /  ",round(right,digits = 1),"%")
            ,icon = icon("stats",lib='glyphicon')
            ,color = "purple")
        }
        
    })
    
    # Accuracy BarChart CrossValidation SAVED ------------------------------------
    
    output$accuracyCVbarSaved <- renderPlotly ({
        if (!is.null(v$accuracySaved)) {
            plot_ly(
                x = c(1:input$foldselection),
                y = c(v$accuracyTabSaved),
                name = "Bar Chart",
                type = "bar"
            )
        }
    })
    
    output$boxBarCharSaved <- renderUI({
        if (!is.null(v$accuracySaved)) {
            fluidRow(
                box( width = 12,
                     title = "Accuracy Bar Chart"
                     ,status = "primary"
                     ,solidHeader = TRUE 
                     ,collapsible = TRUE
                     ,collapsed = TRUE
                     ,plotlyOutput("accuracyCVbarSaved")
                )
            )
        }
    })
    
    # Cost Results SAVED -------------------------------------
    
    output$costresultsvalueSaved <- renderValueBox({
        result <- round(v$resultDataSaved, digits = 0)
        valueBox(
            value = paste("Cost : ",result)
            ,paste('Cost :',result)
            ,icon = icon("menu-hamburger",lib='glyphicon')
            ,color = "green")
    })
    
    # Table from CSV 4 SAVED --------------------------------------------
    
    output$tabLoadedstep4Saved <- renderDataTable(
        v$dataframestep4Saved,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
