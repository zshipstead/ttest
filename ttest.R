library(shiny)
library(ggplot2)
library(car)
library(tidyverse)
#library(DBI)


      #### Templates for final dashboard...see "Page 5" section for calls

  ## Create Instance of Paired Tests Display
pairedTestResultsOutput <- function(testNum) {
  fluidRow(
    column(5,
           h3(textOutput(paste0("headerA", testNum))),
           br(),
           strong("T-Test Results"),
           tableOutput(paste0("testResults", testNum)),
           br(),
           strong("Bonferroni Adjusted Alpha"),
           tableOutput(paste0("bonfA", testNum)),
           br(),
           strong("Descriptive Statistics"),
           tableOutput(paste0("descG1", testNum))
          ),
    column(7,
           # plotOutput(paste0("figure", testNum))
           tabsetPanel(type = "tabs",
                       tabPanel("Bar Chart (Distinct Conditions)", plotOutput(paste0("bFigure", testNum))),
                       tabPanel("Line Chart (Change Over Time)", plotOutput(paste0("lFigure", testNum))),
                       tabPanel("Box Plot", plotOutput(paste0("boxFigure", testNum)))
           )
          ),
    )
}

  ## Create Instance of Paired Tests Display
indepTestResultsOutput <- function(testNum) {
  fluidRow(
    column(5,
           h3(textOutput(paste0("headerA", testNum))),
           br(),
           strong("T-Test Results"),
           br(),
           tableOutput(paste0("testResults", testNum)),
           strong("Bonferroni Adjusted Alpha"),
           br(),
           tableOutput(paste0("bonfA", testNum)),
           strong("Levene's Test of Equality of Variances"),
           br(),
           tableOutput(paste0("leveneResults", testNum)),
           strong("Descriptive Statistics"),
           tableOutput(paste0("descG1", testNum))
    ),
    column(7,
           h1(br()),
           tabsetPanel(type = "tabs",
                       tabPanel("Bar Chart", plotOutput(paste0("bFigure", testNum))),
                       tabPanel("Box Plot", plotOutput(paste0("boxFigure", testNum)))
                       )
           ),
    )
}


## Insert blank lines as needed
blankLine <- function(){
  fluidRow(
    column(12,
           h1(br())
           )
  )
}

      ## End templates ##################

ui <- fluidPage(
  tabsetPanel(
    id = "start",
    type = "hidden",
    tabPanel("page1", div(h3("Welcome to the t-test Application"), align = "center"),
      br(),
      radioButtons("dataOrig", "Upload Data or Use Existing Set?",
                choices = c("Upload My Data (.csv only)" = "2a", "Use an Example Data Set" = "2b"),
                selected = character(0)),
      uiOutput("getStarted")
      ),
    tabPanel("2a", div(h3("Upload .csv File"), align = "center"),
      br(),
      fileInput("upload", "Upload: ", accept = ".csv"),
      textOutput("p2aMess"),
      fluidRow(
        column(1, actionButton("p2ap1", "Go Back")),
        column(1, uiOutput("validFile"))
        ),
      fluidRow(
        column(10,
              offset = 1,
              dataTableOutput("preview"))
      )
    ),
    tabPanel("2b", div(h3("Example Data"), align = "center"),
      br(),
      fluidRow(
        column(2, radioButtons("chooseExample", "Select Data Set",
                               choiceNames = c("Two Groups", "Three Groups", "Eight Groups"),
                               choiceValues = c("TwoGroups", "ThreeGroups", "EightGroups"),
                               selected = character(0))),
        column(10,
               offest = 2,
               wellPanel(
                 textOutput("explainData")
               ))
      ),
      fluidRow(
        column(1, 
               actionButton("p2bp1", "Go Back")),
        column(1, 
               uiOutput("dataSelected"))
      ),
      fluidRow(
        column(10,
               offset = 1,
               dataTableOutput("examplePrev"))
      ),
      
    ),
    tabPanel("3a", div(h3("Choose Test Type"), align = "center"),
      br(),
      radioButtons("testType", "How Were These Data Collected?",
                  choices = c("Within-Subjects (Paired Sample T-Test)" = "TRUE",
                              "Between-Subjects (Independent Samples T-Test)" = "FALSE"),
                  selected = character(0),
                  width=600,
                  ),
      fluidRow(
        column(1, 
               actionButton("p3ap2a", "Go Back")),
        column(1, 
               uiOutput("goTtest"))
        ),
      ),
    tabPanel("3b", div(h3("What Type of T-Test Would you like to Apply to These Data?"), align = "center"),
        br(),
        fluidRow(
          column(4,
                 radioButtons("testTypeB", "Select Test Type",
                              choices = c("Paired Sample T-Test" = "TRUE",
                                          "Independent Samples T-Test" = "FALSE"),
                              selected = character(0),
                              
                              width=600,)),
          column(8,
                 #offset = 1,
                 wellPanel(
                 textOutput("explainTests"))
          )
        ),
        fluidRow(
          column(1, 
                 actionButton("p3bp2b", "Go Back")),
          column(1, uiOutput("goTtestB"))            
        )
             
      ),
    tabPanel("4a", div(h3("Select Variables for T-Test"), align = "center"),
        br(),
        selectInput("PSdv", "Dependent Variable",
                    choices = NULL),
        selectInput("PSiv", "Independent Variable",
                    choices = NULL),
        fluidRow(
          column(4, 
                 offset = 1, 
                 uiOutput("testOptions"),
                 textOutput("levelsWarn")
                ),
        ),
        fluidRow(
          column(1,
                 actionButton("p4ap3a", "Go Back")),
          column(1,
                 uiOutput("goToTest")
                 )
        )
      ),
    tabPanel("5", div(h3("T-Test Results"), align = "center"),
      br(),
      actionButton("p5p4a", "Go Back"),
      uiOutput("varTests"),
      )
  )
)


server <- function(input, output, session) {


  ##############
  ### Page 1 ###
  ##############
  
    ## Create Go-Button After Radio Button Selected
  output$getStarted <- renderUI({
    req(input$dataOrig)
    actionButton("gstd", "Go")
  })

    ## Stores State of Radio Button to Select Data Origin
  p1decide <- reactive({
    req(input$dataOrig)
    input$dataOrig
  })  
  
    ## Perform Database Query and Store Selected Dataset for use
  selectedData <- reactive({
      ##  Code for reading example data from a database
    # showNotification("Accessing Data", id = "Note1", type = "message", duration = NULL)
    # con <- dbConnect(RPostgres::Postgres(),dbname = , 
    #                  host = , 
    #                  port = , 
    #                  user = ,
    #                  password = ')
    # res <- dbSendQuery(con, "SELECT dataset, independentvar, dependentvar FROM ttest")
    # queryRes <-dbFetch(res)
    # dbClearResult(res)
    # dbDisconnect(con)
    # removeNotification("Note1")
    # as.tibble(queryRes)
    
      ##  For reading local example data from .csv
    as.tibble(read.csv("ttest.csv"))
  })
  
    ## When Go-Button Selected, Change Page Using p1decide()
  observeEvent(input$gstd, {
    updateTabsetPanel(inputId = "start", selected = p1decide())
    if (p1decide() == "2b") {
      selectedData()
      output$explainData <- renderText("Select a Data Set for Description")
    }
  })
  
  ###############
  ### Page 2a ###
  ###############
  
    ## Page navigation: Go back
  observeEvent(input$p2ap1, {
    updateTabsetPanel(inputId = "start", selected = "page1")
  })
  
    ## Page navigation: Go forward
  observeEvent(input$chooseTest, {
    updateTabsetPanel(inputId = "start", selected = "3a")
  })
  
  
  
  
    ##Verify that upload is .csv, if so, create "next" button
  dataup <- reactive({
    if (p1decide() == "2a") {
      req(input$upload) 
      ext <- tools::file_ext(input$upload$name)
      if (ext == "csv") {
        ###Next page button----------------
        output$validFile <- renderUI({  
          req(input$upload)
          actionButton("chooseTest", "Next")
          })
        ###return data----------------------
        vroom::vroom(input$upload$datapath, delim = ",")  
        } else validate("Invalid File: Please Select a .csv file")
    } else {
      selectedData() %>% filter(selectedData()$dataset == input$chooseExample)
    }
  })
  
  ##Display data preview
  output$preview <- renderDataTable(
    dataup(), options = list(searching = FALSE, paging = FALSE, scrollY=300, scrollX=800))
  
  
  
  ###############
  ### Page 2b ###
  ###############
  
    ## Go back 
  observeEvent(input$p2bp1, {
    updateTabsetPanel(inputId = "start", selected = "page1")
  })
  
    ## Create button to go to test selection
  output$dataSelected <- renderUI({
    req(input$chooseExample)
    actionButton("p2bp3b", "Next")
    
  })
  
    ## Go to test selection page
  observeEvent(input$p2bp3b, {
    updateTabsetPanel(inputId = "start", selected = "3b")
  })
  
    ## Choice made, show preview table and explain selected data 
  observeEvent(input$chooseExample, {
    output$examplePrev <- renderDataTable(
      dataup(), options = list(searching = FALSE, paging = FALSE,
                                      scrollY = 300, scrollX = 800)
    )
  
    output$explainData <- renderText(
      case_when(
        input$chooseExample == "TwoGroups" ~ "This simple data set contains two groups. Thus it allows for one t-test to be performed, which would be equivalent to a one-way ANOVA",
        input$chooseExample == "ThreeGroups" ~ "This data set contains three groups. It therefore allows 3 t-tests to be performed, which is a fairly standard follow-up analysis after an ANOVA has been performed",
        input$chooseExample == "EightGroups" ~ "This data set contains eight groups. It can therefore be used to perform more t-tests than a reasonalbe study would need. These data can be used to examine the flexibility of the final display."
      )
    )
  })
  
  
  
  ###############
  ### Page 3a ###
  ###############
  
    ## Create Action Button and Navigation
  output$goTtest <- renderUI({
    req(input$testType)
    actionButton("testTime", "Do T-Test")
  })

  
    ## Go back
  observeEvent(input$p3ap2a, {
    updateTabsetPanel(inputId = "start", selected = "2a")
  })
  
    ## Pass column names to selectInputs on Page 4a, and move to appropriate tab
  observeEvent(input$testTime, {
      #restrict dependent variables to numeric
    numVars <- unlist(lapply(dataup(), is.numeric))
    updateSelectInput(inputId = "PSdv", choices = c("_", colnames(dataup()[,numVars])))
      #restrict independent variables to >1 levels
    validIVs <- unlist(lapply(lapply(dataup(), as.factor), nlevels)) > 1
    updateSelectInput(inputId = "PSiv", choices = c("_", colnames(dataup()[,validIVs])))
    updateTabsetPanel(inputId = "start", selected = "4a")
  })
  
  ###############
  ### Page 3b ###
  ###############
  
    ## Go back 
  observeEvent(input$p3bp2b, {
    updateTabsetPanel(inputId = "start", selected = "2b")
  })
  
    ## Create button to proceed to variable selection screen
  output$goTtestB <- renderUI({
    req(input$testTypeB)
    actionButton("testTimeB", "Do T-Test")
  })
  
    ##Select test-type and provide description of current selection. 
  observeEvent(input$testTypeB, {
    output$explainTests <- renderText(
      case_when(
        input$testTypeB == "TRUE" ~ "Paired Samples: In this case data are run under the 
                                    assumption that the same people are re-tested in each 
                                    of the conditions (i.e., within-subjects data). The final 
                                    display allows choice between bar (distinct conditions ) 
                                    and line (change over time) charts",
        input$testTypeB == "FALSE" ~ "Independent Samples: In this case data are run as if different 
                                    people participanted in each condition (i.e., between subjects data). 
                                    This will trigger tests to ensure that the different groups are 
                                    equally variable. If they are not equal, appropriate corrections
                                    will be made."
      )
    )
  })
  
    ## Advance to variable selection screen. Provide potential variable names
    ## updateRadioButtons sets the test type using the equivalent button from path A
  observeEvent(input$testTimeB, {
    updateRadioButtons(session, inputId = "testType", selected = input$testTypeB)
      ## restrict dependent variable to numeric choices
    numVars <- unlist(lapply(dataup(), is.numeric))
    updateSelectInput(inputId = "PSdv", choices = c("_", colnames(dataup()[,numVars])))
      ## restrict independent variable to colums with at least 2 levels
    validIVs <- unlist(lapply(lapply(dataup(), as.factor), nlevels)) > 1
    updateSelectInput(inputId = "PSiv", choices = c("_", colnames(dataup()[,validIVs])))
    updateTabsetPanel(inputId = "start", selected = "4a")
    
  })
  
  
  ###############
  ### Page 4a ###
  ###############
  
    ## Navigation: Go Back
  observeEvent(input$p4ap3a, {
    output$levelsWarn <- renderText(" ")
    if (p1decide() == "2a") updateTabsetPanel(inputId = "start", selected = "3a")
    else updateTabsetPanel(inputId = "start", selected = "3b")
  })
  

    ## Button to proceed to t-test...see top of section 5 for engagement.
  output$goToTest <- renderUI({
    switch((input$PSdv %in% colnames(dataup()) && 
              (input$PSiv %in% colnames(dataup())) &&
              (!is.na(input$testOptions))),
           "TRUE" = actionButton("goToTest", "T-Test"),
           "FALSE" = uiOutput("goToTest"))
  })
  
  # Restrict SelectInput choices
  observeEvent({
    input$PSdv
    input$PSiv
    }, {
      sVars2 <- NULL
        ## IV must have at least 2 levels
      sVarsLevels <- unlist(lapply(lapply(dataup(), as.factor), nlevels)) > 1
      sVarsIV <- colnames(dataup()[sVarsLevels])
        ## DV must be numeric
      sVarsNums <- unlist(lapply(dataup(), is.numeric))
      sVarsDV <- colnames(dataup()[,sVarsNums])
        ## List of already-selected vars
      sVars2 <- append(sVars2, input$PSdv)
      sVars2 <- append(sVars2, input$PSiv)
      updateSelectInput(inputId = "PSdv",
                        choices = c(input$PSdv, sVarsDV[sVarsDV %in% sVars2 == FALSE], "_"))
      updateSelectInput(inputId = "PSiv",
                        choices = c(input$PSiv, sVarsIV[sVarsIV %in% sVars2 == FALSE], "_"))
      res <- try(equalCasesTest(), silent = TRUE)
      if (class(res) != "try-error")
        if (equalCasesTest() == "TRUE") output$levelsWarn <- renderText(" ")
  })
  
    ## Verification that within-subjects levels appear equal times
  equalCasesTest <- reactive({
    levelCount <- count(as.data.frame(dataup()[[input$PSiv]]), dataup()[[input$PSiv]])
    missingCount <- count(as.data.frame(dataup()[[input$PSdv]]), is.na(dataup()[[input$PSdv]]))
    if (var(levelCount$n) == 0 && is.na(missingCount[2,2])) "TRUE"
    else "FALSE"
  })
  
    ## Generate List of Potential T-Tests
  possTests <- reactive({
     ## If this is within-subjects, first verify that the data have equal cases
    if (input$testType == "TRUE") equalCases <- equalCasesTest()
      else equalCases <- "TRUE"
    
    if (equalCases == "TRUE"){
      ivCombos <- NULL
      ivLevels <- unique(dataup()[[input$PSiv]])
      for (i in 1:(length(ivLevels) -1))
         for (j in (i+1):length(ivLevels))
          ivCombos <- append(ivCombos, paste0(ivLevels[i], "  vs. ", ivLevels[j]))
      ivCombos
    } else {
      output$levelsWarn <- renderText("Warning: Unequal number of cases per level of the 
                                      selected indepednent variable and/or missing dependent
                                      variable information. This is inconsistent with
                                      within-subjects data. Please verify your data, select a
                                      different variable, or select a different type of test.")
      NULL
    }
  })

    ## Let user specify t-tests to be performed  
  output$testOptions <- renderUI({
    switch(input$PSiv %in% colnames(dataup()),
      "TRUE" = checkboxGroupInput("testOptions", "T-Tests to Perform (check each)", choices = possTests()),
      "FALSE" = uiOutput("testOptions")
    )
  })
 
  ##############
  ### Page 5 ###
  ##############
  
  ## Navigation  
  ## Enter and initialize page 5.
  observeEvent(input$goToTest, {
    updateTabsetPanel(inputId = "start", selected = "5") 
    
    # Make template for display
    x <- NULL
    ##  testType = TRUE means "paired tests" selected.
    if (input$testType == "TRUE") {
      for (i in 1:length(input$testOptions)){
        x <- c(x, pairedTestResultsOutput(i)[3])
        x <- c(x, blankLine()[3])
      }
    }else{
      for (i in 1:length(input$testOptions)){
        x <- c(x, indepTestResultsOutput(i)[3])
        x <- c(x, blankLine()[3])
      }
    }
    
    # Make display
    output$varTests <- renderUI({
      x
    })
    
    # Populate display
    for  (j in 1:length(input$testOptions)) {
      displayTests(j)
    }
    
  })
  
  
  
  # Present results of t-tests
  displayTests <- function(j){
    # Make names
    headA <- paste0("headerA", j)
    levR <- paste0("leveneResults", j)
    tr <- paste0("testResults", j)
    BA <- paste0("bonfA", j)
    # headB <- paste0("headerB", j)
    dG1 <- paste0("descG1", j)
    bFig <- paste0("bFigure", j)
    lFig <- paste0("lFigure", j)
    boxFig <- paste0("boxFigure", j)
    
    # Format output for Levene's test
    levDesc <- storeTests()[j, c(11, 13, 12)]
    colnames(levDesc) <- c("F", "df", "p")
    
    # Create properly formatted descriptives
    g1Desc <- storeTests()[j, c(1,6,7)]
    g1Desc[2,] <- storeTests()[j, c(2,8,9)]
    colnames(g1Desc) <- c("Group", "Mean", "Std Err")
    
    #Bonferroni
    bfa <- data.frame("Bonferroni Adjusted Alpha"=1)
    bfa[1,1] <- storeTests()[j, 10]
    
    # Populate Tables
    output[[headA]] <- renderText(paste0(ifelse(input$testType=="TRUE", "Paired Samples: ",
                                                "Independent Samples: "),
                                         storeTests()[j,1], " vs. ", storeTests()[j,2]))
    # Levene's test of independent groups
    if (input$testType == "FALSE") output[[levR]] <-renderTable(levDesc)
    output[[tr]] <- renderTable(storeTests()[j, c(3,4,5)])
    output[[BA]] <- renderTable(bfa, digits = 5, colnames = FALSE)
    output[[dG1]] <- renderTable(g1Desc)
    
    # Populate plots
    ##  Make data.frame for plot
    plNames <- c(storeTests()[j,1], storeTests()[j,2])
    plScores <- c(storeTests()[j,6], storeTests()[j,8])
    plBars <- c(storeTests()[j,7], storeTests()[j,9])
    plotData <-data.frame(plNames, plScores, plBars)
    
    ## Make bar chart
    output[[bFig]] <- renderPlot(
      ggplot(plotData, aes(y=plScores, x=plNames))+
        geom_bar(stat = "identity", width = .5)+
        geom_hline(yintercept=0)+
        theme_classic()+
        scale_y_continuous(expand = c(0, 0))+
        geom_errorbar(aes (ymin = plScores - plBars, ymax = plScores + plBars, width = .2),
                      position = position_dodge(0.5))+
        ylab(input$PSdv)+
        xlab(input$PSiv)
      )

    ## Make line chart
    output[[lFig]] <- renderPlot(
      ggplot(plotData, aes(y=plScores, x=plNames, group=1))+
        geom_line()+
        geom_point()+
        theme_classic()+
        expand_limits(y = 0)+
        scale_y_continuous(expand = c(0, 0))+
        geom_errorbar(aes (ymin = plScores - plBars, ymax = plScores + plBars, width = .2))+
        ylab(input$PSdv)+
        xlab(input$PSiv)
    )
    
    ## Make box plot
    conds <- data.frame()
    conds[1,1] <- sub(".[[:space:]]vs.*", "", input$testOptions[j])
    conds[2,1] <- sub(".*vs. ", "", input$testOptions[j])
    boxData <- data.frame(dataup()[dataup()[[input$PSiv]] == conds[1,1] | dataup()[[input$PSiv]] == conds[2,1],])
    
    output[[boxFig]] <- renderPlot(
      ggplot(boxData, aes(y=.data[[input$PSdv]], as.factor(.data[[input$PSiv]])))+
        geom_boxplot()+
        geom_point()+
        theme_classic()+
        expand_limits(y = 0)+
        scale_y_continuous(expand = c(0, 0))+
        ylab(input$PSiv)+
        xlab(input$PSdv)
    )
    

  }
  

  
  ## Perform t-tests, store relevant information for display
  storeTests <- reactive({
    req(input$goToTest)
    IV <- input$PSiv
    DV <- input$PSdv
    statStore <- data.frame(matrix(ncol = 13, nrow = length(input$testOptions)))
    colnames(statStore) <- c("Group1", "Group2", "t", "df", "p", 
                             "Group1_Mean", "Group1_Std_Error",
                             "Group2_Mean", "Group2_Std_Error", "Bonferroni_Alpha",
                             "levF", "levP", "levDF")
    ## Extract descriptives
    descTable <- psych::describeBy(dataup()[[input$PSdv]],
                                   dataup()[[input$PSiv]],
                                   mat = TRUE)
    
    
    ## Cycle through groups
    for (i in 1 :(length(input$testOptions))){
      
      ## Group names
      levelA <- sub(".[[:space:]]vs.*", "", input$testOptions[i])
      levelB <- sub(".*vs. ", "", input$testOptions[i])
      

      ## For independent pairs, equality of variances
      if (input$testType == "FALSE"){
        leveneData <- dataup() %>% filter(dataup()[[input$PSiv]] == levelA | dataup()[[input$PSiv]] == levelB)
        ltest <- leveneTest(leveneData[[input$PSdv]] ~ as.factor(leveneData[[input$PSiv]]), 
                            center = mean)
        statStore$levP[i] <- ltest$`Pr(>F)`[1]
        statStore$levF[i] <- ltest$`F value`[1]
        statStore$levDF[i] <- ltest$Df[1]
        ifelse (ltest$`Pr(>F)`[1] <= .05, doCorrect <- FALSE, doCorrect <- TRUE)
      } else doCorrect <- TRUE
      
      
      ## Conduct t-test
      tTable <-t.test(dataup()[[input$PSdv]] ~ dataup()[[input$PSiv]], 
                      paired = ifelse(input$testType=="TRUE", TRUE, FALSE), 
                      var.equal = doCorrect,
                      subset = dataup()[[input$PSiv]] %in% c(levelA,levelB))
     
      ## Store results
      statStore$Group1[i] <- levelA
      statStore$Group2[i] <- levelB
      statStore$t[i] <- tTable$statistic
      statStore$df[i] <- tTable$parameter
      statStore$p[i] <- tTable$p.value
      statStore$Group1_Mean[i] <- descTable$mean[descTable$group1==levelA]
      statStore$Group1_Std_Error[i] <- descTable$se[descTable$group1==levelA]
      statStore$Group2_Mean[i] <- descTable$mean[descTable$group1==levelB]
      statStore$Group2_Std_Error[i] <- descTable$se[descTable$group1==levelB]
      statStore$Bonferroni_Alpha[i] <- .05 / length(input$testOptions)
    }
    statStore
  })

    ##Go Back
  observeEvent(input$p5p4a, {
    updateTabsetPanel(inputId = "start", selected = "4a")
  })

  
}

shinyApp(ui, server)












