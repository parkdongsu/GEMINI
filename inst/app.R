library(shiny)
library(shinyjs)
library(dplyr)
library(DatabaseConnector)

shinyApp(
  ui <- (navbarPage(
    id='tabs',
    title = 'GEMINI',
    tabPanel('gemini'
             ,useShinyjs()
             ,includeCSS(file.path(.libPaths()[1],'GEMINI','www/gemini.css'))
             
             ,fluidRow(
               column(4
                      ,offset = 2
                      ,align='left'
                      ,h3(textOutput("workingDir"))
               )
             )
             
             ,fluidRow(
               column(4
                      ,offset = 2
                      ,align='left'
                      ,verbatimTextOutput(outputId = 'settingDir',placeholder = T)
               )
               ,column(2
                       ,align='left'
                       ,actionButton('setwd_button','click')
               )
             )
             
             ,fluidRow(
               column(5,offset = 1
                      ,align='center'
                      ,actionButton('Create RDS File',inputId = 'rds_button',width = "100%")
               )
               ,column(5,align='center'
                       ,actionButton('GEMINI',inputId = 'gemini_button',width = "100%")
               )
               ,shinyjs::hidden(
                 div(id='rdsPage'
                     ,column(12,align='left'
                             ,actionButton('back_button','back')
                     )
                     ,fluidRow(
                       column(
                         id='databaseConnect_ui'
                         ,4
                         ,offset = 2
                         ,align='center'
                         ,uiOutput("sqltype")
                         ,textInput("server_ip","Server IP",'',placeholder = '127.0.0.1')
                         ,textInput("dw_db","DW database",'',placeholder = 'cdmName.schema')
                         ,textInput("usr","USER ID",'',placeholder = 'db id')
                         ,passwordInput("pw","PASSWORD",'',placeholder = 'db password')
                         ,actionButton('dbConnection_btn','Connetion')
                       )
                       ,column(
                         id='databaseConnect_log_ui'
                         ,4
                         ,align='center'
                         ,verbatimTextOutput(outputId = 'createRdsLog',placeholder = T)
                         
                       )
                     )
                     
                 )
               )
               ,shinyjs::hidden(
                 div(id='geminiPage',
                     column(12,align='left'
                            ,actionButton('back_button2','back')
                     )
                     
                     ,fluidRow(
                       column(4
                              ,offset = 1
                              ,tags$h3('Target File')))
                     ,fluidRow(
                       column(4
                              ,offset = 1
                              ,align='left'
                              ,verbatimTextOutput(outputId = 'geminiSet_text1',placeholder = T)
                       )
                       ,column(6
                               ,align='left'
                               ,actionButton('geminiSet_button1','click')
                       )
                     )
                     ,fluidRow(
                       column(4
                              ,offset = 1
                              ,align='left'
                              ,verbatimTextOutput(outputId = 'geminiSet_text2',placeholder = T)
                       )
                       ,column(6
                               ,align='left'
                               ,actionButton('geminiSet_button2','click')
                       )
                     )
                     ,fluidRow(
                       column(6
                              ,offset = 1
                              ,align='right'
                              ,actionButton('geminiSetAdd_button1','+')
                       )
                     )
                     ,fluidRow(
                       column(6
                              ,offset = 1
                              ,align='right'
                              ,actionButton('geminiAnalysis_button','Start')
                       )
                     )
                     
                 )
               )
             )
    )
    
    
  )),
  
  server <- (function(input, output) {
    
    ##Main Page UI###########################################################################################
    output$workingDir <- renderText({
      "Working Directory : "
    })
    
    observeEvent(input$setwd_button,{
      tempWd <<- choose.dir()
      if(is.na(tempWd)){
        workDir <- 'C:'
      }
      else{
        workDir <- tempWd
      }
      setwd(workDir)
      output$settingDir <- renderText({ workDir })
      
    })
    
    output$workingDir <- renderText({
      "Working Directory : "
    })
    
    ## hide, show UI###########################################################################################
    observeEvent(input$rds_button,{
      shinyjs::hide('rds_button')
      shinyjs::hide('gemini_button')
      shinyjs::hide('workingDir')
      hide('settingDir')
      hide('setwd_button')
    })
    observeEvent(input$gemini_button,{
      hide('rds_button')
      hide('gemini_button')
      hide('workingDir')
      hide('settingDir')
      hide('setwd_button')
    })
    shinyjs::onclick("rds_button",
                     shinyjs::toggle(id = "rdsPage", anim = F))
    shinyjs::onclick("gemini_button",
                     shinyjs::toggle(id = "geminiPage", anim = F))
    observeEvent(input$back_button,{
      shinyjs::show('rds_button')
      shinyjs::show('gemini_button')
      shinyjs::show('workingDir')
      shinyjs::show('settingDir')
      shinyjs::show('setwd_button')
    })
    observeEvent(input$back_button2,{
      shinyjs::show('rds_button')
      shinyjs::show('gemini_button')
      shinyjs::show('workingDir')
      shinyjs::show('settingDir')
      shinyjs::show('setwd_button')
    })
    shinyjs::onclick("back_button",
                     shinyjs::toggle(id = "rdsPage", anim = F)
    )
    shinyjs::onclick("back_button2",
                     shinyjs::toggle(id = "geminiPage", anim = F)
    )
    
    
    ##Gemini Page UI###########################################################################################
    
    defalutFilePathValue <<- c()
    
    output$geminiSet_text1 <- renderText({
      defalutFilePathValue[1] <<- 'C:/gemini/gemini_RDS/cdm1'
    })
    output$geminiSet_text2 <- renderText({
      defalutFilePathValue[2] <<- 'C:/gemini/gemini_RDS/cdm2'
    })
    
    
    observeEvent(input$geminiSet_button1,{
      targetFile <- choose.files()
      output$geminiSet_text1 <- renderText({
        targetFile
      })
      defalutFilePathValue[1] <<- targetFile
    })
    
    observeEvent(input$geminiSet_button2,{
      targetFile <- choose.files()
      output$geminiSet_text2 <- renderText({
        targetFile
      })
      defalutFilePathValue[2] <<- targetFile
    })
    
    
    observeEvent(input$geminiSetAdd_button1, {
      insertUI(
        selector = "#geminiSetAdd_button1",
        where = "beforeBegin",
        ui = tags$div(
          fluidRow(class=paste0('filePathClass',input$geminiSetAdd_button1),
                   column(8,align='left'
                          ,verbatimTextOutput(paste0("txt", input$geminiSetAdd_button1),placeholder = T)
                   )
                   ,column(2
                           ,align='left'
                           ,actionButton(paste0('fileAddButton',input$geminiSetAdd_button1),'click')
                   )
                   ,column(2
                           ,align='left'
                           ,actionButton(paste0('removeButton',input$geminiSetAdd_button1),'-')
                   )
          )
        )
      )
      
    })
    filePathValue <<- data.frame(stringsAsFactors = F)
    lapply(1:100, function(i) {
      observeEvent(input[[paste0('fileAddButton', i)]],{
        targetFile <- choose.files()
        output[[paste0('txt', i)]] <- renderText({ targetFile })
        
        pathTempDataFrame <- data.frame("num" = i,"path" = targetFile,stringsAsFactors = F)
        filePathValue <<- rbind(filePathValue,pathTempDataFrame)
      })
    }
    )
    
    lapply(1:100, function(i) {
      observeEvent(input[[paste0('removeButton', i)]],{
        removeUI(selector = paste0("#geminiPage > div > .col-sm-6.col-sm-offset-1 > div > ",".row.filePathClass",i))
        if(i %in% filePathValue$num){
          filePathValue <<- filePathValue[-c(which(filePathValue$num == i)),]
        }
      })
    }
    )
    
    observeEvent(input$geminiAnalysis_button,{
      analysisFilePath <<- c(defalutFilePathValue,filePathValue$path)
    })
    
    
    ##Gemini Logic
    
    
    ##Create RDS File UI
    output$createRdsLog <- renderText({
      'my Log!'
    })
    ##Create RDS File Logic
    
    
    
    #Choose DBMS
    output$sqltype <- renderUI({
      selectInput("sqltype", "Select DBMS",
                  choices = c(
                    "sql server" = "sql server"
                  )
      )
    })
    
    
    # connecting to the Database
    DBconnection <- reactive({
      connectionDetails <<- DatabaseConnector::createConnectionDetails(server = input$server_ip
                                                                       ,dbms = input$sqltype
                                                                       ,user = input$usr
                                                                       ,password = input$pw
                                                                       ,schema = input$dw_db)
      
      connection <<- DatabaseConnector::connect(connectionDetails = connectionDetails)
    })
    
    
    observeEvent(input$dbConnection_btn,{
      DBcon <<- DBconnection()
      if(exists("connection")==TRUE){
        showModal(modalDialog(
          title = "Messeage", "Database connection success!!", easyClose = T, footer=modalButton("cancel"), size = "l"
        ))
      }
      
      gemini::create_rds(connectionDetails,getwd())
      
    })
    
    observeEvent(input$geminiAnalysis_button,{
      
      gemini::gemini(dbCount = length(analysisFilePath),work_dir = getwd(),analysisFilePath = analysisFilePath)
      
    })
    
    
  })
)

