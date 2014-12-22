
## IF cpu was loaded before .... do we want to unload it?
## checking if package opencpu is loaded or not
#if ("package:opencpu" %in% search()){
#  detach("package:opencpu",unload=TRUE) 
#}else{
#  library(opencpu)
#}

library(opencpu)
library(httr)
library(jsonlite)
library(distcomp)
library(shiny)
library(digest)


shinyServer(function(input, output, session) {
  
  #verify whether file has extension "RDS"
  checkfileISrds<-function(fileName){
    checkfileName = tryCatch({
      fileName <- readRDS(fileName)
    },  
    warning=function(fileName) {  
      return(fileName)
    },
    error=function(fileName) {  
      return(fileName)
    })
  }
  
  #Read the project file 
  readProject <- reactive({      
    if(input$uploadPro == 0) return()     
    isolate({      
      shiny::validate(
        need(input$projectfile != "", "Please select a project definition file.")
      )              
      inFile <- input$projectfile
      if (is.null(inFile))
        return(NULL)           
      #check file is indeed RDS    
      checkRDS <<- checkfileISrds(inFile$datapath) 
      #if there is an error reading the file
      if ('message' %in% names(checkRDS)){
        proObj <- checkRDS
      }else{
        #there's no error and the file is indeed an RDS
        proObj <- readRDS(inFile$datapath)
      }
      return(proObj)
    })
  })
  
  #print the contents of object project (summary)
  contentInput <- reactive({   
    if(input$uploadPro == 0) return()  
    isolate({
      proObj1<<-readProject() 
      if (!('message' %in% names(checkRDS))){
        printProSumm(proObj1)
      }else{            
        cat('Your data file does not seem to be a \'.RDS\' file.\n')
        if (class(checkRDS)[2]=="error"){
          cat('Error:\n',checkRDS$message)            
        }else{
          cat('Warning:\n',checkRDS$message)
        }   
        print(checkRDS)
      }      
    })    
  })  
  
  # prints the contents of the RDS file (when it's an actual project)
  printProSumm <- function(proObj){
    cat("Project Unique ID:", proObj$UniqueID, "\n")
    cat("Project Name: ", proObj$ProjectName,"\n")
    cat("Project Description: ", proObj$ProjectDesc,"\n")
    cat("Type of computation: ", proObj$TypeComp,"\n")
    cat("Formula: ", proObj$Formula,"\n")
    cat ('Variables needed are: ')
    numvar = length(proObj$Variables)    
    for (i in 1: numvar){
      cat (proObj$Variables[[i]]$Name,' ')
    }  
  }
  
  output$contentPro <- renderPrint({
    contentInput()
  })
  
  output$fileRDS <- reactive({
    fileisRds <- 0
    proObj2<-readProject()
    if (exists("checkRDS")){
      if (!('message' %in% names(checkRDS))){
        fileisRds <- 1
      }
    }
    return(fileisRds)
  })    
  outputOptions(output, "fileRDS", suspendWhenHidden=FALSE) 
  
  output$contentData <- renderPrint({
    contentInputData()
  })
  
  #Data Upload/print summary  
  #verify whether file has extension "CSV"
  checkfileIScsv<-function(fileName){
    checkfileName = tryCatch({
      fileName <- read.csv(fileName)
    },  
    warning=function(fileName) {  
      return(fileName)
    },
    error=function(fileName) {  
      return(fileName)
    })
  }
  
  
  #Create data frame
  createDF <- reactive({      
    if(input$uploadData == 0) return()     
    isolate({      
      shiny::validate(
        need(input$datafile != "", "Please select a data set")
      )              
      inFile2 <- input$datafile
      if (is.null(inFile2))
        return(NULL)           
      nasNocommas=gsub("\\s","", input$naIn)
      navals<-strsplit(nasNocommas,",")[[1]]
      navector<-c(navals[1:length(navals)])
      #check file is indeed CSV    
      checkCSV <<- checkfileIScsv(inFile2$datapath) 
      if (class(checkCSV)[[1]]=="data.frame"){
        df <- read.csv(inFile2$datapath, na.strings = navector)          
      }else{
        df <- checkCSV
      }            
      return(df)
    })    
  })
  
  
  #print the data frame (summary)
  contentInputData <- reactive({   
    if(input$uploadData == 0) return()  
    isolate({
      dfuser<<-createDF() 
      if (is.data.frame(dfuser)){
        writeLines(paste(str(dfuser), collapse= "\n"))  
      }else{            
        cat('Your data file does not seem to be a \'csv\' file.\n')
        if (class(checkCSV)[2]=="error"){
          cat('Error:\n',checkCSV$message)            
        }else{
          cat('Warning:\n',checkCSV$message)
        }   
        print(checkCSV)
      }      
    })    
  })
  
  
  output$fileCSV <- reactive({
    fileisCSV <- 0
    df22<-createDF()
    if (exists("checkCSV")){
      if (class(checkCSV)[[1]]=="data.frame"){
        fileisCSV <- 1
      }
    }
    return(fileisCSV)
  })    
  outputOptions(output, "fileCSV", suspendWhenHidden=FALSE) 
  
  output$remindFormula <- renderPrint({
    proO<-readProject() 
    cat("The project formula is: ", proO$Formula) 
    })
  
  
  output$uiMatch <- renderUI({
    proAgain<-readProject()   
    numVar1=length(proAgain$Variables)
    names2Match=numeric(0)
    for (k in 1: numVar1){
      names2Match=c(names2Match,proAgain$Variables[[k]]$Name)
    }
    dfuserVarChoices<-createDF() ## this allows for changes on the fly
    obj2 <- names2Match ## obj2 is a shorter name
    lapply(1:length(obj2), function(i) {
      selectInput(noquote(obj2[i]), strong(paste0('Select ', noquote(obj2[i]))),
                                  choices = names(dfuserVarChoices))      
      })     
  })
  
  
  output$varMap <- renderPrint({
    
    proAgain<-readProject()  
    
    varsInForm = all.vars(as.formula(proAgain$Formula))
    numVar1=length(proAgain$Variables)
    names2Match=numeric(0)
    for (k in 1: numVar1){
      names2Match=c(names2Match,proAgain$Variables[[k]]$Name)
    }
    obj2 <- names2Match
    for (j in 1:numVar1){
      cat(varsInForm[j],":", input[[ obj2[j] ]],"\n") ## works but with a lot of spaces
    }
    
    })
  
  ##AWESOME function "without"
  "%w/o%" <- function(x, y) x[!x %in% y] #--  x without y  
  #count how many 'valid' rows are in the data frame
  checkEmptySet <- function(newDF){      
    indx2rm=numeric(0)
    allRows=c(1:nrow(newDF))  
    for (i in 1:nrow(newDF)){  
      if (any(is.na(newDF[i,]))){
        ## save index of row to remove
        indx2rm=c(indx2rm,i)
      }    
    }  
    ## remove rows with NA from newDF (data frame for the specific formula)
    validRows=allRows %w/o% indx2rm  
    validRowsLen = length(validRows)   
    ## add different messages for different values of the validRowslen
    return(validRowsLen)  
  }
  
  
  ## create a function that receives as input the project object (.RDS)
  ## and return a vector with the NAMES in the formula
  getProjectFormulaNames <- function(projectObject){
    varsInFormula <- all.vars(as.formula(projectObject$Formula))
    numVars <-length(projectObject$Variables)
    names2Match <- numeric(0)
    for (k in 1: numVars){
        names2Match=c(names2Match,projectObject$Variables[[k]]$Name)
    }     
    namesInProFormula <- names2Match ## this vector contains the NAMES in the PROJECT FORMULA
  
    return(namesInProFormula)
  }
  
  
  ## create a function that receives as input the vector of NAMES in PROJECT
  ## return CHOICES of USER
  
  getUserChoices<-function(namesInFormula){
    namesChosen<-numeric(0)
    for (k in 1:length(namesInFormula)){
        namesChosen<-c(namesChosen, input[[ namesInFormula[k] ]])        
    }
    return(namesChosen)
  }
  
  
  output$validRows <- renderPrint({
    if(input$checkEmpty == 0) return()   
    isolate({   
        dfuserValRows<-createDF() ## create it again to allow for changes on the fly
        proAgain<-readProject()                  
        ## this vector contains the NAMES in the PROJECT FORMULA
        obj2<-getProjectFormulaNames(proAgain)
        ## this vector contains the USER CHOICES for the NAMES in the formula
        chosenColumns<-getUserChoices(obj2)    
        dfDataUser1 <- dfuserValRows[,chosenColumns]
        valrows=checkEmptySet(dfDataUser1)         
        if(valrows==0){
                cat(sprintf('The data frame for your formula is empty!\n')) 
            }else if (valrows==1){
                cat(sprintf('The data frame for your formula has only %i valid row.\n', valrows))        
            }else{      
                cat(sprintf('The data frame for your formula has %i valid rows.\n', valrows))
      }      
    })    
  })
  
  
  checkNumRowsAgain<-reactive({   
    if(input$checkEmpty == 0) return()  
    isolate({  
        valRows<<-0
        dfNonEmptyCheck <- createDF()
        proObjRowsCheck<-readProject() 
        objFormNames<-getProjectFormulaNames(proObjRowsCheck)
        userChoices<-getUserChoices(objFormNames)         
        dfNonEmptySet <- dfNonEmptyCheck[,userChoices]   
        validRowsNewDF <- checkEmptySet(dfNonEmptySet)       
        if (validRowsNewDF>1){
                valRows<<-1    
            }
        })        
  })
  
  
  
  output$validRowsCheck <- reactive({
        vr1<-0
        checkNumRowsAgain()        
        if (exists("valRows")){
            if (valRows==1){
                vr1=1    
            } 
        }
        ### have to make sure the names Chosen are different --- Issue a warning!             
        return(vr1)
  })    
  outputOptions(output, "validRowsCheck", suspendWhenHidden=FALSE) 
  
  
  .makeOpencpuURL <- function(fn, urlPrefix=opencpu$url(), package="distcomp") {
    paste(urlPrefix, "library", package, "R", fn, "json", sep="/")
  }
  
  .distcompSSL.config <- function() {
    list(ssl.verifypeer=FALSE, ssl.verifyhost=FALSE)
  }
  
  .deSerialize <- function(q) {
    cType <- headers(q)['content-type']
    if (cType == "application/json")
      fromJSON(rawToChar(q$content))
    else
      q$content
  }
  
  
    
  checkPOSTreq<-function(executeURLJSON,payload){
    answer = tryCatch({
      q1result <- POST(url = executeURLJSON,
                body = toJSON(payload),
                add_headers("Content-Type" = "application/json"),
                config=.distcompSSL.config()
      )
      answer=.deSerialize(q1result)
    },  
    warning=function(answer) {  
      return(answer)
    },
    error=function(answer) {  
      return(answer)
    })
  }
  
  createIDdirec <-function(object,userDataFrame){
    #create a directory with the uniqueID as name
    directoryID=paste("mkdir", object$UniqueID)
    system(directoryID)
    
    #save data.RDS inside id/
    fileDATA=paste0(object$UniqueID,"/data.RDS")
    saveRDS(userDataFrame,fileDATA)
    
    #save dfn.RDS inside id/
    fileDFN=paste0(object$UniqueID,"/dfn.RDS")
    saveRDS(object,fileDFN)
    
    nameTGZ = paste0(object$UniqueID,".tar.gz")
    makeTGZ=paste("tar -zcvf",nameTGZ, object$UniqueID)
    system(makeTGZ, ignore.stderr = TRUE)
  }
  
  
  checkDirExists<-function(dirName){
    checkDirIs = tryCatch({
      expr <- sprintf('ls %s',dirName)
      isDir <- system(expr, intern = TRUE, ignore.stderr = TRUE)
    },  
    warning=function(isDir) {  
      return(isDir)
    },
    error=function(isDir) {  
      return(isDir)
    })
  }
  
  
  output$printInstructions<-renderPrint({
    if(input$generateDirs == 0) return()
    isolate({ 
        userDF11 <-createDF()
        proVMrun <<- readProject()   
        isDirThere <- checkDirExists(proVMrun$UniqueID)
        if(is.atomic(isDirThere)){
          cat(sprintf('Warning: the directory "%s/" already exists!\n',proVMrun$UniqueID))
        }else{
          #directory does not exist --> create it with the dfn.RDS and data.RDS files inside
          createIDdirec(proVMrun,userDF11)
          cat(sprintf('1) Create "%s/defn" (if it doesn\'t exist already).\n',input$workspaceDir))
          cat(sprintf('2) Copy %s.tar.gz to "%s/defn/" and unzip it.\n',proVMrun$UniqueID,input$workspaceDir))
          
        }            
    })
  })
    
  
  output$serializeMsg<-renderPrint({
    if(input$runOnVM == 0) return()
    isolate({ 
      userDF11 <-createDF()
      proVMrun <- readProject()    
      executeURLJSON1 <- .makeOpencpuURL(urlPrefix=input$slaveURL, fn="makeObject")
      payload1 <- list(defnId = proVMrun$UniqueID,
                       instanceId = 'instanceTest',
                       dataFileName='data.rds')
      vmResult <-checkPOSTreq(executeURLJSON1,payload1)
      
      if (length(class(vmResult))==1){
        cat('Success! Instance created.\n')      
      }else{                
        cat('The message cannot be serialized.\n')
          if (class(vmResult)[2]=="error"){
            cat('Error:\n',vmResult$message)            
          }else{
            cat('Warning:\n',vmResult$message)
          }        
      }      
      })
  })
      
  
  
  
  
  
  #### ----- functions to make tabs active sequentially ----- 
  ## 1: Move from "Project/Data Summary" to "Variable Matching / Data Check" when user clicks on "Proceed to.."
  observe({
    if (input$matchVarsCheckData > 0){ 
        session$sendCustomMessage('activeNavs', 'Variable Matching')
        updateTabsetPanel(session, inputId="mynavlist", selected="varMatch")
    }
  })
  
  observe({
    if (input$varMatchDone > 0){ 
      session$sendCustomMessage('activeNavs', 'Data Check')
      updateTabsetPanel(session, inputId="mynavlist", selected="dataCheck")
    }
  })
  
  
  observe({
     if (input$dataCheckDone > 0){ 
      session$sendCustomMessage('activeNavs', 'Dry Run')
      updateTabsetPanel(session, inputId="mynavlist", selected="dryRun")
    }
  })
  
  
  
  
})