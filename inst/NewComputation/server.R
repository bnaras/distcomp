shinyServer(function(input, output, session) {
    require("digest")

    ##-----Tab: Project/Data Summary

    #Project Name
    nameInput <- reactive({
        if(input$summaryP == 0) return()
            isolate({
            paste("Project name: ", input$nameIn)
            })
    })

    #Project Description
    descInput <- reactive({
        if(input$summaryP == 0) return()
            isolate({
            paste("Description: ", input$descripIn)
        })
    })


    #Data Upload/print summary
    #Create data frame
    createDF <- reactive({
        if(input$upload == 0) return()
        isolate({
            validate(
            need(input$datafile != "", "Please select a data set")
            )
        inFile <- input$datafile
        if (is.null(inFile))
                return(NULL)
        nasNocommas=gsub("\\s","", input$naIn)
        navals<-strsplit(nasNocommas,",")[[1]]
        navector<-c(navals[1:length(navals)])
        #check file is indeed CSV
        checkCSV <<- checkfileIScsv(inFile$datapath)
        if (class(checkCSV)[[1]]=="data.frame"){
            df <- read.csv(inFile$datapath, na.strings = navector)
        }else{
            df <- checkCSV
        }
        return(df)
        })
    })

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

    #print the data frame (summary)
    contentInput <- reactive({
        if(input$upload == 0) return()
        isolate({
        df1<<-createDF()
        if (is.data.frame(df1)){
            writeLines(paste(str(df1), collapse= "\n"))
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


    printProjectSummary <- reactive({
        if(input$summaryP == 0) return()
            isolate({
            paste("Project Summary: ")
        })
    })
    printDataSummary <- reactive({
        if(input$upload == 0) return()
            isolate({
            paste("Data Summary: ")
        })
    })


  #------output functions for tab Project/Data Summary
  output$name <- renderText({
    nameInput()
  })
  output$desc <- renderText({
    descInput()
  })
  output$content <- renderPrint({
    contentInput()
  })
  output$projectSummary <- renderText({
    printProjectSummary()
  })
  output$dataSummary <- renderText({
    printDataSummary()
  })


  ##-----Tab: Data Check

  #check if formula itself is correct
  checkformulaiscorrect<-function(formulaastext){
    checkform11 = tryCatch({
      form1=as.formula(formulaastext)
    },
    error=function(form1) {
      return(form1)
    })
  }

  #check formula conforms to the data frame
  checkformAnddata <-function(formulaastext,dataframe){
    formula1=as.formula(formulaastext)
    result = tryCatch({
      formcheck <- coxSlave$new(formula = formula1,data = dataframe)
    },
    error=function(formcheck) {
      return(formcheck)
    })
  }

  #overall check of the formula with the data frame
  checkFormula <- reactive({
    if(input$checkFormula == 0) return()
    isolate({
      error1=checkformulaiscorrect(input$formulaIn)
      ## checking for errors in the formula itself
      if (length(error1$message)==0){
        #compute error2 (formula with respect to data frame)
        error2=checkformAnddata(input$formulaIn, df1)
        if (length(error2)==1){
          cat('Your formula is correct')
        }
        if (length(error2)>1){
          cat('There is an error in the formula: \n', error2$message)
        }
      }
      if (length(error1$message)>0){
        cat('There is an error in the formula: \n', error1$message)
      }
      })
    })


  #ONLY IF the formula is correct, show the buttons to check for empty dataset and variable names

  # a workaround to make the conditional output work: check the formula again!
  # I should find a more efficient way to do this
  checkFormula2 <- reactive({
    if(input$checkFormula == 0) return()
    isolate({
      correctForm2 <<-0
      error1=checkformulaiscorrect(input$formulaIn)
      ## checking for errors in the formula itself
      if (length(error1$message)==0){
        #compute error2 (formula with respect to data frame)
        error2=checkformAnddata(input$formulaIn, df1)
        if (length(error2)==1){
          correctForm2 <<-1
        }
        }
    })
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

  #form new data frame (only with the variables specified in the formula)
  formNewDF<-function(formulaastext,dataframe){
    form=as.formula(formulaastext)
    vars=all.vars(form) ## vector with the variables
    namesdf=names(dataframe)
    colsNewdf=match(vars,namesdf)
    newDF=dataframe[,colsNewdf]
    return(newDF)
  }

  #find variable names that contain numbers and/or punctuation symbols
  findNames <- function(namesDF){
    ## this represents the punctuation symbols and the integers I want to replace
    pp='[[:punct:]0-9]'
    names2replace=character(0)
    for (i in 1:length(namesDF)){
      name1=namesDF[i]
      if (length(grep(pp,name1))>0){
        names2replace=c(names2replace,name1)
      }
    }
    return(names2replace)
  }

  # creating new names: only removes number and punctuation
  createNewName <- function(oldName){
    #remove punctuation and numbers
    newName=gsub("[[:punct:]]", "", oldName)
    newName=gsub("[0-9]", "", newName)
    return(newName)
  }

  #offer the new names: generate the questions 'Would you like to change to ...?'
  offerNewNames <- function(names2replace){
    for (i in 1:length(names2replace)){
      oldname=names2replace[i]
      newname=createNewName(oldname)
      cat(sprintf('Would you like to replace \"%s\" with \"%s\"? \n', oldname,newname))
    }
  }

  #check the variable names and possibly make suggestions for new names
  checkNames <- reactive({
    if(input$checkVarNames == 0) return()
    isolate({
      names2replace=findNames(names(dfFormula))
      if (length(names2replace)>0){
        offerNewNames(names2replace)
      }else{
        cat('All names look good!')
      }
    })
  })

  # conditional output: ask the user whether he/she wants to replace variable names only if there
  # names to replace (i.e., names with numbers and/or punctuation)
  # workaround (as before): find names to replace again! (think later for a more efficient way)
  checkNames2 <- reactive({
    if(input$checkVarNames == 0) return()
    isolate({
      dfFormula2 <<-formNewDF(input$formulaIn,df1)
      names2replaceRB <<-findNames(names(dfFormula2))
    })
  })




  #------ output functions for tab Data Check
  output$formulaKosherness <- renderPrint({
    checkFormula()
  })

  output$pls <- reactive({
    retval<<-0
    checkFormula2()
    if (exists("correctForm2")){
      if (correctForm2==1){
        retval=1
      }
    }
    return(retval)
  })
  outputOptions(output, "pls", suspendWhenHidden=FALSE)

  output$validRows <- renderPrint({
    if(input$checkEmpty == 0) return()
    isolate({
      dfFormula <<-formNewDF(input$formulaIn,df1)
      valrows=checkEmptySet(dfFormula)
      if(valrows==0){
        cat(sprintf('The data frame for your formula is empty!\n'))
      }else if (valrows==1){
        cat(sprintf('The data frame for your formula has only %i valid row.\n', valrows))
      }else{
        cat(sprintf('The data frame for your formula has %i valid rows.\n', valrows))
      }
    })
  })

  output$newNames <- renderPrint({
    checkNames()
  })

  #conditional output: suggest new names only if there are names to replace
  output$nn <- reactive({
    nn<<-0
    checkNames2()
    if (exists("names2replaceRB")){
      if (length(names2replaceRB)>0){
          nn=1
      }
    }
    return(nn)
  })
  outputOptions(output, "nn", suspendWhenHidden=FALSE)


  ##-----Tab: Dry Run

  #compute length of vector beta required
  calcbetalen <-function(formulaastext){
    form=as.formula(formulaastext)
    betalength=length(attr(terms(form), "term.labels"))
    return(betalength)
  }

  #do the test run and return the result of the run
  checkRun <-function(formulaastext,dataframe,beta){
    form=as.formula(formulaastext)
    testrun<-coxSlave$new(form,dataframe)
    tryCatch({
      result<-testrun$logLik(as.numeric(beta))
    },
    error=function(result) {
      return(result)
    },
    warning=function(result) {
      return(result)
    }
    )
  }

  #return/print the result of the run
  checkrunResult <- reactive({
    if(input$runModel == 0) return()
    isolate({
      #convert input betas to vector
      betasNospaces=gsub("\\s","", input$betaIn)
      betavals<-strsplit(betasNospaces,",")[[1]]
      betavector<-c(betavals[1:length(betavals)])
      runResult = checkRun(input$formulaIn,df1,betavector)

      if (class(runResult) == "error") {
          cat('Your model could not be run.\n')
          cat('Error:\n', runResult$message)
      } else {
        cat('Your model run succesfully! \nResults are: \n')
        print(runResult)
      }


      ## ## print different outputs according to the length of runResult
      ## if (length(runResult)>1){
      ##   cat('Your model could not be run.\n')
      ##   if (class(runResult)[2]=="error"){
      ##     cat('Error:\n',runResult$message)
      ##   }else{
      ##     cat('Warning:\n',runResult$message)
      ##   }
      ## }
      ## if (length(runResult)==1){
      ##   cat('Your model run succesfully! \nResults are: \n')
      ##   print(runResult)
      ## }


  })
})




  #------ output functions for tab Data Check

  #print length of vector beta required
  output$betalen <- renderPrint({
    checkFormula2()
    if (exists("correctForm2")){
      if(correctForm2==1){
        cat(calcbetalen(input$formulaIn))
      }else{
        cat('Please make sure your formula is correct')
      }
    }
  })

  #print result(output) of the test run
  output$testrun <- renderPrint({
    checkrunResult()
  })


  ##-----Tab: Output

  #generate a unique ID for the project
  generateId <- function(object, algo="sha256") digest(object, algo=algo)

  # return list of variables (each element is a list as well)
  variablelist <- function(formulaastext,dataframe){
    varlist=list()
    form=as.formula(formulaastext)
    vars=all.vars(form)
    numvar = length(vars)
    for (i in 1:numvar){
      varContent=dataframe[[vars[i]]]
      varName=vars[i]
      varClass=class(varContent)
      if (varClass=="factor"){
        varLevels=levels(varContent)
        vartoadd=list(Name=varName,Class=varClass,Levels=varLevels)
      }
      if ((varClass=="numeric")|(varClass=="integer")){
        varRange=range(varContent,na.rm=TRUE)
        vartoadd=list(Name=varName,Class=varClass,Range=varRange)
      }
      #varlist[[i]]=vartoadd    ## this is another option
      varlist=c(varlist,list(vartoadd))
    }
    return (varlist)
  }

  # print the project object
  projectList <-reactive({
    if(input$genList == 0) return()
    isolate({
      name=input$nameIn
      description=input$descripIn
      typecomp=input$compType
      formula=input$formulaIn
      variables=variablelist(input$formulaIn,df1)
      projectTemp<-list(ProjectName=name,ProjectDesc=description,TypeComp=typecomp,Formula=formula,Variables=variables)
      uniqueID=generateId(projectTemp)
      project <<- list(UniqueID=uniqueID,ProjectName=name,ProjectDesc=description,TypeComp=typecomp,Formula=formula,Variables=variables)
      print(project)
    })
  })



  #------ output functions for tab Output

  output$projectObj <- renderPrint({
    projectList()
  })

  output$saveData <- downloadHandler(
    filename = function() {
      paste('project', 'RDS', sep = ".")
    },
    content = function(file) {
      # Write to a file specified by the 'file' argument
      saveRDS(project, file)
    }
  )



  #### ----- functions to make tabs active sequentially -----

  ## 1: Move from "Project/Data Summary" to "Data Check" when user clicks on "Done"
  ## AND a data frame exists (this in turn confirms the uploaded file was has a 'csv' extension)
  observe({
    if ((input$data_upload_done > 0)&(exists("df1"))){
      if(is.data.frame(df1)) {
        session$sendCustomMessage('activeNavs', 'Data Check')
      }
    }
  })

  ## 2: Move from "Data Check" to "Dry Run" when user clicks on "Done"
  ## AND the formula is correct
  observe({
        if ((input$data_check_done > 0)&(exists("correctForm2"))) {
          if(correctForm2==1) {
            session$sendCustomMessage('activeNavs', 'Dry Run')
          }
        }
    })

  ## 3: Move from "Dry Run" to "Output" when user clicks on "Done"
  observe({
        if (input$dry_run_done > 0) {
            session$sendCustomMessage('activeNavs', 'Output')
        }
    })

})


