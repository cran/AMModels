library(shiny)
library(shinyBS)
library(AMModels)
source('createTimedAlert.R')
source('textareaInput.R')


# Contents of the values list:
# modelSource: character, either 'Global Env' or 'File'
# selectedModelLib: character, the name of the selected amModelLib
# editedAMModelLib: amModelList, the result of a new from selected/search/save
# editedModelsData: named list with models and data elements--both are named vectors of integer save button values
# currentAMModelLib: the amModelLib with edits made in the left panel (add/delete model, add/delete data, add/delete/edit metadata). Replaces getSelectedModel().
# amModelLibForModals: the amModelLib for add/delete/edit metadata modal creation. Updated only by add/delete model, add/delete data.
# newAMModelLibInfo: named list of info that will be inserted into a new amml object on creation.


shinyServer(
	function(input,output,session){
#	    # Debugger
#        observe(label="console",{
#            if(input$console != 0) {
#              options(browserNLdisabled=TRUE)
#              saved_console<-".RDuetConsole"
#              if (file.exists(saved_console)) load(saved_console)
#              isolate(browser())
#              save(file=saved_console,list=ls(environment()))
#            }
#        })

	    values <- reactiveValues()
        # A list with models and data elements--all are named vectors of checkbox values
        editedModelsData <- list(models=0, data=0)
        
	    # List and store contents of uploaded .RData/.rda or .RDS file
	    getUpload <- reactive({
	        userdata <- input$userDataUpload
	        if(!is.null(userdata)) {
	            ext <- gsub('.+\\.', '', userdata$name)
                userDataEnv <- new.env()
	            if(tolower(ext) %in% c('rdata', 'rda')) {
	                load(userdata$datapath, envir = .GlobalEnv)
	                userdata <- load(userdata$datapath, envir = userDataEnv)
                } else if(tolower(ext) == 'rds') {
                    # Don't need to readRDS to a custom env, only do 
                    # it for consistency in output from this function
                    name <- make.names(gsub('\\..{1,4}$', '', userdata$name))
                    obj <- readRDS(userdata$datapath)
                    userdata <- name
                    assign(x=name, value=obj, pos=userDataEnv)
                    assign(x=name, value=obj, pos=.GlobalEnv)
                }
                list(objnames=userdata, userdata=userDataEnv)
	        }
	    })
	    # List contents of .GlobalEnv
	    getFromGlobalEnv <- function(){
	        obj <- objects(".GlobalEnv")
	        if(!length(obj)) {
	            NULL
	        } else { 
	            obj
            }
	    }
	    
#	    getFromGlobalEnv <- reactive({
#	        input$sendAMMLToGlobalEnv
#	        input$newAMModelLibGo
#	        obj <- objects(".GlobalEnv")
#	        if(!length(obj)) {
#	            NULL
#	        } else { 
#	            obj
#            }
#	    })
	    
	    # selection can be from one of two sources:
	    # --can upload a model as .RData/.rda or RDS
	    observe({
	        userdata <- getUpload()
	        if(!is.null(userdata)) {
	            choices <- userdata$objnames
	            if(input$filterForAMModelLibs) {
	                isamml <- sapply(choices, function(x) is(userdata$userdata[[x]], 'amModelLib'))
	                choices <- choices[isamml]
                }
	            if(!is.null(choices)) {
	                if(length(choices)) {
	                    names(choices) <- choices
	                    output$selectModelUI <- renderUI({
	                        radioButtons('selectAMModelLib', 'Select AMModelLib', choices=choices)
	                    })
	                    values$modelSource <- 'File'
                        values$currentAMModelLib <- NULL
                        editedmodels <- list()
                    } else {
                        output$selectModelUI <- renderUI({
                            div(
                                p(style='font-weight:bold;', 'Select AMModelLib'),
	                            p('No objects in file to list.')
                            )
	                    })
	                    values$modelSource <- NULL
                    }
                }
            }
        })
        # --or can select one from the .GlobalEnv
        observe({
            if(input$listGlobalEnv > 0) {
                choices <- getFromGlobalEnv()
	            if(input$filterForAMModelLibs) {
	                isamml <- sapply(choices, function(x) is(.GlobalEnv[[x]], 'amModelLib'))
	                choices <- choices[isamml]
                }
	            if(!is.null(choices)) {
	                if(length(choices)) {
                        names(choices) <- choices
                        output$selectModelUI <- renderUI({
                            radioButtons('selectAMModelLib', 'Select AMModelLib', choices=choices)
                        })
                        values$modelSource <- 'Global Env'
                    } else {
                        output$selectModelUI <- renderUI({
                            div(
                                p(style='font-weight:bold;', 'Select AMModelLib'),
	                            p('No objects in global environment to list.')
                            )
	                    })
	                    values$modelSource <- NULL
                    }
                }
            }
        })
        
        # --set the .GlobalEnv selector to start
        observe({
            if(is.null(input$selectAMModelLib)) {
                choices <- getFromGlobalEnv()
                isamml <- sapply(choices, function(x) is(.GlobalEnv[[x]], 'amModelLib'))
                choices <- choices[isamml]
	            if(!is.null(choices)) {
	                if(length(choices)) {
                        names(choices) <- choices
                        output$selectModelUI <- renderUI({
                            radioButtons('selectAMModelLib', 'Select AMModelLib', choices=choices)
                        })
                        values$modelSource <- 'Global Env'
                    } else {
                        output$selectModelUI <- renderUI({
                            div(
                                p(style='font-weight:bold;', 'Select AMModelLib'),
	                            p('No objects in global environment to list.')
                            )
	                    })
	                    values$modelSource <- NULL
                    }
                }
            }
        })
        
        # Allow the 'create amModelList' action to update the hidden ui
	    observe({
	        if(!is.null(input$selectAMModelLib)) outputOptions(output, 'selectModelUI', suspendWhenHidden = FALSE)
        })
        # --or can create a new one (in a different modal)
	    observe({
            input$newAMModelLibGo 
            isolate({
                if(!is.null(input[['addinfo-name']])) {
                    amml <- make.names(input$newAMModelLibName)
                    description <- input$newAMModelLibDescription
                    info <- values$newAMModelLibInfo
                    if(input[['addinfo-name']] != '' && input[['addinfo-value']] != '') {
                        newinfo <- list(input[['addinfo-value']])
                        names(newinfo) <- input[['addinfo-name']]
                        info <- c(info, newinfo)
                    }
                    if(is.null(info)) info <- list()
                                
                    # Can only create an amModelLib if it is assigned a name
                    if(input$newAMModelLibName != '') { 
                        amml.obj <- amModelLib(info=info, description=description)
                        assign(amml, amml.obj, pos='.GlobalEnv')
                        # Update: no box, always edit on close if possible
                        # make new amModelLib the current one, if box is checked
#                        if(input$newAMModelLibRemoteActive) { 
                            values$modelSource <- 'Global Env'
                            values$selectedModelLib <- amml
                            choices <- getFromGlobalEnv()
                            # Terri reports that the new one is appearing twice?
                            # My tests show otherwise?
#                            choices <- c(choices, amml)
                            if(is.null(input$filterForAMModelLibs)) {
                                isamml <- sapply(choices, function(x) is(.GlobalEnv[[x]], 'amModelLib'))
                                choices <- choices[isamml]
                            } else {
                                if(input$filterForAMModelLibs) {
	                                isamml <- sapply(choices, function(x) is(.GlobalEnv[[x]], 'amModelLib'))
	                                choices <- choices[isamml]
                                }
                            }
                            names(choices) <- choices
                            updateRadioButtons(session, 'selectAMModelLib', choices=choices, selected=amml)
#                        }
                        
                        values$newAMModelLibInfo <- NULL
                        updateTextInput(session, 'addinfo-name', value='')
                        updateTextInput(session, 'addinfo-value', value='')
                        updateTextInput(session, 'newAMModelLibName', value='')
                        updateTextInput(session, 'newAMModelLibDescription', value='')
                    }
                }
            })
        })
	    # Either way, report which one is currently selected
	    output$reportSelectedModel <- renderUI({
            HTML({paste0('
                <p style="font-weight:bold;">amModelLib:&nbsp<span style="font-weight:normal;">', values$selectedModelLib, '</span></p>'
            )})
	    })
	    
	    # And report the source
	    output$reportSelectedModelSource <- renderUI({
            HTML({paste0('
                <p style="font-weight:bold;">Model source:&nbsp<span style="font-weight:normal;">', values$modelSource, '</span></p>'
            )})
	    })
	    
	    # Display the current amModelLib info
	    output$amModelLibInfoUI <- renderUI({
	        input$okaytoaddinfotocurrent
	        # Make table headings
            th <- '
                <tr>
                    <th style="text-align:right;padding-right:1em;">Remove</th>
                    <th style="text-align:center;padding-right:1em;">Name</th>
                    <th style="text-align:center;">Value</th>
                </tr>
            '
            # Make place for new info
            newinfo <- paste0('
                <tr>
                    <td style="width:8em;text-align:right;padding-right:1.5em;">',
                        actionButton('okaytoaddinfotocurrent', '', icon=icon('thumbs-o-up')),'
                    </td>
                    <td style="width:12em;text-align:center;padding-right:1em;">
                        <span class="form-group shiny-input-container">
                            <input id="addinfotocurrent-name" type="text" class="form-control" value=""/>
                        </span> 
                    </td>
                    <td style="width:40em;text-align:left;">
                        <span class="form-group shiny-input-container">
                            <textarea id="addinfotocurrent-value" rows=1, style="width:100%;"></textarea>
                        </span>
                    </td>
                </tr>'
            )
            # Parse existing info
            exinfo <- values$currentAMModelLib
            if(!is.null(exinfo)) {
                exinfo <- ammlInfo(exinfo)
                if(length(exinfo)) {
                    exinfonm <- names(exinfo)
                    exinfo <- paste0(unlist(lapply(1:length(exinfo), function(x) paste0('
                        <tr>
                            <td style="width:8em;text-align:right;padding-right:2em;">
                                <div class="checkbox">
                                    <label>
                                        <input id="', paste0('selectCurrentInfo-', x),'" type="checkbox"/>
                                    </label>
                                </div> 
                            </td>
                            <td style="width:12em;text-align:center;padding-right:1em;">
                                <span class="form-group shiny-input-container">
                                    <input id="editCurrentInfo-name" type="text" class="form-control" value="', exinfonm[x], '"/>
                                </span> 
                            </td>
                            <td style="width:40em;text-align:left;">
                                <span class="form-group shiny-input-container">
                                    <textarea id="editCurrentInfo-value" rows=1, style="width:100%;">', exinfo[[x]], '</textarea>
                                </span>
                            </td>
                        </tr>'
                    ))), collapse='')
                }
            }
            list(
                HTML(paste0('<table id="addInfoToCurrentAMMLTable">', th, exinfo, newinfo, '</table>')),
                bsTooltip(id='addInfoToCurrentAMMLTable th:nth-child(1)', title='Checked lines are removed and the new line is added when the thumbs-up is pressed', placement = "top", trigger = "hover", options = list(container='body')),
                bsTooltip(id='addInfoToCurrentAMMLTable th:nth-child(2)', title='Coerced to a valid R name: must start with a character and contain no spaces', placement = "top", trigger = "hover", options = list(container='body')),
                bsTooltip(id='addInfoToCurrentAMMLTable th:nth-child(3)', title='Can contain any value', placement = "top", trigger = "hover", options = list(container='body'))                
            )
	    })
	    
	    # Keep the current amModelLib up to date with new amModelLib info
	    observe({
	        go <- input$okaytoaddinfotocurrent
	        isolate({
	            if(!is.null(go)) {
                    amml <- values$currentAMModelLib
                    if(!is.null(amml)) {
                        exinfo <- amml@info
                        if(go) {
                            newinfo <- NULL
                            if(length(exinfo)) {
                                rminfo <- sapply(1:length(exinfo), function(x) input[[paste0('selectCurrentInfo-', x)]])
                                exinfo <- exinfo[!rminfo]
                            }
                            if(input[['addinfotocurrent-value']] != '' && input[['addinfotocurrent-name']] != '') {
                                newinfo <- list(input[['addinfotocurrent-value']])
                                names(newinfo) <- make.names(input[['addinfotocurrent-name']])
                            }
                            amml@info <- c(exinfo, newinfo)
                            values$currentAMModelLib <- amml
                        }
                    }
                }
            })
	    })
	    
	    # Update the current amml description field
#	    observe({
#	        input$selectAMModelLib
#	        input$newAMModelLibGo
#	        input$listGlobalEnv
#	        values$currentAMModelLib
#	        isolate({
#	            browser()
#	            amml <- values$currentAMModelLib
#	            if(!is.null(amml)) {
#	                descr <- ammlDesc(amml)
#	                updateTextInput(session, 'amModelLibDescription', value=descr)
#	            }
#            })
	        
#	    })
	    
	    # Store the selected amModelLib in a dedicated transporter--now a reactiveValues list
#	    getSelectedModel <- reactive({
        observe({
            input$revertchanges
	        if(!is.null(values$modelSource) && !is.null(input$selectAMModelLib)) {
	            if(values$modelSource == 'File' && input$selectAMModelLib != 'NA') {
                    userdata <- getUpload()
	                selectedobject <- userdata$userdata[[input$selectAMModelLib]]
	                if(is(selectedobject, 'amModelLib')) {
                        values$selectedModelLib <- input$selectAMModelLib
                        values$checkedModelsData <- NULL
	                    values$currentAMModelLib <- selectedobject
                        values$amModelLibForModals <- selectedobject
                        editedModelsData <<- list(models=0, data=0)
                    } else {
                        values$selectedModelLib <- NULL
                    }
	            } else if(values$modelSource == 'Global Env' && input$selectAMModelLib != 'NA') {
	                selectedobject <- .GlobalEnv[[input$selectAMModelLib]]
	                if(is(selectedobject, 'amModelLib')) {
                        values$selectedModelLib <- input$selectAMModelLib
                        values$checkedModelsData <- NULL
	                    values$currentAMModelLib <- selectedobject
                        values$amModelLibForModals <- selectedobject
                        editedModelsData <<- list(models=0, data=0)
                    } else {
                        values$selectedModelLib <- NULL
                    }
	            } 
            } else {
                values$currentAMModelLib <- NULL
                values$selectedModelLib <- NULL
                values$amModelLibForModals <- NULL
            }
	    })
	    
	    # report the summary of the selected amModelLib (not the current/possibly edited version)
	    output$selectedModelSummary <- renderPrint({
	        selectedModel <- NULL
	        if(!is.null(values$modelSource) && !is.null(input$selectAMModelLib)) {
	            if(values$modelSource == 'File') {
	                userdata <- getUpload()
	                selectedModel <- userdata$userdata[[input$selectAMModelLib]]
	            } else if(values$modelSource == 'Global Env') {
	                selectedModel <- .GlobalEnv[[input$selectAMModelLib]]
	            } 
            }
#	        selectedModel <- getSelectedModel()
	        if(!is.null(selectedModel)) {
	            tryCatch(summary(selectedModel), error=function(e) print(selectedModel))
	        }
	    })
	    
	    # Keep the values list up to date with new amModelLib info
	    observe({
	        go <- input$okaytoaddinfo
	        isolate({
	            if(!is.null(go)) { 
                    exinfo <- values$newAMModelLibInfo
	                if(go) {
	                    newinfo <- NULL
	                    if(!is.null(exinfo)) {
	                        rminfo <- unlist(lapply(1:length(exinfo), function(x) input[[paste0('selectNewInfo-', x)]]))
                            exinfo <- exinfo[!rminfo]
                        }
	                    if(input[['addinfo-value']] != '' && input[['addinfo-name']] != '') {
	                        newinfo <- list(input[['addinfo-value']])
	                        names(newinfo) <- input[['addinfo-name']]
                        }
                        values$newAMModelLibInfo <- c(exinfo, newinfo)
                    }
                }
            })
	    })
	    
	    # Enter new amModelLib info
	    output$newAMModelLibInfoUI <- renderUI({
	        input$okaytoaddinfo
	        # Make table headings
            th <- '
                <tr>
                    <th style="text-align:right;padding-right:1em;">Remove</th>
                    <th style="text-align:center;padding-right:1em;">Name</th>
                    <th style="text-align:left;">Value</th>
                </tr>
            '
            # Make place for new info
            newinfo <- paste0('
                <tr>
                    <td style="width:8em;text-align:right;padding-right:1.5em;">',
                        actionButton('okaytoaddinfo', '', icon=icon('thumbs-o-up')),'
                    </td>
                    <td style="width:12em;text-align:center;padding-right:1em;">
                        <span class="form-group shiny-input-container">
                            <input id="addinfo-name" type="text" class="form-control" value=""/>
                        </span> 
                    </td>
                    <td style="width:40em;text-align:left;">
                        <span class="form-group shiny-input-container">
                            <textarea id="addinfo-value" rows=1, style="width:100%;"></textarea>
                        </span>
                    </td>
                </tr>'
            )
            # Parse existing info
            exinfo <- values$newAMModelLibInfo
            if(!is.null(exinfo)) {
                if(length(exinfo)) {
                    exinfonm <- names(exinfo)
                    exinfo <- paste0(unlist(lapply(1:length(exinfo), function(x) paste0('
                        <tr>
                            <td style="width:8em;text-align:right;padding-right:2em;">
                                <div class="checkbox">
                                    <label>
                                        <input id="', paste0('selectNewInfo-', x),'" type="checkbox"/>
                                    </label>
                                </div> 
                            </td>
                            <td style="width:12em;text-align:center;padding-right:1em;">
                                <span class="form-group shiny-input-container">
                                    <input id="editNewInfo-name" type="text" class="form-control" value="', exinfonm[x], '"/>
                                </span> 
                            </td>
                            <td style="width:40em;text-align:left;">
                                <span class="form-group shiny-input-container">
                                    <textarea id="editNewInfo-value" rows=1, style="width:100%;">', exinfo[[x]], '</textarea>
                                </span>
                            </td>
                        </tr>'
                    ))), collapse='')
                }
            }
            HTML(paste0('<table id="addInfoToNewAMMLTable">', th, exinfo, newinfo, '</table>'))
	    })
	    
	    # Parse the contents of selected amModelLib into a table
        output$selectedModelContents <- renderUI({
            values$selectedModelLib
            selected.model <- values$currentAMModelLib
#            selected.model <- getSelectedModel()
            if(!is.null(selected.model)) {
                if(is(selected.model, 'amModelLib')) {
                    isolate({
                        # Make table headings
                        th <- '
                            <th style="text-align:right;padding-right:1em;">Select</th>
                            <th style="text-align:center;padding-right:1em;">Name</th>
                            <th style="text-align:center;">Summary</th>
                            <th style="text-align:center;">Edit</th>
                        '
                        # Get model and data names/presence
                        modnames <- names(selected.model@models)
                        datnames <- names(selected.model@data)
                        nmodels <- length(modnames)
                        ndata <- length(datnames)
                        if(nmodels) {
                            # Remove periods from names for js compatibility  
                            modnames <- paste0(gsub('\\.', '', modnames), 1:length(modnames))   
                            # Make table data
                            modrows <- sapply(1:nmodels, function(x) {
                                paste0('
                                    <tr>
                                        <td style="width:8em;text-align:right;padding-right:1em;">
                                           <div class="checkbox">
                                                <label>
                                                    <input id="', paste0('select', modnames[x]),'" type="checkbox"/>
                                                </label>
                                            </div> 
                                        </td>
                                        <td style="width:20em;text-align:center;padding-right:1em;">', names(selected.model@models)[x], '</td>
                                        <td style="width:6em;text-align:center;">
                                            <button id="', paste0('summary', modnames[x]), '" class="btn btn-default action-button shiny-bound-input" data-target="', paste0('#summary', modnames[x], 'modal'), '" data-toggle="modal" type="button">
                                                <i class="fa fa-file-text-o"></i>
                                            </button>
                                        </td>
                                        <td style="width:6em;text-align:center;">
                                            <button id="', paste0('edit', modnames[x]), '" class="btn btn-default action-button shiny-bound-input" data-target="', paste0('#edit', modnames[x], 'modal'), '" data-toggle="modal" type="button">
                                                <i class="fa fa-pencil"></i>
                                            </button>
                                        </td>
                                    </tr>'
                                )
                            })
                        } else {
                            modrows <- '
                                <tr>
                                    <td style="width:8em;text-align:right;padding-right:1em;"></td>
                                    <td style="width:20em;text-align:center;padding-right:1em;">--- There are no models ---</td>
                                    <td style="width:6em;text-align:center;"></td>
                                    <td style="width:6em;text-align:center;"></td>
                                </tr>'
                        }
                        if(ndata) {
                            # Remove periods from names for js compatibility  
                            datnames <- paste0(gsub('\\.', '', datnames), 1:length(datnames))  
                            # Make table data
                            datrows <- sapply(1:ndata, function(x) {
                                paste0('
                                    <tr>
                                        <td style="width:8em;text-align:right;padding-right:1em;">
                                           <div class="checkbox">
                                                <label>
                                                    <input id="', paste0('select', datnames[x]),'" type="checkbox"/>
                                                </label>
                                            </div> 
                                        </td>
                                        <td style="width:20em;text-align:center;padding-right:1em;">', names(selected.model@data)[x], '</td>
                                        <td style="width:6em;text-align:center;">
                                            <button id="', paste0('summary', datnames[x]), '" class="btn btn-default action-button shiny-bound-input" data-target="', paste0('#summary', datnames[x], 'modal'), '" data-toggle="modal" type="button">
                                                <i class="fa fa-file-text-o"></i>
                                            </button>
                                        </td>
                                        <td style="width:6em;text-align:center;">
                                            <button id="', paste0('edit', datnames[x]), '" class="btn btn-default action-button shiny-bound-input" data-target="', paste0('#edit', datnames[x], 'modal'), '" data-toggle="modal" type="button">
                                                <i class="fa fa-pencil"></i>
                                            </button>
                                        </td>
                                    </tr>'
                                )
                            })
                        } else {
                            datrows <- '
                                <tr>
                                    <td style="width:8em;text-align:right;padding-right:1em;"></td>
                                    <td style="width:20em;text-align:center;padding-right:1em;">--- There are no datasets ---</td>
                                    <td style="width:6em;text-align:center;"></td>
                                    <td style="width:6em;text-align:center;"></td>
                                </tr>'
                        }
                        # Assemble model table
                        modtab <- HTML(paste0(
                            span(style='padding-top:0.1em;padding-left:2em;',
                                HTML('
                                    <button id="selectAllModels" style="width: 7.5em;" type="button" class="btn btn-default action-button">
                                        <i class="fa fa-check-square-o"></i>
                                        <i class="fa fa-square-o"></i>
                                    </button>'
                                )
    #                            actionButton("selectAllModels", '', icon=list(icon('check-square-o'), icon('square-o')), width='7.5em')
                            )
                            ,
                            bsTooltip(id='selectAllModels', title='Select/deselect all', placement = "top", trigger = "hover", options = list(container='body')), '
                            <table id="model_table" style="width:100%"><tr>', th, '</tr>', paste0(modrows, collapse=''), '
                            </table>
                            <span style="padding-left:2em;">
                                <button id="addModelToAMML" class="btn btn-default action-button shiny-bound-input" data-target="#addModelToAMMLmodal" data-toggle="modal" type="button">
                                    <i class="fa fa-plus"></i>
                                    Add model
                                </button>
                            </span>'
                        ))
                        # Assemble data table
                        dattab <- HTML(paste0(
                            span(style='padding-top:0.1em;padding-left:2em;',
                                HTML('
                                    <button id="selectAllData" style="width: 7.5em;" type="button" class="btn btn-default action-button">
                                        <i class="fa fa-check-square-o"></i>
                                        <i class="fa fa-square-o"></i>
                                    </button>'
                                )
    #                            actionButton("selectAllData", '', icon=list(icon('check-square-o'), icon('square-o')), width='7.5em')
                            )
                            ,
                            bsTooltip(id='selectAllData', title='Select/deselect all', placement = "top", trigger = "hover", options = list(container='body')), '
                            <table id="data_table" style="width:100%"><tr>', th, '</tr>', paste0(datrows, collapse=''), '</table>
                            <span style="padding-left:2em;">
                                <button id="addDataToAMML" class="btn btn-default action-button shiny-bound-input" data-target="#addDataToAMMLmodal" data-toggle="modal" type="button">
                                    <i class="fa fa-plus"></i>
                                    Add data
                                </button>
                            </span>'
                        ))

                        # Display modelLib metadata in a tab rather than above the tabs
	                    infotab <- div(style='clear:both;padding-bottom:1em;',
                            textareaInput('amModelLibDescription', 'Description', value=ammlDesc(selected.model), rows=3, width = '100%'),
                            p(style='font-weight:bold;', 'Info/Metadata'),
                            uiOutput('amModelLibInfoUI')
                        )
                        
                        # Create tabset
                        tabsetPanel(
                            id='amModelLibSelectedElement',
                            selected=input$amModelLibSelectedElement,
	                        tabPanel('Info/Metadata', infotab),
                            tabPanel('Models', modtab),
                            tabPanel('Data', dattab)
                        )
                    })
                }
            } else {
                tabsetPanel(
	                tabPanel('Info/Metadata'),
	                tabPanel('Models'),
	                tabPanel('Data')
	            )
            }
            
        })
	    
	    outputOptions(output, 'selectedModelContents', suspendWhenHidden = FALSE)

        
        # Monitor checked models/data 'remove' boxes and add model boxes
        observe({
            selected.model <- values$currentAMModelLib
#            selected.model <- getSelectedModel()
            if(!is.null(selected.model)) {
                if(is(selected.model, 'amModelLib')) {
                    # Get model and data names/presence
                    modnames <- names(selected.model@models)
                    datnames <- names(selected.model@data)
                    nmodels <- length(modnames)
                    ndata <- length(datnames)
                    selectedmodels <- selecteddata <- NULL
                    prevselected <- values$checkedModelsData
                    if(nmodels) {
                        # sanitize names for js compatibility
                        modnames <- paste0('select', gsub('\\.', '', modnames), 1:length(modnames))
                        # Check for selections 
                        selectedmodels <- unlist(lapply(modnames, function(x) {
                            if(is.null(input[[x]])) {
                                FALSE
                            } else {
                                input[[x]]
                            }
                        }))
                        if(!is.null(selectedmodels)) names(selectedmodels) <- modnames
                    }
                    if(ndata) {
                        # sanitize names for js compatibility  
                        datnames <- paste0('select', gsub('\\.', '', datnames), 1:length(datnames))
                        # Check for selections 
                        selecteddata <- unlist(lapply(datnames, function(x) {
                            if(is.null(input[[x]])) {
                                FALSE
                            } else {
                                input[[x]]
                            }
                        }))
                        if(!is.null(selecteddata)) names(selecteddata) <- datnames
                    }
                    values$checkedModelsData <- list(models=selectedmodels, data=selecteddata)
                }
            }
        })
        
	    # Select/deselect all models
	    observe({
	        go <- input$selectAllModels
	        if(!is.null(go)) {
	            isolate({
	                if(go) {
                        selectedmodels <- values$checkedModelsData$models
                        if(!is.null(selectedmodels)) {
                            if(any(selectedmodels)) {
                                selectedmodels <- selectedmodels[selectedmodels]
                                z <- lapply(1:length(selectedmodels), function(x) updateCheckboxInput(session, names(selectedmodels)[x], value=FALSE))
                            } else {
                                z <- lapply(names(selectedmodels), function(x) updateCheckboxInput(session, x, value=TRUE))
                            }
                        }
	                }
	            })
	        }
	    })
	    
	    # Select/deselect all data
	    observe({
	        go <- input$selectAllData
	        if(!is.null(go)) {
	            isolate({
	                if(go) {
	                    selecteddata <- values$checkedModelsData$data
                        if(!is.null(selecteddata)) {
                            if(any(selecteddata)) {
                                selecteddata <- selecteddata[selecteddata]
                                z <- lapply(1:length(selecteddata), function(x) updateCheckboxInput(session, names(selecteddata)[x], value=FALSE))
                            } else {
                                z <- lapply(names(selecteddata), function(x) updateCheckboxInput(session, x, value=TRUE))
                            }
                        }
	                }
	            })
	        }
	    })
	    
	    # Delete selected
	    observe({
	        go <- input$deleteSelected
            isolate({
                selectedammodellib <- values$currentAMModelLib
#                selectedammodellib <- getSelectedModel()
                selectedmodels <- values$checkedModelsData$models
                if(!is.null(selectedmodels)) {
                    if(any(selectedmodels)) {
                        selectedammodellib@models <- selectedammodellib@models[!selectedmodels]
                    } 
                }
                selecteddata <- values$checkedModelsData$data
                if(!is.null(selecteddata)) {
                    if(any(selecteddata)) {
                        selectedammodellib@data <- selectedammodellib@data[!selecteddata]
                    } 
                }
                values$currentAMModelLib <- selectedammodellib
                values$amModelLibForModals <- selectedammodellib
#                values$editedAMModelLib <- selectedammodellib
            })
	    })
	    
	    # New amModelList from selected
	    observe({
	        go <- input$newAMMLFromSelected
            isolate({
                selectedammodellib <- values$currentAMModelLib
                selectedmodels <- values$checkedModelsData$models
                if(!is.null(selectedmodels)) {
                    if(any(selectedmodels)) {
                        selectedammodellib@models <- selectedammodellib@models[selectedmodels]
                    }  else {
                        selectedammodellib@models <- list()
                    }
                }
                selecteddata <- values$checkedModelsData$data
                if(!is.null(selecteddata)) {
                    if(any(selecteddata)) {
                        selectedammodellib@data <- selectedammodellib@data[selecteddata]
                    } else {
                        selectedammodellib@data <- list()
                    }
                }
                values$editedAMModelLib <- selectedammodellib
            })
	    })
	    
	    # Get selected
        observe({
	        go <- input$getSelected
            isolate({
                selectedammodellib <- values$currentAMModelLib
                selectedmodels <- values$checkedModelsData$models
                mods <- dat <- NULL
                if(!is.null(selectedmodels)) {
                    if(any(selectedmodels)) {
                        mods <- lapply(which(selectedmodels), function(x) {
                            y <- getAMModel(x, selectedammodellib)
                            assign(names(selectedammodellib@models)[x], y, pos='.GlobalEnv')
                            y
                        })
                        names(mods) <- names(selectedammodellib@models)[selectedmodels]
                    } 
                }
                selecteddata <- values$checkedModelsData$data
                if(!is.null(selecteddata)) {
                    if(any(selecteddata)) {
                        dat <- lapply(which(selecteddata), function(x) {
                            y <- getAMData(x, selectedammodellib)
                            assign(names(selectedammodellib@data)[x], y, pos='.GlobalEnv')
                            y
                        })
                        names(dat) <- names(selectedammodellib@data)[selecteddata]
                    } 
                }
                z <- c(mods, dat)
                if(!is.null(z)) z <- z[!unlist(lapply(z, is.null))]
                if(length(z)) {
                    output$summaryOfEditedModel <- renderPrint({
                        cat('### Objects already saved to .GlobalEnv \n\n')
	                    for(x in 1:length(z)) {
	                        cat('#=================================\n# Object name:', names(z)[x], '\n')
	                        print(summary(z[[x]]))
	                    }
                    })
                }
            })
	    })
	    
        # Get selected as list
        observe({
	        go <- input$getAsListSelected
            isolate({
                selectedammodellib <- values$currentAMModelLib
                selectedmodels <- values$checkedModelsData$models
                mods <- dat <- NULL
                if(!is.null(selectedmodels)) {
                    if(any(selectedmodels)) {
                        mods <- lapply(which(selectedmodels), function(x) {
                            y <- getAMModel(x, selectedammodellib, as.list=TRUE)
                            assign(names(selectedammodellib@models)[x], y, pos='.GlobalEnv')
                            y
                        })
                        names(mods) <- names(selectedammodellib@models)[selectedmodels]
                    } 
                }
                selecteddata <- values$checkedModelsData$data
                if(!is.null(selecteddata)) {
                    if(any(selecteddata)) {
                        dat <- lapply(which(selecteddata), function(x) {
                            y <- getAMData(x, selectedammodellib, as.list=TRUE)
                            assign(names(selectedammodellib@data)[x], y, pos='.GlobalEnv')
                            y
                        })
                        names(dat) <- names(selectedammodellib@data)[selecteddata]
                    } 
                }
                z <- c(mods, dat)
                if(length(z)) {
                    output$summaryOfEditedModel <- renderPrint({
                        cat('### Objects already saved to .GlobalEnv \n\n')
	                    for(x in 1:length(z)) {
	                        cat('#=================================\n# Object name:', names(z)[x], '\n')
	                        print(summary(z[[x]]))
	                    }
                    })
                }
            })
	    })
        
        
        # Render modals to edit metadata and display model/data summaries 
        # Done from a different reactive source than that which edits the embedded  
        # table to avoid deleting a modal from the ui while it is open!
        output$renderedModals <- renderUI({
            # The modal need to be initially created: each time the tabs are clicked 
            input$amModelLibSelectedElement
            selected.model <- values$amModelLibForModals
#            selected.model <- getSelectedModel()
            if(!is.null(selected.model)) { 
                if(is(selected.model, 'amModelLib')) {
                    # this is the original human-readable name  
                    modnames <- names(selected.model@models)
                    datnames <- names(selected.model@data)
                    # these names are sanitized for js compatibility, plus add an integer for uniqueness
                    modnames.s <- paste0(gsub('\\.', '', modnames), 1:length(modnames))
                    datnames.s <- paste0(gsub('\\.', '', datnames), 1:length(datnames))
                    
                    nmodels <- length(modnames)
                    ndata <- length(datnames)
                    if(nmodels) {
                        # Assemble modals
                        modeditui <- lapply(1:length(modnames), function(x) {
                            bsModal(
                                id=paste0('edit', modnames.s[x],'_Modal'), 
                                title=p(style='text-align:center;font-weight:bold;', paste0('Editing ', modnames[x], ' Metadata')),
                                trigger=paste0('edit', modnames.s[x]), 
                                div(style='min-height:400px;',
                                    uiOutput(paste0('modelmetadatatablesUI-', x)),
                                    div(style='float:right;padding:2em;', actionButton(paste0('saveeditsto', modnames.s[x]), 'Save', icon=icon('save')))
                                ),
                                size='large'
                            )
                        })
                        modsummaryui <- lapply(1:length(modnames), function(x) {
                            bsModal(
                                id=paste0('summary', modnames.s[x],'_Modal'), 
                                title=p(style='text-align:center;font-weight:bold;', paste0('Summary of ', modnames[x])),
                                trigger=paste0('summary', modnames.s[x]), 
                                div(style='min-height:400px;overflow-x:scroll;',
                                    textOutput(paste0('modelsummaryUI-', x), container=tags$pre)
#                                    uiOutput(paste0('modelsummaryUI-', x))
                                ),
                                size='large'
                            )
                        })
                    } else {
                        modeditui <- modsummaryui <- NULL
                    }
                    if(ndata) {
                        dateditui <- lapply(1:length(datnames), function(x) {
                            bsModal(
                                id=paste0('edit', datnames.s[x],'_Modal'), 
                                title=p(style='text-align:center;font-weight:bold;', paste0('Editing ', datnames[x], ' Metadata')),
                                trigger=paste0('edit', datnames.s[x]), 
                                div(style='min-height:400px;',
                                    uiOutput(paste0('datametadatatablesUI-', x)),
                                    div(style='float:right;padding:2em;', actionButton(paste0('saveeditsto', datnames.s[x]), 'Save', icon=icon('save')))
                                ),
                                size='large'
                            )
                        })
                        datsummaryui <- lapply(1:length(datnames), function(x) {
                            bsModal(
                                id=paste0('summary', datnames.s[x],'_Modal'), 
                                title=p(style='text-align:center;font-weight:bold;', paste0('Summary of ', datnames[x])),
                                trigger=paste0('summary', datnames.s[x]), 
                                div(style='min-height:400px;overflow-x:scroll;',
                                    textOutput(paste0('datasummaryUI-', x), container=tags$pre)
#                                    uiOutput(paste0('datasummaryUI-', x))
                                ),
                                size='large'
                            )
                        })
                    } else {
                        dateditui <- datsummaryui <- NULL
                    }
                    
                    c(modeditui, modsummaryui, dateditui, datsummaryui)
                }
            }
        })        
        
        outputOptions(output, 'renderedModals', suspendWhenHidden = FALSE)
        
        
        # Create tables to edit metadata and summaries of models/data
        observe({
            # The tables need to be initially created: each time the tabs are clicked 
            input$amModelLibSelectedElement
            selected.model <- values$currentAMModelLib
            if(!is.null(selected.model)) { 
                if(is(selected.model, 'amModelLib')) {
                    # this is the original human-readable name  
                    modnames <- names(selected.model@models)
                    datnames <- names(selected.model@data)
                    allnames <- c(modnames, datnames)
                    # these names are sanitized for js compatibility, plus add an integer for uniqueness
                    modnames.s <- paste0(gsub('\\.', '', modnames), 1:length(modnames))
                    datnames.s <- paste0(gsub('\\.', '', datnames), 1:length(datnames))
                    # Make table headings
                    th <- '
                        <tr>
                            <th style="text-align:right;padding-right:1em;">Delete</th>
                            <th style="text-align:center;padding-right:1em;">Name</th>
                            <th style="text-align:center;">Value</th>
                        </tr>
                    '
                    nmodels <- length(modnames)
                    ndata <- length(datnames)
                    if(nmodels) {
                        # Make model td
                        modrows <- sapply(1:nmodels, function(x) {
                            newmetarow <- paste0('
                                <tr>
                                    <td style="width:8em;text-align:right;padding-right:1em;">
                                        <i class="fa fa-asterisk"></i>
                                    </td>
                                    <td style="width:12em;text-align:center;padding-right:1em;">
                                        <span class="form-group shiny-input-container">
                                            <input id="', paste0('add', modnames.s[x], '-name'), '" type="text" class="form-control" value=""/>
                                        </span> 
                                    </td>
                                    <td style="width:40em;text-align:left;">
                                        <span class="form-group shiny-input-container">
                                            <textarea id="', paste0('add', modnames.s[x], '-value'), '" rows=1, style="width:100%;"></textarea>
                                        </span>
                                    </td>
                                </tr>'
                            )
                            metadat <- modelMeta(selected.model, x)
                            oldmetadat <- sapply(1:length(metadat), function(y) {
                                paste0('
                                    <tr>
                                        <td style="width:8em;text-align:right;padding-right:1em;">
                                           <div class="checkbox">
                                                <label>
                                                    <input id="', paste0('select', modnames.s[x], '-', y),'" type="checkbox"/>
                                                </label>
                                            </div> 
                                        </td>
                                        <td style="width:12em;text-align:center;padding-right:1em;">
                                            <span class="form-group shiny-input-container">
                                                <input id="', paste0('edit', modnames.s[x], '-name-', y), '" type="text" class="form-control" value="', names(metadat)[y],'"/>
                                            </span> 
                                        </td>
                                        <td style="width:40em;text-align:left;">
                                            <span class="form-group shiny-input-container">
                                                <textarea id="', paste0('edit', modnames.s[x], '-value-', y), '" rows=1, style="width:100%;">' , metadat[[y]], '</textarea>
                                            </span>
                                        </td>
                                    </tr>'
                                )
                            })
                            paste0(paste0(oldmetadat, collapse=''), newmetarow, collapse='')
                        })
                    } else {
                        modrows <- NULL
                    }
                    if(ndata) {
                        # Make data td
                        datrows <- sapply(1:ndata, function(x) {
                            newmetarow <- paste0('
                                <tr>
                                    <td style="width:8em;text-align:right;padding-right:1em;">
                                        <i class="fa fa-asterisk"></i>
                                    </td>
                                    <td style="width:12em;text-align:center;padding-right:1em;">
                                        <span class="form-group shiny-input-container">
                                            <input id="', paste0('add', datnames.s[x], '-name'), '" type="text" class="form-control" value=""/>
                                        </span> 
                                    </td>
                                    <td style="width:40em;text-align:left;">
                                        <span class="form-group shiny-input-container">
                                            <textarea id="', paste0('add', datnames.s[x], '-value'), '" rows=1, style="width:100%;"></textarea>
                                        </span>
                                    </td>
                                </tr>'
                            )
                            metadat <- dataMeta(selected.model, x)
                            oldmetadat <- sapply(1:length(metadat), function(y) {
                                paste0('
                                    <tr>
                                        <td style="width:8em;text-align:right;padding-right:1em;">
                                           <div class="checkbox">
                                                <label>
                                                    <input id="', paste0('select', datnames.s[x], '-', y),'" type="checkbox"/>
                                                </label>
                                            </div> 
                                        </td>
                                        <td style="width:12em;text-align:center;padding-right:1em;">
                                            <span class="form-group shiny-input-container">
                                                <input id="', paste0('edit', datnames.s[x], '-name-', y), '" type="text" class="form-control" value="', names(metadat)[y],'"/>
                                            </span> 
                                        </td>
                                        <td style="width:40em;text-align:left;">
                                            <span class="form-group shiny-input-container">
                                                <textarea id="', paste0('edit', datnames.s[x], '-value-', y), '" rows=1, style="width:100%;">' , metadat[[y]], '</textarea>
                                            </span>
                                        </td>
                                    </tr>'
                                )
                            })
                            paste0(paste0(oldmetadat, collapse=''), newmetarow, collapse='')
                        })
                    } else {
                        datrows <- NULL
                    }
                }                        
                # Assemble tables
                nx <- lapply(1:length(modnames.s), function(x) {
                    output[[paste0('modelmetadatatablesUI-', x)]] <- renderUI({
                        HTML({paste0('<table id="', paste0('modelmetadatatablefor', modnames.s[x]), '">',th, paste0(modrows[x], collapse=''), '</table>', collapse='')})
                    })
                })
                ny <- lapply(1:length(datnames.s), function(x) {
                    output[[paste0('datametadatatablesUI-', x)]] <- renderUI({
                        HTML({paste0('<table id="', paste0('datametadatatablefor', datnames.s[x]), '">',th, paste0(datrows[x], collapse=''), '</table>', collapse='')})
                    })
                })
                # Make summaries too
                sx <- lapply(1:length(modnames.s), function(x) {
                    output[[paste0('modelsummaryUI-', x)]] <- renderPrint({
                        summary(selected.model@models[[x]]@model)
                    })
                })
                sy <- lapply(1:length(datnames.s), function(x) {
                    output[[paste0('datasummaryUI-', x)]] <- renderPrint({
                        summary(selected.model@data[[x]]@data)
                    })
                })
            }
        }) 
        
        
        # Customize the add model/data modal titles
        output$addmodeltitle <- renderUI({
            currentammlname <- values$selectedModelLib
            if(!is.null(currentammlname)) {
                p(style='text-align:center;font-weight:bold;', paste0('Add Model to ', currentammlname))
            }
        })
        
        output$adddatatitle <- renderUI({
            currentammlname <- values$selectedModelLib
            if(!is.null(currentammlname)) {
                p(style='text-align:center;font-weight:bold;', paste0('Add Data to ', currentammlname))
            }
        })
        # Generate the modal titles in advance to avoid 0.2 second delay on load 
        outputOptions(output, 'addmodeltitle', suspendWhenHidden = FALSE)
        outputOptions(output, 'adddatatitle', suspendWhenHidden = FALSE)
        
        # Make changes to models/data metadata
        observe({
            # See which button was clicked
            selected.model <- values$currentAMModelLib
            if(!is.null(selected.model)) {
                if(is(selected.model, 'amModelLib')) {
                    # Get model and data names/presence
                    modnames <- names(selected.model@models)
                    datnames <- names(selected.model@data)
                    nmodels <- length(modnames)
                    ndata <- length(datnames)
                    editedmodels <- editeddata <- NULL
                    if(!is.null(input$amModelLibSelectedElement)) {
                        if(nmodels) {
                            # sanitize names for js compatibility  
                            modbuttonnames <- paste0('saveeditsto', gsub('\\.', '', modnames), 1:length(modnames))
                            modmetanames <- paste0('edit', gsub('\\.', '', modnames), 1:length(modnames), '-')
                            modcheckednames <- paste0('select', gsub('\\.', '', modnames), 1:length(modnames), '-')
                            addmodnames <- paste0('add', gsub('\\.', '', modnames), 1:length(modnames), '-')
                            # Compare add model save button values
                            editedmodels <- list()
                            for(x in 1:length(modbuttonnames)) {
                                if(!is.null(input[[modbuttonnames[x]]]) && !is.null(editedModelsData[['models']][x]) && !is.na(editedModelsData[['models']][x])) {
                                    newmeta <- NULL
                                    if(input[[modbuttonnames[x]]] != editedModelsData$models[x]) {
                                        oldmeta <- modelMeta(selected.model, x)
                                        newmeta <- lapply(1:length(oldmeta), function(y) {
                                            input[[paste0(modmetanames[x], 'value-', y)]]
                                        })
                                        # set checked to null for removal
                                        newmeta <- lapply(1:length(oldmeta), function(y) {
                                            if(input[[paste0(modcheckednames[x], y)]]) {
                                                NULL
                                            } else {
                                                newmeta[[y]]
                                            }
                                        })
                                        names(newmeta) <- sapply(1:length(oldmeta), function(y) input[[paste0(modmetanames[x], 'name-', y)]])
                                        # look for new metadata
                                        if(input[[paste0(addmodnames[x], 'value')]] != '' && input[[paste0(addmodnames[x], 'name')]] != '') {
                                            addedmeta <- list(input[[paste0(addmodnames[x], 'value')]])
                                            names(addedmeta) <- input[[paste0(addmodnames[x], 'name')]]
                                        } else {
                                            addedmeta <- NULL
                                        }
                                        newmeta <- c(newmeta, addedmeta)
                                        modelMeta(selected.model, x) <- newmeta
                                    }
                                    editedmodels[[x]] <- input[[modbuttonnames[x]]]
                                } else {
                                    editedmodels[[x]] <- 0
                                }
                            }
                            if(!is.null(editedmodels)) names(editedmodels) <- modbuttonnames
                        }
                        if(ndata) {
#                            # sanitize names for js compatibility  
                            datbuttonnames <- paste0('saveeditsto', gsub('\\.', '', datnames), 1:length(datnames))
                            datmetanames <- paste0('edit', gsub('\\.', '', datnames), 1:length(datnames), '-')
                            datcheckednames <- paste0('select', gsub('\\.', '', datnames), 1:length(datnames), '-')
                            adddatnames <- paste0('add', gsub('\\.', '', datnames), 1:length(datnames), '-')
                            # Compare add data save button values
                            editeddata <- list()
                            for(x in 1:length(datbuttonnames)) {
                                if(!is.null(input[[datbuttonnames[x]]]) && !is.null(editedModelsData[['data']][x]) && !is.na(editedModelsData[['data']][x])) {
                                    newmeta <- NULL
                                    if(input[[datbuttonnames[x]]] != editedModelsData$data[x]) {
                                        oldmeta <- dataMeta(selected.model, x)
                                        newmeta <- lapply(1:length(oldmeta), function(y) {
                                            input[[paste0(datmetanames[x], 'value-', y)]]
                                        })
                                        # set checked to null for removal
                                        newmeta <- lapply(1:length(oldmeta), function(y) {
                                            if(input[[paste0(datcheckednames[x], y)]]) {
                                                NULL
                                            } else {
                                                newmeta[[y]]
                                            }
                                        })
                                        names(newmeta) <- sapply(1:length(oldmeta), function(y) input[[paste0(datmetanames[x], 'name-', y)]])
                                        # look for new metadata
                                        if(input[[paste0(adddatnames[x], 'value')]] != '' && input[[paste0(adddatnames[x], 'name')]] != '') {
                                            addedmeta <- list(input[[paste0(adddatnames[x], 'value')]])
                                            names(addedmeta) <- input[[paste0(adddatnames[x], 'name')]]
                                        } else {
                                            addedmeta <- NULL
                                        }
                                        newmeta <- c(newmeta, addedmeta)
                                        dataMeta(selected.model, x) <- newmeta
                                    }
                                    editeddata[[x]] <- input[[datbuttonnames[x]]]
                                } else {
                                    editeddata[[x]] <- 0
                                }
                            }
                            if(!is.null(editeddata)) names(editeddata) <- datbuttonnames
                        }
                    }
                    
                }
                editedModelsData <<- list(models=editedmodels, data=editeddata)
                values$currentAMModelLib <- selected.model
            }
            
        })
        
        
        # Select a model object to add to the ammodellib
        output$addModelToAMMLUI <- renderUI({
            choices <- getFromGlobalEnv()
            if(!is.null(choices)) {
                if(length(choices)) {
                    names(choices) <- choices
                    radioButtons('selectnewmodelobject', 'Select model', choices=choices)
                } else {
                    div(
                        p(style='font-weight:bold;', 'Select model'),
                        p('No objects to list.')
                    )
                }
            }
        })

        # Add selected model to the amml
        observe({
            input$saveNewModel
            isolate({
                newmodname <- make.names(input$newModelName)
                newmod <- input$selectnewmodelobject
                if(!is.null(newmod) && input$newModelName != '') {
                    if(!is.na(newmod) && newmod != '') {
                        newmod <- .GlobalEnv[[newmod]]
                        newmod <- list(amModel(newmod))
                        names(newmod) <- newmodname
                        values$currentAMModelLib@models <- c(values$currentAMModelLib@models, newmod)
                        values$amModelLibForModals@models <- c(values$currentAMModelLib@models, newmod)
                        updateTextInput(session, 'newModelName', value='')
                    }
                }
            })
        })
        
        # Select a data object to add to the ammodellib
        output$addDataToAMMLUI <- renderUI({
            choices <- getFromGlobalEnv()
            if(!is.null(choices)) {
                if(length(choices)) {
                    names(choices) <- choices
                    radioButtons('selectnewdataobject', 'Select data object', choices=choices)
                } else {
                    div(
                        p(style='font-weight:bold;', 'Select data object'),
                        p('No objects to list.')
                    )
                }
            }
        })
        
        # Add selected data to the amml
        observe({
            input$saveNewData
            isolate({
                newdatname <- make.names(input$newDataName)
                newdat <- input$selectnewdataobject
                if(!is.null(newdat) && input$newDataName != '') {
                    newdat <- .GlobalEnv[[newdat]]
                    newdat <- list(amData(newdat))
                    names(newdat) <- newdatname
                    values$currentAMModelLib@data <- c(values$currentAMModelLib@data, newdat)
                    values$amModelLibForModals@data <- c(values$currentAMModelLib@data, newdat)
                    updateTextInput(session, 'newDataName', value='')
                }
            })
        })
        
        
        # Search models or data
        observe({
            input$searchModel
            isolate({
                if(input$searchModelString != '') {
                    selectedModel <- values$currentAMModelLib
#                    selectedModel <- getSelectedModel()
	                if(!is.null(selectedModel)) {
	                    if(input$searchModelComponent == '') {
	                        search <- 'all'
	                        updateSelectInput(session, 'searchModelComponent', selected='all')
                        } else {
                            search <- input$searchModelComponent
                        } 
	                    values$editedAMModelLib <- grepAMModelLib(input$searchModelString, selectedModel, search=search)
	                }
                }
            })
        })
        
        # Send a summary of edited amModelList to the screen
        output$summaryOfEditedModel <- renderPrint({
            editedModel <- values$editedAMModelLib
	        if(!is.null(editedModel)) {
	            summary(editedModel)
	        }
        })
        
        # Save the edited amModelList to the global environment 
        observe({
            input$sendAMMLToGlobalEnv
            isolate({
                editedModel <- values$editedAMModelLib
                amml <- make.names(input$newObjectName)
                if(amml != '' && !is.null(editedModel)) {
                    assign(amml, editedModel, pos='.GlobalEnv')
                    updateRadioButtons(session, 'selectAMModelLib', selected=input$selectAMModelLib)
                    # create an alert to notify
                    createTimedAlert(session, anchorId='saveAlert', alertId='saveSuccessAlert', title='', content=paste0('<p style="text-align:center;">"', amml, '" is now in your Global Environment.</p>'), timeout=2000, style='info', append=TRUE)
                }
            })
        })
        
        # Set the current amModelList to the edited amModelList
        observe({
            input$saveallchanges
            isolate({
                amml <- values$currentAMModelLib
                if(!is.null(amml)) {
                    exinfo <- amml@info
                    newinfo <- NULL
                    if(length(exinfo)) {
                        rminfo <- sapply(1:length(exinfo), function(x) input[[paste0('selectCurrentInfo-', x)]])
                        exinfo <- exinfo[!rminfo]
                    }
                    if(!is.null(input[['addinfotocurrent-name']])) {
                        if(input[['addinfotocurrent-name']] != '' && input[['addinfotocurrent-value']] != '') {
                            newinfo <- list(input[['addinfotocurrent-value']])
                            names(newinfo) <- make.names(input[['addinfotocurrent-name']])
                        }
                    }
                    amml@info <- c(exinfo, newinfo)
                    if(input$amModelLibDescription != '') ammlDesc(amml) <- input$amModelLibDescription
                    values$editedAMModelLib <- amml
                }
            })
        })
        
		session$onSessionEnded(function() {
		    # Remove the saved console
		    saved_console <- ".RDuetConsole"
		    # remove the aux functions
		    rm(list=c('createTimedAlert', 'textareaInput'), pos='.GlobalEnv')
            if (file.exists(saved_console)) unlink(saved_console)
            stopApp()
        })
    }
)





