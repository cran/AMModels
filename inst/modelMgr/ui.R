library(shiny)
library(shinyBS)

source('textareaInput.R')

shinyUI(
	fluidPage(
		includeCSS("www/css/custom.css"),
#        headerPanel('AMModels Model Manager: organize your models and data'),
        singleton(tags$head(
            tags$title('Model Manager'), 
            tags$script(src="js/timedAlertCreate.js")
        )),
        # Modal for general information
        bsModal(
            id='generalInfoModal', 
            title=p(style='text-align:center;font-weight:bold;', 'AMModels Model Manager General Information'),
            trigger='generalInfo', 
            div(style='min-height:400px;',
                h3('Editing an amModelLib Object:'),
                p('All additions to, deletions from, and edits to metadata made on the left side of the screen are temporary. To make them permanent:'), 
                div(style='padding:0.5em 1.5em 0.5em 0em;background-color:#f5f5f5;', 
                    HTML({'
                        <ol>
                            <li>Click the create binding button below the search box.</li>
                            <li>Type a name for the new object below the summary that appeared in the gray box on the right side of the screen. <div style="margin-left:34px;background-color:#ffffcc;border-left:6px solid #ffeb3b;padding:0.5em 2em;"><p style="padding-top:0.5em;">To overwrite the current object, set the name identical to the object being edited.</p></div></li>
                        <li>Click the create binding button next to the name box. <div style="margin-left:34px;background-color:#ffffcc;border-left:6px solid #ffeb3b;padding:0.5em 2em;"><p style="padding-top:0.5em;">If you overwrote the object being edited, you may need to re-select the object from your Global Environment to verify the changes. You may also do this to revert changes before a binding has been created.</p></div></li>'
                    })
                )
            ),
            size='large'
        ),
        # The modals for editing models and data
        uiOutput('renderedModals'),
        # Modal to import data        
        bsModal(
            id='uploadDataModal', 
            title=p(style='text-align:center;font-weight:bold;', 'Select Source and amModelLib'),
            trigger='getUserModelLib', 
            div(style='min-height:400px;width:100%;',
#                div(style='width:100%;',
                    column(4,
                        div(style='padding-right:1em;padding-top:1.75em;',
                            actionButton('createNewModel', 'New', icon=icon('file'), width='100%'),
                            bsTooltip(id='createNewModel', title='Create a new amModelLib object and then populate it with models and data from your Global Environment', placement = "top", trigger = "hover", options = list(container='body'))
                        )
                    ),
                    column(4,
                        div(style='padding:1.75em 1em 0em 1em;',
                            actionButton('listGlobalEnv', 'List environment', icon=icon('list'), width='100%'),
                            bsTooltip(id='listGlobalEnv', title='List amModelLib objects in your Global Environment', placement = "top", trigger = "hover", options = list(container='body'))
                        )
                    ),
                    column(4,
                        div(id='datauploadcontainer', style='padding-left:1em;',
                            fileInput('userDataUpload', 'Upload file', accept=c('.RData', '.rdata', '.Rdata', '.rda', '.Rda', '.RDA', '.RDS', '.rds', 'application/octet-stream'), width='100%')
                        ),
                        bsTooltip(id='datauploadcontainer', title='Upload a .RData, .rda, or .RDS file containing amModelLib objects', placement = "top", trigger = "hover", options = list(container='body'))
                    ),
                    div(style='clear:both;padding-top:1em',
#                        column(4, offset=4,
                            bsAlert('emptyDataSource'),
                            checkboxInput('filterForAMModelLibs', 'Show only amModelLib objects', value=TRUE),
                            uiOutput('selectModelUI'),
                            textOutput('selectedModelSummary', container=tags$pre)
#                        )
                    )
#                )
            ),
            size='large'
        ),
        # Modal to add models
        bsModal(
            id='addModelToAMMLmodal', 
            title=uiOutput('addmodeltitle'),
            trigger='addModelToAMML', 
            div(style='min-height:400px;',
                column(6,
                    uiOutput('addModelToAMMLUI')
                ),
                column(6,
                    HTML(paste0('   
                        <span class="form-group shiny-input-container" style="margin-bottom:0px;width:30em;">
                            <label for="newModelName">Provide name</label>
                            <input style="margin-top:2px;
                                width:25em;
                                border-radius:4px;
                                height: 34px;
                                padding: 0px 12px 5px 12px;
                                font-size: 14px;
                                line-height: 1.42857143;
                                color: #555;
                                background-color: #fff;
                                background-image: none;
                                border: 1px solid #ccc;
                                box-shadow: inset 0 1px 1px rgba(0,0,0,.075);
                                transition: border-color ease-in-out .15s,box-shadow ease-in-out .15s;" id="newModelName" type="text" value="" placeholder="Model name"/>
                            <script>
                                $(document).keyup(function(event) {
                                    if ($("#newModelName").is(":focus") && (event.keyCode == 13)) {
                                        $("#saveNewModel").click();
                                    }
                                });
                            </script>',
                            actionButton('saveNewModel', '', icon=icon('save')),
                        '</span>'
                    )),
                    p(style='padding-top:1em;', 'You can add metadata by clicking "Edit" in the main display.'),
                    HTML('<p>Names are coerced to valid R names. Valid names consist of "letters, numbers, and the dot and underscore characters, and starts with either a letter or a dot not followed by a number." Valid names may not be reserved words. For more information, see <a href="https://cran.r-project.org/doc/FAQ/R-FAQ.html#What-are-valid-names_003f" target="_blank">the R FAQ about names.</a>')
                )
            ),
            size='large'
        ),
        # Modal to add data
        bsModal(
            id='addDataToAMMLmodal', 
            title=uiOutput('adddatatitle'),
            trigger='addDataToAMML', 
            div(style='min-height:400px;overflow:hidden;',
                column(6,
                    uiOutput('addDataToAMMLUI')
                ),
                column(6,
                    HTML(paste0('   
                        <span class="form-group shiny-input-container" style="margin-bottom:0px;width:30em;">
                            <label for="newDataName">Provide name</label>
                            <input style="margin-top:2px;
                                width:25em;
                                border-radius:4px;
                                height: 34px;
                                padding: 0px 12px 5px 12px;
                                font-size: 14px;
                                line-height: 1.42857143;
                                color: #555;
                                background-color: #fff;
                                background-image: none;
                                border: 1px solid #ccc;
                                box-shadow: inset 0 1px 1px rgba(0,0,0,.075);
                                transition: border-color ease-in-out .15s,box-shadow ease-in-out .15s;" id="newDataName" type="text" value="" placeholder="Data object name"/>
                            <script>
                                $(document).keyup(function(event) {
                                    if ($("#newDataName").is(":focus") && (event.keyCode == 13)) {
                                        $("#saveNewData").click();
                                    }
                                });
                            </script>',
                            actionButton('saveNewData', '', icon=icon('save')),
                        '</span>'
                    )),
                    p(style='padding-top:1em;', 'You can add metadata by clicking "Edit" in the main display.'),
                    HTML('<p>Names are coerced to valid R names. Valid names "consists of letters, numbers, and the dot and underscore characters, and starts with either a letter or a dot not followed by a number." Valid names may not be reserved words. For more information, see <a href="https://cran.r-project.org/doc/FAQ/R-FAQ.html#What-are-valid-names_003f" target="_blank">the R FAQ about names.</a>')
                )
            ),
            size='large'
        ),
        # Modal to create new amModelLib
        bsModal(
            id='createNewModelmodal', 
            title=p(style='text-align:center;font-weight:bold;', 'Create New amModelLib'),
            trigger='createNewModel', 
            div(style='min-height:400px;padding:0em 1em;',
                column(6,
                    textInput('newAMModelLibName', 'amModelLib Name', width='100%')
                ),
                column(6,
                    p(style='padding:2.25em 0em 0em 1em;','Coerced to a valid R name if necessary')
                ),
                div(style='clear:both;',
                    textareaInput('newAMModelLibDescription', 'Description', rows=3, width = '100%'),
                    p(style='font-weight:bold;', 'Add Info/Metadata'),
                    uiOutput('newAMModelLibInfoUI')
                ),
                column(6,
#                    checkboxInput('newAMModelLibRemoteActive', 'Edit on close', value=TRUE), 
                    #actionButton('newAMModelLibGo', 'Create amModelLib')
                    div(style='margin-top:17px;',
                        HTML('
                            <button id="newAMModelLibGo" type="button" class="btn btn-default action-button" data-dismiss="modal">Create amModelLib</button>'
                        ),
                        tags$script('document.getElementById("listGlobalEnv").focus();')
                    )
                )
            ),
            size='large'
        ),
        # Main page header
        HTML(paste0('
            <a id="generalInfo" href="#" class="action-button" style="color: #333;text-decoration:none;">',
                h1(style='text-align:center;', 'AMModels Model Manager: organize your models and data'), '
        
            </a>'
        )),
        bsTooltip(id='generalInfo', title='See general information about using the Model Manager', placement = "bottom", trigger = "hover", options = list(container='body')),
                        
#        h1(style='text-align:center;', 'AMModels Model Manager: organize your models and data'),
        
        
        
        # Main page
        div(style='width:100%;clear:both;',
            div(class='boxed', style='padding:2em;margin-top:16px;',
                column(6,
                    div(style='padding:0em 3em 1em;',
                        actionButton('getUserModelLib', 'Select amModelLib', icon=icon('check-square-o'), width='100%', class='btn-primary'),
                        bsTooltip(id='getUserModelLib', title='Create an amModelLib, upload an .RData or .rda, or select one from your Global Environment', placement = "top", trigger = "hover", options = list(container='body'))
                    ),
                    # Report which modelLib is selected and where it came from
                    div(style='width:100%;overflow:hidden;',
                        span(style='float:left;',
                            uiOutput('reportSelectedModel'),
                            uiOutput('reportSelectedModelSource')
                        ),
                        span(style='float:right;',
                            actionButton('revertchanges', '', icon=icon('undo'), style='margin-right:32px;border:transparent;'),
                            bsTooltip(id='revertchanges', title='Revert all changes', placement = "top", trigger = "hover", options = list(container='body')),
                            
                            actionButton('saveallchanges', '', icon=icon('arrow-circle-right', class='fa-2x'), style='border:transparent;'),
                            bsTooltip(id='saveallchanges', title='Move all changes to the output preview pane', placement = "top", trigger = "hover", options = list(container='body'))
                        )
                    ),
##                   These moved to a tab in the 'selectedModelContents' panel below
#                    div(style='clear:both;padding-bottom:1em;',
#                        textareaInput('amModelLibDescription', 'Description', rows=3, width = '100%'),
#                        p(style='font-weight:bold;', 'Info/Metadata'),
#                        uiOutput('amModelLibInfoUI')
#                    ),
                    
                    # The gray search bar
                    div(style='background-color:#E0E0E0;width:100%;overflow:hidden;margin-bottom:8px;',
#                        span(style='float:left;padding-top:0.25em;', 
#                            actionButton('generalInfo', '', icon=icon('info-circle'), style="background-color:#E0E0E0;border:transparent;color:#337ab7;"),
#                            bsTooltip(id='generalInfo', title='General information about using the Model Manager', placement = "top", trigger = "hover", options = list(container='body'))
#                        ),
                        span(style='float:right;',
                            # Search box and select input are coded manually 
                            HTML(paste0('
                                <span class="form-group shiny-input-container" style="margin:2px 0px;width:12em;padding-right:0.3em;">
                                    <input style="
                                        margin:3px 0px 2px;
                                        width:18em;
                                        border-radius:4px;
                                        height: 34px;
                                        padding: 0px 12px 5px 12px;
                                        font-size: 14px;
                                        line-height: 1.42857143;
                                        color: #555;
                                        background-color: #fff;
                                        background-image: none;
                                        border: 1px solid #ccc;
                                        box-shadow: inset 0 1px 1px rgba(0,0,0,.075);
                                        transition: border-color ease-in-out .15s,box-shadow ease-in-out .15s;" id="searchModelString" type="text" value="" placeholder="Search expression"/>
                                    <script>
                                        $(document).keyup(function(event) {
                                            if ($("#searchModelString").is(":focus") && (event.keyCode == 13)) {
                                                $("#searchModel").click();
                                            }
                                        });
                                    </script>
                                    <span class="form-group shiny-input-container" style="float:right;padding-top:2px;width:10em;margin:1px 0px 2px;">
                                        <select id="searchModelComponent" style="padding: 0px 12px 5px 12px;width:9em;height:34px;background-color:#fff;border-radius:4px;border:1px solid #ccc;">
                                        <!--<option value="" selected>Search in:</option>-->
                                            <option value="all" selected>in: all</option>
                                            <option value="model">in: models</option>
                                            <option value="data">in: data</option>
                                        </select>
                                        <script type="application/json" data-for="searchModelComponent" data-nonempty="">{}</script>
                                    </span>',
                                    actionButton('searchModel', '', icon=icon('search')),
                                '</span>'
                            )),
                            bsTooltip(id='searchModelString', title='Regular expressions are allowed', placement = "top", trigger = "hover", options = list(container='body')),
                            bsTooltip(id='searchModelComponent', title='Search can be done across all models and data, or confined to models or data.', placement = "top", trigger = "hover", options = list(container='body'))
                        )
                    ),
                    # The tabs for editing modelLib
                    wellPanel(style='clear:both;width:100%;overflow:hidden;',
                        div(style='width:100%;',
                            span(style='float:right;padding-right:1em;',
                                p(style='font-weight:bold;display:inline-block;', 'With selected:'),
                                actionButton('deleteSelected', '', icon=icon('times')),
                                actionButton('newAMMLFromSelected', '', icon=icon('external-link')),
                                actionButton('getSelected', '', icon=icon('arrows-alt')),
                                actionButton('getAsListSelected', '', icon=icon('list'))
                            ),
                            bsTooltip(id='deleteSelected', title='Delete', placement = "top", trigger = "hover", options = list(container='body')),
                            bsTooltip(id='newAMMLFromSelected', title='Add to new amModelLib', placement = "top", trigger = "hover", options = list(container='body')),
                            bsTooltip(id='getSelected', title='Extract to .GlobalEnv', placement = "top", trigger = "hover", options = list(container='body')),
                            bsTooltip(id='getAsListSelected', title='Extract as list to .GlobalEnv', placement = "top", trigger = "hover", options = list(container='body'))
                        ),
                        uiOutput('selectedModelContents')
                    )
                ),
                column(6,
                    div(style='padding:1.25em 0em 0em 1em;',
                        uiOutput('reportLastActionTarget'),
                        div(style='width:100%;min-height:50px;',
                            # Name & save box are coded manually
                            HTML(paste0('   
                                <span id="saveAlert" class="sbs-alert" style="float:left;width:25%;"></span>
                                <span style="float:right;">
                                <span class="form-group shiny-input-container" style="margin-bottom:0px;width:12em;padding-left:8em;">
                                    <input style="margin-top:2px;
                                        width:18em;
                                        border-radius:4px;
                                        height: 34px;
                                        padding: 0px 12px 5px 12px;
                                        font-size: 14px;
                                        line-height: 1.42857143;
                                        color: #555;
                                        background-color: #fff;
                                        background-image: none;
                                        border: 1px solid #ccc;
                                        box-shadow: inset 0 1px 1px rgba(0,0,0,.075);
                                        transition: border-color ease-in-out .15s,box-shadow ease-in-out .15s;" id="newObjectName" type="text" value="" placeholder="New library name"/>
                                    <script>
                                        $(document).keyup(function(event) {
                                            if ($("#newObjectName").is(":focus") && (event.keyCode == 13)) {
                                                $("#sendAMMLToGlobalEnv").click();
                                            }
                                        });
                                    </script>',
                                    actionButton('sendAMMLToGlobalEnv', '', icon=icon('save')),
                                '</span></span>'
                            )),
                            bsTooltip(id='newObjectName', title='Name must be provided for object summarized below to be moved to your Global Environment. Coerced to valid R name. Objects in your Global Environment must still be saved to disk using save() or saveRDS().', placement = "top", trigger = "hover", options = list(container='body'))
                        ),
                        div(style='clear:both;',
                            p(style='font-weight:bold;', 'Output Preview:'),
                            textOutput('summaryOfEditedModel', container=tags$pre)
                        )
                    )
                )
            )            
        )
        ######## OBSERVER CONSOLE #########
#        div(style="width:100%;",
#            div(style="clear:both;text-align:center;",
#                actionLink("console","server console")
#            )
#        )
    )
)






























