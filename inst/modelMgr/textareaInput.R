textareaInput <- function (
    inputId, 
    label, 
    value = "", 
    rows=1, 
    width = NULL, 
    placeholder = NULL
) {
    shiny::tags$div(
        class = "form-group shiny-input-container", 
        style = if (!is.null(width)) paste0("width: ", shiny::validateCssUnit(width), ";"), 
        shiny::tags$label(label, `for`= inputId), 
        shiny::HTML(paste0('
            <div class="form-group shiny-input-container" style="width: 100%;">
              <textarea id="', inputId, '" value="', value, '" rows="', rows, '" cols="1" style="width:100%;">', value, '</textarea>
            </div>'
        ))
#        shiny::tags$textarea(id = inputId, 
#            value = value, 
#            placeholder = placeholder,
#            rows=rows, cols=1, style="width:100%;"
#        )
    )
}
