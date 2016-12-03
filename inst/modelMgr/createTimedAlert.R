createTimedAlert <- function (session, anchorId, alertId = NULL, title = NULL, content = NULL, 
    timeout = 2000, style = NULL, dismiss = FALSE, append = FALSE) 
{
    data <- shiny:::dropNulls(list(id = anchorId, alertId = alertId, 
        title = title, content = content, timeout=timeout, style = style, dismiss = dismiss, 
        append = append))
    session$sendCustomMessage(type = "timedAlertCreate", message = data)
}


