# The rest of this documentation is in case.R
#' @param title Title for the requirement defaults to be the base name passed
#'   sans file paths or extensions.
#' @param use_shiny if \code{TRUE}, collect requirements details through a 
#'   shiny gadget in RStudio's viewer pane.   
#'   If RStudio or its viewer pane/dialog APIs are not available in the 
#'   current R environment, then the gadget will be displayed in the system's 
#'   default web browser.
#'
#' @importFrom whisker rowSplit
#'
#' @rdname new_item
#' @export
vt_use_req <- function(name, username = vt_username(), title = NULL, open = interactive(),
                       add_before = NULL, add_after = NULL, use_shiny = FALSE){
  
  # ensure file extensions are of the acceptable type
  name <- vt_set_ext(name, ext = c("md", "rmd"))
  
  is_valid_name(name)
  
  # Create file to write in
  req_name <- create_item("requirements", name)
  
  ## if the file didnt exist before, populate with contents
  if (file.size(req_name) == 0){
    
    if(is.null(title)){
      title <- basename(file_path_sans_ext(req_name))
    }
    
    reqs = " "
    
    if(use_shiny){
      reqList <- vt_use_req_shiny(name, username, title)
      username <- reqList$username
      title <- reqList$title
      editDate <- reqList$editDate
      reqs <- unname(rowSplit(reqList$reqs))
    }
    
    # Create the content to write
    render_template("requirements", output = req_name,
                    data = list(
                      username = username,
                      title = title,
                      editDate = as.character(Sys.Date()),
                      reqs = reqs
                    ))
    
    vt_add_file_to_config(
      filename = name,
      after = {{add_after}},
      before = {{add_before}}
    )
  }
  
  
  if(open){
    edit_file(req_name) # nocov
  }
  
  invisible(req_name)
  
}

#' provide a shiny gadget interface to collect  requirements
#'
#' This function is not intended for use by the end users, and is only called 
#' internally by vt_use_req()
#' 
#' @param name name from vt_use_req()
#' @param username username from vt_use_req()
#' @param title title from vt_use_req()
#' 
#' @importFrom shiny textInput observeEvent reactiveValues paneViewer runGadget stopApp
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom rhandsontable rhandsontable rHandsontableOutput renderRHandsontable hot_to_r
#' 
#' @noRd
vt_use_req_shiny <- function(name, username, title){
  
  dfReqs <- data.frame(ID = "1.1",
                       Requirement = "Start typing here to document requirements.\n Right click to insert or remove rows.",
                       stringsAsFactors = FALSE)
  
  
  # Our ui will display a simple gadget page to collect 
  # user input
  ui <- miniPage(
    gadgetTitleBar(name),
    miniContentPanel(
      textInput("title", "Title:", value= title),
      textInput("username", "Editor's name:", value= username),
      textInput("editDate", "Edit date:", value = as.character(Sys.Date())),
      rHandsontableOutput("reqTable")
    )
  )
  
  server <- function(input, output, session) {
    rvReqs <- reactiveValues(data= dfReqs)
    output$reqTable <- renderRHandsontable(
      rhandsontable(rvReqs$data, rowHeaders = NULL))
    
    #Saves the added or removed row 
    observeEvent(input$reqTable,{
      rvReqs$data <- hot_to_r(input$reqTable)
    })
    
    # Listen for 'done' events. When we're finished, we'll
    # pass on the data in reqList list.
    observeEvent(input$done, {
      lsReqs <- list(username = input$username,
                     title = input$title,
                     editDate = input$editDate,
                     reqs = rvReqs$data)
      stopApp(lsReqs)
    })
    
  }
  
  # Use a paneViewer, and set the minimum height at 400
  viewer <- paneViewer('maximize')
  runGadget(ui, server, viewer = viewer)
}
