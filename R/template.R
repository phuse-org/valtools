#' templating function
#'
#' Access valtools templates and create modified version based on the input data
#'
#' @param path output path to save rendered template to
#' @param template template to use from valtools/inst/templates
#' @param data named list or environment with variables that will be used during rendering
#'
#' @importFrom whisker whisker.render
#' @importFrom rlang abort
#'
#' @noRd
#'
render_template <- function(template, output = template, data = list()){

  template_path <- file.path(
    system.file("templates", package = "valtools"),
    template)

  if(!file.exists(template_path)){
    abort(paste0("Template `",template,"` does not exist."),
          class = "vt.template_exist_fail")
  }

  template_text <- readLines(template_path)

  tryCatch({

    output_text <- whisker.render(
      template = template_text,
      data = data)

    file_con <- file(output)
    on.exit(close(file_con))

    writeLines(
      output_text,
      con = file_con
    )

  }, error = function(e) {
    abort(paste0(c("Error during creation of template `",template,"`. Error: ",
                   e, sep = "\n")),
          class = "vt.template_render_fail")
  })

  invisible(TRUE)

}

