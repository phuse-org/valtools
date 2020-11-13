
#' @title Generate a coverage matrix from the specification reference in test cases
#' @description May be used for specification or test case .Rmd files located
#' in \code{function_specification} folder.
#' @param files path to folder containing .Rmd file to scrape
#' @return data.frame with variables: \code{test_case}, \code{specifications}, and
#' \code{value = "x"}.
#' @export
#' @importFrom stringr str_split str_detect str_replace_all str_extract_all str_pad str_trim str_squish
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr mutate bind_rows select arrange
#' @importFrom purrr map_dfr map
#' @importFrom magrittr set_colnames %>%
scrape_coverage_block <- function(files){
  files %>%
    map_dfr(.f = function(one_file){
      lines <- readLines(one_file)
      rox_block <- gsub(pattern = "#' ", replacement = "",
                        x = lines[grep(pattern = "#' ", x = lines)])

      spec_start <- grep(rox_block, pattern = "@section [Ss]p")
      for(i in spec_start){
        all_sections <- grep(rox_block, pattern = "@section")
        spec_end <- min(length(rox_block), all_sections[all_sections > i])

        out <- rox_block[(i + 1):spec_end] %>%
          str_split(pattern = ":") %>%
          map(magrittr::set_names, c("test_case", "all_specs")) %>%
          map(.f = function(x, ...){
            x["all_specs"] <- str_trim(str_squish(x["all_specs"]))
            data.frame(test_case = paste0("T", unname(x["test_case"])),
                       all_specs = str_split(x["all_specs"], pattern = ", "),
                       stringsAsFactors = FALSE) %>%
              magrittr::set_colnames(c("test_case", "all_specs")) %>%
              mutate(spec_group =  unlist(str_split(all_specs, pattern = "\\."))[1],
                     spec_case = ifelse(str_detect(tolower(all_specs), pattern = "all"),
                                        "All",
                                        paste0("S",
                                               str_replace_all(all_specs,  pattern = "^[[:digit:]]{3}\\.",
                                                               replacement = ""))),
                     all_specs = paste0(spec_group, ".", spec_case))

          }) %>%

          bind_rows() %>%

          mutate(test_group = gsub(pattern = "@title ",
                                   rox_block[grep(rox_block, pattern = "@title")],
                                   replacement = ""),
                 values = "x",
                 spec_case = paste0(ifelse(spec_case == "All",
                                           spec_case,
                                           str_extract_all(spec_case, pattern = "^S[[:digit:]]{2}\\.")),

                                    ifelse(str_detect(tolower(all_specs), pattern = "all"),
                                           "",
                                           str_pad(str_extract_all(all_specs, pattern = "[[:digit:]]{1,2}$"),
                                                   width = 2, side = "left", pad = 0)))) %>%

          arrange(test_group, spec_case) %>%
          select(test_group, test_case, all_specs, values, -spec_group, -spec_case ) %>%
          pivot_wider(names_from = all_specs, values_from = values,
                      values_fn = list(values = unique))

        return(out)
      }
    })
}


