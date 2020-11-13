# fix for check NOTE re: visible bindings of NSE
# reference: https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
utils::globalVariables(c("all_specs", "spec_group", "spec_case", "test_group", "values", "test_case"))
