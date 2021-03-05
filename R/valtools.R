## documenting information for developers

## System Environment Variables

# vt_validation_state:
#   description:
#     environment variable that indicates whether the
#     validation is being performed on an installed package or on
#     source code/build.
#   set by:
#     vt_render_validation_report
#   used by:
#     vt_path - determines whether path is to the system file or uses here::here
# vt_validation_package:
#   description:
#     environment variable that is which installed package is to have the validation
#     report executed for.
#   set by:
#     vt_render_validation_report
#   used by:
#     vt_path - determines which package to pass to system.file when
#       running the validation script for an installed package.
NULL


