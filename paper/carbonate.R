# load pacakges
library(carbonate)

# declare variables
code_file <- "paper/case-study.txt"

# prelim variables
file <- file.path(tempdir(), "paper/carbon.png")
if (file.exists(file)) unlink(file)

# import code
handle <- carbon$new(readLines(code_file))

# customize code rendering
handle$template <- "vscode"
handle$palette <-
  setNames(c(t(col2rgb("#1d211f")), 100), c("r", "g", "b", "a"))
handle$add_window_control <- FALSE
handle$add_drop_shadow <- FALSE
handle$padding_horizontal <- 0
handle$padding_vertical <- 0
handle$auto_adjust_width <- FALSE
handle$relative_export_size <- 4
handle$drop_shadow_blur_radius <- 0
handle$drop_shadow_offset_y <- 0

# print url for manual download
handle$uri()
