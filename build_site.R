# Build the web app as a static website in ./site (Shinylive).
# R runs inside the visitor's browser, so no data is sent to any server
# except Baserow itself.
#
#   Rscript build_site.R
#
# Only the files listed here are copied into the site. Tokens, data and
# results in the project folder are never included.

app_files <- c("app.R", "R", "www")

build_dir <- file.path(tempdir(), "app")
unlink(build_dir, recursive = TRUE)
dir.create(build_dir)
file.copy(app_files, build_dir, recursive = TRUE)

unlink("site", recursive = TRUE)
shinylive::export(build_dir, "site")
message("Site written to ./site")
