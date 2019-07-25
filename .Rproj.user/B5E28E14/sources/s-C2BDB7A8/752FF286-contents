## This app requires OpenCPU 1.0.1 or higher !!!!
## This is just a test for how rmd works on the ocpu server end

#' @export
#'
render_rmd_url <- function(){
  myfile <- RCurl::getURL('https://raw.githubusercontent.com/ningzhibin/metalab/master/inst/rmd/input.Rmd')
  writeLines(myfile, con="input.Rmd");
  rmarkdown::render("input.Rmd",output_format = "html_document", output_file="output.html")

  invisible()
}

render_rmd_server <- function(){
  path_to_input <- system.file("rmd", "input.Rmd", package = "rmdocpu")

  tempfolder <-  tempdir()
  rmarkdown::render(path_to_input, output_format = "html_document",
                    output_dir = tempfolder,
                    intermediates_dir = tempfolder,
                    output_file = "output.html")


  invisible()
}


rmdtext <- function(text){
  writeLines(text, con="input.Rmd");
  rmarkdown::render("input.Rmd", output_file="output.html");
  invisible();
}

