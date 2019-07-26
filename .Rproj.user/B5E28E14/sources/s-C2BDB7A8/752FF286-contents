## This app requires OpenCPU 1.0.1 or higher !!!!
## This is just a test for how rmd works on the ocpu server end

#' @export
#'
render_rmd_url <- function(rmd_url){
  ## for test rmd_url https://raw.githubusercontent.com/ningzhibin/metalab/master/inst/rmd/input.Rmd
  myfile <- RCurl::getURL(rmd_url)
  writeLines(myfile, con="input.Rmd");
  rmarkdown::render("input.Rmd",output_format = "html_document", output_file="output.html")

  invisible()
}


# the rendering function needs to right a temperary file, whether in the same path as the input, or defined by the user
# if the input is on the server, usually the path is not writable, therefore you need to set the intermediate_dir


render_rmd_server <- function(rmd_file){
  ## for test input.Rmd
  path_to_input <- system.file("rmd", rmd_file, package = "rmdocpu")

  tempfolder <-  tempdir()
  rmarkdown::render(path_to_input, output_format = "html_document",
                    #output_dir = tempfolder,
                    intermediates_dir = tempfolder, # the ocpu server does nothave
                    output_file = "output.html")


  invisible()
}


rmdtext <- function(text){
  writeLines(text, con="input.Rmd");
  rmarkdown::render("input.Rmd", output_file="output.html");
  invisible();
}

