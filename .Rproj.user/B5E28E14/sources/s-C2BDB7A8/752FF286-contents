## This app requires OpenCPU 1.0.1 or higher !!!!
## This is just a test for how rmd works on the ocpu server end


# the rendering function needs to right a temperary file, whether in the same path as the input, or defined by the user
# if the input is on the server, usually the path is not writable, therefore you need to set the intermediate_dir
# an workround way is to readin the file, and write into a temp file. therefore everything is going to be in the temp file


#' @export
#'
render_rmd_url <- function(rmd_url){
  ## for test rmd_url https://raw.githubusercontent.com/ningzhibin/metalab/master/inst/rmd/input.Rmd
  myfile <- RCurl::getURL(rmd_url)
  writeLines(myfile, con="input.Rmd");
  rmarkdown::render("input.Rmd",output_format = "html_document", output_file="output.html")

  invisible()
}




render_rmd_server <- function(rmd_file){
  ## for test input.Rmd
  path_to_input <- system.file("rmd", rmd_file, package = "rmdocpu")

  ##myfile <- readLines(path_to_input)
  #writeLines(myfile, con="input.Rmd");
  #rmarkdown::render("input.Rmd",output_format = "html_document", output_file="output.html")

  tempfolder <-  tempdir()
  rmarkdown::render(path_to_input, output_format = "html_document",
                    intermediates_dir = tempfolder, # the ocpu server does nothave
                    output_file = "output.html")


  invisible()
}


rmdtext <- function(text){
  writeLines(text, con="input.Rmd");
  rmarkdown::render("input.Rmd", output_file="output.html");
  invisible();
}

