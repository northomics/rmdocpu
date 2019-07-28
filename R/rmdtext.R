#' wrapper function of rmarkdown::render for opencpu
#'
#' Three general wrapper function to work with opencpu, which can render source r markdown code into html files, to be displacyed on the client end
#'
#' @param text For rmdtext, a string, which is the rmardown coding itself
#' @return no direct return, but write an output.html to the temp session on the opencpu server
#'
#' @seealso \code{\link{render}}  \code{\link{knit}}
#'
#' @examples
#'   path_to_input <- system.file("rmd", rmd_file, package = "rmdocpu")
#'   myfile <- readLines(path_to_input)
#'   rmdtext(myfile)

#' @export
#'
#'
#'
rmdtext <- function(text){
  writeLines(text, con="input.Rmd");
  rmarkdown::render("input.Rmd", output_file="output.html");
  invisible();
}


#' wrapper function of rmarkdown::render for opencpu
#'
#' Three general wrapper function to work with opencpu, which can render source r markdown code into html files, to be displacyed on the client end
#'
#' @param rmd_url For render_rmd_url, a string of public accessible url
#'
#' @return no direct return, but write an output.html to the temp session on the opencpu server
#' @seealso \code{\link{render}}  \code{\link{knit}}
#' @examples
#'   render_rmd_url("https://raw.githubusercontent.com/ningzhibin/metalab/master/inst/rmd/input.Rmd")
#' @export
#'
#'
#'


render_rmd_url <- function(rmd_url){
  ## for test rmd_url https://raw.githubusercontent.com/ningzhibin/metalab/master/inst/rmd/input.Rmd
  myfile <- RCurl::getURL(rmd_url)
  writeLines(myfile, con="input.Rmd");
  rmarkdown::render("input.Rmd",output_format = "html_document", output_file="output.html")

  invisible()
}


#' wrapper function of rmarkdown::render for opencpu
#'
#' Three general wrapper function to work with opencpu, which can render source r markdown code into html files, to be displacyed on the client end
#'
#' @param rmd_file For render_rmd_server, a string of the file name on the same opencpu server in the same package, only for tests
#' @return no direct return, but write an output.html to the temp session on the opencpu server
#' @seealso \code{\link{render}}  \code{\link{knit}}
#' @examples
#'   path_to_input <- system.file("rmd", rmd_file, package = "rmdocpu")
#'   render_rmd_server(path_to_input)
#' @export
#'

render_rmd_server <- function(rmd_file){
  ## for test use input.Rmd
  path_to_input <- system.file("rmd", rmd_file, package = "rmdocpu")

  myfile <- readLines(path_to_input)
  writeLines(myfile, con="input.Rmd");
  rmarkdown::render("input.Rmd",output_format = "html_document", output_file="output.html")

  invisible()
}

# the rendering function needs to right a temperary file, whether in the same path as the input, or defined by the user
# if the input is on the server, usually the path is not writable, therefore you need to set the intermediate_dir
# an workround way is to read in the file, and write into a temp file. therefore everything is going to be in the temp file

