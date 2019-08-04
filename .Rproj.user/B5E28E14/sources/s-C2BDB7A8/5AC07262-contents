# only for debugging


library(jsonlite)
library(httr)
#library(lubridate)


options(stringsAsFactors = FALSE)

# R has the “feature” of turning character strings automatically into factor variables.
# This is great, when doing actual statistical work. It is this magic that allows R to turn multinomial variables into
# dummy variables in regression models and produce nice cross tables. When working with APIs, however,
# this “feature” becomes a hinderance. Let’s just turn it off. Note: this call only affects the current session;
# when you restart R, all settings will be back to normal.




summary_file  <- read_tsv("summary.txt", col_names = TRUE)

t <- toJSON(summary_file)

tt <- paste0("data = '", t,"'")
writeLines(tt, "mydata.json")


# test of the file converted back remains the same structure
mq_smmary <- fromJSON(t)


url_Server <- "https://cloud.opencpu.org/"
url <- "https://cloud.opencpu.org/ocpu/library/stats/R/rnorm"






# this is the way hwo to use r to do the  POST job
#r <- RCurl::postForm(url, n=10)
# for parametr
r <- httr::POST(url, body = list(n=10, mean =5))
# for file uploading
r <- httr::POST("https://cloud.opencpu.org/ocpu/library/utils/R/read.csv", body = list(file = upload_file("summary.txt")))

# parse the response to get the path
# rawToChar(r$content) # or use toString(r)

paths <- strsplit(rawToChar(r$content), "\n")[[1]]
path_target <- paths[grep("DESCRIPTION",paths)]

# download file
curl::curl_download(paste0(url_Server, path_target), "description")






#for rmd
url_server <-"http://206.12.91.148/"
url_api <- "http://206.12.91.148/ocpu/library/rmdocpu/R/render_MQsummary_file"
# for file uploading
r <- httr::POST(url_api, body = list(file = upload_file("summary.txt")))







# POSt usage examples
# b2 <- "http://httpbin.org/post"
# POST(b2, body = "A simple text string")
# POST(b2, body = list(x = "A simple text string"))
# POST(b2, body = list(y = upload_file(system.file("CITATION"))))
# POST(b2, body = list(x = "A simple text string"), encode = "json")

















