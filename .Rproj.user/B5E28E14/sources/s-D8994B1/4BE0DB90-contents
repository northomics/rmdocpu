

modules <- read.delim("module_ordered.txt", header = FALSE)

module_names <- modules$V1


for(i in 1:length(module_names)){


  url <- paste0("http://rest.kegg.jp/get/", module_names[i])
  r <- httr::GET(url)
  t <-rawToChar(r$content)
  c <- strsplit(t, "COMPOUND    ")[[1]][2]


  # for some modules, there are more items, like comment, references, and Rmodule, it ccould be removed by the following script
  c <- strsplit(c, "\nCOMMENT")[[1]][1]
  c <- strsplit(c, "\nREFERENCE")[[1]][1]
  c <- strsplit(c, "\nRMODULE")[[1]][1]


  c <- substr(c, 1, nchar(c)-4) # remove the last ///
  #c_ls <- strsplit(c, "\n            ")[[1]]

  cat(paste0(modules[i,1],"\t", modules[i,2], "\n","            ",c), file = "module_compound_ordered.txt", append = TRUE)

}


