library("knitr")
p <- purl("exam.Rmd")
read_chunk(p)
chunks <- knitr:::knit_code$get()
invisible(mapply(function(chunk, name) {
  writeLines(c(paste0("## ----",name,"----"), chunk), paste0("R/submission_",name,".R"))
}, chunks, names(chunks)))
unlink(p) 
knitr:::knit_code$restore() 