
### peak: A quick look at the data from objects or Url's' #############

peak <- function(x, ... ){
  library(RCurl) # is included in "plotly"
  library(rio) 
  #validate(need(x, 'Choose input!'))
  if (is.object(x)) {
    filesize <- object.size(x) 
    filesize <- setNames(as.character(filesize), "File size")
    classes <- lapply(x, class)
    columns <- ncol(x)
    columns <- setNames(columns, "Columns")
    rows <- nrow(x)
    rows <- setNames(rows, "Rows")
    miss <- anyNA(x) 
    miss <- setNames(miss, "Missing data?")
    Datastructure <- 
      unlist(list(filesize, columns, rows, miss, classes))
    as.data.frame(Datastructure)
  }
  else {  url <- x # has to be character string
  res <- url.exists(url, .header=TRUE)
  accessDate<-res['Date']
  accessDate<-setNames(as.character(accessDate), "Download date")
  filesize<-res['Content-Range']
  filesize <- setNames(as.character(filesize), "File size")
  filetype<-res['Content-Type']
  filetype <- setNames(as.character(filetype), "File type")
  thirty<-import(url, nrows=30)
  columns <- ncol(thirty)
  columns <- setNames(columns, "Columns")
  classes<- lapply(thirty, class)
  miss<- anyNA(thirty, recursive=TRUE)
  miss <- setNames(miss, "Missing data?")
  Datastructure <- 
    unlist(list(accessDate, filesize, filetype, miss, columns, classes))
  as.data.frame(Datastructure)
  }
}
