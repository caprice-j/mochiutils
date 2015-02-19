#'Get string and return it as a data frame
#'
#'@param rawstring
#'
#'@return data(data frame)
#'
#'@export
read_string = function(str){
  oldop <- options(stringsAsFactors=FALSE)

  first_line <- strsplit(str, "\n")[[1]][1]
  n_col <- length( strsplit(first_line, "\\s+")[[1]] )


         mixed <- strsplit(str, "\\s+")[[1]]
   label_lines <- mixed[1:n_col]
    data_lines <- mixed[(n_col+1):length(mixed)]
  n_data_lines <- length(data_lines)


  framed <- c()
  for( i in 1:n_col ){
    framed <- cbind( framed, data_lines[seq(i,length(data_lines),by=n_col)] )
  }
  framed <- data.frame(framed)
  colnames(framed) <- label_lines


  return(framed)
}