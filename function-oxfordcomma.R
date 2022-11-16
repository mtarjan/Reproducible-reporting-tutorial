## Function to create a list of character strings with oxford comma
oxford.comma <- function(x) {
  if (length(x)==1) {y<-x}
  if (length(x)==2) {y<-paste0(x, collapse = " and ")}
  if (length(x)>2) {
    y <- paste0(x[1:(length(x)-1)], collapse = ", ")
    y <- paste0(c(y, x[length(x)]), collapse = ", and ")
  }
  y
}