#' @import dplyr
#' @import fst
#' @import fstplyr
#' @importFrom utils head


#' @export
`||` <- function (e1, e2) UseMethod("||")

#' @export
`||.default` <- function (e1, e2) .Primitive("||")(e1, e2)

#' @export
`||.character` <- function(e1, e2) paste0(e1, e2)


`%<-%` <- function(.table,.expr)
  #' @export
{
	call = as.list(match.call())
	expr.function <- as.list(call$.expr)[[1]]
	table.name <- toString(call$.table)
	time <- system.time(assign(table.name,eval(.expr),envir=parent.frame()))
	message(sprintf("NOTE: The %s '%s' has %d row(s) and %d column(s)."
			   ,head(class(.table),n=1),table.name,nrow(.table),ncol(.table))
	       )
	message(sprintf("NOTE: Function '%s' used:\n      elapsed time %6d seconds\n       system time %6d seconds"
		  	    ,toString(expr.function),as.integer(time[3]),as.integer(time[2]))
		 )
	return(invisible(.table))
	}

`%->%` <- function(.table,.file)
  #' @export
{
	call = as.list(match.call())
	table.name <- toString(call$.table)
	file <- paste0(.file,".fst")
	time <- system.time(write_fst(as.data.frame(.table),file))
	message(sprintf("NOTE: The %s '%s' was written into %s."
			   ,head(class(.table),n=1),table.name,file)
	       )
	message(sprintf("NOTE: Function 'write_fst' used:\n      elapsed time %6d seconds\n       system time %6d seconds"
		  	    ,as.integer(time[3]),as.integer(time[2]))
	       )
	return(invisible(.table))
	}

Q 	<- function(.chars) quo(UQ(as.name(.chars)))
#' @export

DATA 	<- function(.LIB,.TABLE,.WHERE,.KEEP)
  #' @export
{
	data <- tbl(.LIB,.TABLE)
	if (!missing(.WHERE)) {
			where <- enquo(.WHERE)
			data <- filter(data,UQ(where))
			}
	if (!missing(.KEEP)) data <- select(data,UQ(.KEEP))

	as.data.frame(data)
	}
