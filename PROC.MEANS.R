# Version 2 du 12/4/2018

PROC.MEANS <- function(.DATA,.VARS,.CLASSES=NULL,.WEIGHT
			,.N,.MIN,.MAX,.SUM,.MEAN,.VAR,.STD,.MEDIAN
			,.REORDER=TRUE)
#' Produces various weighted statistics on selected columns
#'
#' \code{PROC.MEANS} was inspired by the SAS MEANS procedure.
#' @param .DATA    a data frame
#' @param .VARS	   a non zero length vector of variable names
#' @param .CLASSES a possibly empty vector of variables to group rows with
#' @param .WEIGHT  a possibly missing name of a numeric variable to weight rows with
#' @param .N       a character prefix for the count result variable.
#' Other statistics that may be obtained are:
#'   .MIN .MAX
#'   .SUM .MEAN .VAR .STD .MEDIAN
#' Only statistics for which a prefix has been given are produced.
#' @param .REORDER when true, the statistical results are ordered by source variable, else by statistic.
#' @return a data frame
#' @importFrom stats median var
#' @importFrom Hmisc wtd.mean wtd.var wtd.quantile
#' @import dplyr
#' @import replyr
#' @import tidyr
#' @import purrr
#' @export
#'
#' @examples
#' a<-data.frame(w=1:10,v=1,g=1:2)
#' PROC.MEANS(.DATA=a,.VARS="v",.CLASSES="g",.WEIGHT="w",.MIN="min",.MAX="max",.MEAN="")
	{

	f2 <- function(.ldf,.stat)
		purrr::map(.ldf,function(.df) .stat(.df$.__V__,.df$.__W__))

	f1 <- function(.ldf,.stat)
		purrr::map(.ldf,function(.df) .stat(.df$.__V__))

	g <- function(.df1,.f
		,.N,.fn,.MIN,.fmin,.MAX,.fmax
		,.SUM,.fsum,.MEAN,.fmean,.VAR,.fvar,.STD,.fstd,.MEDIAN,.fmedian)
		{

		h <- function(.df2,.stat,.name)
			{
			df <- dfh %>%
				cbind(data.frame(.__R__=unlist(.f(ldf,.stat)))) %>%
				tidyr::spread(.__VAR__,.__R__)
			df <- df[,(nc+1):length(df),drop=FALSE]
			# TRUE conduit a un vecteur s il n'y a qu une variable
			if (.name!="") colnames(df) <-  paste(.name,colnames(df),sep="_")
			cbind(.df2,df)
			}

		nc <- length(.CLASSES)
		group_number <- (function(){i = 0L; function() i <<- i+1L })()
		df1 <- .df1 %>%
			tidyr::gather(key=".__VAR__",value=".__V__",.VARS) %>%
			group_by_at(c(.CLASSES,".__VAR__")) %>%
			mutate(.__G__=group_number())
		ldf <- df1 %>% replyr::replyr_split(".__G__")
		dfh <- df1 %>% distinct(.__G__) %>% arrange(.__G__) %>% select(-.__G__) %>%
			as.data.frame()
		if (nc!=0)
			df2 <- dfh %>% select(1:(length(dfh)-1)) %>% distinct()
		else 	df2 <- data.frame(.__DUMMY__="Total")
		if (!missing(.N)) 	df2 <- h(df2,.fn,.N)
		if (!missing(.MIN)) 	df2 <- h(df2,.fmin,.MIN)
		if (!missing(.MAX)) 	df2 <- h(df2,.fmax,.MAX)
		if (!missing(.SUM)) 	df2 <- h(df2,.fsum,.SUM)
		if (!missing(.MEAN)) 	df2 <- h(df2,.fmean,.MEAN)
		if (!missing(.VAR)) 	df2 <- h(df2,.fvar,.VAR)
		if (!missing(.STD)) 	df2 <- h(df2,.fstd,.STD)
		if (!missing(.MEDIAN)) 	df2 <- h(df2,.fmedian,.MEDIAN)
		if (nc==0) df2 <- select(df2,-1)
		if (.REORDER)
			{
			ns <- (ncol(df2)-nc)/length(.VARS)
			nv <- (ncol(df2)-nc)/ns
			x <- 1:ncol(df2)
			for (v in 1:nv)
				for (s in 1:ns)
					x[nc+(v-1)*ns+s] <- nc+(s-1)*nv+v
			df2 <- df2[,x]
			}
		df2
		}

	if (missing(.WEIGHT))
		{
		df1 <- .DATA %>%
			select(c(.CLASSES,.VARS))
		g(df1,f1	# %>% pertuberait le fonctionnement de 'missing'
			,.N,purrr::compose(sum,`!`,is.na)
			,.MIN,min
			,.MAX,max
			,.SUM,sum
			,.MEAN,mean
			,.VAR,var
			,.STD,purrr::compose(sqrt,var)
			,.MEDIAN,median)
		}
	else	{
		df1 <- .DATA %>%
			select(c(.CLASSES,.WEIGHT,.VARS)) %>%
			rename_at(.WEIGHT,function(.) ".__W__")
		g(df1,f2
			,.N,purrr::compose(sum,function(.v,.w) !is.na(.v)*.w)
			,.MIN,purrr::compose(min,function(.v,.w) .v)
			,.MAX,purrr::compose(max,function(.v,.w) .v)
			,.SUM,purrr::compose(sum,`*`)
			,.MEAN,Hmisc::wtd.mean
			,.VAR,Hmisc::wtd.var
			,.STD,purrr::compose(sqrt,Hmisc::wtd.var)
			,.MEDIAN,purrr::partial(wtd.quantile,probs=.5))
		}
	}

