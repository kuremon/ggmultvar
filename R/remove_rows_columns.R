as.factor.logical=function(b){
  return(factor(b,levels=c(T,F)));
}


#' @title Vizualize NAs in a data frame
#' @param data a data frame.
#' @param num.vars Index (or names) of the numeric variables of \code{data}.
#' By default all variables are considered.
#' @param ... Additional arguments to \code{\link{see}}
#' @export
see.na=function(data,num.vars=seq(data),...){
  data.num=data[num.vars]
  where.na=is.na(data.num) 
  see(colwise(as.factor.logical)(as.data.frame(where.na)),...,use.rownames=F)+labs(fill="")+
      scale_fill_manual(values=c("TRUE"="red","FALSE"="green"),
                        breaks=c(FALSE,TRUE),
                        labels=c("TRUE"="NA","FALSE"="Not NA"),
                        drop=TRUE)
}

#' @title Remove rows with NA
#' @description 
#' This function detects and remove rows where there is at least one occurence of NA.
#' This function is mainly used before multivariate analyis like PCA to ensure the data are NA-free.
#' In addition a visualisation of the NA precense is given in order to check for the reliability of 
#' the rows removal.  
#' @param data a data frame.
#' @param num.vars Index (or names) of the numeric variables of \code{data}.
#' By default all variables are considered.
#' @return The updated data frame.
#' @export
remove.na.rows=function(data,num.vars=seq(data)){
  data.num=data[num.vars]
  where.na=is.na(data.num)  
  row2rem=apply(where.na,1,any)
  data[!row2rem,]
}

#' @title Remove 0 variance columns in a data frame 
#' @description 
#' This function detects and remove columns with 0 variance.
#' This function is mainly used before multivariate analyis like PCA to ensure there is variability
#' in all parameters.
#' @param data a data frame.
#' @param num.vars Index (or names) of the numeric variables of \code{data}.
#' By default all variables are considered.
#' @return The updated data frame.
#' @export
remove.zero.var.cols=function(data,num.vars=seq(data)){
  if(is.numeric(num.vars)) num.vars=names(data)[num.vars]
  data.num=data[num.vars]
  where.zero.var=colwise(var,na.rm=TRUE)(data.num)==0
  remove.vars(data,num.vars[where.zero.var])
}

#' @title Remove duplicated rows 
#' @description 
#' This function removes any duplicated rows.
#' @param data a data frame.
#' @param id Index (or names) of the id variables of \code{data}.
#' By default all variables are considered.
#' @return The updated data frame.
#' @export
remove.identical.rows=function(data,id=NULL){
  if(is.character(id)) id=match(id,names(data))
  idx=duplicated(data[setdiff(seq(data),id)],MARGIN=1)
  if(length(idx)!=0){
    data=data[-idx,]
  }
  data
}
