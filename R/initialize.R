#' @rdname initialize
#' @title Multi-Headed Data Frames
#'
#' @description 
#' Create a multi-headed data frame which is 
#' an hierarchical collection of variables.
#' 
#' @aliases mlth
#' 
#' @param ...       the collection of named variables or sub-tables, each of class \code{list}. 
#' 					All variables must be of the same length.
#' @param row.names \code{NULL} or character or integer vector to be used as row names, must be of the same
#'                  length as the variables. 
#' @param stringsAsFactors same as for \code{\link{data.frame}}
#' @param defaultName the single character value to fill empty names.
#' @param fixNamesSep the separator which is used when there are more than one empty names.
#' @return \code{mlth.data.frame} object
#' @details
#' A \code{mlth.data.frame} object is a list of variables and \code{mlth.data.frame}s.
#' Each variable must be a vector of atomic data type (e.g., POSIX date will not work).
#' Each variable or sub-table must have a name, the names must be unique within
#' the table/sub-table, but not across sub-tables.
#' 
#' Unlike \code{data.frame}, a multi-headed data frame cannot have zero columns and non-zero rows
#' but can have zero rows and non-zero columns or zero columns and rows.
#' 
#' @examples
#' A<-mlth.data.frame(X=c('A','B','C'),
#' 			Y=list(
#' 				N=1:3,
#' 				M=4:6))
#' 
#' ## The empty names are filled-in
#' B<-mlth.data.frame(X=list(rnorm(10),rnorm(10)),
#' 			Y=list(rnorm(10),rnorm(10)),
#' 			row.names=letters[1:10])
#' 
#' ## str method for mlth.data.frame
#' str(B)
#' 
#' @export

mlth.data.frame<-function(...,row.names = NULL,stringsAsFactors = default.stringsAsFactors(),
		defaultName='X',fixNamesSep=' '){
	# TODO: Check that the row.names are either character or integer
	x<-list(...)
	
	if (length(x)==0){
		attr(x,'class')<-c('list','mlth.data.frame')
		row.names(x)<-NULL
		return(x)
	}		
	
	N<-unique(rapply(x,length))
	if (length(N)>1)
		stop('The columns must be of the same length or empty')
	if (length(N)==0)
		N<-0
	
	if (length(defaultName)!=1)
		stop('defaultName must be a single character value')
	
	# Checking and fixing the names
	fixNames<-function(L,name,nameSep=' '){
		if (!is.list(L))
			return(L)
		
		if (length(names(L))==0){
			names(L)<-paste(rep(name,length(L)),1:length(L),sep=nameSep)
		} else {
			emptyNames<-which(nchar(names(L))==0)
			if (length(emptyNames)>0){
				names(L)[emptyNames]<-paste(rep(name,length(emptyNames)),1:length(emptyNames))
				names(L)<-make.unique(names(L),sep=nameSep)
			}	
		}
		
		for (i in 1:length(L))
			L[i]<-list(fixNames(L[[i]],name=names(L)[i]))
		
		return(L)
	}
	
	x<-fixNames(x,name=defaultName,nameSep=fixNamesSep)
	
	# Filling in empty columns
	x<-lapply(x,function(X)
				if (is.list(X)) lapply(X,sys.function(0)) 
				else if (length(X)==0) rep(NA,N) else X)
	
	# Converting strings to factors
	if (stringsAsFactors)
		x<-lapply(x,function(X)
					if (is.list(X)) lapply(X,sys.function(0)) 
					else if (is.character(X)) as.factor(X) else X)
	
	attr(x,'class')<-c('list','mlth.data.frame')
	row.names(x)<-row.names
	return(x)
	# TODO: Also check row.names if there are any
}

#' @rdname initialize
#' @export
mlth<-mlth.data.frame

#' @rdname coerce
#' @title Coerce to a Multi-Headed Data Frame
#' 
#' @description 
#' Functions to check and coerce the object to a \code{mlth.data.frame}.
#' 
#' @param x any \R{} object
#' @param row.names \code{NULL} or character or integer vector to be used as row names, must be of the same
#'                  length as the variables. 
#' @param ... other arguments passed \code{mlth.data.frame}.
#' 
#' @return 
#' \code{is.mlth.data.frame} returns a multi-headed data frame.
#' \code{as.mlth.data.frame} returns \code{TRUE} if the object has
#' "\code{mlth.data.frame}" within its classes and \code{FALSE} otherwise.
#' 
#' @examples 
#' L<-list(
#' 		X=c('A','B','C'),
#' 		Y=list(
#' 			N=1:3,
#' 			M=4:6))
#' 
#' (A<-as.mlth.data.frame(L))
#' is.mlth.data.frame(A)
#' 
#' @export 

is.mlth.data.frame<-function(x)
	'mlth.data.frame'%in%class(x)

#' @rdname coerce
#' @export
as.mlth.data.frame<-function(x,row.names=NULL,...){
	if (is.null(x)) 
		return(as.data.frame(list()))
	UseMethod("as.mlth.data.frame")
}

#' @rdname coerce
#' @export
as.mlth.data.frame.default<-function(x,...){
	stop(gettextf("cannot coerce class \"%s\" to a mlth.data.frame", deparse(class(x))), 
			domain = NA)
}

#' @rdname coerce
#' @export
as.mlth.data.frame.list<-function(x,row.names=NULL,...)
	do.call('mlth',c(x,list(row.names=row.names),list(...)))

#' @rdname coerce
#' @export
as.list.mlth.data.frame<-function(x){
	class(x)<-'list'
	return(x)
}

#' @export 
row.names.mlth.data.frame<-function(x){
	as.character(attr(x,"row.names"))	
}

#' @export
`row.names<-.mlth.data.frame`<-function(x,value){
	if (length(value)!=0){
		if (length(value)!=nrow(x))
			stop('The length of row.names must be nrow(x) or 0')
		if (!is.integer(value))
			value<-as.character(value)
		value[which(is.na(value))]<-'NA'
		value<-make.unique(value)
	}
	
	attr(x,'row.names')<-value
	return(x)
}

#' @export
str.mlth.data.frame<-function(object,...){
	O<-object
	class(O)<-'list'
	cat(str(O,...))
	cat('\nAttributes:\n')
	cat(str(attributes(object)))
}

#' @export
dim.mlth.data.frame<-function(x) dim(as.data.frame(x))
