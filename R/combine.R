#' @export 
cbind.mlth.data.frame<-function(...){
	L<-list(...)
	L<-lapply(L,as.list)
	L<-do.call('c',L)
	as.mlth.data.frame(L) # TODO: check row names!
}

#' @export
rbind.mlth.data.frame<-function(...){
	compareStructure<-function(l1,l2){
		if (is.list(l1)&is.list(l2)){
			if (length(l1)!=length(l2))
				return(FALSE)
			if (!identical(names(l1),names(l2)))
				return(FALSE)
			Ans<-TRUE
			for (i in 1:length(l1))
				Ans<-compareStructure(l1[[i]],l2[[i]]) & Ans
			return(Ans)
		} else if (!is.list(l1)&!is.list(l2)) return(TRUE)
		else return(FALSE)
	}
	
	rbindLists<-function(...){
		Map(function(...){
					if (is.list(list(...)[[1]])){
						Map(sys.function(),...)
					} else c(...)
				},...)
	}
	
	#TODO: check header
	L<-list(...)
	L<-lapply(L,as.list)
	
	outp<-as.mlth.data.frame(do.call('rbindLists',L))
	#TODO: combine row.names
	return(outp)
}


