#' @importFrom digest digest
#' @export
zip=function(list){
  for(l in list){
    if(!is.list(l))stop('all elements need to be lists')
  }
  len=unique(sapply(list,length))
  if(length(len)>1)stop('all elments need to be of the same length')
  namehash=unique(sapply(list,function(l)digest(names(l))))
  if(length(namehash)>1)stop('all elements need to have the same names')
  out=vector(mode='list',length = len)
  names(out)=names(list[[1]])
  for(i in seq_len(len)){
    out[[i]]=lapply(list,function(l)l[[i]])
  }
  out
}

