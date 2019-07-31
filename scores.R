scores=function(X){

     puntaje=list()
     puntajes=list()
     for(j in 1:length(X)){


     puntaje[[j]]=X[[j]]$fpca$scores
     rownames(puntaje[[j]])=X[[j]]$cn
     puntajes[[j]]=as.data.frame(puntaje[[j]])
     }
     return(puntajes)
}
