varfcok=function(X,modelo,s0){

     n1=nrow(X[[1]]$coords)
     n2=nrow(X[[2]]$coords)
     h1=length(X[[1]]$fpca$harmonics$fdnames[[2]])
     h2=length(X[[2]]$fpca$harmonics$fdnames[[2]])
     #coordenadas1=fixcoord
     coordstot=rbind(X[[1]]$coords,X[[2]]$coords,s0)
     colnames(coordstot)=c("X","Y")
     #coordenadas2=data.frame("X"=coordmov[1:n2],"Y"=coordmov[(n2+1):N2])#ubicar coordenadas
     #coordenadas=rbind(coordenadas1,coordenadas2)
     MatDistances=as.matrix(dist(coordstot))#distancias entre coordenadas
     #covMatrix=variogramLine(modelo,dist_vector=MatDistances)#Calcular matriz de cov
     #colnames(s0)=c("X","Y")
     #varOK=NULL
     #for(i in 1:nrow(s0)){
     #     coordenadas_and_s0i=rbind(coordenadas,s0[i,])#Agregar s0i a coord
     #     DistSample_s0i=as.matrix(dist(coordenadas_and_s0i))[(nrow(coordenadas)+1):nrow(coordenadas_and_s0i),-((nrow(coordenadas)+1):nrow(coordenadas_and_s0i))]
     #     covSample_s0i=variogramLine(modelo,dist_vector=as.matrix(DistSample_s0i))
     #     vari=t(covSample_s0i)%*%covMatrix%*%covSample_s0i
     #     varOK[i]=as.numeric(vari)
     #}
     diff1s0=as.matrix(MatDistances[1:n1,n1+n2+1])
     diff2s0=as.matrix(MatDistances[(n1+1):(n1+n2),n1+n2+1])

     diff11=MatDistances[1:n1,1:n1]

     diff22=MatDistances[(n1+1):(n1+n2),(n1+1):(n1+n2)]

     diff12=MatDistances[1:n1,((n1+1):(n1+n2))]
     #modelo= cok$model$model
     totmodelos=length(modelo)-2
     sq11y22=totmodelos
     b=2
     while(sq11y22[1]>1){

          sq11y22=append(sq11y22,sq11y22[1]-b,after = 0)
          b=b+1
     }
     i3=NULL
     if(h1==1){
          i3=2:(h2+1)
     } else{
     i1=(h2+h1-1):(h2+1)
     i2=h1+1

     c=1
     for(i in i1){
          i2=append(i2,i2[c]+i)
          c=c+1
     }
          if(h2==1){
               i3=i2
               }else {
                    for(i in 1:h2){
                         i3=append(i3,i2+i-1)
                    }
               }
     }
     sq11=sq11y22[1:h1]
     sq22=sq11y22[(h1+1):(h1+h2)]
     sq12=sort(i3)
     variogmatrix11=list()
     v1=list()
     for(i in 1:length(sq11)){
          variogmatrix11[[i]]=variogramLine(modelo[[sq11[i]]],dist_vector =  diff11)
          v1[[i]]=variogramLine(modelo[[sq11[i]]],dist_vector =  diff1s0)

     }
     variogmatrix22=list()
     for(i in 1:length(sq22)){
          variogmatrix22[[i]]=variogramLine(modelo[[sq22[i]]],dist_vector =  diff22)
     }
     variogmatrix12=list()
     v2=list()
     for(i in 1:length(sq12)){
          variogmatrix12[[i]]=variogramLine(modelo[[sq12[i]]],dist_vector =  diff12)
          v2[[i]]=variogramLine(modelo[[sq12[i]]],dist_vector =  diff2s0)
     }
     sigma11=variogmatrix11[[1]]-variogmatrix11[[1]]
     c1=v1[[1]]-v1[[1]]
     for(i in 1:length(sq11)){
          sigma11=sigma11+variogmatrix11[[i]]
          c1=c1+v1[[i]]
     }

     sigma22=variogmatrix22[[1]]-variogmatrix22[[1]]
     for(i in 1:length(sq22)){
          sigma22=sigma22+variogmatrix22[[i]]
     }

     diag(sigma11)=sum(X[[1]]$fpca$varprop)
     diag(sigma22)=sum(X[[2]]$fpca$varprop)



     fd1=eval.fd(1:nrow(X[[1]]$X),X[[1]][["fpca"]][["harmonics"]])
     fd2=eval.fd(1:nrow(X[[2]]$X),X[[2]][["fpca"]][["harmonics"]])
     comb=cbind(rep(1:h1,rep(h2,h1)),1:h2)
     c=c()

     for(i in 1:nrow(comb)){
          c=append(c,fd1[,comb[i,1]]%*%fd2[,comb[i,2]])
     }
     c2=v2[[1]]-v2[[1]]
     sigma12=variogmatrix12[[1]]-variogmatrix12[[1]]
     for(i in 1:length(sq12)){
          sigma12=sigma12+c[i]*variogmatrix12[[i]]
          c2=c2+c[i]*v2[[i]]
     }
     sigma=rbind(cbind(sigma11,sigma12),cbind(t(sigma12),sigma22))



     C=rbind(c1,c2)
     L=solve(sigma, C)


}
