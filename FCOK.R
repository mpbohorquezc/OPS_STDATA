fcok=function(X,newcoords,model,j=1,fill.all=T){
     #
     #agregar sugerencia de vp> 0.5 por componente
     puntaje=list()
     puntajes=list()
     for(k in 1:length(X)){
          puntaje[[k]]=X[[k]]$fpca$scores
          rownames(puntaje[[k]])=X[[k]]$cn
          puntajes[[k]]=as.data.frame(puntaje[[k]])
          coordinates(puntajes[[k]])=X[[k]]$coords
     }
     #Estimador de la silla: varianza de cada componente (valor propio del comp principal)
     #todos son estacionarios de segundo orden y tienen silla y es exactamente igual al vp de cada comp principal
     #para estimar la silla del semivariog se tiene metodos de geoestadistica o tomando como silla vaprop
     colnames(newcoords)=c('x','y')
     coordinates(newcoords)=~x+y
     #model=vgm(1000,'Gau',11000)
     if(length(X)>=1){
          if(ncol(puntajes[[1]])>=1){
               g=gstat(,paste(colnames(X[[1]][["fpca"]][["harmonics"]][["coefs"]])[1],1,sep="."),puntajes[[1]][[1]]~1,puntajes[[1]])}
          if(ncol(puntajes[[1]])>=2){
               g=gstat(g,paste(colnames(X[[1]][["fpca"]][["harmonics"]][["coefs"]])[2],1,sep="."),puntajes[[1]][[2]]~1,puntajes[[1]])}
          if(ncol(puntajes[[1]])>=3){
               g=gstat(g,paste(colnames(X[[1]][["fpca"]][["harmonics"]][["coefs"]])[3],1,sep="."),puntajes[[1]][[3]]~1,puntajes[[1]])}
          if(ncol(puntajes[[1]])>=4){
               g=gstat(g,paste(colnames(X[[1]][["fpca"]][["harmonics"]][["coefs"]])[4],1,sep="."),puntajes[[1]][[4]]~1,puntajes[[1]])}
          if(ncol(puntajes[[1]])>=5){
               g=gstat(g,paste(colnames(X[[1]][["fpca"]][["harmonics"]][["coefs"]])[5],1,sep="."),puntajes[[1]][[5]]~1,puntajes[[1]])}
          if(ncol(puntajes[[1]])>=6){
               g=gstat(g,paste(colnames(X[[1]][["fpca"]][["harmonics"]][["coefs"]])[6],1,sep="."),puntajes[[1]][[6]]~1,puntajes[[1]])}
          if(ncol(puntajes[[1]])>=7){
               g=gstat(g,paste(colnames(X[[1]][["fpca"]][["harmonics"]][["coefs"]])[7],1,sep="."),puntajes[[1]][[7]]~1,puntajes[[1]])}
          if(ncol(puntajes[[1]])>=8){
               g=gstat(g,paste(colnames(X[[1]][["fpca"]][["harmonics"]][["coefs"]])[8],1,sep="."),puntajes[[1]][[8]]~1,puntajes[[1]])}
          if(ncol(puntajes[[1]])>=9){
               g=gstat(g,paste(colnames(X[[1]][["fpca"]][["harmonics"]][["coefs"]])[9],1,sep="."),puntajes[[1]][[9]]~1,puntajes[[1]])}
          if(ncol(puntajes[[1]])>=10){
               g=gstat(g,paste(colnames(X[[1]][["fpca"]][["harmonics"]][["coefs"]])[10],1,sep="."),puntajes[[1]][[10]]~1,puntajes[[1]])}
     }
     if(length(X)>=2){
          if(ncol(puntajes[[2]])>=1){
               g=gstat(g,paste(colnames(X[[2]][["fpca"]][["harmonics"]][["coefs"]])[1],2,sep="."),puntajes[[2]][[1]]~1,puntajes[[2]])}
          if(ncol(puntajes[[2]])>=2){
               g=gstat(g,paste(colnames(X[[2]][["fpca"]][["harmonics"]][["coefs"]])[2],2,sep="."),puntajes[[2]][[2]]~1,puntajes[[2]])}
          if(ncol(puntajes[[2]])>=3){
               g=gstat(g,paste(colnames(X[[2]][["fpca"]][["harmonics"]][["coefs"]])[3],2,sep="."),puntajes[[2]][[3]]~1,puntajes[[2]])}
          if(ncol(puntajes[[2]])>=4){
               g=gstat(g,paste(colnames(X[[2]][["fpca"]][["harmonics"]][["coefs"]])[4],2,sep="."),puntajes[[2]][[4]]~1,puntajes[[2]])}
          if(ncol(puntajes[[2]])>=5){
               g=gstat(g,paste(colnames(X[[2]][["fpca"]][["harmonics"]][["coefs"]])[5],2,sep="."),puntajes[[2]][[5]]~1,puntajes[[2]])}
          if(ncol(puntajes[[2]])>=6){
               g=gstat(g,paste(colnames(X[[2]][["fpca"]][["harmonics"]][["coefs"]])[6],2,sep="."),puntajes[[2]][[6]]~1,puntajes[[2]])}
          if(ncol(puntajes[[2]])>=7){
               g=gstat(g,paste(colnames(X[[2]][["fpca"]][["harmonics"]][["coefs"]])[7],2,sep="."),puntajes[[2]][[7]]~1,puntajes[[2]])}
          if(ncol(puntajes[[2]])>=8){
               g=gstat(g,paste(colnames(X[[2]][["fpca"]][["harmonics"]][["coefs"]])[8],2,sep="."),puntajes[[2]][[8]]~1,puntajes[[2]])}
          if(ncol(puntajes[[2]])>=9){
               g=gstat(g,paste(colnames(X[[2]][["fpca"]][["harmonics"]][["coefs"]])[9],2,sep="."),puntajes[[2]][[9]]~1,puntajes[[2]])}
          if(ncol(puntajes[[2]])>=10){
               g=gstat(g,paste(colnames(X[[2]][["fpca"]][["harmonics"]][["coefs"]])[10],2,sep="."),puntajes[[2]][[10]]~1,puntajes[[2]])}
     }
     if(length(X)>=3){

          if(ncol(puntajes[[3]])>=1){
               g=gstat(g,paste(colnames(X[[3]][["fpca"]][["harmonics"]][["coefs"]])[1],3,sep="."),puntajes[[3]][[1]]~1,puntajes[[3]])}
          if(ncol(puntajes[[3]])>=2){
               g=gstat(g,paste(colnames(X[[3]][["fpca"]][["harmonics"]][["coefs"]])[2],3,sep="."),puntajes[[3]][[2]]~1,puntajes[[3]])}
          if(ncol(puntajes[[3]])>=3){
               g=gstat(g,paste(colnames(X[[3]][["fpca"]][["harmonics"]][["coefs"]])[3],3,sep="."),puntajes[[3]][[3]]~1,puntajes[[3]])}
          if(ncol(puntajes[[3]])>=4){
               g=gstat(g,paste(colnames(X[[3]][["fpca"]][["harmonics"]][["coefs"]])[4],3,sep="."),puntajes[[3]][[4]]~1,puntajes[[3]])}
          if(ncol(puntajes[[3]])>=5){
               g=gstat(g,paste(colnames(X[[3]][["fpca"]][["harmonics"]][["coefs"]])[5],3,sep="."),puntajes[[3]][[5]]~1,puntajes[[3]])}
          if(ncol(puntajes[[3]])>=6){
               g=gstat(g,paste(colnames(X[[3]][["fpca"]][["harmonics"]][["coefs"]])[6],3,sep="."),puntajes[[3]][[6]]~1,puntajes[[3]])}
          if(ncol(puntajes[[3]])>=7){
               g=gstat(g,paste(colnames(X[[3]][["fpca"]][["harmonics"]][["coefs"]])[7],3,sep="."),puntajes[[3]][[7]]~1,puntajes[[3]])}
          if(ncol(puntajes[[3]])>=8){
               g=gstat(g,paste(colnames(X[[3]][["fpca"]][["harmonics"]][["coefs"]])[8],3,sep="."),puntajes[[3]][[8]]~1,puntajes[[3]])}
          if(ncol(puntajes[[3]])>=9){
               g=gstat(g,paste(colnames(X[[3]][["fpca"]][["harmonics"]][["coefs"]])[9],3,sep="."),puntajes[[3]][[9]]~1,puntajes[[3]])}
          if(ncol(puntajes[[3]])>=10){
               g=gstat(g,paste(colnames(X[[3]][["fpca"]][["harmonics"]][["coefs"]])[10],3,sep="."),puntajes[[3]][[10]]~1,puntajes[[3]])}
     }
     if(length(X)>=4){
          if(ncol(puntajes[[4]])>=1){
               g=gstat(g,paste(colnames(X[[4]][["fpca"]][["harmonics"]][["coefs"]])[1],4,sep="."),puntajes[[4]][[1]]~1,puntajes[[4]])}
          if(ncol(puntajes[[4]])>=2){
               g=gstat(g,paste(colnames(X[[4]][["fpca"]][["harmonics"]][["coefs"]])[2],4,sep="."),puntajes[[4]][[2]]~1,puntajes[[4]])}
          if(ncol(puntajes[[4]])>=3){
               g=gstat(g,paste(colnames(X[[4]][["fpca"]][["harmonics"]][["coefs"]])[3],4,sep="."),puntajes[[4]][[3]]~1,puntajes[[4]])}
          if(ncol(puntajes[[4]])>=4){
               g=gstat(g,paste(colnames(X[[4]][["fpca"]][["harmonics"]][["coefs"]])[4],4,sep="."),puntajes[[4]][[4]]~1,puntajes[[4]])}
          if(ncol(puntajes[[4]])>=5){
               g=gstat(g,paste(colnames(X[[4]][["fpca"]][["harmonics"]][["coefs"]])[5],4,sep="."),puntajes[[4]][[5]]~1,puntajes[[4]])}
          if(ncol(puntajes[[4]])>=6){
               g=gstat(g,paste(colnames(X[[4]][["fpca"]][["harmonics"]][["coefs"]])[6],4,sep="."),puntajes[[4]][[6]]~1,puntajes[[4]])}
          if(ncol(puntajes[[4]])>=7){
               g=gstat(g,paste(colnames(X[[4]][["fpca"]][["harmonics"]][["coefs"]])[7],4,sep="."),puntajes[[4]][[7]]~1,puntajes[[4]])}
          if(ncol(puntajes[[4]])>=8){
               g=gstat(g,paste(colnames(X[[4]][["fpca"]][["harmonics"]][["coefs"]])[8],4,sep="."),puntajes[[4]][[8]]~1,puntajes[[4]])}
          if(ncol(puntajes[[4]])>=9){
               g=gstat(g,paste(colnames(X[[4]][["fpca"]][["harmonics"]][["coefs"]])[9],4,sep="."),puntajes[[4]][[9]]~1,puntajes[[4]])}
          if(ncol(puntajes[[4]])>=10){
               g=gstat(g,paste(colnames(X[[4]][["fpca"]][["harmonics"]][["coefs"]])[10],4,sep="."),puntajes[[4]][[10]]~1,puntajes[[4]])}
     }
     if(length(X)>=5){
          if(ncol(puntajes[[5]])>=1){
               g=gstat(g,paste(colnames(X[[5]][["fpca"]][["harmonics"]][["coefs"]])[1],5,sep="."),puntajes[[5]][[1]]~1,puntajes[[5]])}
          if(ncol(puntajes[[5]])>=2){
               g=gstat(g,paste(colnames(X[[5]][["fpca"]][["harmonics"]][["coefs"]])[2],5,sep="."),puntajes[[5]][[2]]~1,puntajes[[5]])}
          if(ncol(puntajes[[5]])>=3){
               g=gstat(g,paste(colnames(X[[5]][["fpca"]][["harmonics"]][["coefs"]])[3],5,sep="."),puntajes[[5]][[3]]~1,puntajes[[5]])}
          if(ncol(puntajes[[5]])>=4){
               g=gstat(g,paste(colnames(X[[5]][["fpca"]][["harmonics"]][["coefs"]])[4],5,sep="."),puntajes[[5]][[4]]~1,puntajes[[5]])}
          if(ncol(puntajes[[5]])>=5){
               g=gstat(g,paste(colnames(X[[5]][["fpca"]][["harmonics"]][["coefs"]])[5],5,sep="."),puntajes[[5]][[5]]~1,puntajes[[5]])}
          if(ncol(puntajes[[5]])>=6){
               g=gstat(g,paste(colnames(X[[5]][["fpca"]][["harmonics"]][["coefs"]])[6],5,sep="."),puntajes[[5]][[6]]~1,puntajes[[5]])}
          if(ncol(puntajes[[5]])>=7){
               g=gstat(g,paste(colnames(X[[5]][["fpca"]][["harmonics"]][["coefs"]])[7],5,sep="."),puntajes[[5]][[7]]~1,puntajes[[5]])}
          if(ncol(puntajes[[5]])>=8){
               g=gstat(g,paste(colnames(X[[5]][["fpca"]][["harmonics"]][["coefs"]])[8],5,sep="."),puntajes[[5]][[8]]~1,puntajes[[5]])}
          if(ncol(puntajes[[5]])>=9){
               g=gstat(g,paste(colnames(X[[5]][["fpca"]][["harmonics"]][["coefs"]])[9],5,sep="."),puntajes[[5]][[9]]~1,puntajes[[5]])}
          if(ncol(puntajes[[5]])>=10){
               g=gstat(g,paste(colnames(X[[5]][["fpca"]][["harmonics"]][["coefs"]])[10],5,sep="."),puntajes[[5]][[10]]~1,puntajes[[5]])}
     }
     if(length(X)>=6){
          stop("Too much variables")}

     g <- gstat(g, model=model, fill.all=fill.all)

     vg <- variogram(g)
     mcl = fit.lmc(vg, g,fit.method=2)

     plot(vg, model = mcl)

     z = predict(mcl, newdata = newcoords)
     #CV

     #cv <- krige.cv(puntajes$V1~1, model= fv, nmax = 40)
     p=0
     for( i in 1:length(X)){
          p=p+ncol(puntajes[[i]])
     }
     ##para retornar
     for (i in 1:p){
          grid.arrange(spplot(z[2*i-1], main = "ordinary kriging predictions"),spplot(z[2*i], main = "ordinary kriging variance"))  }
     #falta meter los datos predichos
     pred=z[1][[1]]
     vari=z[2][[1]]
     if(p>1){
          for (k in 2:p){
               pred=cbind(pred,z[2*k-1][[1]])
               vari=cbind(vari,z[2*k][[1]])
          }
     }


     # plot(x=c(0,1000),y=c(-50,140))
     # for (i in 1:nrow(pred)){
     #       lines(X[[j]][["fpca"]][["meanfd"]]+sum((pred[i,]*X[[j]][["fpca"]][["harmonics"]])),col=i)
     # }
     if(j==1){
          fpred=list()
          fvari=list()
          for( i in 1:nrow(pred)){
               fpred[[i]]=X[[j]][["fpca"]][["meanfd"]]+sum((pred[i,1:ncol(puntajes[[j]])]*X[[j]][["fpca"]][["harmonics"]]))
               fvari[[i]]=X[[j]][["fpca"]][["meanfd"]]+sum((vari[i,1:ncol(puntajes[[j]])]*X[[j]][["fpca"]][["harmonics"]]))

          }
     }else{r=0
     for( i in 1:(j-1)){
          r=r+ncol(puntajes[[i]])
     }
     fpred=list()
     for( i in 1:nrow(pred)){
          fpred[[i]]=X[[j]][["fpca"]][["meanfd"]]+sum((pred[i,r:r+ncol(puntajes[[j]])]*X[[j]][["fpca"]][["harmonics"]]))
          fvari[[i]]=X[[j]][["fpca"]][["meanfd"]]+sum((vari[i,r:r+ncol(puntajes[[j]])]*X[[j]][["fpca"]][["harmonics"]]))

     }

     }


     # plot(x=c(0,1000),y=c(-5000,80000))
     # for (i in 1:nrow(vari)){
     #       lines(X[[j]][["fpca"]][["meanfd"]]+sum((vari[i,]*X[[j]][["fpca"]][["harmonics"]])),col=i)
     # }


     ret=list(X=X,model=mcl,fpred=fpred,fvar=fvari)
     # hacer cv o no? agregar krigin(K) o no?
     class(ret)='fcok'
     return(ret)
}
