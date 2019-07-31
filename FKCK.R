fkck=function(X,newcoords,model,j=1,fill.all=T){
     #
     #agregar sugerencia de vp> 0.5 por componente
     puntaje=X[[j]]$fpca$scores
     rownames(puntaje)=X[[j]]$cn
     puntajes=as.data.frame(puntaje)
     coordinates(puntajes)=X[[j]]$coords

     #Estimador de la silla: varianza de cada componente (valor propio del comp principal)
     #todos son estacionarios de segundo orden y tienen silla y es exactamente igual al vp de cada comp principal
     #para estimar la silla del semivariog se tiene metodos de geoestadistica o tomando como silla vaprop
     colnames(newcoords)=c('x','y')
     coordinates(newcoords)=~x+y
     #model=vgm(1000,'Gau',11000)
     #  for (i in 2:ncol(puntajes)){
     #    g=gstat(g,colnames(X[[j]][["fpca"]][["harmonics"]][["coefs"]])[i],puntajes[[i]]~1,puntajes)
     #  }
     if(ncol(puntajes)>=1){
          g=gstat(,colnames(X[[j]][["fpca"]][["harmonics"]][["coefs"]])[1],puntajes[[1]]~1,puntajes)}
     if(ncol(puntajes)>=2){
          g=gstat(g,colnames(X[[j]][["fpca"]][["harmonics"]][["coefs"]])[2],puntajes[[2]]~1,puntajes)}
     if(ncol(puntajes)>=3){
          g=gstat(g,colnames(X[[j]][["fpca"]][["harmonics"]][["coefs"]])[3],puntajes[[3]]~1,puntajes)}
     if(ncol(puntajes)>=4){
          g=gstat(g,colnames(X[[j]][["fpca"]][["harmonics"]][["coefs"]])[4],puntajes[[4]]~1,puntajes)}
     if(ncol(puntajes)>=5){
          g=gstat(g,colnames(X[[j]][["fpca"]][["harmonics"]][["coefs"]])[5],puntajes[[5]]~1,puntajes)}
     if(ncol(puntajes)>=6){
          g=gstat(g,colnames(X[[j]][["fpca"]][["harmonics"]][["coefs"]])[6],puntajes[[6]]~1,puntajes)}
     if(ncol(puntajes)>=7){
          g=gstat(g,colnames(X[[j]][["fpca"]][["harmonics"]][["coefs"]])[7],puntajes[[7]]~1,puntajes)}
     if(ncol(puntajes)>=8){
          g=gstat(g,colnames(X[[j]][["fpca"]][["harmonics"]][["coefs"]])[8],puntajes[[8]]~1,puntajes)}
     if(ncol(puntajes)>=9){
          g=gstat(g,colnames(X[[j]][["fpca"]][["harmonics"]][["coefs"]])[9],puntajes[[9]]~1,puntajes)}
     if(ncol(puntajes)>=10){
          g=gstat(g,colnames(X[[j]][["fpca"]][["harmonics"]][["coefs"]])[10],puntajes[[10]]~1,puntajes)}
     if(ncol(puntajes)>=11){
          stop("Too much harmonics")}

     g <- gstat(g, model=model, fill.all=fill.all)

     vg <- variogram(g)
     mcl = fit.lmc(vg, g)

     plot(vg, model = mcl)

     z = predict(mcl, newdata = newcoords)
     #CV

     #cv <- krige.cv(puntajes$V1~1, model= fv, nmax = 40)

     ##para retornar
     for (i in 1:ncol(puntajes)){
          grid.arrange(spplot(z[2*i-1], main = "ordinary kriging predictions"),spplot(z[2*i], main = "ordinary kriging variance"))  }
     ##falta meter los datos predichos
     pred=z[1][[1]]
     if(ncol(puntajes)>1){
          for (i in 2:ncol(puntajes)){
               pred=cbind(pred,z[2*i-1][[1]])
          }
     }


     # plot(x=c(0,1000),y=c(-50,140))
     # for (i in 1:nrow(pred)){
     #       lines(X[[j]][["fpca"]][["meanfd"]]+sum((pred[i,]*X[[j]][["fpca"]][["harmonics"]])),col=i)
     # }

     fpred=list()
     for( i in 1:nrow(pred)){
          fpred[[i]]=X[[j]][["fpca"]][["meanfd"]]+sum((pred[i,]*X[[j]][["fpca"]][["harmonics"]]))
     }
     vari=z[2][[1]]
     if(ncol(puntajes)>1){
          for (i in 2:ncol(puntajes)){
               vari=cbind(vari,z[2*i][[1]])
          }
     }
     # plot(x=c(0,1000),y=c(-5000,80000))
     # for (i in 1:nrow(vari)){
     #       lines(X[[j]][["fpca"]][["meanfd"]]+sum((vari[i,]*X[[j]][["fpca"]][["harmonics"]])),col=i)
     # }


     fvari=list()
     for( i in 1:nrow(vari)){
          fvari[[i]]=X[[j]][["fpca"]][["meanfd"]]+sum((vari[i,]*X[[j]][["fpca"]][["harmonics"]]))
     }

     ret=list(X=X,model=mcl,fpred=fpred,fvar=fvari)
     # hacer cv o no? agregar krigin(K) o no?
     class(ret)='fkck'
     return(ret)
}
