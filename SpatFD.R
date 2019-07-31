SpatFD=function(X,coords,basis="Bsplines",nbasis=4,lambda=0,nharm=NULL,vp=NULL,add=NULL,...){
     #----------------------------------------------------------------------------
     #           VALIDANDO ARGUMENTOS *
     #----------------------------------------------------------------------------
     #X
     if(!(is.matrix(X) || is.array(X) || is.data.frame(X) ||is.fdSmooth(X)||is.fd(X))){
          stop("ERROR: Wrong class of X object")
     }
     if(any(is.na(X))){
          stop("ERROR: There is some NA value in X")
     }
     #coords

     if(!(is.matrix(coords) || is.data.frame(coords))){
          stop("ERROR: Wrong class of coords object")
     }else if(!all(apply(coords, c(1,2), is.numeric))){
          stop("ERROR: coords must be numeric data")
     }else if(any(is.na(coords))){
          stop("ERROR: There is some NA value in coords")
     }
     #Coincidan tamaños
     if(is.matrix(X)||is.data.frame(X)|| is.array(X)){
          cx=dim(X)[2]
     }else if(is.fdSmooth){
          cx=dim(X$fd$coefs)[2]
     }else if(is.fd(X)){
          cx=dim(X$coefs)[2]
     }

     fc=dim(coords)[1]

     if(cx!=fc){
          stop("ERROR: number of columns of Xmust be equal to number of rows of coords")
     }


     #basis
     if (!(is.character(basis)&& length(basis)==1)){
          stop("ERROR: Wrong class of basis object")
     } else if (!(basis=="Fourier" || basis =="Bsplines")){
          stop("ERROR: basis not specified")
     }
     #nbasis
     if (!(((is.fdSmooth(X)||is.fd(X) )&&is.null(nbasis))  || (is.numeric(nbasis)&& length(nbasis)==1))){
          stop("ERROR: Wrong class of nbasis object")
     }
     #nharm
     if (!(is.null(nharm)  || (is.numeric(nharm)&& length(nharm)==1))){
          stop("ERROR: Wrong class of nharm object")
     }
     #lambda
     if (!(((is.fdSmooth(X)||is.fd(X) )&&is.null(lambda))  || (is.numeric(lambda)&& length(lambda)==1))){
          stop("ERROR: Wrong class of lambda object")
     }
     #vp
     if (!( is.null(vp)  || (is.numeric(vp)&& length(vp)==1))){
          stop("ERROR: Wrong class of vp object")
     }
     #add
     if(!(is.null(add) || is.SpatFD(add))){
          stop("ERROR: Wrong class of add object")
     }
     #----------------------------------------------------------------------------
     #           DEJANDO LISTO PARA FPCA
     #----------------------------------------------------------------------------


     if(is.matrix(X) || is.array(X) || is.data.frame(X)){

          MX=as.matrix(X)
          if(!is.numeric(MX)){
               stop("ERROR: Object X is not numeric")
          }

          hr <- c(1,nrow(MX))
          oplfd <- vec2Lfd(c(1,ncol(MX)), hr)

          #bases funcionales
          if(basis=="Bsplines"){
               hourbasis <- create.bspline.basis(hr,nbasis,...)
          } else if(basis=="Fourier"){
               hourbasis <- create.fourier.basis(hr,nbasis,...)
          }

          X_fdPar<-fdPar(fdobj=hourbasis,Lfdobj=oplfd,lambda)
          X_fdSm <- smooth.basis(argvals=1:nrow(MX),MX,X_fdPar)
          X_fd=X_fdSm$fd
          cn=X_fd$fdnames$reps

     }  else if (is.fdSmooth(X)){
          X_fdSm = X
          X_fd=X_fdSm$fd
          cn=X_fd$fdnames$reps
     }  else if (is.fd(X)){
          X_fd=X
          cn=X_fd$fdnames$reps
     }

     #----------------------------------------------------------------------------
     #            FPCA
     #----------------------------------------------------------------------------

     if (!is.null(nharm)){
          fpca=pca.fd(X_fd,nharm=nharm)
     }  else if(!is.null(vp)){
          nh=1
          repeat{
               fpca=pca.fd(X_fd,nharm = nh)
               if(! sum(fpca$varprop)<vp){ break }
               nh=nh+1
          }
     }  else if(is.null(nharm) && is.null(vp)){
          fpca=pca.fd(X_fd)
     }


     # puntaje=fpca$scores
     # rownames(puntaje)=cn
     # puntajes=as.data.frame(puntaje)
     # coordinates(puntajes)=coords
     #----------------------------------------------------------------------------
     #           OUTPUT *
     #   -  decidir si agregar puntajes a la lista de salida o no
     #----------------------------------------------------------------------------

     if(is.null(add)){
          s=list(list(X=X,coords=coords,coordsnames=cn,fpca=fpca))
          class(s)="SpatFD"
     }  else if (class(add)=="SpatFD"){
          s=list(list(X=X,coords=coords,cn=cn,fpca=fpca))
          s=append(add,s)
          class(s)="SpatFD"
     }

     return(s)
}
