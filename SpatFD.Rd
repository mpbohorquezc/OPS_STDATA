\name{SpatFD}
\alias{SpatFD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create an object SpatFD
     %%  ~~function to do ... ~~
}
\description{ Function that creates SpatFD objects; objects that hold all the information necessary for univariate or multivariate geostatistical prediction for functional data.
     %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
     SpatFD(X,coords,basis="Bsplines",nbasis=4,lambda=0,nharm=NULL,vp=NULL,add=NULL,...)
     SpatFD(X, coord, lambda=0,  nharm=NULL, add=NULL,...)
     SpatFD(X, coord, lambda=0,  vp=NULL, add=NULL,...)
     SpatFD(X, coord, lambda=0, add=NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
     \item{X}{ Is a matrix, data frame or fdata with the observed data
          %%     ~~Describe \code{x} here~~
     }
     \item{coords}{A data frame or a matrix with two columns indicating X and Y coordinates}
     \item{basis}{A character specifying the basis functions, ``Bsplines'' or ``Fourier''}
     \item{nbasis}{An integer variable specifying the number of basis functions. 4 by default. }
     \item{lambda}{A non negative real number specifying the amount of smoothing to be applied to the estimated functional parameter. 0 by default}
     \item{nharm}{The number of harmonics or principal components to compute.}
     \item{vp}{The percentage of variance to calculate the number of harmonics or principal components to compute.}
     \item{add}{An object of class SpatFD to be added.}
     \item{...}{Arguments to be passed to the basis function creation (see create.bsplines.basis or create.fourier.basis).}
}
%\details{
     %%  ~~ If necessary, more details than the description above ~~
          %}
\value{
     An object of class SpatFD, which inherits from list. Its components are:
          \item{X} {An object of class SpatFD, where is specified the original data, the functional data and the components of the FPCA}
     %%  ~Describe the value returned
     %%  If it is a LIST, use
     %%  \item{comp1 }{Description of 'comp1'}
     %%  \item{comp2 }{Description of 'comp2'}
     %% ...
}
\references{
     %% ~put references to the literature/web site here ~
}
\author{ Angie Villamil, Martha Bohorquez
     %%  ~~who you are~~
}
\note{
     %%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

     \seealso{ \code{\link{FKSK}}, \code{\link{FKCK}}, \code{\link{FCOK}}
          %% ~~objects to See Also as \code{\link{help}}, ~~~
     }
\examples{
     ##---- Should be DIRECTLY executable !! ----
     ##-- ==>  Define data, use random,
     ##--	or do  help(data=index)  for the standard data sets.

     ## The function is currently defined as
     function (x)
     {
     }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
