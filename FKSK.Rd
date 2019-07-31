\name{FKSK}
\alias{FKSK}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
     %%  ~~function to do ... ~~
}
\description{
     %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
     FKSK(X, newcoords,model,j=1,fill.all=T,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
     \item{X}{Is an object of class SpatFD.}
     \item{newcoords}{A data fame with two columns indicating X and Y coordinates.}
     \item{model}{An object of class vgm or a list of objects of class vgm.}
     \item{j}{A numeric value to specifying the position of the variable or a character value specifying the name of the variable. 1 by default.}
     \item{fill.all}{A boolean value to fit the same model to all scores. TRUE by default.}
     \item{...}{ Arguments to be passed to fit.variogram.}
}
\details{
     %%  ~~ If necessary, more details than the description above ~~
}
\value{
     An object of class FKSK, which inherits from list. Its components are:
          \item{X}{the object of class FKSK where are specified the functional predictions.}

     %%  ~Describe the value returned
     %%  If it is a LIST, use
     %%  \item{comp1 }{Description of 'comp1'}
     %%  \item{comp2 }{Description of 'comp2'}
     %% ...
}
\references{
     %% ~put references to the literature/web site here ~
}
\author{
     Angie Villamil and Martha Bohorquez
     %%  ~~who you are~~
}
\note{
     %%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

     \seealso{
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
