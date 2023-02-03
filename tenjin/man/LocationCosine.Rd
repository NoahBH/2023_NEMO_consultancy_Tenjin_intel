\name{LocationCosine}
\alias{LocationCosine}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
LocationCosine
}
\description{
Then, `LocationCosine()` can be used to compute the orientation of given coordinates compared to the basis vector $\hat{j}=(0,1)$.
}
\usage{
LocationCosine(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
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

## The function is currently defined as
function (df)
{
    basis <- c(0, 1)
    df \%>\% rowwise() \%>\% mutate(angle = crossprod(c(x, y), basis)/(sqrt(crossprod(c(x,
        y))) * sqrt(crossprod(basis))), angle = as.numeric(acos(angle)),
        angle = abs(2 * pi * (x < 0) - angle)) \%>\% ungroup() \%>\%
        return
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
