\name{ChangeCoordinate}
\alias{ChangeCoordinate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ChangeCoordinate
}
\description{
Extra Precautions to Recognise if an individual is at a tablet location

Although in some situations Euclidean distance from the tablets suffice, this section was added to allow for even stricter filtering of the data.

In addition to the Euclidean distance, we compute the angle (radians) and compare this to the angle of the tablets. This only makes sense if the origin is at the centre tablet (i.e. x = 3.33, y = 3.97).

Therefore, `ChangeCoordinate()` below translates all coordinates in accordance to the new origin provided.
}
\usage{
ChangeCoordinate(df, origin)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{origin}{
%%     ~~Describe \code{origin} here~~
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
function (df, origin)
{
    df \%>\% mutate(x = x - origin[1], y = y - origin[2]) \%>\% return
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
