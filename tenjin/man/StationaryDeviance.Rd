\name{StationaryDeviance}
\alias{StationaryDeviance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
StationaryDeviance
}
\description{
This function computes a measure of the noise when someone is stationary at a given tablet.
}
\usage{
StationaryDeviance(df, tablet_positions)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{tablet_positions}{
%%     ~~Describe \code{tablet_positions} here~~
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
function (df, tablet_positions)
{
    df \%>\% filter(at_tablet_id != 0) \%>\% group_by(at_tablet_id) \%>\%
        mutate(deviance_x = x - mean(x), deviance_y = y - mean(y),
            at_tablet_id = as.factor(at_tablet_id)) \%>\% ungroup() \%>\%
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
