\name{TowardsTablet}
\alias{TowardsTablet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
TowardsTablet
}
\description{
The function below checks which tablet the position is closest to in terms of angle, given that the given coordinate is flagged as stationary.
}
\usage{
TowardsTablet(df, tablet_positions, centre = "10")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{tablet_positions}{
%%     ~~Describe \code{tablet_positions} here~~
}
  \item{centre}{
%%     ~~Describe \code{centre} here~~
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
function (df, tablet_positions, centre = "10")
{
    tablet_positions \%<>\% mutate(angle2 = 2 * pi + angle)
    tablets <- c(tablet_positions$angle, tablet_positions$angle2)
    df \%>\% mutate(change = at_tablet_id != centre & (slow & at_tablet)) \%>\%
        rowwise \%>\% mutate(at_tablet_id = case_when(change ~
        which.min(abs(tablets - angle))\%\%nrow(tablet_positions),
        at_tablet_id == centre ~ as.integer(10)), at_tablet_id = as.factor(at_tablet_id)) \%>\%
        ungroup \%>\% return
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
