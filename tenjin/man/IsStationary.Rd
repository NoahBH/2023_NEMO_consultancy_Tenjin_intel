\name{IsStationary}
\alias{IsStationary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
IsStationary
}
\description{
The function below infers stationarity using:

* Speed information
* Proximity to a tablet

Heuristics are applied to the information above, and thus the tagging of data points as "stationary" involve some level of uncertainty. In order to give full control of this, there are two arguments:

* `benchmark` requires either a function or a numerical value, with which to infer slow speed. The default is `mean()` and thus speeds lower than average are flagged as slow.
* `b` is the minimum Euclidean distance necessary to be flagged as near a tablet. The default is 1 meter.
}
\usage{
IsStationary(df, tablet_positions, b = 1, benchmark = mean)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{tablet_positions}{
%%     ~~Describe \code{tablet_positions} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
}
  \item{benchmark}{
%%     ~~Describe \code{benchmark} here~~
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
function (df, tablet_positions, b = 1, benchmark = mean)
{
    euclid <- function(x, y) {
        sqrt(x^2 + y^2) \%>\% return
    }
    if (is_function(benchmark)) {
        df \%>\% mutate(slow = speed <= benchmark(speed)) \%>\% rowwise \%>\%
            mutate(at_tablet = case_when(slow ~ min(euclid(x -
                tablet_positions$x, y - tablet_positions$y)) <=
                b), at_tablet_id = case_when(!is.na(at_tablet) ~
                which.min(euclid(x - tablet_positions$x, y -
                  tablet_positions$y)))) \%>\% replace_na(list(at_tablet = FALSE,
            at_tablet_id = 0)) \%>\% ungroup \%>\% mutate(at_tablet_id = as.factor(at_tablet_id *
            at_tablet)) \%>\% return
    }
    else if (is.numeric(benchmark)) {
        df \%>\% mutate(slow = speed <= benchmark(speed)) \%>\% rowwise \%>\%
            mutate(at_tablet = case_when(slow ~ min(euclid(x -
                tablet_positions$x, y - tablet_positions$y)) <=
                b), at_tablet_id = case_when(!is.na(at_tablet) ~
                which.min(euclid(x - tablet_positions$x, y -
                  tablet_positions$y)))) \%>\% replace_na(list(at_tablet = FALSE,
            at_tablet_id = 0)) \%>\% ungroup \%>\% mutate(at_tablet_id = as.factor(at_tablet_id *
            at_tablet)) \%>\% return
    }
    else {
        stop("Supply either a function or numeric value to `benchmark`")
    }
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
