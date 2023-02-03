\name{AddGoals}
\alias{AddGoals}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
AddGoals
}
\description{
The function links the timestamp to the goal of a given person ID.
}
\usage{
AddGoals(id, data, type = "person_id")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{
%%     ~~Describe \code{id} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
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
function (id, data, type = "person_id")
{
    ID = id
    TheData <- data
    Goals <- tasks \%>\% filter(person_id == ID)
    TheData$tablet_to_id <- NA
    TheData$tablet_from_id <- NA
    ContainsTime <- list()
    for (j in 1:length(Goals$adjust_starttime)) {
        ContainsTime[[j]] <- (ymd_hms(TheData$time) >= ymd_hms(Goals$adjust_starttime[j]) &
            ymd_hms(TheData$time) <= ymd_hms(Goals$adjust_endtime[j]))
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
