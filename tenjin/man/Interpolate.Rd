\name{Interpolate}
\alias{Interpolate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interpolate
}
\description{
Linear interpolation for the missing data.
}
\usage{
Interpolate(id, data, type = "person_id")
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
    if (type == "person_id") {
        dp <- data \%>\% filter(person_id == ID) \%>\% mutate(difference = ymd_hms(timestamp) -
            lag(ymd_hms(timestamp))) \%>\% replace_na(list(difference = dseconds(x = 0))) \%>\%
            mutate(timelapse = cumsum(as.double(difference))) \%>\%
            mutate(period = timelapse\%/\%0.5)
        dp$difference[1]
    }
    else {
        dp <- data \%>\% filter(tag_id == ID) \%>\% mutate(difference = ymd_hms(timestamp) -
            lag(ymd_hms(timestamp))) \%>\% replace_na(list(difference = dseconds(x = 0))) \%>\%
            mutate(timelapse = cumsum(as.double(difference))) \%>\%
            mutate(period = timelapse\%/\%0.5)
    }
    xx <- numeric()
    yy <- numeric()
    if (sum(dp$difference > 0.5)) {
        index <- which(dp$difference > 0.5)
        for (i in 1:length(index)) {
            if (dp$difference[index[i]] < 2) {
                da <- dp \%>\% slice(index[i] - 1, index)
                dt <- as.double((mean(ymd_hms(da$timestamp)) -
                  ymd_hms(da$timestamp[1])))/as.double((ymd_hms(da$timestamp[2]) -
                  ymd_hms(da$timestamp[1])))
                xx[i] <- dt * (da$x[2] - da$x[1]) + da$x[1]
                yy[i] <- dt * (da$y[2] - da$y[1]) + da$y[1]
                dp <- dp \%>\% add_row(tag_id = da$tag_id[1], experiment_id = da$experiment_id[1],
                  person_id = da$person_id[1], timestamp = as.character(ymd_hms(da$timestamp[2]) +
                    (seconds(da$difference[2])/2)), x = xx[i],
                  y = yy[i], .before = index[i])
                return(dp)
            }
            else {
                dp <- dp \%>\% add_row(tag_id = dp$tag_id[1], experiment_id = dp$experiment_id[1],
                  person_id = dp$person_id[1], timestamp = NA,
                  x = NA, y = NA, .before = index[i])
                return(dp)
            }
        }
    }
    else {
        return(dp)
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
