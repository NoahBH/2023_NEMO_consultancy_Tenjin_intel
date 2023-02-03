\name{AddSpeed}
\alias{AddSpeed}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{AddSpeed
}
\description{The function below is designed to add derived speed and acceleration to the data object. It assumes that smoothing of these variable is preferred, and thus signal smoothing is automatically done using FFT. Using the argument `smooth`, one can set the _proportion of frequencies_ to keep in the signal (default is 10%).
}
\usage{
AddSpeed(df, dt = 0.5, smooth = 10^-1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{dt}{
%%     ~~Describe \code{dt} here~~
}
  \item{smooth}{
%%     ~~Describe \code{smooth} here~~
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
function (df, dt = 0.5, smooth = 10^-1)
{
    Derivate <- function(var) {
        abs(var - lag(var))/dt
    }
    Mutate <- function(df_) {
        df_ \%>\% mutate(speed_x = Derivate(x), speed_y = Derivate(y),
            speed = sqrt(speed_x^2 + speed_y^2), acc_x = Derivate(speed_x),
            acc_y = Derivate(speed_y)) \%>\% replace_na(list(speed_x = 0,
            speed_y = 0, speed = 0, acc_x = 0, acc_y = 0)) \%>\%
            mutate(across(speed_x:acc_y, fft)) \%>\% mutate(row = row_number() <=
            round(nrow(.) * smooth) | row_number() >= round(nrow(.) -
            nrow(.) * smooth), speed_x = Re(fft(speed_x * row,
            inverse = TRUE)/nrow(.)), speed_y = Re(fft(speed_y *
            row, TRUE)/nrow(.)), speed = Re(fft(speed * row,
            TRUE)/nrow(.)), acc_x = Re(fft(acc_x * row, TRUE)/nrow(.)),
            acc_y = Re(fft(acc_y * row, TRUE)/nrow(.))) \%>\% select(-row) \%>\%
            return
    }
    df \%>\% group_by(person_id) \%>\% group_modify(~Mutate(.x)) \%>\%
        ungroup() \%>\% return
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
