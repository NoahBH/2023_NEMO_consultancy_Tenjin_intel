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
AddGoals(id, data, task_df, type = "person_id")
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
  function(id, data, task_df, type = "person_id"){
  ID = id
  #Replace interpolate with your own function.
  #IMPORTANT check what the time column is called.
  #Data for one given person
  TheData <- data
  #Data for one given tag
  Goals <- task_df %>% filter(, person_id == ID)


  #Important to add these columns to the data
  TheData$tablet_to_id <- NA
  TheData$tablet_from_id <- NA

  #ContainsTime at the end of the loop will contains a list of vectors.
  #Each vector is a comparison of whether each row in TheData$time is within
  #A given timespan of a goals that is in the Goals table
  ContainsTime <- list()
  #I have seperate columns for the time and the adjusted time
  #Make sure naming of the time column is consistent between data & function
  #
  for (j in 1:length(Goals$start_time)){
    ContainsTime[[j]] <- (ymd_hms(TheData$time) >= ymd_hms(Goals$start_time[j]) &
                            ymd_hms(TheData$time) <= ymd_hms(Goals$end_time[j]))
  }

  #We loop over each entry in ContainsTime, use which to get the positions that are
  #TRUE than replace the positions of the TRUE's in position k in the list with the goals
  #in position from the subsetted task table
  for (k in 1:length(ContainsTime)){
    #
    TheData$tablet_to_id[which(ContainsTime[[k]])] <- Goals$tablet_to_id[k]
    TheData$tablet_from_id[which(ContainsTime[[k]])] <- Goals$tablet_from_id[k]
  }
  return(TheData)
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
