\name{CompleteData}
\alias{CompleteData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CompleteData
}
\description{
Function which loops over each ID, first interpolating data, then summarising it, adding the goals for participants. If verbose = TRUE then after each ID is processed it will give the maximum time jump and mean time jump for that given ID.
}
\usage{
function(vectors, data, task_df, type = "person_id", verbose = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vectors}{
%%     ~~Describe \code{vectors} here~~
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
  function(vectors, data, task_df, type = "person_id", verbose = F){
    FinalDataframe <- data.frame()
    maxjumps <- numeric()
    meanjumps <- numeric()
    for (i in 1:length(vectors)){

      PartialData <- Interpolate(vectors[i],data,type)
      maxjumps[i] <- max(PartialData$difference)
      meanjumps[i] <- mean(PartialData$difference, na.rm = TRUE)
      # compute deviance within periods
      PartialData %>% group_by(person_id, period) %>%
        suppressMessages(summarise(n_data = n(),
                                   time = first(timestamp),
                                   meanx = mean(x, na.rm = TRUE) ,
                                   meany = mean(y, na.rm = TRUE),
                                   x_maxdev = max(mean(x, na.rm = TRUE) -x),
                                   y_maxdev = max(mean(y, na.rm = TRUE) -y),
                                   at_tablet_id = first(at_tablet_id),
                                   slow = as.logical(round(mean(slow, na.rm = TRUE))),
                                   at_tablet = as.logical(round(mean(at_tablet, na.rm = TRUE))),
                                   deviance_x = mean(deviance_x, na.rm = TRUE),
                                   deviance_y = mean(deviance_y, na.rm = TRUE),
                                   speed_x = mean(speed_x, na.rm = TRUE),
                                   speed_y = mean(speed_y, na.rm = TRUE),
                                   speed = mean(speed, na.rm = TRUE),
                                   acc_x = mean(acc_x, na.rm = TRUE),
                                   acc_y = mean(acc_y, na.rm = TRUE)))

      #Selection for person_id or tag_id
      if(type == "person_id"){
        PartialData$person_id <- vectors[i]
        PartialData <- AddGoals(vectors[i], PartialData, task_df)
      }
      else{
        PartialData$tag_id <- vectors[i]
      }
      FinalDataframe <- rbind(FinalDataframe,PartialData)
      print(paste("ID", i , "of", length(vectors)))
      if(verbose == T){
        print(paste("with maximum timejump:", maxjumps[i],"and mean timejump:", meanjumps[i]))
      }
    }
    return(FinalDataframe)
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
