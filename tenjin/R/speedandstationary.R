library("lubridate")
library("tidyverse")
library("data.table")
library("scales")
library("dplyr")
library("magrittr")

## Kinesiological Variables
# derive speed and acceleration

AddSpeed <- function(df, dt=.5, smooth=10^-1){
  # df: tibble containing data
  # dt: time interval in seconds
  # smooth: proportion of FFT frequencies to keep when smoothing. 1 = no smoothing


  # util function to use inside dplyr pipes
  Derivate <- function(var) {
    abs(var - lag(var)) / dt
  }

  # util function to use inside dplyr pipes
  Mutate <- function(df_) {
    df_ %>%
      # absolute change in position / seconds = speed
      mutate(speed_x = Derivate(x),
             speed_y = Derivate(y),

             # get the added speed
             speed = sqrt(speed_x^2 + speed_y^2),

             # change in speed / seconds = acceleration
             acc_x = Derivate(speed_x),
             acc_y = Derivate(speed_y)) %>%

      replace_na(list("speed_x"=0, "speed_y"=0, 'speed'=0,
                      "acc_x"=0, "acc_y"=0)) %>%

      # smooth signal using FFT
      mutate(across(speed_x:acc_y, fft)) %>%


      mutate(row = row_number()<=round(nrow(.)*smooth) |
               row_number()>=round(nrow(.) - nrow(.)*smooth),
             speed_x = Re(fft(speed_x * row, inverse=TRUE)/nrow(.)),
             speed_y = Re(fft(speed_y * row, TRUE)/nrow(.)),
             speed = Re(fft(speed * row, TRUE)/nrow(.)),
             acc_x = Re(fft(acc_x * row, TRUE)/nrow(.)),
             acc_y = Re(fft(acc_y * row, TRUE)/nrow(.))) %>%

      select(-row) %>%

      return
  }

  df %>%
    group_by(person_id) %>%
    group_modify(~ Mutate(.x)) %>%
    ungroup() %>%

    return

}

## Infer Stationarity
# function to flag each data point as stationary or not; at the centre or not
IsStationary <- function(df, tablet_positions, b = 1, benchmark=mean) {
  # df: Tibble of data
  # tablet_positions: Tibble with tablet location
  # b: Maximum Euclidean distance allowed from a tablet location
  # benchmark: either a function to compute a central tendency or
  # a numerical value used to identify slow speed

  # Euclidean distance calculation
  euclid <- function(x, y) {

    sqrt(x^2+y^2) %>%
      return

  }

  if (is_function(benchmark)) {

    df %>%
      # if speed is lower than average
      mutate(slow = speed <= benchmark(speed)) %>%

      rowwise %>%

      # if speed is lower than benchmark, check if close to a tablet
      mutate(at_tablet = case_when(slow ~
                                     min(euclid(x-tablet_positions$x, y-tablet_positions$y)) <= b),

             # get tablet_id of closest
             at_tablet_id = case_when(!is.na(at_tablet) ~
                                        which.min(euclid(x-tablet_positions$x, y-tablet_positions$y)))) %>%

      replace_na(list(at_tablet = FALSE, at_tablet_id = 0)) %>%
      ungroup %>%
      mutate(at_tablet_id = as.factor(at_tablet_id * at_tablet)) %>%

      return

  } else if (is.numeric(benchmark)) {

    df %>%
      # if speed is lower than average
      mutate(slow = speed <= benchmark(speed)) %>%

      rowwise %>%

      # if speed is lower than benchmark, check if close to a tablet
      mutate(at_tablet = case_when(slow ~
                                     min(euclid(x-tablet_positions$x, y-tablet_positions$y)) <= b),

             # get tablet_id of closest
             at_tablet_id = case_when(!is.na(at_tablet) ~
                                        which.min(euclid(x-tablet_positions$x, y-tablet_positions$y)))) %>%

      replace_na(list(at_tablet = FALSE, at_tablet_id = 0)) %>%
      ungroup %>%
      mutate(at_tablet_id = as.factor(at_tablet_id * at_tablet)) %>%

      return

  } else {

    stop("Supply either a function or numeric value to `benchmark`")

  }

}

# INVESTIGATE NOISE PATTERNS
# function to change the coordinate system (only translation of the origin)
ChangeCoordinate <- function(df, origin) {
  # df: data with columns x and y to change
  # origin: new origin a vector of length 2

  df %>%
    mutate(x = x - origin[1],
           y = y - origin[2]) %>%
    return

}

# returns randian angle compared to (0,1)
LocationCosine <- function(df) {
  # df: data with columns x and y

  # vector from which to compute angle
  basis <- c(0,1)

  df %>%
    rowwise() %>%
    mutate(angle = crossprod(c(x, y), basis) /  # numerator
             (sqrt(crossprod(c(x, y))) * sqrt(crossprod(basis))), # denominator
           angle = as.numeric(acos(angle)), # arccos

           # acos only returns range of 0.0 to pi
           # but we need the angle in the range of 0.0 to 2pi :
           angle = abs(2*pi * (x < 0) - angle)) %>%
    ungroup() %>%


    return
}


# finds tablet closest in angle to each location
TowardsTablet <- function(df, tablet_positions, centre="10") {
  # df: tibble with x,y location, stationary (bool), and at_tablet columns
  # tablet_positions: dataframe with xy tablet positions and angle from origin
  # HAS to be arranged by tablet_id


  # take into account oblique angles
  tablet_positions %<>% mutate(angle2 = 2*pi + angle)


  tablets <- c(tablet_positions$angle, tablet_positions$angle2)

  df %>%
    # vectorised logical
    mutate(change = at_tablet_id != centre & (slow & at_tablet)) %>%
    rowwise %>%

    # if stationary and not close to tablet centre,
    # towards which tablet is the location
    mutate(at_tablet_id = case_when(change ~
                                      which.min(abs(tablets - angle)) %% nrow(tablet_positions),

                                    # if close to tablet 10, return same value
                                    at_tablet_id == centre ~ as.integer(10.0)),
           at_tablet_id = as.factor(at_tablet_id)) %>%

    ungroup %>%

    return
}


## Deviance from the mean position when stationary
StationaryDeviance <- function(df, tablet_positions) {

  df %>%
    filter(at_tablet_id != 0) %>% # decrease data
    group_by(at_tablet_id) %>%

    mutate(deviance_x = x-mean(x),
           deviance_y = y-mean(y),
           at_tablet_id = as.factor(at_tablet_id)) %>%
    ungroup() %>%
    return

}

