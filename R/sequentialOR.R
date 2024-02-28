#' Sequential outlier rejection
#'
#' Iteratively fit a model to data and remove the most extreme residual. Three models are implemented here: ordinary least squares (lm), generalized linear model (glm), generalized additive model (GAM), or smooth spline (ss)
#' @author Sean Rohan, updates by Caitlin Allen Akselrud
#' 
#' @param data Data frame containing predictor and response variables.
#' @param method Character vector of length one indicating which function should be used to fit models. Options are 'lm', 'glm', 'gam', or 'ss'
#' @param formula Formula to be passed to the model. Formula must be valid for the selected method. Variables must included in `data` and the extraction operator ($) should not be used in the forumla.
#' @param n.reject Numerical vector of length one. Number of observations to be rejected in each iteration. Larger values speed the funciton up, but may impede precise detection of break points.
#' @param n.stop Numerical vector of length one which indicating the number of observations remaining or proportion of initial observations which should be remaining before outlier rejection stops. For example, with 400 initial observations, n.stop set to 40 or 0.1 (= 400*0.1) would stop the algorithm when 40 observations remain.
#' @param threshold.stop Optional. Threshold value for stopping sequentialOR as a 1L numeric vector. If provided, ends sequentialOR calculations when the maximum absolute residual is less than the threshold.stop.
#' @param tail Character vector of length one indicating whether to reject from the lower tail, upper tail, or both tails. Default = "both".
#' @param plot Logical vector indicating whether the function should print a plot of observations versus RMSE.
#' @param ... Additional arguments passed to function calls (family, offset, etc.)
#'
#' @return The function returns a list with (1) input data frame with an additional column indicating the order in which observations were rejected, (2) a data frame containing the number of observations used for model-fitting and the associated RMSE. If argument `plot=T`, also prints a plot of observations versus RMSE given the remaining observations.
#'
#' @references Kotwicki, S., M. H. Martin, and E. A. Laman. 2011. Improving area swept estimates from bottom trawl surveys. Fisheries Research 110(1):198–206.
#' @export

sequentialOR <- function(data, method = 'lm', formula, n.reject = 1, n.stop, threshold.stop = NULL, tail = "both", ...) {

  vess <- unique(data$vessel)
  hauln <- unique(data$haul)
  
  tail <- tolower(tail)
  method <- tolower(method)

  # Calculate absolute stopping point if n.stop is a proportion
  if(n.stop < 1) { #function will run until you hit some proportion of the data (eg .4 = 40% data looped over until it stops)
    n.stop <- n.stop * nrow(data)
  }

  # Initialize output
  iter <- floor((nrow(data) - n.stop) / n.reject) #number of samples to run through
  data$SOR_RANK <- NA
  RMSE <- rep(NA, iter)
  NN <- rep(NA, iter) #num point rejected?
  PARSLOPE <- rep(NA, iter) #slope for model
  data$index <- 1:nrow(data)
  # ca: add max residual

  # Rejection counter
  rejection <- 1 #number of points rejected
  
  
  for(i in 1:iter) {

    # Subset data that hasn't been rejected
    data.sub <- subset(data, is.na(SOR_RANK), drop = TRUE)

    # Select model
    if(method == 'lm') mod <- lm(formula, data = data.sub); #, ...); 
    if(method == 'glm') mod <- glm(formula, data = data.sub, ...);
    if(method == 'gam') mod <- mgcv::gam(formula, data = data.sub, ...);
    # CIA mod here- add smooth spline to options
    if(method == 'ss') {
      attach(data.sub, warn.conflicts = FALSE)
      
      mod <- stats::smooth.spline(x = formula, # where formula = data.sub$response_var~data.sub$predictor.var
                                  spar = .8) 
    }

    # Append residuals to subsetted data
    data.sub$resid <- resid(mod)

    # Assign order of rejection to input data frame based on residual rank-order #ca: finds max residual (use both to reject resid on either side)

    if(tail == "both") data$SOR_RANK[which(data$index == data.sub$index[which.max(abs(data.sub$resid))])] <- i
    if(tail == "upper") data$SOR_RANK[which(data$index == data.sub$index[which.max(data.sub$resid)])] <- i
    if(tail == "lower") data$SOR_RANK[which(data$index == data.sub$index[which.min(data.sub$resid)])] <- i

    # Calculate RMSE for iteration
    RMSE[i] <- mean(sqrt(residuals(mod)^2)) #all values still in dataset; magnitude of RMSE flips when you omitted a certain number of points
    NN[i] <- i #counter

    # ca: add max residual

    # Stop based on a threshold, as in Kotwicki et al. (2011); same as sk, but not hard-coded stopping rule, need to pass threshold for stop
    if(!is.null(threshold.stop)) {
      resids <- resid(mod)
      if(sd(resids)<5) { #ca: stop distance calc, based on sd of hauls; equation from 2011 paper; threshold stopping value (hard-coded); would need to add to sean's code
        threshold.stop=-0.3034*sd(resids)^2 + 2.9428*sd(resids)-0.1112
      } else {threshold.stop=7}
      
      if(max(abs(resids)) < threshold.stop) {
        RMSE <- RMSE[1:i]
        NN <- NN[1:i]
        print(paste0("sequentialOR: Stopping threshold reached. Stopped after iteration " , i))
        mean_spread <- data %>% 
          dplyr::filter(is.na(SOR_RANK)) %>% 
          dplyr::summarize(n_pings = n(),
                           mean = mean(measurement_value))
        results_row <- dplyr::bind_cols(mean_spread, sd = sd(resids))
        
        return(list(results = results_row,
                    obs_rank = data,
                    rmse = data.frame(N = NN, RMSE = RMSE )))
      }
    }

    #Update rejection index counter
    rejection <- rejection + n.reject
    detach(data.sub)
  }
  
  mean_spread <- data %>% 
    dplyr::filter(is.na(SOR_RANK)) %>% 
    summarize(n_pings = n(),
              mean = mean(measurement_value))
  results_row <- bind_cols(mean_spread, sd = sd(resids))

  return(list(results = results_row,
              obs_rank = data,
              rmse = data.frame(N = NN, RMSE = RMSE),
              info = c(vessel = vess, haul = hauln)))

}
