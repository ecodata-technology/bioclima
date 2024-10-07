#' Calculate custom version of periodic bioclims
#'
#' @param target_var Climate variable to summarize over defined periods
#' @param target_agg Method to aggregate target variable
#' @param period_var Climate variable used to define the periods (e.g. wettest period is defined by precipitation)
#' @param period_agg Method to select target period (e.g. 'max' for wettest period or 'min' or driest)
#' @param period numeric. Length of period to summarize data (e.g., quarters,
#' semesters). If using monthly data, a quarter (3-months) will be used.
#' @param circular TRUE/FALSE. Calculate periods that include first and last
#' units. For example, if using mean monthly data and quarters, circular=TRUE will also
#' calculate Nov-Dec-Jan and Dec-Jan-Feb.
#' @param out_name Optional, variable name in output raster
#' @details
#' For 'target_agg' the default value is 'mean'. This is typically correct, except when aggregating precipitation, 
#' then target_agg should be 'sum.'
#' 
#' @export
#'
#'

custom_periodic <- function(target_var, target_agg='mean',
                            period_var, period_agg, 
                            period = 3, circular = FALSE,
                            checkNA = TRUE, stopNA = TRUE,
                            out_name=NA) {
  
  # Check for same extent, number of rows and columns, projection,
  # resolution, and origin
  sameGeom <- class(purrr::reduce(list(tmin, tmax, tavg, prcp) |>
                                    purrr::discard(is.null),
                                  bioclima::testGeom))
  if (sameGeom == "SpatRaster") {
    message("SpatRasters have same extent, number of rows and columns, ",
            "projection, resolution, and origin")
  }
  
  message("Warning: this function does not (yet) check for or handle NA values in inputs. NAs in output may result.")
  
  period_summed <- bioclima::sum_period(period_var, period, circular)
  target_summed <- bioclima::sum_period(target_var, period, circular)
  
  # if target variable is temperature, return mean of temperature input, not sum
  # for example, minimum temperature (quarterly mean)
  if (target_agg == 'mean') {
    message("Returning periodic mean of target variable. For precipitation as target variable, 
            this is incorrect. Instead, set target_agg to 'sum'.")
    target_summed = target_summed / period
  }
  
  message(
    paste0("Custom periodic calculations used a period of ", period,
           " units with", if(circular == FALSE) "out", " circularity.")
  )
  
  if (period_agg == 'max') {
    custom_bc <- terra::selectRange(target_summed, terra::which.max(period_summed))
  } else if (period_agg == 'min') {
    custom_bc <- terra::selectRange(target_summed, terra::which.min(period_summed))
  }
  
  if (!is.na(out_name)) {
    names(custom_bc) <- out_name
  }
  return(custom_bc)
  
}
