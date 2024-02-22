plot_bands <- function(
    base_plot, 
    predictions, 
    levels = c(.5, .8, .9),
    fill = "#3A448F",
    alpha = 0.6,
    linewidth = 0.05) {
  innames <- names(predictions)
  n <- length(levels)
  alpha <- alpha / (n - 1)
  l <- (1 - levels) / 2
  l <- c(rev(l), 1 - l)
  
  ntarget_dates <- dplyr::n_distinct(predictions$target_date)
  
  predictions <- predictions %>%
    dplyr::mutate(.pred_distn = dist_quantiles(quantile(.pred_distn, l), l)) %>%
    pivot_quantiles_wider(.pred_distn)
  qnames <- setdiff(names(predictions), innames)
  
  for (i in 1:n) {
    bottom <- qnames[i]
    top <- rev(qnames)[i]
    if (i == 1) {
      if (ntarget_dates > 1L) {
        base_plot <- base_plot +
          ggplot2::geom_ribbon(
            data = predictions,
            ggplot2::aes(x = target_date, ymin = .data[[bottom]], ymax = .data[[top]]),
            alpha = 0.2, linewidth = linewidth, fill = fill
          )
      } else {
        base_plot <- base_plot +
          ggplot2::geom_linerange(
            data = predictions,
            ggplot2::aes(x = target_date, ymin = .data[[bottom]], ymax = .data[[top]]),
            alpha = 0.2, linewidth = 2, color = fill
          )
      }
    } else {
      if (ntarget_dates > 1L) {
        base_plot <- base_plot +
          ggplot2::geom_ribbon(
            data = predictions,
            ggplot2::aes(x = target_date, ymin = .data[[bottom]], ymax = .data[[top]]),
            fill = fill, alpha = alpha
          )
      } else {
        base_plot <- base_plot +
          ggplot2::geom_linerange(
            data = predictions,
            ggplot2::aes(x = target_date, ymin = .data[[bottom]], ymax = .data[[top]]),
            color = fill, alpha = alpha, linewidth = 2
          )
      }
    }
  }
  base_plot
}
