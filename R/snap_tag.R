#' Take a snap of the temporal data from internal tag data
#' @description
#' This function produces a plot for a time range from the tag observations. The plot shows depth, light level and temperature.
#' @param tag tbl or data.frame Object containing the tag information
#' @param temporal string Name of the variable containing the date and time
#' @param time_begin string Starting point of the time window
#' @param time_end string Ending point of the time window
#' @param temperature logical Set whether to present temperature in the plot
#' @param depth logical Set whether to present depth in the plot
#' @param light_level logical Set whether to present the light level in the plot
#' @param interval string indicating by how much time should date ticks appear (e.g. "12 hour" or "5 min")
#' @param depth_range vector indicating desired Depth limits to be ploted
#' @param tmp_range vector indicating desired Temperature limits to be ploted
#' @param main string, title to be included in the plot, default NULL
#'
#' @return Base R plot
#' @export
snap_tag <- function(tag, temporal, time_begin, time_end, temperature = TRUE, depth = TRUE, light_level = TRUE, interval, depth_range, tmp_range = NULL, main = NULL) {
  
  snap <- tag[tag[[{{ temporal }}]] >= time_begin & tag[[{{ temporal }}]] <= time_end, ]
  
  if (depth == TRUE) {
    graphics::par(mgp = c(3.5, 2, 0), mar = c(5, 4, 2, 4) + 0.3)
    plot(x = snap[[{{ temporal }}]], y = snap$Depth, type = "l", xaxt = "n", ylim = rev(depth_range), xlab = "Date", ylab = "Depth (m)", main = main, xaxs = "i")
    x_axis_time <- seq(min(snap[[{{ temporal }}]]), max(snap[[{{ temporal }}]]), by = interval)
    x_ticks <- axis(1, at = x_axis_time, labels = strftime(x_axis_time, format = "%H:%M\n %d/%m"))
    y_ticks <- axis(2, labels = FALSE)
    abline(v = x_ticks, h = y_ticks, lwd = 1, lty = 3, col = "lightgray")
    box()
  }
  
  if (light_level == TRUE) {
    graphics::par(new = TRUE)
    
    plot(x = snap[[{{ temporal }}]], y = snap$Light_Level, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", col = "lightgreen", ylim = c(0, 300))
    graphics::mtext("", side = 4, line = 3)
  }
  
  if (temperature == TRUE) {
    graphics::par(new = TRUE)
    
    plot(x = snap[[{{ temporal }}]], y = snap$Temperature, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", col = "firebrick1", ylim = tmp_range, xaxs = "i")
    axis(4)
    graphics::mtext("Temperature (\u00B0C)", side = 4, line = 3)
  }
}
