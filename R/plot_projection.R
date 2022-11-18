#' Plot Projection
#'
#' Plot the results of running one of the projection methods. By default
#' will plot population, births, NRR, and life expectancy at birth over
#' time in a multi-panel plot
#'
#' @param projection_object output from a projection method
#' @param include which plots to include, not yet implemented
#' @param plot_predicted_peaks calculate and mark the times of peak
#' population and peak births
#' @param plot_generational_slopes plot line segments showing average change in
#' population and births for one generation
#' @param generation_length length of one generation in years, defaults to 30
#' @param generation_t time at which to plot generational slope. Slope will span
#' between (generation_t - generation_length, generation_t)
#' @param plot_replacement_nrr mark replacement fertility (NRR=1) on the NRR plot
#'
#'
#' @export


plot_projection <-  function(projection_object,
                             include = c('population', 'births', 'nrr', 'e0'),
                             plot_predicted_peaks = FALSE,
                             plot_generational_slopes = FALSE,
                             generation_length = 30,
                             generation_t = 50,
                             plot_replacement_nrr = TRUE) {

  # place everything into a data.frame
  pop_vec <- colSums(projection_object$K_mat)
  t = projection_object$time_vec
  proj <- data.frame(time = t,
                     Nt = pop_vec,
                     Bt = projection_object$births_vec,
                     nrr_t = projection_object$nrr_vec,
                     e0_t = projection_object$e0_vec)


  # predicted peaks
  tN.exact <- NA
  tB.exact <- NA
  if(plot_predicted_peaks) {
    tN.int <- t[which.max(pop_vec)]
    tN.exact <- get_approx_peak(t=t, ft=pop_vec)
    tB.int <- t[which.max(projection_object$births_vec)]
    tB.exact <- get_approx_peak(t=t, ft=projection_object$births_vec)
    #print(tB.exact)
  }

  # build basic plots
  nrr_plot <- proj %>% ggplot2::ggplot(aes(x=time, y=nrr_t)) +
    ggplot2::geom_line(color = 'red', size = 0.9) +
    cowplot::theme_half_open() +
    cowplot::background_grid(color.major = "grey90", color.minor = "grey90") +
    ggplot2::labs(title = 'Net Reproduction Rate', x = 'time', y= NULL)

  e0_plot <- proj %>% ggplot2::ggplot(aes(x=time, y=e0_t)) +
    ggplot2::geom_line(color = 'black', size = 0.9) +
    cowplot::theme_half_open() +
    cowplot::background_grid() +
    ggplot2::labs(title = 'e(0)', x = 'time', y= NULL)

  population_plot <- proj %>% ggplot2::ggplot(aes(x=time, y=Nt)) +
    geom_line(color = 'cornflowerblue', size = 0.9) +
    cowplot::theme_half_open() +
    cowplot::background_grid() +
    ggplot2::labs(title = 'Population', x = 'time', y= NULL)

  births_plot <- proj %>% ggplot2::ggplot(aes(x=time, y=Bt)) +
    geom_line(color = 'forestgreen', size = 0.9) +
    cowplot::theme_half_open() +
    cowplot::background_grid() +
    ggplot2::labs(title = 'Births', x = 'time', y= NULL)

  # add peaks
  if(plot_predicted_peaks & !is.na(tN.exact) & !is.na(tB.exact)) {
    # population
    pop_peak <- ggplot2::geom_segment(aes(x=tN.exact,y=0, xend = tN.exact, yend= max(pop_vec)),
                             size = 0.1, linetype = "dashed", color = "gray25")

    population_plot <- population_plot + pop_peak +
      ggplot2::annotate("text", label = round(tN.exact, 1), tN.exact, max(pop_vec)/2,
               size = 3.5, color = "black", hjust=1.2)

    # Births
    births_peak <- ggplot2::geom_segment(aes(x=tB.exact, y=0, xend = tB.exact,
                                yend= max(projection_object$births_vec)),
                                size = 0.1, linetype = "dashed", color = "gray25")

    births_plot <- births_plot + births_peak +
      ggplot2::annotate("text", label = round(tB.exact, 1), tB.exact, max(projection_object$births_vec)/2,
               size = 3.5, color = "black", hjust=1.2)
  }

  # generational slope between time (t, t-g)
  if(plot_generational_slopes) {
    # make sure range is ok.
    start_time <- generation_t - generation_length
    end_time <- generation_t
    population_plot <- population_plot +
      ggplot2::geom_segment(aes(x=start_time, y = proj[proj$time == start_time, ]$Nt,
                       xend = end_time,yend = proj[proj$time == end_time, ]$Nt)) +
      ggplot2::annotate("text", label = paste0('t-', generation_length),
                        x=start_time, y = proj[proj$time == start_time, ]$Nt,
               vjust = 1.5) +
      ggplot2::annotate("text", label = 't', x=end_time, y = proj[proj$time == end_time, ]$Nt,
               vjust = 1.5) +
      ggplot2::geom_point(aes(x=start_time, y = proj[proj$time == start_time, ]$Nt)) +
      ggplot2::geom_point(aes(x=end_time, y = proj[proj$time == end_time, ]$Nt))

    births_plot <- births_plot +
      ggplot2::geom_segment(aes(x=start_time, y = proj[proj$time == start_time, ]$Bt,
                       xend = end_time,yend = proj[proj$time == end_time, ]$Bt)) +
      ggplot2::annotate("text", label = paste0('t-', generation_length), x=start_time,
               y = proj[proj$time == start_time, ]$Bt, vjust = 1.5) +
      ggplot2::annotate("text", label = 't', x=end_time, y = proj[proj$time == end_time, ]$Bt,
               vjust = 1.5) +
      ggplot2::geom_point(aes(x=start_time, y = proj[proj$time == start_time, ]$Bt)) +
      ggplot2::geom_point(aes(x=end_time, y = proj[proj$time == end_time, ]$Bt))
  }

  if(plot_replacement_nrr) {
    #nrr_line <- geom_hline(yintercept = 1, color = "gray40", size = 0.5)
    nrr_plot <- nrr_plot + ggplot2::geom_hline(yintercept = 1, color = "gray30", size = 0.5) +
      ggplot2::annotate("text", label = "replacement", x = min(t)+10, y = 1.2, color = "gray30",
               size = 3)
  }


  return(nrr_plot + e0_plot + births_plot + population_plot + patchwork::plot_layout(ncol = 2))
}



