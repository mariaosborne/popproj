
get_approx_peak <- function(t, ft)
{
  dft <- center_diff(ft)
  peak <- stats::approx(x = dft, y = t, xout = 0)$y
  return(peak)
}
