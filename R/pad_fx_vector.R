
# Pads ASFR vector so that it is the same length as Lx vector

pad_fx_vector <- function(fx) {
  # age grps 12 - 55
  # we want ages 0- 110
  # pad 12 zeros at beginning for ages 0-11
  # pad 55 zeros for ages 56-110 at end
  fx_padded <- c(rep(0,12), fx, rep(0, 55))
  return(fx_padded)
}
