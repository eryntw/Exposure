calculate_wingload <- function(ws_cm,
                               wl_cm,
                               s1_cm,
                               mass_g) {
  # ellipse-triangle model
  wingarea <-
    1 / 3 * wl_cm * s1_cm * (pi + 1) + # inner segment
    (ws_cm - 2 * wl_cm) * s1_cm # outer segment
  
  # mass g -> kg, times gravity gives N; area cm^2 -> m^2
  wingload <- round((mass_g / 1000 * 9.81) / (wingarea / 10000), 2)
  
  return(wingload)
}