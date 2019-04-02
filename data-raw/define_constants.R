# Definitions of constants for internal use
ROOT3 <- sqrt(3)
ROOT3X3 <- 3 * sqrt(3)
ROOT3DIV3 <- sqrt(3) / 3
INV_4THROOT3 <- 3^(-0.25)

devtools::use_data(ROOT3, ROOT3DIV3, ROOT3X3, INV_4THROOT3, internal = TRUE, overwrite = TRUE)
