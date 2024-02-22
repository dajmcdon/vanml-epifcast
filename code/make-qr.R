qrdat <- function(text, ecl = c("L", "M", "Q", "H")) {
  x <- qrcode::qr_code(text, ecl)
  n <- nrow(x)
  s <- seq_len(n)
  tib <- tidyr::expand_grid(x = s, y = rev(s))
  tib$z <- c(x)
  tib
}