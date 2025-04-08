#converted by ChatGPT to R from FractalLandscape.nb (Mathematica) by Henrique Pereira 
#revised by Lukas Griem

## generate fractal landscape
RandomFractal2D <- function(H, k, nIn = NULL) {
  n <- if (is.null(nIn)) ceiling(k / 2) else nIn
  
  grid <- expand.grid(i = 1:n, j = 1:n)
  mask <- !(grid$i == 1 & grid$j == 1)
  
  distances <- (grid$i - 1)^2 + (grid$j - 1)^2
  sd_vals <- (distances^(-0.5 * (1 + H)))[mask]
  
  c_real <- numeric(k^2)
  c_imag <- numeric(k^2)
  indices <- (grid$i - 1) * k + grid$j
  
  rnorm_vals <- rnorm(sum(mask), mean = 0, sd = sd_vals)
  runif_vals <- runif(sum(mask), min = 0, max = 2 * pi)
  
  c_real[indices[mask]] <- rnorm_vals * cos(runif_vals)
  c_imag[indices[mask]] <- rnorm_vals * sin(runif_vals)
  
  c <- matrix(c_real + 1i * c_imag, nrow = k, ncol = k)
  c <- c + Conj(t(apply(c, 2, rev))) 
  
  result <- fft(c, inverse = TRUE) / k^2
  Re(result)
}

ConvertCell <- function(x, hab) {
  findInterval(x, hab, all.inside = TRUE) + 1
}


ConvertToLandscape <- function(data, p) {
  data_flat <- as.vector(data)
  cp <- cumsum(p)
  breaks <- quantile(data_flat, probs = cp, na.rm = TRUE)
  breaks <- c(-Inf, breaks, Inf)  # Extend range to avoid NA values
  matrix(as.numeric(cut(data, breaks = breaks, labels = FALSE)), nrow = nrow(data))
}


generate_fractal_landscape <- function(pIn, H, k = 100, n = NULL) {
  fractal <- RandomFractal2D(H, 2 * k, n)[1:k, 1:k]
  p <- pIn
  ConvertToLandscape(fractal, p)
}

land <- generate_fractal_landscape (.1,.01, 160)
image(land)
