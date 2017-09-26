# Get the HEX of a colour
col2hex <- function(col) {
  do.call(grDevices::rgb, as.list(grDevices::col2rgb(col) / 255))
}

# Get the most similar R colours to a given colour
closestColHex <- function(target, n = 3,
                          superset = grDevices::colours(distinct = TRUE)) {
  target <- as.numeric(grDevices::col2rgb(target))
  dist_mat <- grDevices::col2rgb(superset) - target
  dist <- apply(dist_mat, 2, function(x) sum(abs(x)))
  closest <- order(dist)[1:n]
  superset[closest]
}

# Determine if a colour is dark or light
isColDark <- function(colhex) {
  rgba <- hex2rgba(colhex)
  if (!is.null(rgba[['a']]) && rgba[['a']] < 0.5) {
    return(FALSE)
  }
  return(getLuminance(colhex) <= 0.22)
}

# Calculate the luminance of a colour
getLuminance <- function(colhex) {
  colrgb <- grDevices::col2rgb(colhex)
  lum <- lapply(colrgb, function(x) {
    x <- x / 255;
    if (x <= 0.03928) {
      x <- x / 12.92
    } else {
      x <- ((x + 0.055) / 1.055)^2.4
    }
  })
  lum <- lum[[1]]*0.2326 + lum[[2]]*0.6952 + lum[[3]]*0.0722;
  lum
}

createColsMap <- function() {
  allcols <- grDevices::colours(distinct = TRUE)
  rgbVals <- as.vector(grDevices::col2rgb(allcols)) / 255
  reds <- rgbVals[seq(from = 1, to = length(rgbVals), by = 3)]
  greens <- rgbVals[seq(from = 2, to = length(rgbVals), by = 3)]
  blues <- rgbVals[seq(from = 3, to = length(rgbVals), by = 3)]
  hex <- grDevices::rgb(reds, greens, blues)
  colsMap <- allcols
  names(colsMap) <- hex
  colsMap
}

getColNameOrHex <- function(hex) {
  if (is.null(.globals$colsMap)) {
    .globals$colsMap <- createColsMap()
  }

  if (hex %in% names(.globals$colsMap)) {
    unname(.globals$colsMap[hex])
  } else {
    hex
  }
}

# Return a list of numeric rgb (or rgba) values for a HEX code
# Assumes a 6 or 8 digit HEx code with leading # symbol
hex2rgba <- function(hex) {
  # If a colour name instead of hex is given
  if (substring(hex, 1, 1) != "#") {
    rgb <- grDevices::col2rgb(hex)
    return(
      list(
        r = rgb[1],
        g = rgb[2],
        b = rgb[3]
      )
    )
  }
  hex <- substring(hex, 2)

  result <- list(
    r = as.integer(as.hexmode(substring(hex, 1, 2))),
    g = as.integer(as.hexmode(substring(hex, 3, 4))),
    b = as.integer(as.hexmode(substring(hex, 5, 6)))
  )
  if (nchar(hex) == 8) {
    a <- as.integer(as.hexmode(substring(hex, 7, 8)))
    if (a < 255) {
      result[['a']] <- a / 255
    }
  }
  result
}

# Return a CSS-like rgb() or rgba() value for a HEX code
# Assumes a 6 or 8 digit HEx code with leading # symbol
hex2rgba_str <- function(hex) {
  rgba <- hex2rgba(hex)
  if (is.null(rgba[['a']]) || rgba[['a']] == 255) {
    return(sprintf("rgb(%s, %s, %s)", rgba[['r']], rgba[['g']], rgba[['b']]))
  } else {
    return(sprintf("rgba(%s, %s, %s, %s)", rgba[['r']], rgba[['g']], rgba[['b']], rgba[['a']]))
  }
}
