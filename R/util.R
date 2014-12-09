## Add a label to a plot at a fixed relative location.
label <- function(px, py, lab, ..., adj=c(0, 1)) {
  lab <- LETTERS[lab]
  usr <- par("usr")
  x <- usr[1] + px*(usr[2] - usr[1])
  y <- usr[3] + py*(usr[4] - usr[3])
  if (par("xlog")) x <- 10^x
  if (par("ylog")) y <- 10^y
  text(x, y, lab, adj=adj, ...)
}

## Draw the outline of a histogram
hist_outline <- function(h, ..., density=TRUE) {
  xy <- hist_xy(h, density)
  lines(xy, ...)
}
hist_fill <- function(h, ..., density=TRUE) {
  xy <- hist_xy(h, density)
  polygon(xy, ...)
}

hist_xy <- function(h, density=TRUE) {
  dx <- diff(h$mids[1:2])
  xx <- rep(with(h, c(mids - dx/2, mids[length(mids)] + 
                      dx/2)), each = 2)
  yy <- c(0, rep(if (density) h$density else h$counts, each = 2), 0)
  list(x=xx, y=yy)
}

mix <- function(cols, col2, p) {
  m <- col2rgb(cols)
  m2 <- col2rgb(rep(col2, length=length(cols)))
  m3 <- (m * p + m2 * (1-p))/255
  rgb(m3[1,], m3[2,], m3[3,])
}

log_seq_range <- function(x, n) {
  exp(seq(log(min(x)), log(max(x)), length=n))
}

add.alpha <- function (col, alpha=0.5) {
  if (length(alpha) > 1 && any(is.na(alpha))) {
    n <- max(length(col), length(alpha))
    alpha <- rep(alpha, length.out = n)
    col <- rep(col, length.out = n)
    ok <- !is.na(alpha)
    ret <- rep(NA, length(col))
    ret[ok] <- add.alpha(col[ok], alpha[ok])
    ret
  } else {
    tmp <- col2rgb(col)/255
    rgb(tmp[1, ], tmp[2, ], tmp[3, ], alpha = alpha)
  }
}

render <- function(input, output) {
  opts <- setdiff(markdownHTMLOptions(TRUE), 'base64_images')
  markdownToHTML(input, output, options=opts,
                 stylesheet='stylesheet.css',
                 template='.template.html')
}

download_file <- function(..., quiet=TRUE) {
  oo <- options(warn=2)
  on.exit(oo)
  download(..., quiet=quiet)
}
