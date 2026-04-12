
name    <- "edges" 
version <- 23

source(here::here("source", "common.R"), echo = FALSE)

grow_polygon_1 <- NULL # hack to shut the lintr up
grow_polygon_2 <- NULL # hack to shut the lintr up
cpp_files <- c("polygon_01a.cpp", "polygon_02a.cpp")
for (cpp_file in cpp_files) Rcpp::sourceCpp(here::here("source", cpp_file))

art_generator <- function(seed) {
  
  set.seed(seed)
  output <- output_path(name, version, seed, "png")
  message("generating ", output)

  p_mix <- runif(1)
  grow_polygon <- function(...) {
    if (runif(1) < p_mix) return(grow_polygon_2(...))
    grow_polygon_1(...)
  }

  grow_polygon_l <- function(polygon, iterations, noise, seed = NULL) {
    if(!is.null(seed)) set.seed(seed)
    polygon <- grow_polygon(polygon, iterations, noise) |>
      tibble::as_tibble() |>
      dplyr::arrange(position) |>
      dplyr::select(x, y, seg_len)
    return(polygon)
  }

  vec_rot <- function(x) c(x[-1], x[1])

  reallocate <- function(shape, prob = .975, max_it = 5, noise = .05) {
    max_len <- quantile(shape$seg_len, prob)
    yy <- shape
    ii <- 0
    while(max(yy$seg_len) > max_len) {
      if(ii > max_it) break
      x0 <- yy |> dplyr::mutate(.tmp_id = dplyr::row_number() * 2)
      x1 <- x0 |> 
        dplyr::mutate(
          x = (x + vec_rot(x))/2 + rnorm(dplyr::n()) * noise,
          y = (y + vec_rot(y))/2 + rnorm(dplyr::n()) * noise,
          .tmp_id = .tmp_id + 1
        ) |> 
        dplyr::filter(seg_len  > max_len)

      yy <- dplyr::bind_rows(x0, x1) |> 
        dplyr::arrange(.tmp_id) |> 
        dplyr::mutate(seg_len = edge_length(x, y, vec_rot(x), vec_rot(y))) |> 
        dplyr::select(-.tmp_id)

      yy$seg_len[nrow(yy)] <- 0
      ii <- ii + 1
    }

    return(yy)
  }

  smooth_polygon_l <- function(polygon, smoothing = 25) {
    for (i in 1:smoothing) {
      polygon <- polygon |> 
        dplyr::mutate(
          x = (x + vec_rot(x))/2,
          y = (y + vec_rot(y))/2,
        )
    }
    polygon
  }

  grow_multipolygon_l <- function(base_shape, n, seed = NULL, ...) {
    if(!is.null(seed)) set.seed(seed)
    polygons <- list()
    for(i in 1:n) {
      polygons[[i]] <- grow_polygon_l(base_shape, ...) |>
        dplyr::mutate(id = i)
    }
    polygons <- dplyr::bind_rows(polygons)
    polygons
  }

  show_multipolygon <- function(polygon, fill, alpha = .75, ...) {
    ggplot2::ggplot(polygon, ggplot2::aes(x, y, group = id)) +
      ggplot2::geom_polygon(colour = NA, alpha = alpha, fill = fill, ...) + 
      ggplot2::coord_equal() + 
      ggplot2::theme_void()
  }

  edge_length <- function(x1, y1, x2, y2) {
    sqrt((x1 - x2)^2 + (y1 - y2)^2)
  }

  smudged_polygon <- function(seed, shape, noise1 = 0, noise2 = 2, noise3 = 0.5) {
    set.seed(seed)

    base <- shape |> 
      grow_polygon_l(
        iterations = 20, 
        noise = noise1
      )
    
    # define intermediate-base-shapes in clusters
    polygons <- list()
    ijk <- 0
    for(i in 1:1) {
      base_i <- base |> 
        grow_polygon_l(
          iterations = 100, 
          noise = noise2
        )
      
      for(j in 1:1) {

        base_j <- base_i |> 
          grow_polygon_l(
            iterations = 20, 
            noise = noise2
          )
        
        # grow n polygons per intermediate-base
        for(k in 1:10) {
          ijk <- ijk + 1

          polygons[[ijk]] <- base_j

          for (ll in 1:8) {

            polygons[[ijk]] <- polygons[[ijk]] |>
              grow_polygon_l(
                iterations = 400, 
                noise = noise3
              ) 
            
            if (runif(1) < .995) {
              polygons[[ijk]] <- polygons[[ijk]] |> 
              reallocate(prob = .99, noise = .05)
            }

          }

          polygons[[ijk]] <- polygons[[ijk]] |>
            reallocate(prob = .5, max_it = 20, noise = 0) |> 
            smooth_polygon_l() |> 
            dplyr::mutate(id = ijk)
        }
      }
    }

    # return as data frame
    dplyr::bind_rows(polygons)
  }

  expand_palette <- function(shades, to = 1024L) {
    (colorRampPalette(shades))(to)
  }

  append_palette <- function(shades, add, n = 1L) {
    c(shades, rep(add, n))
  }

  thicken_palette <- function(shades, n = 5L) {
    as.vector(t(replicate(n, shades)))
  }

  generate_palette <- function(seed, n) {
    set.seed(seed)
    base <- here::here("source", "palettes") |>
      fs::dir_ls() |> 
      purrr::map(~ readr::read_csv(., show_col_types = FALSE)) |> 
      dplyr::bind_rows() |> 
      dplyr::slice_sample(n = 1L) |> 
      unlist() |> 
      sample(size = n, replace = TRUE)
    base
  }

  generate_lgbtiq_palette <- function(seed, n) {

    # https://www.flagcolorcodes.com/flags/pride
    queers <- list(
      rainbow   = c("#e50000", "#ff8d00", "#ffee00", "#028121", "#004cff", "#770088"),
      trans     = c("#5BCEFA", "#F5A9B8", "#FFFFFF"),
      bisexual  = c("#D60270", "#9B4F96", "#0038A8"),
      gay_man   = c("#078D70", "#26CEAA", "#98E8C1", "#FFFFFF", "#7BADE2", "#5049CC", "#3D1A78"),
      pansexual = c("#FF218C", "#FFD800", "#21B1FF"),
      lesbian   = c("#D52D00", "#EF7627", "#FF9A56", "#FFFFFF", "#D162A4", "#B55690", "#A30262"), 
      aromantic = c("#3DA542", "#A7D379", "#FFFFFF", "#A9A9A9", "#000000"),
      asexual   = c("#000000", "#A3A3A3", "#FFFFFF", "#800080"),
      nonbinary = c("#FCF434", "#FFFFFF", "#9C59D1", "#2C2C2C"),
      bear      = c("#613704", "#D46300", "#FDDC62", "#FDE5B7", "#FFFFFF", "#545454", "#000000"),
      intersex  = c("#FFD800", "#7902AA")
    )

    shades <- unlist(sample(queers, 1))
    origin <- gsub("[0-9]$", "", names(shades[1]))
    ns <- length(shades)
    sample(shades, n, TRUE)
  }

  n_row <- 1
  n_col <- 32
  n <- n_row * n_col
  dat <- list()
  
  sides <- 4
  theta <- (0:sides) * pi * 2 / sides
  theta <- theta - pi / sides # choose rotation (pi/sides is "squares not diamonds")

  shape <- tibble::tibble(
    x = sin(theta) / cos(pi / sides) * .7,
    y = cos(theta) / cos(pi / sides) * 3,
    seg_len = edge_length(x, y, dplyr::lead(x), dplyr::lead(y))
  )
  shape$seg_len[sides + 1] <- 0

  #hex_shade <- generate_lgbtiq_palette(seed, n = n + 1)
  hex_shade <- generate_palette(seed, n = 3)
  if (runif(1) < .8) {
    hex_shade <- sample(c(hex_shade, "#000000", "#222222"), n + 1, TRUE) 
  } else if (runif(1) < .5) {
    hex_shade <- sample(c(hex_shade, "#ffffff", "#cccccc"), n + 1, TRUE) 
  }
  hex_seed <- sample(1:10000, n)
  if (runif(1) < .5) {
    hex_xloc <- seq(-5, 5, length.out = n)
  } else {
    hex_xloc <- 15 * (rbeta(n, 3, 3) - .5)
  }
  if (runif(1) < .75) {
    hex_yloc <- runif(n, -.1, .1)
  } else {
    hex_yloc <- runif(n, -.8, .8)
  }
  bg <- hex_shade[1]
  #hex_shade[1] <- bg

  i <- 0
  for(r in 1:n_row) {
    for(c in 1:n_col) {
        i <- i + 1
        dat[[i]] <- smudged_polygon(
          seed = hex_seed[i], 
          shape = shape,
          noise1 = 0, 
          noise2 = 1.1, 
          noise3 = .2
        ) |>
        dplyr::mutate(
          fill = hex_shade[i], 
          x = x + hex_xloc[i],
          y = y + hex_yloc[i],
          id = paste0("id", id, "hex", i, sep = "_")
        )
      }
  }
  
  dat <- dplyr::bind_rows(dat) |>
    dplyr::group_by(id) |> 
    dplyr::mutate(
      dilution = 1 / (max(x) - min(x)) / (max(y) - min(y)),
      dilution = dilution ^ .15
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(dilution = dilution / max(dilution))
  
  if (runif(1) < .5) dat$x <- -dat$x
  if (runif(1) < .5) dat$y <- -dat$y

  #print(dplyr::distinct(dat, id, fill, s, dilution))
  
  pic <- dat |> 
    ggplot2::ggplot(ggplot2::aes(
      x, y, 
      group = id, 
      fill = fill, 
      alpha = dilution)
    ) +
    ggplot2::geom_polygon(colour = NA, show.legend = FALSE) + 
    ggplot2::scale_fill_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::coord_equal(
      xlim = c(-6, 6), 
      ylim = c(-3, 3)
    ) + 
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_void()
  
  pixels_wide <- 4000
  pixels_high <- 2000

  ggplot2::ggsave(
    filename = output, 
    plot = pic,
    width = pixels_wide,
    height = pixels_high,
    units = "px",
    dpi = 300,
    bg = bg
  )  

}


default_seeds <- function(version) 0:99 + version * 100
seeds <- default_seeds(version)
for(s in seeds) art_generator(s)

