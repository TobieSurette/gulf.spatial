#' Coordinates and Grids
#'
#' @description Functions to assign a grid label(s) given coordinates, and vice versa.
#'
#' @param label Logical value specifying whether to display label inside the grids when using \code{plot.grid}.
#' @param col Background colour when plotting a grid using \code{plot.grid}.
#' @param border Border line colour when plotting a grid using \code{plot.grid}.
#' @param vertices Logical value specifiying whether to output the corner coordinates of each grid.
#'
#' @examples
#' # Generate set of snow crab 10'x10' grids:
#' x <- expand.grid(seq(-66.5 + 1/12, -60 - 1/12, by  = 1/6),
#'                  seq(45.5-1/12, 49 + 1/12, by  = 1/6))
#' x$grid <- deg2grid(x[,1], x[,2])
#' x <- grid2deg(x$grid, vertices = TRUE) # Grid corner coordinates.
#' plot(c(-66.5, -60), c(45, 49), type = "n")
#' coast()
#' plot.grid(x$grid)
#'
#' @seealso \code{\link{deg2grid}}

#' @export plot.grid
plot.grid <- function(x, label = FALSE, col = "white", border = "black", ...){
   # Get grid coordinates:
   grid <- grid2deg(x,  ...)

   # Draw grid:
   rect(grid$left, grid$bottom, grid$right, grid$top, col = col, border = border, ...)

   # Print label:
   if (label) text((grid$left + grids$right) / 2, (grid$bottom + grids$top) / 2, x, ...)
}
