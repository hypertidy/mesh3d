## silicate has something like this, just want x_, y_, z_ = 0 as necessary
canhasz <- function(xyz, xn = c("x_", "y_", "z_") ) {
  if (dim(xyz)[2] == 2){
    xyz[["z_"]] <- 0
 }
  if (dim(xyz)[2] > 3) {
    if (all(xn %in% names(xyz))) {
       xyz <- xyz[xn]
      } else {
      xyz <- xyz[1:3]
      }
  }
  xyz
}

#' Convert to mesh object
#'
#' Converting silicate forms to segment types.
#'
#' @param x
#'
#' @param ...
#' @param points_only shortcut primitives and just give points
#'
#' @importFrom rgl as.mesh3d
#' @export as.mesh3d
#' @importFrom anglr as.mesh3d
#' @export
#' @name as.mesh3d
#' @examples
#' x <-   silicate::SC0(minimal_mesh)
as.mesh3d.SC0 <- function(x, type = "segments", ...) {
  xyz <- canhasz(silicate::sc_vertex(x))
  segs <- do.call(rbind, x$object$topology_)[c(".vx0", ".vx1")]
  if (type == "quads") stop("type = 'quads' not available for segment models SC0/SC")
  if (type == "triangles") stop("type = 'triangles' not available for segment models SC0/SC, \ntry PATH0/PATH instead, or anglr::DEL0 which can convert SC0/SC to TRI :")
      out <- rgl::mesh3d(xyz,
                         segments = if (type == "segments") t(as.matrix(segs)),
                         points = if (type == "points") c(segs[[".vx0"]], segs[[".vx1"]][dim(segs)[1L]]),
                         ...)
  out
}

#' @export
#' @name as.mesh3d
as.mesh3d.SC <- function(x, ...) {
  as.mesh3d(silicate::SC0(x), ...)
}



# mesh3d is the universal, and can do everything
# qmesh3d is quads
# tmesh3d is triangles
# smesh3d is segments (needed)
# pmesh3d is points (needed)

## then, as.mesh3d could
