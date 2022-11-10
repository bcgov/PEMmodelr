#' Tile index.
#'
#' Creates a polygon tile index of a raster.  Attributes of this polygon layer can be used to define \emph{pseudo-tiles} or perhaps better named \emph{virtual-tiles}
#'
#' @param rFile  Character, location of the raster file to index
#' @param pxCount   Specify how many pixels the tile should be in both x and y directions (long. and lat.)
#' @keywords raster, tiles
#' @export
#' @examples
# Create an raster example
#' r1 <- raster::raster(ncol=200, nrow=200, xmn=547000, xmx=552000, ymn=5985000, ymx=5990000)
#' raster::crs(r1) <- "+init=EPSG:3157"
#' r1 <- raster::setValues(r1, seq(1:length(r1)))
#' raster::writeRaster(r1, "e:/tmp/tmp.tif", overwrite = TRUE)
#' # call function
#' tiles <- tile_index("e:/tmp/tmp.tif", pxCount = 50)
#' mapview::mapview(tiles, alpha = 0.5)
# 
#rFile = cov[4]
# pxCount = 500

 # GSIF requires a GDAL object
tile_index <- function(rFile, pxCount){
  r <- rgdal::GDALinfo(rFile)
  #r <- rgdal::readGDAL(rFile)
  dist <- pxCount * r[6] ## GSIF uses a distance for the tile ... not pixel count
  p_tiles <- GSIF::getSpatialTiles(r, block.x = dist, return.SpatialPolygons = TRUE)
                                   #overlap.percent = Buffer / pxCount *100) ## add later
  #p_tiles <- meteo::tiling("./Deception_AOI/1_map_inputs/covariates/5m/dah.tif", tilesize = pxCount, overlapping = 0)
  p_tiles <- sf::st_as_sf(p_tiles)
  p_tiles$id <- seq(1:nrow(p_tiles))
  t_tiles <- GSIF::getSpatialTiles(r, block.x = dist, return.SpatialPolygons = FALSE)
  t_tiles$id <- seq(1:nrow(t_tiles))

  p_tiles <- dplyr::left_join(p_tiles, t_tiles)
  return(p_tiles)
}

