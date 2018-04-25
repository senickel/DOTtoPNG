#' DOT_calc_height
#'
#' Calculate the height of the graph.
#' @param width Custom width.
#' @param correction Pixel correction to get the exact pixel size.
#' @param wh List with two elements, "width" and "height".
#' @author Sebastian Nickel
#' @importFrom magrittr %>%
#' @export

DOT_calc_height<-function(width,correction=1.25,wh) {
  round(wh["height"]/wh["width"]*(width/correction)) %>%
    as.numeric
}
