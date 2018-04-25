#' writeDOTtoPNG
#'
#' Write DOT graph to PNG
#' @param graphvizobj A DOT graph.
#' @param path File path to PNG file.
#' @param width Optional custom width. Height is set relationally.
#' @author Sebastian Nickel
#' @importFrom magrittr %>%
#' @export

writeDOTtoPNG<-function(graphvizobj,
                        path,
                        width=NULL) {

  # Checks if graphvizobj is a DOT graph
  obj<-tryCatch(DiagrammeR::grViz(graphvizobj),
                error=function(x) {stop("Object is not a DOT graph.")})
  exp_g1<-tryCatch(DiagrammeRsvg::export_svg(obj),
                   error=function(x) {stop("Object is not a DOT graph.")})
  # Checks width
  stopifnot(is.numeric(width)|is.null(width))

  # Get the height and width of the graph
  wh<-DOT_width_height(exp_g1)


  if (!is.null(width)) {
    new_height<-DOT_calc_height(width,1.25,wh)
  } else {
    new_height<-DOT_calc_height(wh["width"],1.25,wh)
    width<-wh["width"]
  }


  p1<-exp_g1 %>%
    strsplit("<svg") %>%
    get_element_from_list(.,1) %>%
    paste0("<svg")

  p2<-paste0(" width=\"",round(width/1.25),"pt\" height=\"",new_height,"pt\"")

  p3<-exp_g1 %>%
    strsplit("<svg") %>%
    get_element_from_list(.,2) %>%
    strsplit("\n viewBox") %>%
    get_element_from_list(.,2) %>%
    paste0("\n viewBox",.)


  exp_g2<-paste0(p1,p2,p3)


  exp_g2 %>%
    charToRaw %>%
    rsvg::rsvg() %>%
    png::writePNG(path)

}
