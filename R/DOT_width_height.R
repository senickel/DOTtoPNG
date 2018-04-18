#' DOT_width_height
#'
#' Get the width and height of the graph.
#' @param grV_obj XML of a DOT graph.
#' @author Sebastian Nickel
#' @importFrom magrittr %>%
#'

DOT_width_height<-function(grV_obj) {
  width_height<-grV_obj %>%
    strsplit("<svg") %>%
    get_element_from_list(.,2) %>%
    strsplit("\n") %>%
    get_element_from_list(.,1) %>%
    gsub("\"","",.) %>%
    strsplit(.," ") %>%
    unlist %>%
    gsub("pt","",.)
  width_height<-width_height[width_height!=""] %>%
    strsplit("=") %>%
    do.call(rbind,.)

  w_h<-width_height[,2] %>%
    unlist %>%
    as.numeric
  names(w_h)<-width_height[,1]
  w_h
}


