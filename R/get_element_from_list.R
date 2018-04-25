#' get_element_from_list
#'
#' Get specific element from a list.
#' @param list A list.
#' @param el Which element from the list should be returned
#' @author Sebastian Nickel
#' @importFrom magrittr %>%
#' @export
get_element_from_list<-function(list,el) {
  lapply(list,function(x) x[el]) %>%
    unlist
}
