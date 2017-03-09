#' Make a single node node table
#'
#' @param .data a data frame with one row per (node, link)
#' @param .id id of a node
#' @param node name of the node variable
#' @param link name of the link variable
#'
#' @return a data frame with one row per node, node : ie one row per link
#' @export
#'
#' @examples
#' ## Toy data
#' toy_data <- tibble::tibble(
#' node = c("A", "B", "C", "A", "B"),
#' link = c("a", "a", "b", "b", "b")
#' )
#' make_single_node_node_table_(
#' .data = toy_data,
#' .id = "A",
#' node = "node",
#' link = "link"
#' )
#'
make_single_node_node_table_ <- function(.data, .id, node, link) {
  dplyr::semi_join(
    x = .data,
    y = dplyr::distinct_(
      .data = dplyr::filter_(
        .data = .data,
        .dots = list(
          lazyeval::interp(
            ~ var == .id,
            var = as.name(node)
          )
        )
      ),
      .dots = lazyeval::interp(
        ~ var,
        var = as.name(link))
    ),
    by = link
  ) %>%
    dplyr::mutate_(
      .dots = list(
        "node_A" = ~ .id
      )
    ) %>%
    dplyr::select_(
      .dots = list(
        ~ node_A,
        "node_B" = lazyeval::interp(~ var, var = as.name(node))
      )
    ) %>%
    dplyr::filter_(.dots = list(~ node_A != node_B))
}


#' Make node node table
#'
#' @param .data a data frame with one row per node, link
#' @param node name of the node variable
#' @param link name of the link variable
#'
#' @return a data frame with one row per link (node, node)
#' @export
#'
#' @examples
#'
#' toy_data <- tibble::tibble(
#' node = c("A", "B", "C", "A", "B"),
#' link = c("a", "a", "b", "b", "b")
#' )
#' make_node_node_table_(
#' .data = toy_data,
#' node = "node",
#' link = "link"
#' )
#'
make_node_node_table_ <- function(.data, node, link) {

  plyr::ldply(
    .data = dplyr::distinct_(
      .data = .data,
      .dots = lazyeval::interp(~ x, x = as.name(node))
    )[[1]],
    .fun = function(id) {
      make_single_node_node_table_(
        .data = .data,
        .id = id,
        node = node,
        link = link
      )
    }
  )
}

