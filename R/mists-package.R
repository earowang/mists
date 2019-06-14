#' mists: missingess of time series
#'
#' @aliases NULL mists-package
#' @import rlang
#' @import vctrs
#' @importFrom utils head tail
#' @importFrom stats median quantile
#' @importFrom tsibble is_tsibble key key_vars key_data key_rows index index_var
#' @importFrom tsibble interval_pull time_unit group_by_key measures units_since
#' @importFrom dplyr distinct mutate transmute new_grouped_df group_nest n
#' @importFrom dplyr arrange filter bind_rows bind_cols group_by ungroup
#' @importFrom dplyr right_join left_join anti_join inner_join semi_join
#' @importFrom dplyr select select_if
#' @importFrom dplyr summarise summarise_all count
#' @importFrom dplyr tibble as_tibble
#' @importFrom ggplot2 ggplot geom_segment geom_point geom_bar geom_rect aes
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous theme labs
#' @importFrom ggplot2 element_blank GeomSegment GeomPoint facet_wrap
#' @importFrom ggplot2 autoplot
"_PACKAGE"

#' @export
ggplot2::autoplot
