#' Feature Rank plot
#'
#' @param value             Value used for ranking and x-axis (i.e. UMI count).
#' @param group             Grouping variable for comparison (i.e. titration).
#' @param group_names       Rename group levels (i.e. can be used to append concentration or other info that is marker specific). Named vector (or list of such if split is set).
#' @param split             Splitting variable (generates individual plot for each group)
#' @param celltype_group    Celltype for grouping cells (i.e. lineage).
#' @param cell_color_by     Variable for coloring cells (i.e. celltype). Used for "barcode" plot.
#' @param threshold_y       If not NULL, a threshold line will be drawn at a the set rank. If a single value, same threshold is set for all splits. When splitting, multiple values can be given in a vector (named by split)
#' @param combine           Should plot components be combined (default). If FALSE, a list of plot components will be returned.
#' @param nrow              Number of rows to use when wrapping
#' @param title             If not NA, title will be set. If a single value title only first panel will get title. When splitting, multiple values can be given in a vector (named by split)
#' @param remove_ylabel     Should y-labels for all but the first panel be removed?
#' @param remove_ylabel_width Decrease relative width of non-first plots by this factor to account for removed y-labels in others
#' @param ...               Passed on to plot_feature_rank_single().
#'
#' @return ggplot2 object or list of ggplot2 objects (if combine=FALSE)
#' @export

plot_feature_rank <- function(value, group=NULL, group_names=NA, split=NULL, celltype_group=NULL, cell_color_by=NULL, combine=TRUE, nrow=1, threshold_y=NA, title=NA, remove_ylabel=FALSE, remove_ylabel_width=0.92, labels=c(), widths=NULL, ...){
  plotData <- data.frame(value=value)

  if(!is.null(group)) plotData$group=group
  if(!is.null(celltype_group)) plotData$celltype_group=celltype_group
  if(!is.null(cell_color_by)) plotData$cell_color_by=cell_color_by

  split <- as.factor(split)

  if(length(split) == length(value)){
    data_list <- plotData %>% mutate(split=as.factor(split)) %>%
      group_by(split) %>% group_split() %>%
      setNames(levels(split))

    if(is.list(labels)){
      labels_list <- labels
    } else {
      labels_list <- lapply(seq_along(data_list), function(x) labels)
    }
    widths <- rep(1, length(data_list))

    if(remove_ylabel == TRUE){
      labels_list[-1] <- lapply(labels_list[-1], function(x) c(c("y.rank"=NA, "y.density"=NA),labels))

      widths[-1] <- sapply(widths[1], function(x) remove_ylabel_width)
    }

    ## Check if different thresholds are needed for each split
    if(length(threshold_y) != length(data_list)){
      threshold_y <- rep(threshold_y, length(data_list))
    }
    if(is.null(names(threshold_y))) names(threshold_y) <- names(data_list)

    ## If only a single title is set, only use it for first panel
    if(length(title) != length(data_list)){
      title_empty <- rep("", length(data_list))
      title_empty[1] <- title
      title <- title_empty
    }
    if(is.null(names(title))) names(title) <- names(data_list)

    if(!is.list(group_names)){
      group_names <- lapply(seq_along(data_list), function(x) group_names)
    }
    if(is.null(names(group_names))) names(group_names) <- names(data_list)

    plot_list <- lapply(names(data_list),
                        function(x) with(data_list[[x]],
                                         plot_feature_rank_single(value=value,
                                                                  group=group,
                                                                  group_names=group_names[[x]],
                                                                  celltype_group=celltype_group,
                                                                  cell_color_by=cell_color_by,
                                                                  subtitle=x,
                                                                  threshold_y=threshold_y[x],
                                                                  combine=combine,
                                                                  title=title[x],
                                                                  labels=labels_list[[x]],
                                                                  ...)))

    if(combine == TRUE){
      plot <- cowplot::plot_grid(plotlist=plot_list, align="h", axis="tb", nrow=nrow, rel_widths=widths)
    } else {
      plot <- plot_list
    }

  } else {
    plot <- plot_feature_rank_single(value=value,
                                     group=group,
                                     group_names=group_names,
                                     celltype_group=celltype_group,
                                     cell_color_by=cell_color_by,
                                     threshold_y=threshold_y,
                                     combine=combine,
                                     title=title,
                                     labels=labels,
                                     ...)
  }

  return(plot)
}
