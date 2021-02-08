#' Feature Rank plot Seurat
#'
#' Create feature rank plot from Seurat object
#'
#' @param object            Seurat object
#' @param feature           Meta.data column for ranking and x-axis (i.e. UMI count).
#' @param group.by          Meta.data column for grouping variable for comparison (i.e. titration).
#' @param split.by          Meta.data column for splitting plots
#' @param celltype_group    Meta.data column celltype for grouping cells (i.e. lineage).
#' @param cell_color_by     Meta.data column for coloring cells (i.e. celltype). Used for "barcode" plot.
#' @param slot              Slot that the value should be extracted from (default "counts")
#'
#' @return ggplot2 object or list of ggplot2 objects (if combine=FALSE)
#' @export
#' @importFrom Seurat FetchData

seurat_plot_feature_rank <- function(object,
                                     feature,
                                     group.by=NULL,
                                     split.by=NULL,
                                     celltype_group=NULL,
                                     cell_color_by=NULL,
                                     slot="counts",
                                     ...){

  if(!is.null(split.by)){
    split <-  Seurat::FetchData(object, split.by)[[1]]
  } else {
    split <- NULL
  }

  if(!is.null(cell_color_by)) cell_color_by <- Seurat::FetchData(object, cell_color_by)[[1]]
  if(!is.null(celltype_group)) celltype_group <- Seurat::FetchData(object, celltype_group)[[1]]
  if(!is.null(group.by)) group.by <- Seurat::FetchData(object, group.by)[[1]]

  return(plot_feature_rank(value=Seurat::FetchData(object, feature, slot=slot)[[1]],
                           group=group.by,
                           split=split,
                           celltype_group=celltype_group,
                           cell_color_by=cell_color_by,
                           ...))

}
