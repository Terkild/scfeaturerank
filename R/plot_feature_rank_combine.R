#' Combine Feature Rank plot
#'
#' Function for combining components of the feature rank plot.
#'
#' @param components    List of components (returned from feature_rank_plot() when combine=FALSE)
#' @param height_density Relative height of density plot
#' @param height_rank   Relative height of rank plot
#' @param width_rank    Relative width of rank plot
#' @param width_violin  Relative width of violin plot
#' @param hjust         Horizontal justification of title. Passed on to cowplot::plot_grid()
#' @param vjust         Vertical justification of title. Passed on to cowplot::plot_grid()
#' @param label_x       Label x-position. Passed on to cowplot::plot_grid()
#' @param ...           Passed on to cowplot::plot_grid()
#'
#' @export
#' @importFrom patchwork wrap_plots plot_annotation

plot_feature_rank_combine <- function(components,
                                      title=NA,
                                      subtitle=NULL,
                                      height_rank=8,
                                      height_density=2,
                                      height_barplot=1.6,
                                      width_rank=7.8,
                                      width_celltype=2.2,
                                      hjust=0.5,
                                      vjust=1,
                                      label_x=0.55,
                                      ...){

  labels <- NULL
  if(is.na(title)) title <- NULL

  patchwork::wrap_plots(components[["density"]],
                        patchwork::wrap_plots(components[["squares"]], components[["barplot"]],
                                              heights=c((height_density-height_barplot),height_barplot), ncol=1),
                        components[["rank"]],
                        components[["celltype"]],
                        ncol=2,
                        widths=c(width_rank,width_celltype),
                        heights=c(height_density, height_rank)) +
    patchwork::plot_annotation(title=title, subtitle=subtitle, theme=theme(plot.subtitle=element_text(hjust=0.5)))
}
