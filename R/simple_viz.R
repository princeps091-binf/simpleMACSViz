
#' A simple visualisation for MACS results
#'
#' @param bed_file BED file for peaks detected by MACS
#' @param track_file BDG file created by MACS
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_segment scale_x_continuous xlab ylab theme_classic
#' @return A ggplot object
#' @export
#'
#' @examples
simple_viz<-function(bed_file,track_file){

  MACS_track_tbl<- vroom::vroom(track_file,delim="\t",col_names = F)

  MACS_peak_tbl<-vroom::vroom(bed_file,col_names=F)

  max_coord<-as.integer(max(MACS_track_tbl$X3) +(1e6 - max(MACS_track_tbl$X3) %% 1e6))
  min_coord<-as.integer(min(MACS_track_tbl$X3) - (min(MACS_track_tbl$X3) %% 1e6))
  peak_band_position<-ceiling(log10(max(MACS_track_tbl$X4)))
  MACS_track_tbl %>%
    mutate(flog=log10(.data$X4 + 1)) %>%
    #  filter(f1 > 1.5e7 & f2 > 1.5e7 & f1 < 1.75e7 & f2 < 1.75e7) %>%
    ggplot(aes(x=.data$X2,xend=.data$X3,y=.data$flog,yend=.data$flog))+
    geom_segment(linewidth=5,color="red")+
    geom_segment(data=MACS_peak_tbl,
                 aes(x=.data$X2,xend=.data$X3,y=peak_band_position,yend=peak_band_position),
                 color="black",linewidth=5)+
    scale_x_continuous(breaks = seq(min_coord, max_coord, by=5e6),
                       limits=c(min_coord,max_coord),
                       labels = paste0(seq(min_coord, max_coord, by=5e6)/1e6,"Mb"))+
    xlab("chromosome coordinate")+
    ylab("ATAC")+
    theme_classic()

}
