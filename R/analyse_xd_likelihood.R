#' @importFrom stats fisher.test
analyse_xd_likelihood <- function(cues, inf_const) {
  inf_const_table <- table(cues[inf_const$phaenotype_father == 1 &
                                  inf_const$phaenotype_mother == 0,
                                c("female_child", "phaenotype_child")])
  if (! any(cues$AD %in% c("confirmed", "likely")) &&
      ! any(cues$XD %in% c("excluded", "unlikely")) &&
      # any(cues$AR %in% c("excluded", "unlikely")) &&
      # any(cues$XR %in% c("excluded", "unlikely")) &&
      identical(dim(inf_const_table), as.integer(c(2, 2)))) {
    ft <- stats::fisher.test(inf_const_table)
    ft$p.value
  } else {
    1
  }
}
