analyse_fam_const <- function(x) {
  cue <- data.frame(id = 0,
                    AD = "neutral",
                    AR = "neutral",
                    XD = "neutral",
                    XR = "neutral")

  if (x["phaenotype_father"] == 1 &&
      x["phaenotype_mother"] == 1 &&
      x["phaenotype_child"] == 0 &&
      x["female_child"] == 1) {
    cue <- data.frame(id = 103,
                      AD = "confirmed",
                      AR = "excluded",
                      XD = "excluded",
                      XR = "excluded")

  } else if (x["phaenotype_father"] == 1 &&
             x["phaenotype_mother"] == 1 &&
             x["phaenotype_child"] == 0 &&
             x["female_child"] == 0) {
    cue <- data.frame(id = 104,
                      AD = "neutral",
                      AR = "excluded",
                      XD = "neutral",
                      XR = "excluded")

  } else if (x["phaenotype_father"] == 1 &&
             x["phaenotype_child"] == 0 &&
             x["female_child"] == 1) {
    cue <- data.frame(id = 106,
                      AD = "neutral",
                      AR = "neutral",
                      XD = "excluded",
                      XR = "neutral")

  } else if (x["phaenotype_mother"] == 1 &&
             x["phaenotype_child"] == 0 &&
             x["female_child"] == 0) {
    cue <- data.frame(id = 108,
                      AD = "neutral",
                      AR = "neutral",
                      XD = "neutral",
                      XR = "excluded")

  } else if (x["phaenotype_father"] == 0 &&
             x["phaenotype_mother"] == 0 &&
             x["phaenotype_child"] == 1 &&
             x["female_child"] == 1) {
    cue <- data.frame(id = 101,
                      AD = "excluded",
                      AR = "confirmed",
                      XD = "excluded",
                      XR = "excluded")

  } else if (x["phaenotype_father"] == 0 &&
             x["phaenotype_child"] == 1 &&
             x["female_child"] == 1) {
    cue <- data.frame(id = 107,
                      AD = "neutral",
                      AR = "neutral",
                      XD = "neutral",
                      XR = "excluded")

  } else if (x["phaenotype_father"] == 0 &&
             x["phaenotype_mother"] == 0 &&
             x["phaenotype_child"] == 1 &&
             x["female_child"] == 0) {
    cue <- data.frame(id = 102,
                      AD = "excluded",
                      AR = "neutral",
                      XD = "excluded",
                      XR = "neutral")

  } else if (x["phaenotype_mother"] == 0 &&
             x["phaenotype_child"] == 1 &&
             x["female_child"] == 0) {
    cue <- data.frame(id = 105,
                      AD = "neutral",
                      AR = "neutral",
                      XD = "excluded",
                      XR = "neutral")

  }

  cue
}
