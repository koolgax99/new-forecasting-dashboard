by <- "site"
types <- c("FORECAST", "ANALYSIS", "OBS")
CI <- c(0.025, 0.975)
unit <- list(AbvGrndWood = "Mg/ha", LAI = "m2/m2", SoilMoistFrac = "", TotSoilCarb = "kg/m2")
style <- list(general_color = c("FORECAST" = "blue", "ANALYSIS" = "red", "OBS" = "black"),
             fill_color = c("FORECAST" = "yellow", "ANALYSIS" = "green", "OBS" = "grey"),
             title_color = "red")

time_points <- names(FORECAST)
site_ids <- attributes(FORECAST[[1]])$Site
var_names <- attributes(FORECAST[[1]])$dimnames[[2]]

diag_fix <- function(vector){
  if (length(vector)>1){
    return(diag(vector))
  }else if (length(vector)==1){
    return(vector)
  }
}


pft <- utils::read.csv("C:/Users/HP/Desktop/gsoc/NewForecastingDashboard/R/site_pft.csv")

DB <- data.frame()
obs.mean <- NULL
obs.cov <- NULL

for (id in sort(unique(site_ids))) {
  for (time_point in time_points) {
    for (var_name in sort(unique(var_names))) {
      for (type in types) {
        if(type == "OBS") {
          obs_mean <- obs.mean[[time_point]][[id]][[var_name]]
          if(length(obs_mean) == 0 | is.null(obs_mean)){
            next
          }else{
            obs_cov <- diag_fix(obs.cov[[time_point]][[id]])[which(var_name == names(obs.mean[[time_point]][[id]]))]
            MIN <- obs_mean - 1.96*sqrt(obs_cov)
            MAX <- obs_mean + 1.96*sqrt(obs_cov)
            MEAN <- obs_mean
          }
        } else {
          temp_Dat <- get(type)[[time_point]]
          site_ind <- which(id == site_ids)
          var_ind <- which(var_name == var_names)
          ind <- var_ind[which(var_ind %in% site_ind)]
          MEAN <- mean(temp_Dat[,ind])
          MIN <- stats::quantile(temp_Dat[,ind], CI[1])
          MAX <- stats::quantile(temp_Dat[,ind], CI[2])
        }
        if(MIN < 0) MIN <- 0
        DB <- rbind(DB, list(id = id, date = time_point, var_name = var_name, type = type, upper = MAX, lower = MIN, mean = MEAN))
      }
    }
  }
}
#if we plot by each site.
if(by == "site") {
  PDF_w <- 10
  PDF_h <- 8
  p <- list()
  for (site.id in sort(unique(site_ids))) {
    site_p <- list()
    for (var.name in sort(unique(var_names))) {
      site_p <- rlist::list.append(site_p, dplyr::filter(DB, id == site.id & var_name == var.name) %>%
                                     dplyr::select(-c(id, var_name)) %>%
                                     dplyr::mutate(date = lubridate::ymd(date)) %>%
                                     ggplot2::ggplot(ggplot2::aes(x=date)) +
                                     ggplot2::geom_ribbon(ggplot2::aes(x = .data$date, ymin = .data$lower, ymax = .data$upper, fill=.data$type), inherit.aes = FALSE, alpha = 0.5) +
                                     ggplot2::geom_line(ggplot2::aes(y=mean, color=type),lwd=0.5,linetype=2) +
                                     ggplot2::geom_point(ggplot2::aes(y=mean, color=type), size=1.5, alpha=0.75) +
                                     ggplot2::scale_fill_manual(values = style$fill_color) +
                                     ggplot2::scale_color_manual(values = style$general_color) +
                                     ggplot2::ylab(paste0(var.name, " (", unit[var.name], ")")))
    }
    p <- rlist::list.append(p, ggpubr::annotate_figure(ggpubr::ggarrange(plotlist = site_p, common.legend = TRUE),
                                                       top = ggpubr::text_grob(site.id, color = style$title_color, face = "bold", size = 14)))
  }
}
  #if we plot by each state variable
# } else if (by == "var") {
#   PDF_w <- 20
#   PDF_h <- 16
#   p <- list()
#   for (var.name in sort(unique(var_names))) {
#     var_p <- list()
#     for (site.id in sort(unique(site_ids))) {
#       var_p <- rlist::list.append(var_p, dplyr::filter(DB, id == site.id & var_name == var.name) %>%
#                                     dplyr::select(-c(id, var_name)) %>%
#                                     dplyr::mutate(date = lubridate::ymd(date)) %>%
#                                     ggplot2::ggplot(ggplot2::aes(x=date)) +
#                                     ggplot2::geom_ribbon(ggplot2::aes(x = .data$date, ymin = .data$lower, ymax = .data$upper, fill=.data$type), inherit.aes = FALSE, alpha = 0.5) +
#                                     ggplot2::geom_line(ggplot2::aes(y=mean, color=type),lwd=0.5,linetype=2) +
#                                     ggplot2::geom_point(ggplot2::aes(y=mean, color=type), size=1.5, alpha=0.75) +
#                                     ggplot2::scale_fill_manual(values = style$fill_color) +
#                                     ggplot2::scale_color_manual(values = style$general_color) +
#                                     ggplot2::ylab(paste0(var.name, " (", unit[var.name], ")")) +
#                                     ggplot2::ggtitle(site.id))
#     }
#     p <- rlist::list.append(p, ggpubr::annotate_figure(ggpubr::ggarrange(plotlist = var_p, common.legend = TRUE),
#                                                        top = ggpubr::text_grob(var.name, color = style$title_color, face = "bold", size = 14)))
#   }

  #if we plot by each (pft * state variable)
# } else if (by == "pft") {
#   if (!exists("pft")) {
#     PEcAn.logger::logger.info("Please provide the pdf path!")
#     return(0)
#   } else {
#     PDF_w <- 20
#     PDF_h <- 16
#     p <- list()
#     for (PFT in sort(unique(pft$pft))) {
#       site_id_pft <- pft$site[which(pft$pft == PFT)]
#       var_p <- list()
#       for (var.name in sort(unique(var_names))) {
#         site_p <- list()
#         for (site.id in sort(site_id_pft)) {
#           site_p <- rlist::list.append(site_p, dplyr::filter(DB, id == site.id & var_name == var.name) %>%
#                                          dplyr::select(-c(id, var_name)) %>%
#                                          dplyr::mutate(date = lubridate::ymd(date)) %>%
#                                          ggplot2::ggplot(ggplot2::aes(x=date)) +
#                                          ggplot2::geom_ribbon(ggplot2::aes(x = .data$date, ymin = .data$lower, ymax = .data$upper, fill=.data$type), inherit.aes = FALSE, alpha = 0.5) +
#                                          ggplot2::geom_line(ggplot2::aes(y=mean, color=type),lwd=0.5,linetype=2) +
#                                          ggplot2::geom_point(ggplot2::aes(y=mean, color=type), size=1.5, alpha=0.75) +
#                                          ggplot2::scale_fill_manual(values = style$fill_color) +
#                                          ggplot2::scale_color_manual(values = style$general_color) +
#                                          ggplot2::ylab(paste0(var.name, " (", unit[var.name], ")")) +
#                                          ggplot2::ggtitle(site.id))
#         }
#         var_p <- rlist::list.append(var_p, ggpubr::annotate_figure(ggpubr::ggarrange(plotlist = site_p, common.legend = TRUE),
#                                                                    top = ggpubr::text_grob(paste(PFT, var.name), color = style$title_color, face = "bold", size = 14)))
#       }
#       p <- rlist::list.append(p, var_p)
#     }
#   }
# }

print(p)

grDevices::pdf(file.path("C:/Users/HP/Desktop/gsoc/NewForecastingDashboard/output", paste0("SDA_", by, ".pdf")),width = PDF_w, height = PDF_h)
print(p)
grDevices::dev.off()

