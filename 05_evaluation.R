# Run this file with the previously created .RData-files in your working directory.

library(ggplot2)
library(tidyverse)
library(data.table)
library(stringr)

if (!dir.exists("Plots")) dir.create("Plots")

#######################################################################
# plot time series

load("data_sensors_oc.RData")

plot_time_series = function(data, col, from = -Inf, to = Inf, title = "") {
  data = data[data$time >= from & data$time <= to, ]

  classes = c("Loading", "Unloading", "Driving.straight.", "Driving.curve.", "Lifting.raising.",
    "Lifting.lowering.", "Lifting.tilting.", "Lifting.and.Driving", "Standing", "Docking", "Undocking",
    "Forks..entering.or.leaving.the.pallet.", "Wrapping", "Wrapping.preparation.", "Rotation", "Error",
    "Synchronization", "None")
  colors = c("maroon1", "darkblue", "darkmagenta", "blue", "lawngreen",
    "turquoise", "saddlebrown", "grey60", "gold", "darkorange", "orchid1",
    "firebrick3", "forestgreen", "deepskyblue", "darkcyan", "red",
    "black", "burlywood"
  )

  d = gather(data, key = "variable", value = "value", intersect(col, colnames(data)))
  vars = intersect(colnames(d), c("time", "variable", "value", "class"))
  data_l = d[, vars]

  if ("class" %in% vars) {
    data_l$class = factor(data_l$class, levels = classes)

    gg = ggplot(data = data_l, mapping = aes(x = time, y = value, color = class)) +
      scale_color_manual(drop = FALSE, values = colors)
  } else {
    gg = ggplot(data = data_l, mapping = aes(x = time, y = value))
  }

  gg = gg +
    geom_path(aes(group = 1)) +
    theme_bw() +
    ggtitle(title)

  if (length(col) > 1) {
    gg = gg + facet_grid(variable ~ "sensor", scales = "free")
  } else {
    gg = gg + ylab(col)
  }


  return(gg)
}

all_data_plotter = function(data, cols) {
  for (i in 1:nrow(data)) {
    p = plot_time_series(data$data[[i]], col = cols,
      title = paste("Scenario", data$scenario[i], "Experiment", data$experiment[i]))
    print(p)
  }
  return(NULL)
}


pdf(file = "Plots/timeseries_MMS.pdf", width = 20, height = 10)
all_data_plotter(data_mms_oc, c("MMS.Acc.x", "MMS.Acc.y", "MMS.Acc.z"))
dev.off()

pdf(file = "Plots/timeseries_MSR.pdf", width = 20, height = 10)
all_data_plotter(data_msr_oc, c("MSR.Acc.x", "MSR.Acc.y", "MSR.Acc.z"))
dev.off()

pdf(file = "Plots/timeseries_F0.pdf", width = 20, height = 10)
all_data_plotter(data_f0_oc, c("F0.Acc.x", "F0.Acc.y", "F0.Acc.z"))
dev.off()

pdf(file = "Plots/timeseries_F1.pdf", width = 20, height = 10)
all_data_plotter(data_f1_oc, c("F1.Acc.x", "F1.Acc.y", "F1.Acc.z"))
dev.off()

pdf(file = "Plots/timeseries_K5.pdf", width = 20, height = 10)
all_data_plotter(data_k5_oc, c("K5.Acc.x", "K5.Acc.y", "K5.Acc.z"))
dev.off()


rm(list = ls())


#############################################################################
# evaluate random forest results

load("results_WGTL.RData")

# renaming for nice names in plots
levels_all = c("Driving (curve)", "Driving (straight)", "Lifting and Driving", "Lifting (lowering)", "Lifting (raising)", "Lifting (tilting)",
  "Standing", "Docking", "Forks entering or leaving the pallet", "Wrapping (preparation)", "Wrapping", "Rotation", "Loading")
levels_all_super = c("Driving", "Lifting", "Standing", "Wrapping", "Loading")

rename_classes = function(class) {
  class = as.character(class)
  class[class == "Driving.curve."] = "Driving (curve)"
  class[class == "Driving.straight."] = "Driving (straight)"
  class[class == "Lifting.and.Driving"] = "Lifting and Driving"
  class[class == "Lifting.lowering."] = "Lifting (lowering)"
  class[class == "Lifting.raising."] = "Lifting (raising)"
  class[class == "Lifting.tilting."] = "Lifting (tilting)"
  class[class == "Forks..entering.or.leaving.the.pallet."] = "Forks entering or leaving the pallet"
  class[class == "Wrapping.preparation."] = "Wrapping (preparation)"
  return(class)
}

rename_res_classes = function(res) {
  res$predicted = rename_classes(res$predicted)
  res$truth = rename_classes(res$truth)
  return(res)
}

result$result = lapply(result$result, rename_res_classes)


result$problem[result$problem == "MSR"] = "MSR 145"
result$problem[result$problem == "MMS"] = "MetaMotionS"
result$problem[result$problem == "F0"] = "MPU6000"
result$problem[result$problem == "F1"] = "LSM303D"
result$problem[result$problem == "K5"] = "Kistler"

result$problem = factor(result$problem,
  levels = c("MSR 145", "MetaMotionS", "MPU6000", "LSM303D", "Kistler"))



######################################################################################################

# confusion matrices on original classes
conf = lapply(1:nrow(result), function(i) {
  r = result$result[[i]]

  if (result$scenarioID[i] == 1) {
    levels_truth = c("Driving (curve)", "Driving (straight)", "Lifting and Driving", "Lifting (lowering)", "Lifting (raising)", "Lifting (tilting)",
      "Standing", "Docking", "Forks entering or leaving the pallet", "Wrapping (preparation)", "Rotation", "Wrapping")
    levels_predicted = c(levels_truth, "Loading")
  } else if (result$scenarioID[i] == 2) {
    levels_truth = c("Driving (curve)", "Driving (straight)", "Lifting (lowering)", "Lifting (raising)", "Standing", "Loading")
    levels_predicted = c(levels_truth, "Lifting and Driving", "Lifting (tilting)", "Docking",
      "Forks entering or leaving the pallet", "Wrapping (preparation)", "Rotation", "Wrapping")
  } else {
    levels_truth = c("Driving (curve)", "Driving (straight)", "Lifting and Driving", "Lifting (lowering)", "Lifting (raising)",
      "Standing", "Forks entering or leaving the pallet")
    levels_predicted = c(levels_truth, "Lifting (tilting)", "Docking", "Wrapping (preparation)", "Rotation", "Wrapping", "Loading")
  }

  truth = as.character(r$truth)
  truth = factor(truth, levels = levels_truth)
  predicted = as.character(r$predicted)
  predicted = factor(predicted, levels = levels_predicted)

  tab = table(truth, predicted, useNA = "ifany")
  stopifnot(all.equal(rownames(tab), colnames(tab)[1:nrow(tab)]))
  return(tab)
})


# confusion matrices on super classes
conf.super = lapply(1:nrow(result), function(i) {
  r = result$result[[i]]

  if (result$scenarioID[i] == 1) {
    levels_truth = c("Driving", "Lifting", "Standing", "Wrapping")
    levels_predicted = c(levels_truth, "Loading")
  } else if (result$scenarioID[i] == 2) {
    levels_truth = c("Driving", "Lifting", "Standing", "Loading")
    levels_predicted = c(levels_truth, "Wrapping")
  } else {
    levels_truth = c("Driving", "Lifting", "Standing")
    levels_predicted = c(levels_truth, "Wrapping", "Loading")
  }

  truth = as.character(r$truth.super)
  truth = factor(truth, levels = levels_truth)
  predicted = as.character(r$predicted.super)
  predicted = factor(predicted, levels = levels_predicted)

  tab = table(truth, predicted)
  stopifnot(all.equal(rownames(tab), colnames(tab)[1:nrow(tab)]))
  return(tab)
})


# F1 scores
f1 = function(tab) {
  recall = diag(tab / rowSums(tab))
  if (any(is.na(recall))) recall[is.na(recall)] = 0

  precision = diag(t(t(tab) / colSums(tab)))[1:nrow(tab)]
  if (any(is.na(precision))) precision[is.na(precision)] = 0

  f1 = 2  / (1 / precision + 1 / recall)
  return(f1)
}

f1scores = lapply(conf, f1)
f1scores.super = lapply(conf.super, f1)


# data format for ggplot
todt = function(x) {
  x2 = as.data.table(matrix(x, nrow = 1))
  colnames(x2) = names(x)
  return(x2)
}

f1dt.super = lapply(f1scores.super, todt)
f1data.super = Reduce(function(x, y) rbind(x, y, fill = TRUE), f1dt.super)

resultf1.super = cbind(result[, -c(1, 3, 6, 8)], f1data.super)
resultf1.super$scenarioID = paste("Scenario", resultf1.super$scenarioID)
resultf1l.super = pivot_longer(resultf1.super, cols = colnames(f1data.super))
resultf1l.super$name = factor(resultf1l.super$name, levels = levels_all_super)

resultf1.super_diff = resultf1.super[same_scenario == TRUE, ]
for (i in 1:nrow(resultf1.super_diff)) {
  false_part = resultf1.super[
    problem == resultf1.super_diff$problem[i] &
      scenarioID == resultf1.super_diff$scenarioID[i] &
      experimentID == resultf1.super_diff$experimentID[i] &
      same_scenario == FALSE
  ]
  if (nrow(false_part) == 1) {
    resultf1.super_diff[i, 5:9] = false_part[, 5:9]- resultf1.super_diff[i, 5:9]
  } else if (nrow(false_part) == 0) {
    resultf1.super_diff[i, 5:9] = NA
  } else {
    stop(paste0("Row ", i, " has ", nrow(false_part), "matches!"))
  }
}
resultf1l.super_diff = pivot_longer(resultf1.super_diff, cols = colnames(f1data.super))
resultf1l.super_diff$name = factor(resultf1l.super_diff$name, levels = levels_all_super)


# boxplots
gg.same.diff.super = ggplot(data = resultf1l.super_diff, mapping = aes(x = problem, y = value, color = problem)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_boxplot() +
  theme_bw() +
  facet_wrap("name", nrow = 1) +
  ylab("Difference in F1 score") +
  xlab("Sensor") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = "none")

ggsave(gg.same.diff.super, file = "Plots/same_diff_super.pdf", height = 4, width = 10)


gg.class.super = ggplot(data = resultf1l.super[resultf1l.super$same_scenario == FALSE, ],
  mapping = aes(x = problem, y = value, color = problem)) +
  geom_boxplot() +
  theme_bw() +
  facet_grid(name ~ scenarioID) +
  ylab("F1 score") +
  xlab("Sensor") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = "none")

ggsave(gg.class.super, file = "Plots/class_super.pdf", height = 8, width = 4)



# aggregate confusion matrices
# this requires a quadratic matrix structure
conf_combine_fun = function(colist) {
  n = sapply(colist, sum)
  w = (1 / n) / sum(1 / n)

  colist2 = lapply(seq_along(colist), function(i) {
    colist[[i]] * w[i]
  })

  res = Reduce("+", colist2)
  return(res)
}

pns = which(result$same_scenario == FALSE)
conf_sym = lapply(pns, function(i) {
  r = result$result[[i]]
  truth = as.character(r$truth)
  truth = factor(truth, levels = levels_all)
  predicted = as.character(r$predicted)
  predicted = factor(predicted, levels = levels_all)

  tab = table(truth, predicted, useNA = "ifany")
  stopifnot(all.equal(rownames(tab), colnames(tab)[1:nrow(tab)]))
  return(tab)
})

conf_sym_super = lapply(pns, function(i) {
  r = result$result[[i]]
  truth = as.character(r$truth.super)
  truth = factor(truth, levels = levels_all_super)
  predicted = as.character(r$predicted.super)
  predicted = factor(predicted, levels = levels_all_super)

  tab = table(truth, predicted, useNA = "ifany")
  stopifnot(all.equal(rownames(tab), colnames(tab)[1:nrow(tab)]))
  return(tab)
})


conf_all_combined = conf_combine_fun(conf_sym)
conf_all_rel_row = conf_all_combined / rowSums(conf_all_combined)
conf_all_rel_col =  t(t(conf_all_combined) / colSums(conf_all_combined))

conf_all_combined_super = conf_combine_fun(conf_sym_super)
conf_all_rel_row_super = conf_all_combined_super / rowSums(conf_all_combined_super)
conf_all_rel_col_super =  t(t(conf_all_combined_super) / colSums(conf_all_combined_super))


# plot function for confusion matrices
plot_mat = function(dat, color, perfm, title = "") {

  if (perfm == "predicted") {
    leg.lab = "proportion \nof predicted \nclass"
  } else {
    leg.lab = "proportion \nof true \nclass"
  }


  r_names = rownames(dat)
  c_names = colnames(dat)
  plt.data = as.data.frame(dat)
  colnames(plt.data) = c("variable", "measure", "value")

  gg = ggplot(plt.data, aes(measure, variable)) +
    geom_tile(aes(fill = value), colour = "white") +
    scale_fill_gradient(low = "white", high = color, limits = c(0, 1), name = leg.lab) +
    theme_grey() +
    labs(x = "predicted class", y = "true class") +
    scale_x_discrete(expand = c(0, 0), labels = c_names) +
    scale_y_discrete(expand = c(0, 0), labels = r_names) +
    theme(axis.ticks = element_blank()) +
    coord_equal(ratio = 1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_text(mapping = aes(color = abs(value) >= 0.5,
      label = sprintf("%.2f", round(value, 2))),
      size = 4) +
    scale_color_manual(guide = "none", values = c("black", "white"))

  if (nrow(dat) > 6) {
    gg = gg + theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 13),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12)
    )
  } else {
    gg = gg + theme(
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 11),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10)
    )
  }

  if (title != "") {
    gg = gg +
      ggtitle(title) +
      theme(title = element_text(size = 13))
  }

  print(gg)

  return(NULL)
}


pdf("Plots/confusion_matrices_true_all_super.pdf", height = 4, width = 5)
plot_mat(conf_all_rel_row_super, color = "darkred",
  perfm = "true")
dev.off()

pdf("Plots/confusion_matrices_predicted_all_super.pdf", height = 4, width = 5)
plot_mat(conf_all_rel_col_super, color = "darkblue",
  perfm = "predicted")
dev.off()


conf_all_rel_row_abbrev = conf_all_rel_row
conf_all_rel_col_abbrev = conf_all_rel_col

colnames(conf_all_rel_row_abbrev)[colnames(conf_all_rel_row_abbrev) == "Forks entering or leaving the pallet"] = "Forks enter/leave"
rownames(conf_all_rel_row_abbrev)[rownames(conf_all_rel_row_abbrev) == "Forks entering or leaving the pallet"] = "Forks enter/leave"
colnames(conf_all_rel_col_abbrev)[colnames(conf_all_rel_col_abbrev) == "Forks entering or leaving the pallet"] = "Forks enter/leave"
rownames(conf_all_rel_col_abbrev)[rownames(conf_all_rel_col_abbrev) == "Forks entering or leaving the pallet"] = "Forks enter/leave"

colnames(conf_all_rel_row_abbrev)[colnames(conf_all_rel_row_abbrev) == "Wrapping (preparation)"] = "Wrapping (prep.)"
rownames(conf_all_rel_row_abbrev)[rownames(conf_all_rel_row_abbrev) == "Wrapping (preparation)"] = "Wrapping (prep.)"
colnames(conf_all_rel_col_abbrev)[colnames(conf_all_rel_col_abbrev) == "Wrapping (preparation)"] = "Wrapping (prep.)"
rownames(conf_all_rel_col_abbrev)[rownames(conf_all_rel_col_abbrev) == "Wrapping (preparation)"] = "Wrapping (prep.)"


pdf("Plots/confusion_matrices_true_all_abbrev.pdf", height = 6.3, width = 7.5)
plot_mat(conf_all_rel_row_abbrev, color = "darkred",
  perfm = "true")
dev.off()

pdf("Plots/confusion_matrices_predicted_all_abbrev.pdf", height = 6.3, width = 7.5)
plot_mat(conf_all_rel_col_abbrev, color = "darkblue",
  perfm = "predicted")
dev.off()



############################################################################
# duration and proportion of classes

load("annotation.RData")

dls = split(data_labels, data_labels$scenario)

dur_class_fun = function(dl) {
  n.experiments = nrow(dl)

  durations = sapply(dl$data, function(d) {
    d2 = d[!class %in% c("None", "Error", "Synchronization")]
    max(d2$time)
  })
  sum.duration = round(sum(durations))

  classes = Reduce(c, lapply(dl$data, function(d) d$class))
  classes = classes[!classes %in% c("None", "Error", "Synchronization")]
  classes = rename_classes(classes)
  classes = factor(classes, levels = levels_all)
  tab = table(classes)
  tab.rel = round(tab / sum(tab) * 100)

  res = c(n = n.experiments, duration = sum.duration, tab.rel)
}

Reduce(rbind, c(lapply(dls, dur_class_fun), list(dur_class_fun(data_labels))))
