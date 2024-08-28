# Preferably run this file on a high performance compute cluster. Keep the results on the HPC for the next step.

library(batchtools)
library(data.table)
load("data_sensors_oc.RData")

reg = makeExperimentRegistry("FE", seed = 14202)

pickData = function(job, data, experiment, scenarioID, experimentID) {
  index = which(data$scenario == scenarioID & data$experiment == experimentID)
  data$data[[index]]
}

addProblem("MMS", data = data_mms_oc, fun = pickData)
addProblem("MSR", data = data_msr_oc, fun = pickData)
addProblem("F0", data = data_f0_oc, fun = pickData)
addProblem("F1", data = data_f1_oc, fun = pickData)
addProblem("K5", data = data_k5_oc, fun = pickData)


addAlgorithm("FE", fun = function(job, data, instance, max_lag, cols) {
  generate_features = function(data, cols, max_lag) {
    d = data[, c("time", "class", "transportation", "container", "No.loading", cols), with = FALSE]

    features = lapply(cols, function(col) {
      quants = (0:20) / 20
      n.quants = length(quants)
      n.quants2 = (length(quants) - 1) / 2

      q.abs = matrix(0, nrow = nrow(d) - max_lag, ncol = n.quants)
      q.orig = matrix(0, nrow = nrow(d) - max_lag, ncol = n.quants)
      q.abs.diff = matrix(0, nrow = nrow(d) - max_lag, ncol = n.quants2)
      q.orig.diff = matrix(0, nrow = nrow(d) - max_lag, ncol = n.quants2)
      sds = matrix(0, nrow = nrow(d) - max_lag, ncol = 8)

      for (i in 1:nrow(q.abs)) {
        part = d[(i + 1):(i + max_lag), ][[col]]

        q.abs[i, ] = unname(quantile(abs(part), p = quants, type = 2))
        q.orig[i, ] = unname(quantile(part, p = quants, type = 2))

        lower =  unname(quantile(abs(part), p = quants[1:n.quants2], type = 2))
        upper =  unname(quantile(abs(part), p = quants[n.quants:(n.quants2 + 2)], type = 2))
        q.abs.diff[i, ] = upper - lower

        lower2 =  unname(quantile(part, p = quants[1:n.quants2], type = 2))
        upper2 =  unname(quantile(part, p = quants[n.quants:(n.quants2 + 2)], type = 2))
        q.orig.diff[i, ] = upper2 - lower2

        sds[i, 1] = sd(abs(part))
        sds[i, 2] = sd(part)
        sds[i, 3] = mad(abs(part))
        sds[i, 4] = mad(part)
        sds[i, 5] = mean(abs(abs(part) - mean(abs(part))))
        sds[i, 6] = mean(abs(part - mean(part)))
        sds[i, 7] = mean(abs(abs(part) - median(abs(part))))
        sds[i, 8] = mean(abs(part - median(part)))
      }

      colnames(q.abs) = paste0(col, ".qAbs.", quants * 100, "_", max_lag)
      colnames(q.orig) = paste0(col, ".qOrig.", quants * 100, "_", max_lag)
      colnames(q.abs.diff) = paste0(col, ".qAbsDiff.", quants[1:n.quants2] * 100, "_", max_lag)
      colnames(q.orig.diff) = paste0(col, ".qOrigDiff.", quants[1:n.quants2] * 100, "_", max_lag)
      sds.names.part = paste0(rep(c("Abs", "Orig"), 4),
        rep(c("SD", "MAD", "MDMean", "MDMedian"), each = 2))
      colnames(sds) = paste0(col, ".Deviation", sds.names.part, "_", max_lag)

      feats = cbind(q.orig, q.abs, q.orig.diff, q.abs.diff, sds)
      return(feats)
    })

    features_all = Reduce(cbind, features)
    exclude = 1:max_lag
    d2 = cbind(d[-exclude, ], features_all)
    return(d2)
  }

  data_tmp = instance[!instance$class %in% c("Error", "Synchronization", "None"), ]
  dat = generate_features(data_tmp, cols = cols, max_lag = max_lag)

  return(dat)
})



prob_design_fun = function(data, exclude = NULL) {
  des = data[, 1:2]
  if (!is.null(exclude)) {
    des = des[-exclude, ]
  }
  colnames(des) = paste0(colnames(des), "ID")
  return(des)
}


algo_design_fun = function(max_lags, cols) {
  des = data.table(
    max_lag = max_lags,
    cols = lapply(max_lags, function(i) cols)
  )
  return(des)
}

prob_design_MMS = prob_design_fun(data = data_mms_oc, exclude = 1)
prob_design_MSR = prob_design_fun(data = data_msr_oc)
prob_design_F0 = prob_design_fun(data = data_f0_oc)
prob_design_F1 = prob_design_fun(data = data_f1_oc)
prob_design_K5 = prob_design_fun(data = data_k5_oc)

algo_design_MMS = algo_design_fun(max_lags = 50,
  cols = c("MMS.Acc.x", "MMS.Acc.y", "MMS.Acc.z"))
algo_design_MSR = algo_design_fun(max_lags = 25,
  cols = c("MSR.Acc.x", "MSR.Acc.y", "MSR.Acc.z"))
algo_design_F0 = algo_design_fun(max_lags = 500,
  cols = c("F0.Acc.x", "F0.Acc.y", "F0.Acc.z"))
algo_design_F1 = algo_design_fun(max_lags = 500,
  cols = c("F1.Acc.x", "F1.Acc.y", "F1.Acc.z"))
algo_design_K5 = algo_design_fun(max_lags = 2500,
  cols = c("K5.Acc.x", "K5.Acc.y", "K5.Acc.z"))

addExperiments(prob.designs = list(MMS = prob_design_MMS), algo.designs = list(FE = algo_design_MMS))
addExperiments(prob.designs = list(MSR = prob_design_MSR), algo.designs = list(FE = algo_design_MSR))
addExperiments(prob.designs = list(F0 = prob_design_F0), algo.designs = list(FE = algo_design_F0))
addExperiments(prob.designs = list(F1 = prob_design_F1), algo.designs = list(FE = algo_design_F1))
addExperiments(prob.designs = list(K5 = prob_design_K5), algo.designs = list(FE = algo_design_K5))


saveResults = function() {
  done = findDone()
  pars = flatten(getJobPars(done))
  res = reduceResultsDataTable(pars)

  result = merge(pars, res, by = "job.id")
  result = result[, c("problem", "scenarioID", "experimentID", "max_lag", "result"), with = FALSE]
  colnames(result) = c("sensor", "scenario", "experiment", "max_lag", "data")

  rs = split(result, result$sensor)
  datas = lapply(rs, function(x) x[, c("scenario", "experiment", "data"), with = FALSE])
  save(datas, file = "datas_FE.RData")
  return(NULL)
}


# submitJobs()
# wait until all jobs have finished
# saveResults()
