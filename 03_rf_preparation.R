# Preferably run this file on a high performance compute cluster.
# Create a folder "Data" in the working directory of your HPC jobs.
# Keep the results on the HPC for the next step.

library(batchtools)
library(data.table)

load("datas_FE.RData")
load("data_sensors_oc.RData")

reg = makeExperimentRegistry("RF_prep", seed = 4258)

test1 = function(job, data, scenarioID, experimentID, same_scenario, nmax) {
  if (same_scenario) {
    indices = which(data$scenario == scenarioID)
  } else {
    indices = 1:nrow(data)
  }

  test_ind = which(data$scenario == scenarioID & data$experiment == experimentID)
  train_inds = setdiff(indices, test_ind)

  test = data$data[[test_ind]]

  train_list = data$data[train_inds]
  train = Reduce(rbind, train_list)
  train_s = split(train, train$class)

  train_s2 = lapply(train_s, function(d) {
    if (nrow(d) <= nmax) {
      return(d)
    } else {
      inds = sample(nrow(d), nmax)
      return(d[inds, ])
    }
  })

  train2 = Reduce(rbind, train_s2)
  train3 = train2[sample(nrow(train2)), ]

  inst = list(train = train3, test = test)

  path = paste0("Data/", job$prob.name, "_scenario", scenarioID, "_experiment", experimentID,
    "_same", same_scenario, "_", nmax, ".RData")
  save(inst, file = path)

  return(inst)
}


addProblem("MMS", data = datas$MMS, fun = test1)
addProblem("MSR", data = datas$MSR, fun = test1)
addProblem("F0", data = datas$F0, fun = test1)
addProblem("F1", data = datas$F1, fun = test1)
addProblem("K5", data = datas$K5, fun = test1)


addAlgorithm("ident", fun = function(job, data, instance) {
  return(instance)
})


prob_design_fun = function(data, nmax, exclude = NULL) {
  des = data[, 1:2]
  if (!is.null(exclude)) {
    des = des[-exclude, ]
  }
  colnames(des) = paste0(colnames(des), "ID")
  des = cbind(des, nmax = nmax)

  des2 = rbind(des, des)
  des = cbind(des2, same_scenario = rep(c(TRUE, FALSE), each = nrow(des)))

  return(des)
}

prob_design_MMS = prob_design_fun(data = data_mms_oc, nmax = 1e5, exclude = 1)
prob_design_MSR = prob_design_fun(data = data_msr_oc, nmax = 1e5)
prob_design_F0 = prob_design_fun(data = data_f0_oc, nmax = 1e5)
prob_design_F1 = prob_design_fun(data = data_f1_oc, nmax = 1e5)
prob_design_K5 = prob_design_fun(data = data_k5_oc, nmax = 1e5)


prob_list = list(
  MMS = prob_design_MMS, MSR = prob_design_MSR,
  F0 = prob_design_F0, F1 = prob_design_F1,
  K5 = prob_design_K5
)


addExperiments(prob.designs = prob_list)


# submitJobs()
# wait until all jobs have finished
# saveResults()
