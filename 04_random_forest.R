# Preferably run this file on a high performance compute cluster and transfer the result file to your computer when finished.
# Make sure the folder "Data" with the files from the previous step is located in your HPC jobs' working directory.

library(batchtools)
library(data.table)

load("data_sensors_oc.RData")

reg = makeExperimentRegistry("RF", packages = "ranger", seed = 8381)


load1 = function(job, data, scenarioID, experimentID, same_scenario, nmax) {
  path = paste0("Data/", job$prob.name, "_scenario", scenarioID, "_experiment", experimentID,
    "_same", same_scenario, "_", nmax, ".RData")
  load(path)

  return(inst)
}


addProblem("MMS", data = NULL, fun = load1)
addProblem("MSR", data = NULL, fun = load1)
addProblem("F0", data = NULL, fun = load1)
addProblem("F1", data = NULL, fun = load1)
addProblem("K5", data = NULL, fun = load1)


addAlgorithm("RF", fun = function(job, data, instance) {

  superClass = function(cl) {
    if (cl == "Driving.curve." | cl == "Driving.straight." | cl == "Lifting.and.Driving") {
      return("Driving")
    } else if (cl == "Lifting.lowering." | cl == "Lifting.raising." | cl == "Lifting.tilting.") {
      return("Lifting")
    } else if (cl == "Rotation" | cl == "Wrapping") {
      return("Wrapping")
    } else if (cl == "Standing" | cl == "Wrapping.preparation." | cl == "Docking" | cl == "Forks..entering.or.leaving.the.pallet.") {
      return("Standing")
    } else if (cl == "Loading") {
      return(cl)
    } else {
      stop(paste("Class", cl, "not in list of classes!"))
    }
  }

  train.class = instance$train$class
  train.super = sapply(train.class, superClass)

  test.class = instance$test$class
  test.super = sapply(test.class, superClass)

  train = instance$train[, -c("time", "transportation", "container", "No.loading"), with = FALSE]
  train$class = as.factor(train.class)

  test = instance$test[, -c("time", "class", "transportation", "container", "No.loading"), with = FALSE]

  n_per_class = table(train.class)
  w = 1 / n_per_class
  ww = sapply(train.class, function(cl) which(names(w) == as.character(cl)))
  cw = w[ww]

  ran = ranger(class ~ ., data = train, case.weights = cw)
  pred = predict(ran, data = test)$predictions
  pred = as.character(pred)
  pred.super = sapply(pred, superClass)

  res = list(
    predicted = pred, predicted.super = pred.super,
    truth = test.class, truth.super = test.super)
  return(res)
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


saveResults = function() {
  done = findDone()
  pars = flatten(getJobPars(done))
  res = reduceResultsDataTable(done)
  result = merge(pars, res, by = "job.id")
  save(result, file = "results_WGTL.RData")
  return(NULL)
}


# submitJobs()
# wait until all jobs have finished
# saveResults()
