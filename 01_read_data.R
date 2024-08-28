# General information:
# Create folder SPARL2 with subfolders Flightcontroller_Holybro_PX4FMU, Kistler_KiDaQ_5512A_5kHz,
# MetaMotionS_raw_data, MSR_145_raw_data and Video_Frames_annotated containing the sensor data
# and labels, respectively. Depending on the folder structure you chose, you may need to modify
# the setwd() commands in all files to set your working directory accordingly.

library(stringr)
library(data.table)
setwd("..\\SPARL2")

remove_offset = function(dat) {
  cm = apply(dat[, 2:4], 2, median)
  dat[, 2] = dat[, 2] - cm[1]
  dat[, 3] = dat[, 3] - cm[2]
  dat[, 4] = dat[, 4] - cm[3]
  dat = as.data.table(dat)
  return(dat)
}

extract_scenario = function(file) {
  s = str_split_1(file, "/")
  s = s[length(s)]
  s = str_split_1(s, "_")[1]
  s = str_split_1(s, "S")[2]
  s = as.numeric(s)
  return(s)
}

extract_experiment = function(file) {
  e = str_split_1(file, "Record")[2]
  e = str_split_1(e, "_|/")[1]
  e = as.numeric(e)
  return(e)
}


pretty_data = function(res, files) {
  dat = data.table(
    scenario = sapply(files, extract_scenario),
    experiment = sapply(files, extract_experiment),
    data = res
  )
  return(dat)
}




# MetaMotionS
files.mms = paste("MetaMotionS_raw_data", list.files("MetaMotionS_raw_data", pattern = ".csv"), sep = "/")

res_mms = lapply(files.mms, function(file) {
  d = read.csv(file)
  colnames(d) = c("epoch.time", "time", "s.elapsed", "MMS.Acc.x", "MMS.Acc.y", "MMS.Acc.z")
  dat = d[c("s.elapsed", "MMS.Acc.x", "MMS.Acc.y", "MMS.Acc.z")]
  colnames(dat)[1] = "time"

  dat[, 2:4] = 10 * dat[, 2:4]
  dat = remove_offset(dat)
  return(dat)
})

data_mms_abs = pretty_data(res_mms, files.mms)



# MSR 145
files.msr = paste("MSR_145_raw_data", list.files("MSR_145_raw_data", pattern = ".csv"), sep = "/")

res_msr = lapply(files.msr, function(file) {
  d = read.csv2(file, header = FALSE, skip = 27, dec = ".")
  colnames(d) = c("time", "MSR.Acc.x", "MSR.Acc.y", "MSR.Acc.z")
  d$time = as.POSIXct(d$time)

  t = as.numeric(d$time - min(d$time), unit = "secs")
  t = round(t, 2)
  d$time = t

  dat = remove_offset(d)
  return(dat)
})

data_msr_abs = pretty_data(res_msr, files.msr)



# Flightcontroller
paths.fc = paste0("Flightcontroller_Holybro_PX4FMU/", c("Scenario1", "Scenario2", "Scenario3"))
files.fc = unlist(sapply(paths.fc, function(p) paste(p, list.files(p, pattern = ".csv"), sep = "/")))
files.f0 = files.fc[str_detect(files.fc, "Acc_MPU6000")]
files.f1 = files.fc[str_detect(files.fc, "Acc_LSM303D")]

fc_fun = function(file) {
  d = read.csv(file, header = TRUE)
  if (str_detect(file, "MPU6000")) {
    colnames(d) = c("time", "F0.Acc.x", "F0.Acc.y", "F0.Acc.z")
  } else {
    colnames(d) = c("time", "F1.Acc.x", "F1.Acc.y", "F1.Acc.z")
  }

  t = d$time - min(d$time)
  t = round(t / 1e6, 4)
  d$time = t

  dat = remove_offset(d)
  return(dat)
}

res_f0 = lapply(files.f0, fc_fun)
res_f1 = lapply(files.f1, fc_fun)

data_f0_abs = pretty_data(res_f0, files.f0)
data_f1_abs = pretty_data(res_f1, files.f1)



# Kistler
files.kistler = paste("Kistler_KiDaQ_5512A_5kHz", list.files("Kistler_KiDaQ_5512A_5kHz", pattern = ".txt"), sep = "/")

res_k5 = lapply(files.kistler, function(file) {
  d = read.table(file, skip = 1, sep = ";", dec = ",", header = TRUE)

  if ("Anzahl" %in% colnames(d)) {
    h = scan(file, nmax = 1, what = character())
    h2 = str_split_1(h, ";")
    colnames(d) = h2
  }

  t = which(str_detect(colnames(d), "Time"))
  x = which(str_detect(colnames(d), "_X"))
  y = which(str_detect(colnames(d), "_Y"))
  z = which(str_detect(colnames(d), "_Z"))

  d = d[, c(t, x, y, z)]
  colnames(d) = c("time", "K5.Acc.x", "K5.Acc.y", "K5.Acc.z")

  dat = remove_offset(d)
  return(dat)
})

data_k5_abs = pretty_data(res_k5, files.kistler)




# data sets with time relative to beginning

begin_mms = c(36.55, 44.02, 20.44, 28.55, 64.10, 58.72, 50.78, 48.52, 60.38, 57.32,
  56.18, 49.83, 52.38, 46.41, 52.12, 64.00, 32.04, 46.02, 36.55, 24.80)

end_mms = c(NA, 472.10, 368.70, 380.97, 159.33, 122.26, 110.27, 106.54, 155.09, 142.24,
  135.99, 125.19, 128.14, 110.06, 114.02, 128.91, 223.27, 202.50, 147.63, 137.85) -
  begin_mms


begin_msr = c(78.38, 64.98, 46.17, 39.00, 59.45, 56.62, 48.16, 46.13, 56.43, 54.59,
  53.79, 47.13, 50.39, 44.26, 49.71, 61.23, 168.65, 43.24, 40.98, 90.16)

end_msr = c(537.34, 495.51, 397.77, 389.18, 154.53, 119.75, 107.27, 103.77, 150.49, 138.94,
  133.07, 121.97, 125.64, 107.48, 111.19, 125.72, 358.59, 198.69, 151.35, 202.68) -
  begin_msr


begin_f0 = c(62.2770, 11.0031, 3.1170, 4.3048, 34.6980, 35.2931, 22.9580, 21.0021, 27.3950, 34.5360,
  14.6550, 22.8389, 25.3351, 25.4060, 24.4501, 38.1750, 171.0511, 25.9161, 13.0040, 21.4051)

end_f0 = c(521.2502, 441.5390, 354.7290, 354.4718, 129.7680, 98.4202, 82.0640, 78.6460, 121.4709, 118.8910,
  93.9350, 97.6950, 100.5971, 88.6431, 85.9310, 102.6608, 360.9970, 181.3600, 123.3940, 133.9301) -
  begin_f0


begin_f1 = c(62.2740, 11.0010, 3.1141, 4.3024, 34.6970, 35.2920, 22.9571, 21.0001, 27.3950, 34.5341,
  14.6540, 22.8390, 25.3331, 25.4031, 24.4480, 38.1728, 171.0490, 25.9150, 13.0021, 21.4041)

end_f1 = c(521.2471, 441.5380, 354.7271, 354.4704, 129.7669, 98.4190, 82.0631, 78.6451, 121.4699, 118.8901,
  93.9340, 97.6950, 100.5961, 88.6410, 85.9301, 102.6599, 360.9950, 181.3590, 123.3921, 133.9290) -
  begin_f1


begin_k5 = c(91.3738, 40.0204, 32.6470, 27.4108, 19.5372, 28.0136, 18.8126, 18.8778, 19.5080, 27.6816,
  18.3284, 17.1680, 19.0358, 19.4960, 22.0698, 30.6664, 166.0914, 17.5204, 16.9480, 13.5548)

end_k5 = c(550.3222, 470.5332, 384.2408, 377.5592, 114.6028, 91.1376, 77.9158, 76.5190, 113.5778, 112.0320,
  97.6044, 92.0204, 94.2938, 82.7316, 83.5488, 95.1492, 356.0278, 172.9564, 127.3326, 126.0738) -
  begin_k5



rel_data = function(data, begins) {
  for (i in 1:nrow(data)) {
    data$data[[i]]$time = data$data[[i]]$time - begins[i]
  }
  return(data)
}

data_mms = rel_data(data_mms_abs, begin_mms)
data_msr = rel_data(data_msr_abs, begin_msr)
data_f0 = rel_data(data_f0_abs, begin_f0)
data_f1 = rel_data(data_f1_abs, begin_f1)
data_k5 = rel_data(data_k5_abs, begin_k5)


# annotation
files.labels = paste("Video_Frames_annotated", list.files("Video_Frames_annotated", pattern = ".csv"), sep = "/")

res_labels = lapply(files.labels, function(file) {
  d = read.csv(file, header = TRUE)
  d = d[, 1:(ncol(d) - 2)]

  ind.start = min(which(d$Synchronization == 1))
  time = 1:nrow(d) * 1/30 - ind.start * 1/30
  d$time = time

  dat = as.data.table(d)
  return(dat)
})

data_labels = pretty_data(res_labels, files.labels)


# one class
for (i in 1:nrow(data_labels)) {
  names_c = names(data_labels$data[[i]])[1:18]
  w = apply(data_labels$data[[i]][, 1:18], 1, function(x) {
    if (any(x == 1)) {
      return(which(x == 1))
    } else {
      return(18)
    }
  })

  if (is.list(w)) {
    inds = which(lengths(w) > 1)
    # if there is more than one class, is "Error" one of the classes?
    ers = sapply(w[inds], function(x) 16 %in% x)
    if (all(ers)) {
      w[inds] = 16
      w = unlist(w)
    } else {
      stop("Multiple classes!")
    }
  }

  data_labels$data[[i]]$class = names_c[w]


  # transportation
  names_t = names(data_labels$data[[i]])[19:22]
  transportation = apply(data_labels$data[[i]][, 19:22], 1, function(x) {
    w = which(x == 1)
    if (length(w) == 0) return("None")
    else if (length(w) == 1) return(names_t[w])
    else stop("Multiple classes!")
  })

  data_labels$data[[i]]$transportation = transportation

  # container
  names_co = names(data_labels$data[[i]])[23:24]
  container = apply(data_labels$data[[i]][, 23:24], 1, function(x) {
    w = which(x == 1)
    if (length(w) == 0) return("None")
    else if (length(w) == 1) return(names_co[w])
    else stop("Multiple classes!")
  })

  data_labels$data[[i]]$container = container
}


save(data_labels, file = "annotation.RData")






# add class information to sensor data
add_labels = function(data, ends = rep(NA, nrow(data))) {
  for (i in 1:nrow(data)) {
    print(i)
    ind.labels = which(data_labels$scenario == data$scenario[i] &
        data_labels$experiment == data$experiment[i])
    if (length(ind.labels) > 0) {
      dl = data_labels$data[[ind.labels]][, c("time", "class", "transportation", "container", "No.loading"), with = FALSE]

      if (!is.na(ends[i])) {
        times = dl[class == "Synchronization", ]$time
        sync_end = times[which.max(diff(times)) + 1]
        dl$time = dl$time * ends[i] / sync_end
      }

      row_ind = sapply(data$data[[i]]$time, function(ti) {
        which.min(abs(ti - dl$time))
      })
      data$data[[i]] = cbind(data$data[[i]], dl[row_ind, -1])
    } else {
      warning(paste0("No classes available for row ", i, "!"))
    }
  }
  return(data)
}



data_mms_oc = add_labels(data_mms, end_mms)
data_msr_oc = add_labels(data_msr, end_msr)
data_f0_oc = add_labels(data_f0, end_f0)
data_f1_oc = add_labels(data_f1, end_f1)
data_k5_oc = add_labels(data_k5, end_k5)


save(data_mms_oc, data_msr_oc, data_f0_oc, data_f1_oc, data_k5_oc,
  file = "data_sensors_oc.RData")
