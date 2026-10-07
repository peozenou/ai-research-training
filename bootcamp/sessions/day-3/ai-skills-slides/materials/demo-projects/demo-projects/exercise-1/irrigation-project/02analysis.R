#### 3) SETUP ####

#### VARIABLE NAMES ####

# LEFT HAND SIDE

hpbalvar <- data.frame(var = c("lareasp", "own", "owned5", "rentedoutfarmer", "slope"),
                       name = c("log area", "Own plot", "Owned plot $>$5 years", "Rented out, farmer",
                                "Slope"),
                       digits = c(3, 3, 3, 3, 3))

hpbalvarother <- data.frame(var = c("terraced", "rentedoutinvestor", "elevation"),
                            name = c("Terraced", "Rented out, comm. farmer", "Elevation"),
                            digits = c(3, 3, 1))

hpbalmipvar_small <- bind_rows(
  hpbalvar,
  data.frame(var = c("casp"), name = c("Command area"), digits = c(3))
) %>%
  dplyr::mutate(var = paste0(var, "_mip"))

hbalvar <- data.frame(var = c("hhhfem", "hhhage", "hhhprim",
                              "hhhjob", "nplots", "nmembers",
                              "blhhofffarm", "nadultmembers", "blhouseexp",
                              "blassetallprc"),
                      name = c("HHH female", "HHH age", "HHH completed primary",
                               "HHH worked off farm", "\\# of plots", "\\# of HH members",
                               "\\hspace{0.5em}\\# who worked off farm", "\\# of HH members (15-64)", "Housing expenditures",
                               "Asset index"),
                      digits = c(3, 1, 3, 3, 2, 2, 2, 2, 1, 2))

hwelfvar <- data.frame(var = c("houseexp", "assetallprc", "foodsecprc", "andconsindex"),
                       name = c("Housing expenditures", "Asset index", "Food security index",
                                "Overall index"),
                       digits = c(2, 2, 2, 2))

attritvar <- data.frame(var = c("attrit_trackedsuccess",
                                "attrit_any", "attrit_hh", "attrit_investor", "attrit_transact"),
                        name = c("Tracked", "Missing", "\\hspace{0.5em}HH attrition",
                                 "\\hspace{0.5em}Rented out comm. farmer",
                                 "\\hspace{0.5em}Transaction (not tracked)"),
                        digits = c(3, 3, 3, 3, 3))

hpscropvar <- data.frame(var = c("cultivated", "irrigated", "maincrphort", "maincrpbanana"),
                         name = c("Cultivated", "Irrigated", "Horticulture", "Banana"),
                         digits = c(3, 3, 3, 3),
                         ylab = c("Share", "Share", "Share", "Share")) %>%
  dplyr::mutate(figname = name)
hpsinputvar <- data.frame(var = c("hhlabha", "inputexpha", "hiredlabexpha"),
                          name = c("HH labor/ha", "Input exp./ha", "Hired labor exp./ha"),
                          digits = c(1, 1, 1),
                          ylab = c("Person days/ha", "'000 RwF/ha", "'000 RwF/ha")) %>%
  dplyr::mutate(figname = c("HH labor", "Input exp.", "Hired labor exp."))
hpsprodvar <- data.frame(var = c("yield", "salesha", "profits", "profits800"),
                         name = c("Yield", "Sales/ha", "\\hspace{0.5em}Shadow wage = 0",
                                  "\\hspace{0.5em}Shadow wage = 800"),
                         digits = c(1, 1, 1, 1),
                         ylab = c("'000 RwF/ha", "'000 RwF/ha", "'000 RwF/ha", "'000 RwF/ha")) %>%
  dplyr::mutate(figname = c("Yield", "Sales", "Profits (w* = 0)", "Profits (w* = 800)"))
hpsvar <- bind_rows(hpscropvar, hpsinputvar, hpsprodvar)

mkvar <- data.frame(var = c("minikittakeup", "maincrphort", "irrigated"),
                    name = c("Minikit takeup", "Horticulture", "Irrigated"),
                    digits = c(3, 3, 3))

landsatvar <- data.frame(var = c("ndvix100"),
                         name = c("100 * NDVI"),
                         digits = c(3))
landsatbalvar <- data.frame(var = c("ndvix100meanpreconstr",
                                    "ndvix100meanpreconstrdry",
                                    "ndvix100meanpreconstrrainy"),
                            name = c("100 * NDVI (pre-2009)",
                                     "100 * NDVI (pre-2009, Dry)",
                                     "100 * NDVI (pre-2009, Rainy)"),
                            digits = c(3, 3, 3))

# RIGHT HAND SIDE

xsecrhs2 <- data.frame(var = c("nmembers", "blassetallprc", "lareasp", "nadultmembers", "hhhfem", "hhhprim", "nplots"),
                       name = c("\\# of HH members", "Asset index", "log area", "\\# of HH members (15-64)",
                                "HHH female", "HHH completed primary", "\\# of plots"))

mkrhs <- data.frame(var = c("regminikit2", "mksat_sp"),
                    name = c("Assigned minikit", "Minikit saturation"))

rdrhs <- data.frame(var = c("casp_sp"),
                    name = c("CA"))

landsatrhs <- data.frame(var = c("casp_sp", "rainyseasonxcasp_sp", "dryseasonxcasp_sp",
                                 "maincrpbanana", "rainyseasonxmaincrpbanana",
                                 "dryseasonxmaincrpbanana"),
                         name = c("CA", "Rainy seasons * CA", "Dry season * CA",
                                  "Banana", "Rainy seasons * Banana",
                                  "Dry season * Banana"))

rdhetrhs <- data.frame(var = c("casp_sp", "casp_spxcasp_mip", "casp_spxnmembers",
                               "casp_spxblassetallprc"),
                       name = c("CA", "CA * MIP CA", "CA * \\# of HH members",
                                "CA * Asset index"))


#### SPECIFICATIONS ####

rdhpsspec <- data.frame(spec = c(" ~ casp_sp + carv_sp + caspxcarv_sp | sitegeo_spxseason | 0 | nearwug",
                                 " ~ casp_sp + carv_sp + caspxcarv_sp + lareasp_sp | sitegeo_spxseason | 0 | nearwug"),
                        name = c("RDD", "RDD*"))
rdhpsspec$controls <- list(list("Site-by-season FE" = T, "Distance to boundary" = T, "log area" = F),
                           list("Site-by-season FE" = T, "Distance to boundary" = T, "log area" = T))

miphpsspec <- data.frame(spec = c(" ~ casp_sp + carv_sp + caspxcarv_sp | sitegeo_spxseason | 0 | nearwug",
                                  paste0(" ~ casp_sp + carv_sp + caspxcarv_sp + lareasp_sp ",
                                         "+ lareasp_mip + casp_mip | sitegeo_spxseason | 0 | nearwug")),
                         name = c("RDD", "RDD*"))
miphpsspec$controls <- list(list("Site-by-season FE" = T, "Distance to boundary" = T, "log area" = F,
                                 "MIP log area" = F, "MIP CA" = F),
                            list("Site-by-season FE" = T, "Distance to boundary" = T, "log area" = T,
                                 "MIP log area" = T, "MIP CA" = T))

rdhspec <- rdhpsspec %>% filter(name == "RDD*") %>%
  dplyr::mutate(spec = gsub("season", "survey", spec),
         controls = lapply(controls, function(x) setNames(x, gsub("season", "survey", names(x)))))

rdbalspec <- bind_rows(
  data.frame(spec = " ~ casp_sp | 0 | 0 | nearwug", name = "Zero") %>%
    dplyr::mutate(controls = list(list("Site FE" = F, "Distance to boundary" = F, "log area" = F))),
  rdhpsspec %>%
    dplyr::mutate(spec = gsub("xseason", "", spec),
           controls = lapply(controls, function(x) setNames(x, gsub("-by-season", "", names(x)))))
)

mipbalspec <- bind_rows(
  data.frame(spec = " ~ casp_sp | 0 | 0 | nearwug", name = "Zero") %>%
    dplyr::mutate(controls = list(list("Site FE" = F, "Distance to boundary" = F, "log area" = F,
                                "MIP log area" = F, "MIP CA" = F))),
  miphpsspec %>%
    dplyr::mutate(spec = gsub("xseason", "", spec),
           controls = lapply(controls, function(x) setNames(x, gsub("-by-season", "", names(x)))))
)

miphethpsspec <- data.frame(
  spec = c(paste0(" ~ casp_sp + casp_spxcasp_mip + carv_sp + caspxcarv_sp + lareasp_sp ",
                  "+ lareasp_mip + casp_mip | sitegeo_spxseason | 0 | nearwug"),
           paste0(" ~ casp_sp + casp_spxnmembers + casp_spxblassetallprc + nmembers + blassetallprc ",
                  "+ carv_sp + caspxcarv_sp + lareasp_sp + lareasp_mip + casp_mip | sitegeo_spxseason | 0 | nearwug")),
  name = c("RDD* CA", "RDD* LM")
)
miphethpsspec$controls <- list(
  list("Site-by-season FE" = T, "Distance to boundary" = T, "log area" = T, "MIP log area" = T, "MIP CA" = T),
  list("\\# of HH members" = T, "Asset index" = T,
       "Site-by-season FE" = T, "Distance to boundary" = T, "log area" = T, "MIP log area" = T, "MIP CA" = T)
)


xsecspec2 <- data.frame(spec = c(" ~ nmembers + lareasp | sitegeoxseason | 0 | hhcode",
                                 " ~ blassetallprc + lareasp | sitegeoxseason | 0 | hhcode",
                                 paste0(" ~ nmembers + blassetallprc + lareasp + nadultmembers + ",
                                        "hhhfem + hhhprim + nplots | sitegeoxseason | 0 | hhcode"),
                                 paste0(" ~ nmembers + blassetallprc + lareasp + nadultmembers + ",
                                        "hhhfem + hhhprim + nplots | sitegeoxseasonxcrop | 0 | hhcode")))
xsecspec2$controls <- list(list("Site-by-season FE" = T, "Site-by-season-by-crop FE" = F),
                           list("Site-by-season FE" = T, "Site-by-season-by-crop FE" = F),
                           list("Site-by-season FE" = T, "Site-by-season-by-crop FE" = F),
                           list("Site-by-season FE" = T, "Site-by-season-by-crop FE" = T))

mkspec <- data.frame(spec = c(" ~ regminikit2 + mksat_sp | nminikitlottery + regmonitor_sp + zone_sp | 0 | wug_sp"))
mkspec$controls <- list(list("\\# of lotteries entered" = T, "O\\&M treatment" = T, "Zone FE" = T))

landsatspec <- data.frame(
  spec = c(paste0(" ~ casp_sp + carv_sp + caspxcarv_sp + lareasp_sp | n",
                  " | 0 | nearwug + n"),
           paste0(" ~ rainyseasonxcasp_sp + dryseasonxcasp_sp + dry * carv_sp +",
                  " dry * caspxcarv_sp + dry * lareasp_sp | n",
                  " | 0 | nearwug + n"),
           paste0(" ~ maincrpbanana | n",
                  " | 0 | nearwug + n"),
           paste0(" ~ rainyseasonxmaincrpbanana + dryseasonxmaincrpbanana | n",
                  " | 0 | nearwug + n"))
)
landsatspec$controls <- list(
  list("Site-by-image FE" = T, "Distance to boundary" = T, "log area" = T,
       "Dry season * Distance to boundary" = F, "Dry season * log area" = F),
  list("Site-by-image FE" = T, "Distance to boundary" = T, "log area" = T,
       "Dry season * Distance to boundary" = T, "Dry season * log area" = T),
  list("Site-by-image FE" = T, "Distance to boundary" = F, "log area" = F,
       "Dry season * Distance to boundary" = F, "Dry season * log area" = F),
  list("Site-by-image FE" = T, "Distance to boundary" = F, "log area" = F,
       "Dry season * Distance to boundary" = F, "Dry season * log area" = F)
)

landsatmipspec <- data.frame(
  spec = c(paste0(" ~ casp_sp + carv_sp + caspxcarv_sp + lareasp_sp +",
                  " lareasp_mip + casp_mip | n",
                  " | 0 | nearwug + n"),
           paste0(" ~ rainyseasonxcasp_sp + dryseasonxcasp_sp + dry * carv_sp +",
                  " dry * caspxcarv_sp + dry * lareasp_sp + dry * lareasp_mip +",
                  "dry * casp_mip | n | 0 | nearwug + n"))
)
landsatmipspec$controls <- list(
  list("Site-by-image FE" = T, "Distance to boundary" = T, "log area" = T,
       "MIP log area" = T, "MIP CA" = T,
       "Dry season * Distance to boundary" = F, "Dry season * log area" = F,
       "Dry season * MIP log area" = F, "Dry season * MIP CA" = F),
  list("Site-by-image FE" = T, "Distance to boundary" = T, "log area" = T,
       "MIP log area" = T, "MIP CA" = T,
       "Dry season * Distance to boundary" = T, "Dry season * log area" = T,
       "Dry season * MIP log area" = T, "Dry season * MIP CA" = T)
)

#### SAMPLE RESTRICTIONS ####

rdhpssamprestr <- data.frame(spec = "rdsampgeo & sampleplot == 1 & season %in% c(7, 10, 13)",
                             name = "RD, Dry season")
rdhpssamprestr <- rdhpssamprestr %>%
  bind_rows(data.frame(spec = "rdsampgeo & sampleplot == 1 & season %in% c(6, 8, 9, 11, 12)",
                       name = "RD, Rainy seasons"))
rdhpssamprestr <- rdhpssamprestr %>%
  bind_rows(data.frame(spec = "rdsampgeo & sampleplot == 1 & season %in% c(1)",
                       name = "RD, Dry season, 14C only"))
rdhpssamprestr <- rdhpssamprestr %>%
  bind_rows(data.frame(spec = "rdsampgeo & sampleplot == 1 & season %in% c(2, 3)",
                       name = "RD, Rainy seasons, 15AB only"))

rdhsamprestr <- data.frame(spec = "rdsampgeo_sp & survey %in% c(2:4)", name = "RD")

miphpssamprestr <- rdhpssamprestr %>%
  filter(!grepl("mipsamp", spec, fixed = T)) %>%
  dplyr::mutate(spec = spec %>% gsub(pattern = " & sampleplot == 1", replacement = "", fixed = T) %>%
           gsub(pattern = "rdsampgeo", replacement = "rdsampgeo_sp"),
         name = name %>% gsub(pattern = "RD", replacement = "MIP"))

rdbalsamprestr <-  data.frame(spec = "sampleplot == 1", name = "Zero")
rdbalsamprestr <- rdbalsamprestr %>%
  bind_rows(data.frame(spec = "rdsampgeo & sampleplot == 1", name = "RD"))

rdhbalsamprestr <-  data.frame(spec = "T", name = "Zero")
rdhbalsamprestr <- rdhbalsamprestr %>%
  bind_rows(data.frame(spec = "rdsampgeo_sp", name = "RD"))

mipbalsamprestr <- data.frame(spec = "T", name = "Zero")
mipbalsamprestr <- mipbalsamprestr %>%
  bind_rows(data.frame(spec = "rdsampgeo_sp", name = "MIP"))

#### HELPER FUNCTIONS ####

getnclosest <- function(dfhp, n) {
  fn <- dfhp$filename
  stopifnot(all(fn %in% rownames(distm)))
  distmfn <- distm[fn,fn]
  lapply(fn, function(f) {
    fnf <- fn[order(distmfn[,f])][1:n]
    data.frame(filename = f, closen = 1:n, filenamen = fnf, distn = distmfn[fnf,f])
  }) %>% bind_rows() %>%
    left_join(dfhp %>% select(hhcode, plot, filename)) %>%
    left_join(dfhp %>% select(hhcode, plot, filename) %>%
                dplyr::rename(hhcoden = hhcode, plotn = plot, filenamen = filename))
}
gendm <- function(df, v, f = NULL, nclose = 5) {
  stopifnot(all(c("hhcode", "plot", "filename", v, f) %in% names(df)))
  stopifnot(nrow(df) == nrow(df %>% filter_at(c("hhcode", "plot", "filename", v, f), all_vars(!is.na(.)))))
  stopifnot(!any(paste0(v, "n") %in% v))
  ### SPLIT BY f (DEMEANING VARIABLE)
  if(length(f) == 0) {
    df <- list(df)
  } else {
    df <- split(df, lapply(f, function(fi) { df[,fi,drop=T] }))
  }
  ### FOR EACH f
  dfdm <- lapply(df, function(dfi) {
    ### ONE OBSERVATION PER hhcode-plot FOR EACH f
    stopifnot(nrow(dfi) == nrow(dfi %>% select(hhcode, plot) %>% distinct))
    # GETTING 5 CLOSEST NEIGHBORS USING filename
    dfin <- getnclosest(dfi, nclose)
    neighbordf <- dfin
    dfin <- dfin %>%
      left_join(dfi %>% select_at(vars(c("hhcode", "plot", v))) %>% dplyr::rename(hhcoden = hhcode, plotn = plot)) %>%
      group_by(hhcode, plot) %>%
      # MEAN FOR NEIGHBORS
      dplyr::summarise_at(vars(v), function(x) { mean(x) }) %>%
      ungroup %>% dplyr::rename_at(vars(v), function(vi) paste0(vi, "n")) %>%
      left_join(dfi %>% select_at(vars(c("hhcode", "plot", "nearwug", "nearzone", v))))
    # DO THE DEMEAN
    for(vi in v) dfin[,vi] <- dfin[,vi] - dfin[,paste0(vi, "n")]
    dfin <- dfin %>% select_at(vars(c("hhcode", "plot", "nearwug", "nearzone", v))) %>%
      left_join(dfi %>% select_at(vars(c("hhcode", "plot", f))))
    return(list("df" = dfin, "neighbordf" = neighbordf))
  })
  # CREATE "SYMMETRIC" DATA FRAME OF PLOTS AND THEIR NEIGHBORS
  neighbordf <- lapply(dfdm, function(.) bind_rows(.$neighbordf %>% select(hhcode, plot, hhcoden, plotn),
                                                   .$neighbordf %>% dplyr::rename(hhcode = hhcoden, hhcoden = hhcode,
                                                                           plot = plotn, plotn = plot) %>%
                                                     select(hhcode, plot, hhcoden, plotn))) %>%
    bind_rows() %>% distinct
  stopifnot(identical(neighbordf %>% dplyr::count(hhcoden, plotn) %>% dplyr::count(n),
                      neighbordf %>% dplyr::count(hhcode, plot) %>% dplyr::count(n)))
  uniquehhcodeplot <- paste0(neighbordf$hhcode, "_", neighbordf$plot) %>% unique
  # CREATE BINARY MATRIX IDENTIFYING NEIGHBORS
  neighborm <- matrix(0, nrow = length(uniquehhcodeplot), ncol = length(uniquehhcodeplot))
  rownames(neighborm) <- uniquehhcodeplot; colnames(neighborm) <- uniquehhcodeplot
  for(i in 1:nrow(neighbordf)) {
    neighborm[paste0(neighbordf$hhcode[i], "_", neighbordf$plot[i]),
              paste0(neighbordf$hhcoden[i], "_", neighbordf$plotn[i])] <- 1
  }
  return(list("df" = lapply(dfdm, function(.) .$df) %>% bind_rows(),
              "neighbordf" = neighbordf, "neighborm" = neighborm))
}
conleyfelm <- function(df, regfelm, neighborm, neighborsofneighbors) {
  # df = DF USED FOR REGRESSION (NO MISSINGS)
  # regfelm = felm OBJECT (keepCX = T)
  # neighborm = MATRIX OF "ever demeaned together", ROW/COL NAMES ALL hhcode_plot
  # neighborsofneighbors = WHETHER TO CALCULATE STANDARD ERRORS USING neighborsofneighbors
  cX <- regfelm$cX; e <- regfelm$resid
  stopifnot(nrow(df) == nrow(cX))
  conleym <- paste0(df$hhcode, "_", df$plot)
  if(neighborsofneighbors) {
    # USING NEIGHBORS OF NEIGHBORS AS CONNECTIONS
    conleym <- (neighborm %*% neighborm)[conleym, conleym] > 0
    # HACKY WAY TO CONVERT TO INTEGER MATRIX
    conleym[1,1] <- as.integer(conleym[1,1])
  } else {
    # USING NEIGHBORS AS CONNECTIONS
    conleym <- neighborm[conleym, conleym]
  }
  stopifnot(isSymmetric(conleym))
  xpxinv <- solve(t(cX) %*% cX)
  vcov <- xpxinv %*% (t(cX) %*% ((e %*% t(e)) * conleym) %*% cX) %*% xpxinv
  # STORING OUTPUTS IN felm OBJECT
  regfelm$clustervcv <- vcov
  regfelm$cpval <- setNames(as.numeric(1 - abs(2*pnorm(regfelm$coef / sqrt(diag(regfelm$clustervcv))) - 1)),
                            names(diag(vcov)))
  regfelm$clustervar <- NULL
  regfelm$neffectiveclu <- 1 / mean(conleym)
  regfelm$data <- df
  regfelm$neighborm <- neighborm
  return(regfelm)
}
sfefelm <- function(formula, data, conley = T, neighborsofneighbors = T) {
  stopifnot(length(as.character(formula)) == 3 & as.character(formula)[1] == "~")
  lhs <- as.character(formula)[2]
  stopifnot(!grepl("[+]", lhs))
  rhs <- as.character(formula)[3] %>% strsplit("[|]") %>% unlist %>% trimws
  stopifnot(length(rhs) >= 1 & length(rhs) <= 4 & (length(rhs) <= 2 | rhs[3] == "0"))
  regvars <- c(lhs, rhs %>% subset(. != "0") %>% lapply(function(x) strsplit(x, "[+]") %>% unlist) %>%
                 unlist %>% trimws) %>% unname
  data <- data %>% filter_at(regvars, all_vars(!is.na(.)))
  f <- NULL
  if(length(rhs) >= 2 & rhs[2] != "0") f <- rhs[2]
  stopifnot(length(f) == 0 | !grepl("[+]", rhs[2]))
  # GENERATES DEMEANED DATA (list with "df" and "neighborm") USING hhcode/plot/filename | f
  datadm <- gendm(data, v = c(lhs, rhs[1] %>% strsplit("[+]") %>% unlist %>% trimws), f = f)
  if(length(f) == 1) formula <- paste0(lhs, " ~ ", as.character(formula)[3] %>%
                                         gsub(pattern = f, replacement = "0")) %>% as.formula
  regsfe <- felm(formula, data = datadm$df, keepCX = T)
  # FIXES STANDARD ERRORS AND PVALUES IF CONLEY (INSTEAD OF CLUSTER ROBUST)
  if(conley) regsfe <- conleyfelm(df = datadm$df, regfelm = regsfe, neighborm = datadm$neighborm,
                                  neighborsofneighbors = neighborsofneighbors)
  return(regsfe)
}

# GENERATE CONLEY VCOV MATRIX FOR SUR REGRESSIONS
getsurconleyvcov <- function(df1, cX1, e1, neighborm1, df2, cX2, e2, neighborm2,
                             neighborsofneighbors = T) {
  # dfi = DF USED FOR REGRESSION i (NO MISSINGS)
  # cXi/ei = xs/errors
  # neighbormi = MATRIX OF "ever demeaned together", ROW/COL NAMES ALL hhcode_plot
  stopifnot(nrow(df1) == nrow(cX1) & nrow(df2) == nrow(cX2))
  hp1 <- paste0(df1$hhcode, "_", df1$plot)
  hp2 <- paste0(df2$hhcode, "_", df2$plot)
  missingrows1 <- unique(hp2) %>% subset(!. %in% rownames(neighborm1))
  missingrows2 <- unique(hp1) %>% subset(!. %in% rownames(neighborm2))
  if(length(missingrows1) > 0) {
    neighborm1 <- cbind(neighborm1, matrix(NA, ncol = length(missingrows1), nrow = nrow(neighborm1)))
    neighborm1 <- rbind(neighborm1, matrix(NA, ncol = ncol(neighborm1), nrow = length(missingrows1)))
    rownames(neighborm1)[seq(nrow(neighborm1) - length(missingrows1) + 1, nrow(neighborm1), 1)] <- missingrows1
    colnames(neighborm1)[seq(nrow(neighborm1) - length(missingrows1) + 1, nrow(neighborm1), 1)] <- missingrows1
  }
  if(length(missingrows2) > 0) {
    neighborm2 <- cbind(neighborm2, matrix(NA, ncol = length(missingrows2), nrow = nrow(neighborm2)))
    neighborm2 <- rbind(neighborm2, matrix(NA, ncol = ncol(neighborm2), nrow = length(missingrows2)))
    rownames(neighborm2)[seq(nrow(neighborm2) - length(missingrows2) + 1, nrow(neighborm2), 1)] <- missingrows2
    colnames(neighborm2)[seq(nrow(neighborm2) - length(missingrows2) + 1, nrow(neighborm2), 1)] <- missingrows2
  }
  stopifnot(all(rownames(neighborm1) %in% rownames(neighborm2)) & all(rownames(neighborm2) %in% rownames(neighborm1)))
  if(neighborsofneighbors) {
    # USING NEIGHBORS OF NEIGHBORS AS CONNNECTIONS
    conleym1 <- (neighborm1 %*% neighborm1)[hp1, hp2] > 0
    conleym2 <- (neighborm2 %*% neighborm2)[hp1, hp2] > 0
    # HACKY WAY TO CONVERT TO INTEGER MATRIX
    conleym1[1,1] <- as.integer(conleym1[1,1])
    conleym2[1,1] <- as.integer(conleym2[1,1])
  } else {
    # USING NEIGHBORS AS CONNECTIONS
    conleym1 <- neighborm1[hp1, hp2]
    conleym2 <- neighborm2[hp1, hp2]
  }
  conleym1[is.na(conleym1)] <- 0
  conleym2[is.na(conleym2)] <- 0
  conleym <- pmax(conleym1, conleym2)
  vcov <- solve(t(cX1) %*% cX1) %*% (t(cX1) %*% ((e1 %*% t(e2)) * conleym) %*% cX2) %*% solve(t(cX2) %*% cX2)
  return(vcov)
}

genregF <- function(reg, tvar, conley = F) {
  fstat <- (t(reg$coef[tvar,]) %*% solve(reg$clustervcv[tvar,tvar]) %*% reg$coef[tvar,]) / length(tvar)
  fp  <- 1 - pf(fstat, length(tvar),
                ifelse(conley, Inf, max(sapply(reg$clustervar, function(x) length(unique(x)))) - 1))
  list("fstat" = fstat, "fp" = fp)
}

genomnibusF <- function(speclist, tvar, conley = F, neighborsofneighbors = T) {
  stopifnot(length(tvar) == 1)
  coef <- sapply(speclist, function(speci) speci[["reg"]]$coef[tvar,])
  e <- lapply(speclist, function(speci) speci[["reg"]]$resid)
  d <- lapply(speclist, function(speci) {
    di <- speci[["reg"]]$cX[,tvar]
    cXi <- speci[["reg"]]$cX %>% `[`(,-which(colnames(.) == tvar),drop=F)
    if(ncol(cXi) == 0) return(di)
    # (I - X (X'X)^-1 X') D
    else return(di - cXi %*% solve(t(cXi) %*% cXi, t(cXi) %*% di))
  })
  if(!conley) {
    clu <- lapply(speclist, function(speci) {
      clui <- speci[["reg"]]$clustervar
      stopifnot(length(clui) == 1)
      clui[[1]] %>% as.character
    })
    vcov <- sapply(1:length(speclist), function(i) {
      sapply(1:length(speclist), function(j) {
        # GENERATE COV HERE
        (lapply(unique(c(clu[[i]], clu[[j]])), function(cluij) {
          iclu <- clu[[i]] == cluij; jclu = clu[[j]] == cluij
          # within cluster, d1i e1i e2j d2j for all i,j in same cluster
          sum((d[[i]][iclu] * e[[i]][iclu]) %*% t(d[[j]][jclu] * e[[j]][jclu]))
          # divided by (appropriately scaled) var(d1i) * var(d2i)
        }) %>% unlist %>% sum) / (sum(d[[i]]^2) * sum(d[[j]]^2))
      })
    })
    # DEGREES OF FREEDOM CORRECTION
    dofc <- sapply(1:length(speclist), function(i) {
      # COMPARE DIAGONAL TERMS TO FELM SE
      (summary(speclist[[i]][["reg"]])$coef[tvar, "Cluster s.e."]^2) / vcov[i,i]
    })
    # APPLY CORRECTION
    vcov <- vcov * sqrt(dofc %*% t(dofc))
  } else {
    vcov <- sapply(1:length(speclist), function(i) {
      sapply(1:length(speclist), function(j) {
        getsurconleyvcov(df1 = speclist[[i]][["reg"]]$data, cX1 = d[[i]], e1 = e[[i]],
                         neighborm1 = speclist[[i]][["reg"]]$neighborm,
                         df2 = speclist[[j]][["reg"]]$data, cX2 = d[[j]], e2 = e[[j]],
                         neighborm2 = speclist[[j]][["reg"]]$neighborm, neighborsofneighbors = T)
      })
    })
  }
  fstat <- t(coef) %*% solve(vcov) %*% coef / length(coef)
  fp <- 1 - pf(fstat, length(coef), ifelse(conley, Inf, length(unique(clu %>% unlist)) - 1))
  return(list("fstat" = fstat, "fp" = fp))
}

genregsumdf <- function(reg, dfrhs, var, varname, digits, spec, specname, controls = NULL, conley = F,
                        forcecontrolmean = F) {
  rhsvars <- dfrhs[which(dfrhs$var %in% row.names(reg$coef)),]
  colinreg <- rep(grepl(var, spec), nrow(rhsvars))
  if(colinreg) {
    regf <- list("fstat" = NA, "fp" = NA)
  } else {
    regf <- genregF(reg = reg, tvar = rhsvars$var, conley = conley)
  }
  df <- data.frame(var = var, varname = varname, spec = spec, specname = specname,
                   tvar = rhsvars$var, tvarname = rhsvars$name,
                   coef = ifelse(colinreg, NA, reg$coef[rhsvars$var, var] %>%
                                   sapply(function(x) format(x, digits = 0, nsmall = digits))),
                   se = ifelse(colinreg, NA, sqrt(diag(reg$clustervcv))[rhsvars$var] %>%
                                 sapply(function(x) format(x, digits = 0, nsmall = digits))),
                   p = ifelse(colinreg, NA, reg$cpval[rhsvars$var] %>%
                                sapply(function(x) format(x, digits = 0, nsmall = 3))),
                   n = reg$N %>% format(digits = 0, big.mark = ","),
                   nclu = ifelse(conley, NA,
                                 sapply(reg$clustervar, function(x) length(unique(x))) %>% max %>%
                                   format(digits = 0, big.mark = ",")),
                   neffectiveclu = ifelse(conley, reg$neffectiveclu, NA),
                   fstat = regf$fstat %>% format(digits = 0, nsmall = 1),
                   fp = regf$fp %>% format(digits = 0, nsmall = 3))
  if("Spatial FE" %in% names(controls)) df %<>% dplyr::mutate(sfe = controls$`Spatial FE`)
  if(class(reg$model) == "data.frame") {
    if(all(reg$model[,first(rhsvars$var)] == 0 | reg$model[,first(rhsvars$var)] == 1) |
       forcecontrolmean)  {
      df$controlmean <- NA; df$controlsd <- NA
      df$controlmean[1] <- subset(reg$model, reg$model[,first(rhsvars$var)] == 0)[,var] %>% mean %>%
        format(digits = 0, nsmall = digits)
      df$controlsd[1] <- subset(reg$model, reg$model[,first(rhsvars$var)] == 0)[,var] %>% sd %>%
        format(digits = 0, nsmall = digits)
    }
  }
  df$controls <- lapply(1:nrow(df), function(i) controls)
  return(df)
}

removesitecontrols <- function(controls) {
  if(any(grepl("Site", names(controls)))) controls[[which(grepl("Site", names(controls)))]] <- F
  return(controls)
}


#### 4) ANALYSIS ####

#### STYLIZED FACTS ####

styfactdf <- analysishps %>%
  dplyr::mutate(staple = maincrp %in% staplenames, maize = maincrp == staplenames[2], beans = maincrp == staplenames[1],
         maincrphort = maincrp %in% hortnames, maincrpbanana = maincrp %in% banananames,
         hortrainy = maincrphort & season %in% c(2:3, 5:6, 8:9, 11:12),
         hortdry = maincrphort & season %in% c(1, 4, 7, 10, 13),
         anyhiredlab = as.numeric(hiredlabha > 0),
         rented = as.numeric(samp_rentedin | samp_rentedout)) %>%
  filter(!is.na(maincrp) & !is.na(yield))
styfactdf <- mapply(function(v, n) {
  styfactdf %>% filter_at(vars(v), any_vars(. == 1)) %>%
    left_join(hbaseline %>% select(hhcode, site)) %>%
    dplyr::mutate(rainy = season %in% c(2:3, 5:6, 8:9, 11:12),
           shareobs = (n() / nrow(styfactdf)),
           nobs = n()) %>%
    dplyr::summarise_at(c("rented", "anyhiredlab",
                   "hiredlabha", "hiredlabexpha", "hhlabha",
                   "yield",
                   "inputexpha", "salesshare", "irrigated", "daysirrigated",
                   "rainy", "lareasp",
                   "rainy", "shareobs", "nobs"),
                 function(x) mean(x, na.rm = T)) %>%
    dplyr::mutate(var = n)
}, v = c("staple", "maize", "beans", "maincrpbanana", "maincrphort",
         "hortrainy", "hortdry"),
n = c("Staples", "Maize", "Beans", "Bananas", "Horticulture",
      "Rainy", "Dry"), SIMPLIFY = F) %>%
  bind_rows() %>%
  dplyr::mutate(profits0 = yield - hiredlabexpha - inputexpha,
         profits800 = yield - 0.8*hhlabha - hiredlabexpha - inputexpha,
         profits480 = yield - 0.48*hhlabha - hiredlabexpha - inputexpha)
styfactdfoutall <- styfactdf %>% gather(row, val, -var) %>% spread(var, val) %>% dplyr::rename(var = row) %>%
  `[`(,c("var", "Staples", "Maize", "Beans", "Bananas", "Horticulture", "Rainy", "Dry"))
styfactdfoutall <- rbind(
  styfactdfoutall %>%
    filter(var %in% c("yield", "hiredlabha", "hiredlabexpha", "hhlabha",
                      "inputexpha", "profits0", "profits480", "profits800",
                      "daysirrigated")) %>%
    dplyr::mutate_at(c("Staples", "Maize", "Beans", "Bananas", "Horticulture", "Rainy", "Dry"),
              function(x) format(x, digits = 0, nsmall = 0) %>%
                gsub(pattern = " ", replacement = "\\hphantom{0}", fixed = T)),
  styfactdfoutall %>%
    filter(var %in% c("rented", "anyhiredlab",
                      "salesshare", "irrigated", "rainy", "lareasp", "shareobs")) %>%
    dplyr::mutate_at(c("Staples", "Maize", "Beans", "Bananas", "Horticulture", "Rainy", "Dry"),
              function(x) format(x, digits = 0, nsmall = 2) %>%
                gsub(pattern = " ", replacement = "\\hphantom{0}", fixed = T)),
  styfactdfoutall %>%
    filter(var %in% c("nobs")) %>%
    dplyr::mutate_at(c("Staples", "Maize", "Beans", "Bananas", "Horticulture", "Rainy", "Dry"),
              function(x) format(x, nsmall = 0, big.mark = ","))
)
styfactvarnames <- c("yield" = "Yield",
                     "anyhiredlab" = "Any hired labor",
                     "hiredlabha" = "Hired labor (days)", "hiredlabexpha" = "Hired labor expenditures",
                     "hhlabha" = "HH labor (days)",
                     "inputexpha" = "Inputs",
                     "profits0" = "\\hspace{0.5em}Shadow wage = 0 RwF/day",
                     "profits480" = "\\hspace{0.5em}Shadow wage = 480 RwF/day",
                     "profits800" = "\\hspace{0.5em}Shadow wage = 800 RwF/day",
                     "salesshare" = "Sales share", "irrigated" = "Irrigated",
                     "daysirrigated" = "Days irrigated", "rainy" = "Rainy",
                     "lareasp" = "log GPS area", "shareobs" = "Share of obs.",
                     "nobs" = "\\# of obs.")

styfactdfout <- styfactdfoutall %>%
  filter(var %in% names(styfactvarnames)) %>%
  dplyr::mutate(var = factor(styfactvarnames[var], levels = styfactvarnames %>% unname)) %>%
  arrange(var) %>% dplyr::mutate_at("var", as.character)
styfactdfout <- c("\\begin{tabular}{lccccccc}", "\\hline \\hline",
                  " & \\multicolumn{3}{c}{Staples} & & \\multicolumn{3}{c}{Horticulture} \\\\",
                  "\\cmidrule(lr){2-4}\\cmidrule(lr){6-8}",
                  paste0(" & ", names(styfactdfout)[2:8] %>% gsub(pattern = "Horticulture", replacement = "All") %>%
                           paste(collapse = " & "), " \\\\"),
                  paste0(" & ", paste(paste0("(", 1:7, ")"), collapse = " & "), " \\\\"),
                  "\\hline",
                  lapply(1:nrow(styfactdfout), function(i) {
                    c(paste0("Profits ", paste(rep("& ", 7), collapse = ""), "\\\\")[i == 7],
                      paste0(styfactdfout[i,] %>% as.character %>% paste(collapse = " & "), " \\\\"))
                  }) %>% unlist,
                  "\\hline \\hline", "\\end{tabular}")

#### OTHER ANALYSIS ####

reg_xseccontrol <- mapply(function(var, varname, digits) {
  mapply(function(spec, controls) {
    dfreg <- analysishps
    reg <- felm(paste0(var, spec) %>% as.formula, data = dfreg)
    genregsumdf(reg = reg, dfrhs = xsecrhs2, var = var, varname = varname, digits = digits,
                spec = spec, specname = NA, controls = controls)
  }, spec = xsecspec2$spec, controls = xsecspec2$controls,
  SIMPLIFY = F, USE.NAMES = F) %>% bind_rows()
}, var = hpsinputvar$var, varname = hpsinputvar$name, digits = hpsinputvar$digits,
SIMPLIFY = F, USE.NAMES = F) %>% bind_rows()

#### FLIP THE MIP ####

flippeddf <- list("Sample plot" = list("true" = "sampleplot == 1",
                                       "false" = "sampleplot == 0"),
                  "Largest other plot" = list("true" = "sampleplot == 0",
                                              "false" = "sampleplot == 1"))

flippedreg <- sapply(names(flippeddf), function(n) {
  l <- flippeddf[[n]]
  regdf <- analysishps %>%
    dplyr::rename(plot_lhs = plot, filename_lhs = filename) %>%
    left_join(hpbaseline %>%
                left_join(nearwug %>% select(filename, nearwug)) %>%
                group_by(hhcode) %>%
                filter(any(sampleplot == 1)) %>%
                ungroup %>%
                group_by(hhcode) %>% filter(!! rlang::parse_expr(l$true)) %>%
                ungroup %>%
                select(hhcode, plot, filename,
                       casp_rhs = casp, carv_rhs = carv,
                       caspxcarv_rhs = caspxcarv, lareasp_rhs = lareasp,
                       rdsampgeo_rhs = rdsampgeo, sitegeo_rhs = sitegeo,
                       nearwug_rhs = nearwug)) %>%
    dplyr::mutate(sitegeo_rhsxseason = paste0(sitegeo_rhs, "x", season),
           caspxcasp_rhs = casp * casp_rhs) %>%
    filter(rdsampgeo_rhs & season %in% c(7, 10, 13)) %>%
    select(-nearwug) %>% dplyr::rename(nearwug = nearwug_rhs)
  spformula <- paste0(" ~ casp_rhs ",
                      "+ carv_rhs + caspxcarv_rhs + lareasp_rhs ",
                      "| sitegeo_rhsxseason | 0 | nearwug")
  mipformula <- paste0(" ~ casp_rhs ",
                       "+ carv_rhs + caspxcarv_rhs + lareasp_rhs ",
                       "+ lareasp + casp ",
                       "| sitegeo_rhsxseason | 0 | nearwug")
  mipxcaformula <- paste0(" ~ casp_rhs + caspxcasp_rhs ",
                          "+ carv_rhs + caspxcarv_rhs + lareasp_rhs ",
                          "+ lareasp + casp ",
                          "| sitegeo_rhsxseason | 0 | nearwug")
  spdata <- regdf %>% inner_join(
    hpbaseline %>% group_by(hhcode) %>%
      filter(!! rlang::parse_expr(l$true)) %>% ungroup %>%
      select(hhcode, plot_lhs = plot)
  )
  mipdata <- regdf %>% inner_join(
    hpbaseline %>% group_by(hhcode) %>%
      filter(!! rlang::parse_expr(l$false)) %>% ungroup %>%
      select(hhcode, plot_lhs = plot),
  )
  var <- "irrigated"
  shortn <- c("Sample plot" = "SP", "Largest other plot" = "LOP")
  shortmipn <- c("Sample plot" = "LOP", "Largest other plot" = "SP")
  regs <- list(
    "rdd" = list(
      "sp" = felm(formula = paste0(var, spformula) %>% as.formula, data = spdata, keepModel = T),
      "mip" = felm(formula = paste0(var, mipformula) %>% as.formula, data = mipdata, keepModel = T),
      "mipxca" = felm(formula = paste0(var, mipxcaformula) %>% as.formula, data = mipdata, keepModel = T)
    ),
    "sfe" = list(
      "sp" = sfefelm(formula = paste0(var, spformula) %>% as.formula, data = spdata,
                     conley = T, neighborsofneighbors = T),
      "mip" = sfefelm(formula = paste0(var, mipformula) %>% as.formula, data = mipdata,
                      conley = T, neighborsofneighbors = T),
      "mipxca" = sfefelm(formula = paste0(var, mipxcaformula) %>% as.formula, data = mipdata,
                         conley = T, neighborsofneighbors = T)
    ))
  dfrhs <- data.frame(var = c("casp_rhs", "caspxcasp_rhs"),
                      name = c(paste0(shortn[n], " CA"),
                               paste0(shortn[n], " CA * ", shortmipn[n], " CA")))
  bind_rows(
    regs$rdd$sp %>% genregsumdf(
      dfrhs = dfrhs, var = "irrigated", varname = "Irrigated", digits = 3,
      spec = spformula, specname = spformula,
      controls = list("Distance to boundary" = T, "Spatial FE" = F, forcecontrolmean = T), conley = F 
    ),
    regs$rdd$mip %>% genregsumdf(
      dfrhs = dfrhs, var = "irrigated", varname = "Irrigated", digits = 3,
      spec = mipformula, specname = mipformula,
      controls = list("Distance to boundary" = T, "Spatial FE" = F, forcecontrolmean = T), conley = F
    ),
    regs$rdd$mipxca %>% genregsumdf(
      dfrhs = dfrhs, var = "irrigated", varname = "Irrigated", digits = 3,
      spec = mipxcaformula, specname = mipxcaformula,
      controls = list("Distance to boundary" = T, "Spatial FE" = F, forcecontrolmean = T), conley = F
    ),
    regs$sfe$sp %>% genregsumdf(
      dfrhs = dfrhs, var = "irrigated", varname = "Irrigated", digits = 3,
      spec = spformula, specname = spformula,
      controls = list("Distance to boundary" = T, "Spatial FE" = T), conley = T
    ),
    regs$sfe$mip %>% genregsumdf(
      dfrhs = dfrhs, var = "irrigated", varname = "Irrigated", digits = 3,
      spec = mipformula, specname = mipformula,
      controls = list("Distance to boundary" = T, "Spatial FE" = T), conley = T
    ),
    regs$sfe$mipxca %>% genregsumdf(
      dfrhs = dfrhs, var = "irrigated", varname = "Irrigated", digits = 3,
      spec = mipxcaformula, specname = mipxcaformula,
      controls = list("Distance to boundary" = T, "Spatial FE" = T), conley = T
    ),
  )
}, simplify = F, USE.NAMES = T)

#### RD ####

genballist <- function(spec, samp, df, dfrhs, dfvar, jointf = T) {
  sapply(
    list("ca" = list("spec" = spec, "samp" = samp)),
    function(specsampdf) {
      mapply(function(samprestr, sampname) {
        mapply(function(spec, specname, controls) {
          print(paste0(sampname, ",", specname))
          dfreg <- df %>% filter(!! rlang::parse_expr(samprestr))
          # RDD SPECIFICATIONS
          sampspec <- mapply(function(var, varname, digits) {
            reg <- felm(paste0(var, spec) %>% as.formula, data = dfreg, keepCX = T, keepModel = T)
            list("reg" = reg, "var" = var, "varname" = varname, "digits" = digits)
          }, var = dfvar$var, varname = dfvar$name, digits = dfvar$digits,
          SIMPLIFY = F, USE.NAMES = F)
          # RDD OMNIBUS F
          omnibusf <- list("fstat" = NA, "fp" = NA)
          if(jointf) {
            omnibusf <- genomnibusF(speclist = sampspec %>% subset(sapply(., function(x) !grepl(x[["var"]], spec))),
                                    tvar = dfrhs$var, conley = F)
          }
          # SFE SPECIFICATIONS
          sampspecsfe <- mapply(function(var, varname, digits) {
            sfereg <- sfefelm(paste0(var, spec) %>% as.formula, data = dfreg,
                              conley = T, neighborsofneighbors = T)
            list("reg" = sfereg, "var" = var, "varname" = varname, "digits" = digits)
          }, var = dfvar$var, varname = dfvar$name, digits = dfvar$digits,
          SIMPLIFY = F, USE.NAMES = F)
          # SFE OMNIBUS F
          omnibusfsfe <- list("fstat" = NA, "fp" = NA)
          if(jointf) {
            omnibusfsfe <- genomnibusF(speclist = sampspecsfe %>% subset(sapply(., function(x) !grepl(x[["var"]], spec))),
                                       tvar = dfrhs$var, conley = T, neighborsofneighbors = T)
          }
          # GENERATE SUMMARY DF
          bind_rows(lapply(sampspec, function(s) {
            genregsumdf(reg = s$reg, dfrhs = dfrhs, var = s$var, varname = s$varname, digits = s$digits,
                        spec = spec, specname = specname, controls = controls %>% append(list("Spatial FE" = F)))
          }) %>% bind_rows() %>% dplyr::mutate(omnibusfstat = omnibusf$fstat %>% format(digits = 0, nsmall = 1),
                                        omnibusfp = omnibusf$fp %>% format(digits = 0, nsmall = 3)),
          lapply(sampspecsfe, function(s) {
            genregsumdf(reg = s$reg, dfrhs = dfrhs, var = s$var, varname = s$varname, digits = s$digits,
                        spec = spec, specname = specname,
                        controls = controls %>% append(list("Spatial FE" = T)) %>% removesitecontrols,
                        conley = T)
          }) %>% bind_rows() %>% dplyr::mutate(omnibusfstat = omnibusfsfe$fstat %>% format(digits = 0, nsmall = 1),
                                        omnibusfp = omnibusfsfe$fp %>% format(digits = 0, nsmall = 3))) %>%
            dplyr::mutate(sample = sampname)
        }, spec = specsampdf$spec$spec, specname = specsampdf$spec$name, controls = specsampdf$spec$controls,
        SIMPLIFY = F, USE.NAMES = F) %>% bind_rows()
      }, samprestr = specsampdf$samp$spec, sampname = specsampdf$samp$name,
      SIMPLIFY = F, USE.NAMES = F) %>% bind_rows()
    }, simplify = F, USE.NAMES = T)
}

reg_balplotrdall <- genballist(spec = rdbalspec,
                               samp = rdbalsamprestr,
                               df = balancesphp, dfrhs = rdrhs,
                               dfvar = bind_rows(landsatbalvar, hpbalvar))

reg_balplotrdother <- genballist(spec = rdbalspec %>% filter(name == "RDD*"),
                                 samp = rdbalsamprestr %>% filter(name == "RD"),
                                 df = balancesphp, dfrhs = rdrhs,
                                 dfvar = hpbalvarother)

reg_balhrd <- genballist(spec = rdbalspec,
                         samp = rdhbalsamprestr,
                         df = balanceh, dfrhs = rdrhs,
                         dfvar = hbalvar)

reg_balmipall <- genballist(spec = mipbalspec,
                            samp = mipbalsamprestr,
                            df = balancemiphp, dfrhs = rdrhs,
                            dfvar = bind_rows(landsatbalvar, hpbalmipvar_small))

reg_hrd <- sapply(
  list("ca" = list("spec" = rdhspec, "samp" = rdhsamprestr)),
  function(specsampdf) {
    mapply(function(samprestr, sampname) {
      mapply(function(var, varname, digits) {
        mapply(function(spec, specname, controls) {
          print(paste0(sampname, ",", specname, ",", var))
          dfreg <- analysish %>% filter(!! rlang::parse_expr(samprestr))
          # RDD SPECIFICATIONS
          reg <- felm(paste0(var, spec) %>% as.formula, data = dfreg, keepCX = T, keepModel = T)
          # SFE SPECIFICATIONS
          sfereg <- sfefelm(paste0(var, spec) %>% as.formula, data = dfreg,
                            conley = T, neighborsofneighbors = T)
          # GENERATE SUMMARY DF
          bind_rows(genregsumdf(reg = reg, dfrhs = rdrhs, var = var, varname = varname, digits = digits,
                                spec = spec, specname = specname,
                                controls = controls %>% append(list("Spatial FE" = F))),
                    genregsumdf(reg = sfereg, dfrhs = rdrhs, var = var, varname = varname, digits = digits,
                                spec = spec, specname = specname,
                                controls = controls %>% append(list("Spatial FE" = T)) %>% removesitecontrols,
                                conley = T)) %>% dplyr::mutate(sample = sampname)
        }, spec = specsampdf$spec$spec, specname = specsampdf$spec$name, controls = specsampdf$spec$controls,
        SIMPLIFY = F, USE.NAMES = F) %>% bind_rows()
      }, var = hwelfvar$var, varname = hwelfvar$name, digits = hwelfvar$digits,
      SIMPLIFY = F, USE.NAMES = F) %>% bind_rows()
    }, samprestr = specsampdf$samp$spec, sampname = specsampdf$samp$name,
    SIMPLIFY = F, USE.NAMES = F) %>% bind_rows()
  }, simplify = F, USE.NAMES = T)

reg_plotrd <- sapply(
  list("ca" = list("spec" = rdhpsspec %>% filter(name == "RDD*"),
                   "samp" = rdhpssamprestr)),
  function(specsampdf) {
    mapply(function(samprestr, sampname) {
      mapply(function(var, varname, digits) {
        mapply(function(spec, specname, controls) {
          print(paste0(sampname, ",", specname, ",", var))
          if(var %in% c("attrit_hh", "attrit_trackedsuccess") &
             (grepl("14C", sampname, fixed = T) | grepl("15AB", sampname, fixed = T))) return(NULL)
          dfreg <- analysishps %>% filter(!! rlang::parse_expr(samprestr))
          # RDD SPECIFICATIONS
          reg <- felm(paste0(var, spec) %>% as.formula, data = dfreg, keepCX = T, keepModel = T)
          # SFE SPECIFICATIONS
          sfereg <- sfefelm(paste0(var, spec) %>% as.formula, data = dfreg,
                            conley = T, neighborsofneighbors = T)
          # GENERATE SUMMARY DF
          bind_rows(genregsumdf(reg = reg, dfrhs = rdrhs, var = var, varname = varname, digits = digits,
                                spec = spec, specname = specname,
                                controls = controls %>% append(list("Spatial FE" = F))),
                    genregsumdf(reg = sfereg, dfrhs = rdrhs, var = var, varname = varname, digits = digits,
                                spec = spec, specname = specname,
                                controls = controls %>% append(list("Spatial FE" = T)) %>% removesitecontrols,
                                conley = T)) %>% dplyr::mutate(sample = sampname)
        }, spec = specsampdf$spec$spec, specname = specsampdf$spec$name, controls = specsampdf$spec$controls,
        SIMPLIFY = F, USE.NAMES = F) %>% bind_rows()
      }, var = bind_rows(hpsvar, attritvar)$var,
      varname = bind_rows(hpsvar, attritvar)$name,
      digits = bind_rows(hpsvar, attritvar)$digits,
      SIMPLIFY = F, USE.NAMES = F) %>% bind_rows()
    }, samprestr = specsampdf$samp$spec, sampname = specsampdf$samp$name,
    SIMPLIFY = F, USE.NAMES = F) %>% bind_rows()
  }, simplify = F, USE.NAMES = T)

reg_plotmip <- sapply(
  list("ca" = list(
    "spec" = miphpsspec %>% filter(name == "RDD*"),
    "samp" = miphpssamprestr %>% filter(name %in% c("MIP, Dry season", "MIP, Dry season, 14C only",
                                                    "MIP, Rainy seasons")))),
  function(specsampdf) {
    mapply(function(samprestr, sampname) {
      mapply(function(var, varname, digits) {
        mapply(function(spec, specname, controls) {
          print(paste0(sampname, ",", specname, ",", var))
          if(var %in% c("attrit_hh", "attrit_trackedsuccess") &
             (grepl("14C", sampname, fixed = T) | grepl("15AB", sampname, fixed = T))) return(NULL)
          dfreg <- analysismiphps %>% filter(!! rlang::parse_expr(samprestr))
          # RDD SPECIFICATIONS
          reg <- felm(paste0(var, spec) %>% as.formula, data = dfreg, keepCX = T, keepModel = T)
          # SFE SPECIFICATIONS
          sfereg <- sfefelm(paste0(var, spec) %>% as.formula, data = dfreg,
                            conley = T, neighborsofneighbors = T)
          # GENERATE SUMMARY DF
          bind_rows(genregsumdf(reg = reg, dfrhs = rdrhs, var = var, varname = varname, digits = digits,
                                spec = spec, specname = specname,
                                controls = controls %>% append(list("Spatial FE" = F))),
                    genregsumdf(reg = sfereg, dfrhs = rdrhs, var = var, varname = varname, digits = digits,
                                spec = spec, specname = specname,
                                controls = controls %>% append(list("Spatial FE" = T)) %>% removesitecontrols,
                                conley = T)) %>% dplyr::mutate(sample = sampname)
        }, spec = specsampdf$spec$spec, specname = specsampdf$spec$name, controls = specsampdf$spec$controls,
        SIMPLIFY = F, USE.NAMES = F) %>% bind_rows()
      }, var = hpsvar$var, varname = hpsvar$name, digits = hpsvar$digits,
      SIMPLIFY = F, USE.NAMES = F) %>% bind_rows()
    }, samprestr = specsampdf$samp$spec, sampname = specsampdf$samp$name,
    SIMPLIFY = F, USE.NAMES = F) %>% bind_rows()
  }, simplify = F, USE.NAMES = T)

reg_plotmiphet <- sapply(
  list("ca" = list("spec" = miphethpsspec,
                   "samp" = miphpssamprestr %>% filter(name %in% c("MIP, Dry season", "MIP, Dry season, 14C only",
                                                                   "MIP, Rainy seasons")))),
  function(specsampdf) {
    mapply(function(samprestr, sampname) {
      mapply(function(var, varname, digits) {
        mapply(function(spec, specname, controls) {
          print(paste0(sampname, ",", specname, ",", var))
          dfreg <- analysismiphps %>% filter(!! rlang::parse_expr(samprestr))
          # RDD SPECIFICATIONS
          reg <- felm(paste0(var, spec) %>% as.formula, data = dfreg, keepCX = T, keepModel = T)
          # SFE SPECIFICATIONS
          sfereg <- sfefelm(paste0(var, spec) %>% as.formula, data = dfreg,
                            conley = T, neighborsofneighbors = T)
          # GENERATE SUMMARY DF
          bind_rows(genregsumdf(reg = reg, dfrhs = rdhetrhs, var = var, varname = varname, digits = digits,
                                spec = spec, specname = specname,
                                controls = controls %>% append(list("Spatial FE" = F))),
                    genregsumdf(reg = sfereg, dfrhs = rdhetrhs, var = var, varname = varname, digits = digits,
                                spec = spec, specname = specname,
                                controls = controls %>% append(list("Spatial FE" = T)) %>% removesitecontrols,
                                conley = T)) %>% dplyr::mutate(sample = sampname)
        }, spec = specsampdf$spec$spec, specname = specsampdf$spec$name, controls = specsampdf$spec$controls,
        SIMPLIFY = F, USE.NAMES = F) %>% bind_rows()
      }, var = hpsvar$var, varname = hpsvar$name, digits = hpsvar$digits,
      SIMPLIFY = F, USE.NAMES = F) %>% bind_rows()
    }, samprestr = specsampdf$samp$spec, sampname = specsampdf$samp$name,
    SIMPLIFY = F, USE.NAMES = F) %>% bind_rows()
  }, simplify = F, USE.NAMES = T)

#### ADOPTION DYNAMICS ####

seasonnames <- c("2014 Dry", "2015 Rainy 1", "2015 Rainy 2", "2015 Dry", "2016 Rainy 1",
                 "2016 Rainy 2", "2016 Dry", "2017 Rainy 1", "2017 Rainy 2", "2017 Dry",
                 "2018 Rainy 1", "2018 Rainy 2", "2018 Dry")
adoptiondynamics <- analysishps %>% filter(rdsampgeo & sampleplot == 1) %>%
  split(.$season) %>%
  lapply(function(df) {
    lapply("irrigated",
           function(v) {
             regv <- felm(paste0(v, rdbalspec$spec[rdbalspec$name == "Zero"]) %>% as.formula,
                          data = df %>% dplyr::mutate(casp = as.numeric(casp))) %>% summary()
             meannoca <- df %>% filter(casp == 0) %>% select_at(vars(v)) %>% unlist %>% mean(na.rm = T)
             meanca <- df %>% filter(casp == 1) %>% select_at(vars(v)) %>% unlist %>% mean(na.rm = T)
             data.frame(var = v, loc = c("Outside command area", "Command area"),
                        coef = c(meannoca, meanca), se = c(NA, regv$coef["casp_sp", "Cluster s.e."]))
           }) %>% bind_rows() %>%
      dplyr::mutate(season = unique(df$season))
  }) %>% bind_rows() %>%
  dplyr::mutate(lci = coef + se*qnorm(.025), uci = coef + se*qnorm(.975),
         season = seasonnames[season] %>% factor(levels = seasonnames),
         loc = factor(loc, levels = c("Outside command area", "Command area"))) %>%
  complete(season, var, loc)

#### RD FIGURES ####

rdhpsfigdf <- analysishps %>% filter(rdsampgeo & sampleplot == 1 & season %in% c(7, 10, 13))
rdhpsfig <- mapply(function(var, name, ylab) {
  figreg <- felm(as.formula(paste0(var, " ~ casp_sp + carv_sp + caspxcarv_sp | 0 | 0 | nearwug")),
                 data = rdhpsfigdf)
  figdf <- rdhpsfigdf %>% dplyr::mutate(carv_sp = floor(carv_sp/10)*10 + 5) %>%
    group_by(carv_sp) %>% dplyr::summarise_at(var, function(x) mean(x, na.rm = T)) %>% ungroup
  figdf2 <- data.frame(x = seq(-50, 0, 0.1)) %>%
    dplyr::mutate(y = matrix(c(rep(1, length(x)), rep(0, length(x)), x, rep(0, length(x))),
                      byrow = F, ncol = 4) %*% figreg$coef,
           var = (matrix(c(rep(1, length(x)), rep(0, length(x)), x, rep(0, length(x))),
                         byrow = F, ncol = 4) %*% figreg$clustervcv %*%
                    t(matrix(c(rep(1, length(x)), rep(0, length(x)), x, rep(0, length(x))),
                             byrow = F, ncol = 4))) %>% diag,
           group = "outside") %>%
    bind_rows(data.frame(x = seq(0, 50, 0.1)) %>%
                dplyr::mutate(y = matrix(c(rep(1, length(x)), rep(1, length(x)), x, x),
                                  byrow = F, ncol = 4) %*% figreg$coef,
                       var = (matrix(c(rep(1, length(x)), rep(1, length(x)), x, x),
                                     byrow = F, ncol = 4) %*% figreg$clustervcv %*%
                                t(matrix(c(rep(1, length(x)), rep(1, length(x)), x, x),
                                         byrow = F, ncol = 4))) %>% diag,
                       group = "inside")) %>%
    dplyr::mutate(uci = y + sqrt(var) * qnorm(0.975), lci = y + sqrt(var) * qnorm(0.025))
  ggplot() +
    geom_point(data = figdf, aes_string(x = "carv_sp", y = var)) +
    geom_path(data = figdf2, aes(x = x, y = y, group = group)) +
    geom_path(data = figdf2, aes(x = x, y = uci, group = group), linetype = 2) +
    geom_path(data = figdf2, aes(x = x, y = lci, group = group), linetype = 2) +
    geom_vline(linetype = 2, xintercept = 0) +
    scale_x_continuous(breaks = seq(-40, 40, 20), limits = c(-50, 50), name = "Meters to boundary (SP)") +
    scale_y_continuous(limits = c(0, max(figdf[,var])), name = ylab) +
    theme_bw() + ggtitle(name)
}, var = hpsvar$var, name = hpsvar$figname, ylab = hpsvar$ylab,
SIMPLIFY = F, USE.NAMES = F)

miphpsfigdf <- analysismiphps %>% filter(rdsampgeo_sp & season %in% c(7, 10, 13))
miphpsfig <- mapply(function(var, name, ylab) {
  figreg <- felm(as.formula(paste0(var, " ~ casp_sp + carv_sp + caspxcarv_sp | 0 | 0 | nearwug")),
                 data = rdhpsfigdf)
  figregmip <- felm(as.formula(paste0(var, " ~ casp_sp + carv_sp + caspxcarv_sp | 0 | 0 | nearwug")),
                    data = miphpsfigdf)
  figdf <- rdhpsfigdf %>% dplyr::mutate(carv_sp = floor(carv_sp/10)*10 + 5) %>%
    group_by(carv_sp) %>% dplyr::summarise_at(var, function(x) mean(x, na.rm = T)) %>% ungroup %>%
    dplyr::mutate(col = "sample plot") %>%
    bind_rows(miphpsfigdf %>% dplyr::mutate(carv_sp = floor(carv_sp/10)*10 + 5) %>%
                group_by(carv_sp) %>% dplyr::summarise_at(var, function(x) mean(x, na.rm = T)) %>% ungroup %>%
                dplyr::mutate(col = "mip")) %>%
    dplyr::mutate(col = factor(col, levels = c("sample plot", "mip")))
  figdf2 <- data.frame(x = seq(-50, 0, 0.1)) %>%
    dplyr::mutate(y = matrix(c(rep(1, length(x)), rep(0, length(x)), x, rep(0, length(x))),
                      byrow = F, ncol = 4) %*% figreg$coef,
           var = (matrix(c(rep(1, length(x)), rep(0, length(x)), x, rep(0, length(x))),
                         byrow = F, ncol = 4) %*% figreg$clustervcv %*%
                    t(matrix(c(rep(1, length(x)), rep(0, length(x)), x, rep(0, length(x))),
                             byrow = F, ncol = 4))) %>% diag,
           group = "sample plot, outside", col = "sample plot") %>%
    bind_rows(data.frame(x = seq(0, 50, 0.1)) %>%
                dplyr::mutate(y = matrix(c(rep(1, length(x)), rep(1, length(x)), x, x),
                                  byrow = F, ncol = 4) %*% figreg$coef,
                       var = (matrix(c(rep(1, length(x)), rep(1, length(x)), x, x),
                                     byrow = F, ncol = 4) %*% figreg$clustervcv %*%
                                t(matrix(c(rep(1, length(x)), rep(1, length(x)), x, x),
                                         byrow = F, ncol = 4))) %>% diag,
                       group = "sample plot, inside", col = "sample plot")) %>%
    bind_rows(data.frame(x = seq(-50, 0, 0.1)) %>%
                dplyr::mutate(y = matrix(c(rep(1, length(x)), rep(0, length(x)), x, rep(0, length(x))),
                                  byrow = F, ncol = 4) %*% figregmip$coef,
                       var = (matrix(c(rep(1, length(x)), rep(0, length(x)), x, rep(0, length(x))),
                                     byrow = F, ncol = 4) %*% figregmip$clustervcv %*%
                                t(matrix(c(rep(1, length(x)), rep(0, length(x)), x, rep(0, length(x))),
                                         byrow = F, ncol = 4))) %>% diag,
                       group = "mip, outside", col = "mip")) %>%
    bind_rows(data.frame(x = seq(0, 50, 0.1)) %>%
                dplyr::mutate(y = matrix(c(rep(1, length(x)), rep(1, length(x)), x, x),
                                  byrow = F, ncol = 4) %*% figregmip$coef,
                       var = (matrix(c(rep(1, length(x)), rep(1, length(x)), x, x),
                                     byrow = F, ncol = 4) %*% figregmip$clustervcv %*%
                                t(matrix(c(rep(1, length(x)), rep(1, length(x)), x, x),
                                         byrow = F, ncol = 4))) %>% diag,
                       group = "mip, inside", col = "mip")) %>%
    dplyr::mutate(uci = y + sqrt(var) * qnorm(0.975), lci = y + sqrt(var) * qnorm(0.025)) %>%
    dplyr::mutate(col = factor(col, levels = c("sample plot", "mip")))
  ggplot() +
    geom_point(data = figdf, aes_string(x = "carv_sp", y = var, col = "col")) +
    geom_path(data = figdf2, aes(x = x, y = y, group = group, col = col)) +
    geom_path(data = figdf2, aes(x = x, y = uci, group = group, col = col), linetype = 2) +
    geom_path(data = figdf2, aes(x = x, y = lci, group = group, col = col), linetype = 2) +
    geom_vline(linetype = 2, xintercept = 0) +
    scale_x_continuous(breaks = seq(-40, 40, 20), limits = c(-50, 50), name = "Meters to boundary (SP)") +
    scale_y_continuous(limits = c(0, max(figdf[,var])), name = ylab) +
    scale_color_manual(values = c("sample plot" = "black", "mip" = watercols5[3]),
                       labels = c("Sample plot", "Largest other plot")) +
    theme_bw() + theme(legend.direction = "horizontal", legend.title =element_blank()) +
    ggtitle(name)
}, var = c(hpscropvar$var, hpsinputvar$var), name = c(hpscropvar$figname, hpsinputvar$figname),
ylab = c(hpscropvar$ylab, hpsinputvar$ylab),
SIMPLIFY = F, USE.NAMES = F)
miphpsfiglegend <- cowplot::get_legend(
  miphpsfig[[1]] + 
    guides(color = guide_legend(override.aes = list(linetype = NA, size = 4))) +
    theme(legend.position = "top", legend.text = element_text(size = 16))
)
miphpsfig <- lapply(miphpsfig, function(i) i + theme(legend.position = "none"))

#### MINIKIT ####

reg_mk_h <- mapply(function(var, varname, digits) {
  mapply(function(spec, controls) {
    dfreg <- analysish %>%
      filter(mksamp & !is.na(wug_sp) & survey %in% c(2, 3))
    reg <- felm(paste0(var, spec) %>% as.formula, data = dfreg, keepModel = T)
    genregsumdf(reg = reg, dfrhs = mkrhs, var = var, varname = varname, digits = digits,
                spec = spec, specname = NA, controls = controls,
                forcecontrolmean = T)
  }, spec = mkspec$spec, controls = mkspec$controls,
  SIMPLIFY = F, USE.NAMES = F) %>% bind_rows()
}, var = mkvar$var[1], varname = mkvar$name[1], digits = mkvar$digits[1],
SIMPLIFY = F, USE.NAMES = F) %>% bind_rows()
reg_mk_hps <- mapply(function(var, varname, digits) {
  mapply(function(spec, controls) {
    dfreg <- analysishps %>%
      filter(mksamp & sampleplot == 1 & !is.na(wug_sp) & season %% 3 == 1 & season >= 8)
    reg <- felm(paste0(var, spec) %>% as.formula, data = dfreg, keepModel = T)
    genregsumdf(reg = reg, dfrhs = mkrhs, var = var, varname = varname, digits = digits,
                spec = spec, specname = NA, controls = controls,
                forcecontrolmean = T)
  }, spec = mkspec$spec, controls = mkspec$controls,
  SIMPLIFY = F, USE.NAMES = F) %>% bind_rows()
}, var = mkvar$var[2:3], varname = mkvar$name[2:3], digits = mkvar$digits[2:3],
SIMPLIFY = F, USE.NAMES = F) %>% bind_rows()
reg_mk <- bind_rows(reg_mk_h, reg_mk_hps)

omrhs <- data.frame(var = "farmermonitor_sp", name = "Farmer monitor")
sbrhs <- data.frame(var = "totalsubs", name = "Subsidy")

reg_om <- list(
  felm(daysnotenoughwater ~ farmermonitor_sp | mksat_sp + zone_sp | 0 | wug_sp,
       data = analysishps %>% filter(sampleplot == 1 & !is.na(wug_sp) & season %in% c(7, 10, 13)) %>%
         dplyr::mutate(farmermonitor_sp = as.numeric(farmermonitor_sp)), keepModel = T) %>%
    genregsumdf(dfrhs = omrhs, var = "daysnotenoughwater",
                varname = "Days w/o enough water", digits = 2,
                spec = " ~ farmermonitor_sp | mksat_sp + zone_sp | 0 | wug_sp",
                specname = NA,
                controls = list("\\# of subsidy lotteries entered" = F,
                                "Minikit saturation" = T, "Zone FE" = T)),
  felm(daysirrigated ~ farmermonitor_sp | mksat_sp + zone_sp | 0 | wug_sp,
       data = analysishps %>% filter(sampleplot == 1 & !is.na(wug_sp) & season %in% c(7, 10, 13)) %>%
         dplyr::mutate(farmermonitor_sp = as.numeric(farmermonitor_sp),
                # MAKING COMPARABLE TO daysnotenoughwater
                daysirrigated = ifelse(irrigated == 0, NA, daysirrigated)), keepModel = T) %>%
    genregsumdf(dfrhs = omrhs, var = "daysirrigated", varname = "Days irrigated", digits = 2,
                spec = " ~ farmermonitor_sp | mksat_sp + zone_sp | 0 | wug_sp",
                specname = NA,
                controls = list("\\# of subsidy lotteries entered" = F,
                                "Minikit saturation" = T, "Zone FE" = T)),
  felm(irrigated ~ farmermonitor_sp | mksat_sp + zone_sp | 0 | wug_sp,
       data = analysishps %>% filter(sampleplot == 1 & !is.na(wug_sp) & season %in% c(7, 10, 13)) %>%
         dplyr::mutate(farmermonitor_sp = as.numeric(farmermonitor_sp)), keepModel = T) %>%
    genregsumdf(dfrhs = omrhs, var = "irrigated", varname = "Irrigated", digits = 3,
                spec = " ~ farmermonitor_sp | mksat_sp + zone_sp | 0 | wug_sp",
                specname = NA,
                controls = list("\\# of subsidy lotteries entered" = F,
                                "Minikit saturation" = T, "Zone FE" = T)),
  felm(maincrphort ~ farmermonitor_sp | mksat_sp + zone_sp | 0 | wug_sp,
       data = analysishps %>% filter(sampleplot == 1 & !is.na(wug_sp) & season %in% c(7, 10, 13)) %>%
         dplyr::mutate(farmermonitor_sp = as.numeric(farmermonitor_sp)), keepModel = T) %>%
    genregsumdf(dfrhs = omrhs, var = "maincrphort", varname = "Horticulture", digits = 3,
                spec = " ~ farmermonitor_sp | mksat_sp + zone_sp | 0 | wug_sp",
                specname = NA,
                controls = list("\\# of subsidy lotteries entered" = F,
                                "Minikit saturation" = T, "Zone FE" = T))
) %>% bind_rows()

reg_omsb_meansubs <- balanceh %>%
  filter(sampleplot == 1 & !is.na(wug_sp)) %>%
  dplyr::mutate(totalsubs = reglotteryff2 * 1 + reglotteryfn2 * 0.5 +
           reglotteryhn2 * 0.25 + reglotterynn2 * 0) %>%
  filter(totalsubs > 0) %>%
  select(totalsubs) %>% unlist %>% mean(na.rm = T)

reg_sb <- list(
  felm(feesowed ~ totalsubs | nsubsidylottery + zone_sp + mksat_sp | 0 | wug_sp,
       data = balanceh %>%
         filter(sampleplot == 1 & !is.na(wug_sp)) %>%
         left_join(analysishs %>% filter(season %in% 8:9) %>%
                     group_by(hhcode) %>% dplyr::summarise(feesowed = sum(fullfees - subsfees)) %>%
                     ungroup) %>%
         dplyr::mutate(totalsubs = (reglotteryff2 * 1 + reglotteryfn2 * 0.5 +
                  reglotteryhn2 * 0.25 + reglotterynn2 * 0) / reg_omsb_meansubs), keepModel = T) %>%
    genregsumdf(dfrhs = sbrhs, var = "feesowed", varname = "Fees owed (Admin)", digits = 2,
                spec = " ~ totalsubs | nsubsidylottery + zone_sp + mksat_sp | 0 | wug_sp",
                specname = NA,
                controls = list("\\# of subsidy lotteries entered" = T,
                                "Minikit saturation" = T, "Zone FE" = T),
                forcecontrolmean = T),
  felm(paidfees ~ totalsubs | nsubsidylottery + zone_sp + mksat_sp | 0 | wug_sp,
       data = balanceh %>%
         filter(sampleplot == 1 & !is.na(wug_sp)) %>%
         left_join(analysishs %>% filter(season %in% 8:9) %>%
                     group_by(hhcode) %>% dplyr::summarise(paidfees = sum(paidfees)) %>%
                     ungroup) %>%
         dplyr::mutate(totalsubs = (reglotteryff2 * 1 + reglotteryfn2 * 0.5 +
                  reglotteryhn2 * 0.25 + reglotterynn2 * 0) / reg_omsb_meansubs), keepModel = T) %>%
    genregsumdf(dfrhs = sbrhs, var = "paidfees", varname = "Fees paid (Admin)", digits = 2,
                spec = " ~ totalsubs | nsubsidylottery + zone_sp + mksat_sp | 0 | wug_sp",
                specname = NA,
                controls = list("\\# of subsidy lotteries entered" = T,
                                "Minikit saturation" = T, "Zone FE" = T),
                forcecontrolmean = T),
  felm(irrigated ~ totalsubs | nsubsidylottery + zone_sp + mksat_sp | 0 | wug_sp,
       data = analysishps %>% filter(sampleplot == 1 & !is.na(wug_sp) & season %in% c(10, 13)) %>%
         dplyr::mutate(totalsubs = (reglotteryff2 * 1 + reglotteryfn2 * 0.5 +
                  reglotteryhn2 * 0.25 + reglotterynn2 * 0) / reg_omsb_meansubs), keepModel = T) %>%
    genregsumdf(dfrhs = sbrhs, var = "irrigated", varname = "Irrigated", digits = 3,
                spec = " ~ totalsubs | nsubsidylottery + zone_sp + mksat_sp | 0 | wug_sp",
                specname = NA,
                controls = list("\\# of subsidy lotteries entered" = T,
                                "Minikit saturation" = T, "Zone FE" = T),
                forcecontrolmean = T)
) %>% bind_rows()
reg_omsb <- bind_rows(reg_sb, reg_om)

#### LANDSAT ####

reg_landsat <- mapply(function(var, varname, digits) {
  dflandsatpre <- analysislandsat %>% filter(y <= 2008 & rdsampgeo)
  dflandsatpost <- analysislandsat %>% filter(y >= 2015 & rdsampgeo)
  regs <- list(
    felm(paste0(var, landsatspec$spec[1]) %>% as.formula, data = dflandsatpre),
    felm(paste0(var, landsatspec$spec[2]) %>% as.formula, data = dflandsatpre),
    felm(paste0(var, landsatspec$spec[1]) %>% as.formula, data = dflandsatpost),
    felm(paste0(var, landsatspec$spec[2]) %>% as.formula, data = dflandsatpost),
    felm(paste0(var, landsatspec$spec[3]) %>% as.formula, data = dflandsatpost),
    felm(paste0(var, landsatspec$spec[4]) %>% as.formula, data = dflandsatpost)
  )
  regspecs <- landsatspec$spec[c(1, 2, 1, 2, 3, 4)]
  regcontrols <- landsatspec$controls[c(1, 2, 1, 2, 3, 4)]
  regpre <- c(T, T, F, F, F, F)
  mapply(function(reg, spec, controls, pre, i) {
    if(pre) {
      controls <- append(list("Pre-construction (Year $\\leq$2008)" = T,
                              "Post-construction (Year $\\geq$2015)" = F), controls)
    } else {
      controls <- append(list("Pre-construction (Year $\\leq$2008)" = F,
                              "Post-construction (Year $\\geq$2015)" = T), controls)
    }
    genregsumdf(reg = reg, dfrhs = landsatrhs, var = var,
                varname = varname, digits = digits,
                spec = i,
                specname = i, controls = controls)
  }, reg = regs, spec = regspecs, controls = regcontrols, pre = regpre, i = 1:6,
  SIMPLIFY = F, USE.NAMES = F) %>% bind_rows()
}, var = landsatvar$var, varname = landsatvar$name, digits = landsatvar$digits,
SIMPLIFY = F, USE.NAMES = F) %>% bind_rows()

reg_landsatmip <- mapply(function(var, varname, digits) {
  dflandsatmippre <- analysismiplandsat %>% filter(y <= 2008 & rdsampgeo_sp)
  dflandsatmippost <- analysismiplandsat %>% filter(y >= 2015 & rdsampgeo_sp)
  regs <- list(
    felm(paste0(var, landsatmipspec$spec[1]) %>% as.formula, data = dflandsatmippre),
    felm(paste0(var, landsatmipspec$spec[2]) %>% as.formula, data = dflandsatmippre),
    felm(paste0(var, landsatmipspec$spec[1]) %>% as.formula, data = dflandsatmippost),
    felm(paste0(var, landsatmipspec$spec[2]) %>% as.formula, data = dflandsatmippost)
  )
  regspecs <- landsatmipspec$spec[c(1, 2, 1, 2)]
  regcontrols <- landsatmipspec$controls[c(1, 2, 1, 2)]
  regpre <- c(T, T, F, F)
  mapply(function(reg, spec, controls, pre, i) {
    if(pre) {
      controls <- append(list("Pre-construction (Year $\\leq$2008)" = T,
                              "Post-construction (Year $\\geq$2015)" = F), controls)
    } else {
      controls <- append(list("Pre-construction (Year $\\leq$2008)" = F,
                              "Post-construction (Year $\\geq$2015)" = T), controls)
    }
    genregsumdf(reg = reg, dfrhs = landsatrhs, var = var,
                varname = varname, digits = digits,
                spec = i,
                specname = i, controls = controls)
  }, reg = regs, spec = regspecs, controls = regcontrols, pre = regpre, i = 1:4,
  SIMPLIFY = F, USE.NAMES = F) %>% bind_rows()
}, var = landsatvar$var, varname = landsatvar$name, digits = landsatvar$digits,
SIMPLIFY = F, USE.NAMES = F) %>% bind_rows()


#### 5) OUTPUT ####

#### HELPER FUNCTIONS ####

genrdsumtex <- function(regsumdflist, depvar = NULL, inserts = NULL, clearnaming = F,
                        rhs = "SP CA",
                        clearnamingdepvarheader = "Dependent variable") {
  if(length(depvar) == 0) depvar <- setNames(rep(T, length(regsumdflist)), names(regsumdflist))
  stopifnot(identical(names(regsumdflist), names(depvar)))
  # NAMES OF LIST ARE HEADERS
  rdsumcols <- lapply(names(regsumdflist), function(n) {
    regsumdflist[[n]] %>%
      select(spec, controls) %>% unique %>%
      dplyr::mutate(group = n) %>% dplyr::mutate(i = row_number() == 1 & depvar[n])
  }) %>% bind_rows() %>% dplyr::mutate(i = row_number() + cumsum(i))
  rdsumrows <- regsumdflist %>% bind_rows %>% select(varname) %>% unlist %>% unique
  rdsumcontrols <- names(unlist(bind_rows(regsumdflist)$controls)) %>% unique
  headers <- c(paste0("\\begin{tabular}{l", "l"[clearnaming],
                      rep("c", nrow(rdsumcols) + sum(depvar)) %>%
                        paste(collapse = ""), "}"),
               "\\hline \\hline",
               # MAIN HEADERS
               paste0(" & ", "& "[clearnaming],
                      sapply(names(regsumdflist), function(groupi) {
                        paste0("\\multicolumn{", depvar[groupi] + sum(rdsumcols$group == groupi),
                               "}{c}{", groupi, "}")
                      }) %>% paste(collapse = " & "), " \\\\"),
               # MAIN CMIDRULE
               sapply(names(regsumdflist), function(groupi) {
                 paste0("\\cmidrule(lr){",
                        1 - depvar[groupi] + clearnaming + min(rdsumcols$i[which(rdsumcols$group == groupi)]),
                        "-", 1 + clearnaming + max(rdsumcols$i[which(rdsumcols$group == groupi)]), "}")
               }) %>% paste(collapse = ""))
  gencoefheaderrow <- function(x1, x2) {
    paste0(" & ", "& "[clearnaming],
           sapply(names(regsumdflist), function(groupi) {
             paste0(ifelse(depvar[groupi], paste0(x1, " & "), ""),
                    "\\multicolumn{",
                    sum(rdsumcols$group == groupi), "}{c}{", x2, "}")
           }) %>% paste(collapse = " & "), " \\\\")
  }
  if(!clearnaming) {
    # COEF HEADERS
    headers <- c(headers, gencoefheaderrow("Dep. var.", "Coef. (SE) [p]"))
  } else {
    # COEF HEADERS
    headers <- c(headers,
                 gencoefheaderrow("Dep. var. mean", paste0(rhs, " Coef.")),
                 gencoefheaderrow("(Dep. var. SD)", "(SE)"),
                 gencoefheaderrow("\\# of obs.", "[p-value]"))
  }
  headers <- c(headers,
               # COEF CMIDRULE
               sapply(names(regsumdflist), function(groupi) {
                 paste0("\\cmidrule(lr){",
                        1 + clearnaming + min(rdsumcols$i[which(rdsumcols$group == groupi)]),
                        "-", 1 + clearnaming + max(rdsumcols$i[which(rdsumcols$group == groupi)]), "}")
               }) %>% paste(collapse = ""),
               # COLUMN NUMBERS
               paste0(" & ", "& "[clearnaming],
                      paste0("(", 1:(nrow(rdsumcols) + sum(depvar)), ")") %>%
                        paste(collapse = " & "), " \\\\"),
               "\\hline")
  if(clearnaming) {
    headers <- c(headers,
                 paste0("\\multicolumn{2}{l}{", clearnamingdepvarheader, "} ",
                        rep("& ", nrow(rdsumcols) + sum(depvar)) %>%
                          paste(collapse = ""), "\\\\"),
                 "\\cmidrule(lr){1-2}")
  }
  meat <- lapply(rdsumrows, function(rowi) {
    meatlist <- sapply(unique(bind_rows(regsumdflist)$tvarname), function(tj) {
      coefsep <- lapply(names(regsumdflist), function(n) {
        regsumdflistnij <- regsumdflist[[n]] %>% select(sample, spec, sfe) %>% distinct %>%
          left_join(regsumdflist[[n]] %>%
                      select(sample, spec, sfe, tvarname, varname, coef, se, p,
                             controlmean, controlsd, n) %>%
                      subset(tvarname == tj & varname == rowi))
        nij <- list(coef = ifelse(is.na(regsumdflistnij$coef), "", regsumdflistnij$coef),
                    se = ifelse(is.na(regsumdflistnij$se), "", paste0("(", regsumdflistnij$se, ")")),
                    p = ifelse(is.na(regsumdflistnij$p), "", paste0("[", regsumdflistnij$p, "]")))
        if(depvar[n] & any(!is.na(regsumdflistnij$controlmean))) {
          nij$coef <- c(first(na.omit(regsumdflistnij$controlmean)), nij$coef)
          nij$se <- c(paste0("(", first(na.omit(regsumdflistnij$controlsd)), ")"), nij$se)
          nij$p <- c(first(na.omit(regsumdflistnij$n)), nij$p)
        } else if(depvar[n]) {
          nij$coef <- c("", nij$coef); nij$se <- c("", nij$se); nij$p <- c("", nij$p)
        }
        return(nij)
      })
      coefsep <- list(coef = lapply(coefsep, function(x) x$coef) %>% unlist,
                      se = lapply(coefsep, function(x) x$se) %>% unlist,
                      p = lapply(coefsep, function(x) x$p) %>% unlist)
      return(coefsep)
    }, USE.NAMES = T, simplify = F)
    if(length(meatlist) == 1) {
      if(rowi == last(rdsumrows) & all(sapply(regsumdflist, function(x) "omnibusfstat" %in% names(x)))) {
        omnibusfstats <- lapply(names(regsumdflist), function(n) {
          omnibusfstatsn <- regsumdflist[[n]] %>% select(sample, spec, sfe) %>% distinct
          omnibusfstatsn <- sapply(1:nrow(omnibusfstatsn), function(i) {
            omnibusfstatsni <- regsumdflist[[n]] %>%
              subset(sample == omnibusfstatsn$sample[i] & spec == omnibusfstatsn$spec[i] &
                       sfe == omnibusfstatsn$sfe[i])
            c("omnibusfstat" = first(omnibusfstatsni$omnibusfstat),
              "omnibusfp" = paste0("[", first(omnibusfstatsni$omnibusfp), "]"))
          })
          omnibusfstatsn <- list("omnibusfstat" = omnibusfstatsn["omnibusfstat",],
                                 "omnibusfp" = omnibusfstatsn["omnibusfp",])
          if(depvar[n]) {
            omnibusfstatsn$omnibusfstat <- c("", omnibusfstatsn$omnibusfstat)
            omnibusfstatsn$omnibusfp <- c("", omnibusfstatsn$omnibusfp)
          }
          return(omnibusfstatsn)
        })
        omnibusfstats <- list("omnibusfstat" = lapply(omnibusfstats, function(x) x$omnibusfstat) %>% unlist,
                              "omnibusfp" = lapply(omnibusfstats, function(x) x$omnibusfp) %>% unlist)
        omnibusfstats <- c(paste0("\\multicolumn{2}{l}{"[clearnaming],
                                  "Omnibus F-stat [p]", "}"[clearnaming],
                                  " & ",
                                  paste(omnibusfstats$omnibusfstat, collapse = " & "), " \\\\"),
                           paste0("& "[clearnaming], " & ",
                                  paste(omnibusfstats$omnibusfp, collapse = " & "), " \\\\[0.5em]"))
      } else {
        omnibusfstats <- NULL
      }
      c(paste0(" & "[clearnaming],
               rowi, " & ", meatlist[[1]]$coef %>% paste(collapse = " & "), " \\\\"),
        paste0(" & ", "& "[clearnaming],
               meatlist[[1]]$se %>% paste(collapse = " & "), " \\\\"),
        paste0(" & ", "& "[clearnaming],
               meatlist[[1]]$p %>% paste(collapse = " & "), " \\\\",
               ifelse(rowi == last(rdsumrows) & is.null(omnibusfstats), "", "[0.5em]")),
        omnibusfstats)
    } else {
      fstats <- lapply(names(regsumdflist), function(n) {
        fstatsn <- regsumdflist[[n]] %>% select(sample, spec, sfe) %>% distinct
        fstatsn <- sapply(1:nrow(fstatsn), function(i) {
          fstatsni <- regsumdflist[[n]] %>%
            subset(sample == fstatsn$sample[i] & spec == fstatsn$spec[i] & sfe == fstatsn$sfe[i] &
                     varname == rowi)
          if(nrow(fstatsni) > 1) c("fstat" = first(fstatsni$fstat), "fp" = paste0("[", first(fstatsni$fp), "]"))
          else c("fstat" = "", "fp" = "")
        })
        fstatsn <- list("fstat" = fstatsn["fstat",], "fp" = fstatsn["fp",])
        if(depvar[n]) {
          fstatsn$fstat <- c("", fstatsn$fstat); fstatsn$fp <- c("", fstatsn$fp)
        }
        return(fstatsn)
      })
      fstats <- list("fstat" = lapply(fstats, function(x) x$fstat) %>% unlist,
                     "fp" = lapply(fstats, function(x) x$fp) %>% unlist)
      if(all(fstats$fstat == "")) {
        fstats <- NULL
      } else {
        fstats <- c(paste0(" & "[clearnaming], "\\hspace{1em}Joint F-stat [p] & ", paste(fstats$fstat, collapse = " & "),
                           " \\\\"),
                    paste0(" &"[clearnaming], " & ", paste(fstats$fp, collapse = " & "), " \\\\",
                           ifelse(rowi == last(rdsumrows), "", "[0.5em]")))
      }
      c(paste0(" & "[clearnaming], rowi,
               rep(" &", length(meatlist[[1]]$coef)) %>% paste(collapse = ""), " \\\\"),
        ifelse(clearnaming, "\\cmidrule(lr){2-2}", "\\cmidrule(lr){1-1}"),
        lapply(1:length(meatlist), function(i) {
          c(paste0(" & "[clearnaming], "\\hspace{1em}", names(meatlist)[[i]], " & ",
                   meatlist[[i]]$coef %>% paste(collapse = " & "), " \\\\"),
            paste0(" &"[clearnaming], " & ", meatlist[[i]]$se %>% paste(collapse = " & "), " \\\\"),
            paste0(" &"[clearnaming], " & ", meatlist[[i]]$p %>% paste(collapse = " & "), " \\\\",
                   ifelse(i == length(meatlist) & rowi == last(rdsumrows) & is.null(fstats), "", "[0.5em]")))
        }) %>% unlist %>% unname,
        fstats)
    }
  }) %>% unlist
  if(length(inserts) > 0) {
    meat <- lapply(1:length(meat), function(i) {
      if(i %in% names(inserts)) c(inserts[[which(names(inserts) == i)]], meat[i])
      else meat[i]
    }) %>% unlist
  }
  controls <- sapply(rdsumcontrols, function(i) {
    paste0(" & "[clearnaming], i, " & ",
           sapply(names(regsumdflist), function(groupi) {
             paste0(ifelse(depvar[groupi], " & ", ""),
                    ifelse(rdsumcols[rdsumcols$group == groupi, "controls"] %>%
                             sapply(function(x) identical(x[[i]], T)),
                           "X", "") %>% paste(collapse = " & "))
           }) %>% paste(collapse = " & "), " \\\\")
  }) %>% unlist %>% unname
  if(clearnaming) {
    controls <- c(paste0("\\multicolumn{2}{l}{Controls} ",
                         rep("& ", nrow(rdsumcols) + sum(depvar)) %>%
                           paste(collapse = ""), "\\\\"),
                  "\\cmidrule(lr){1-2}",
                  controls)
  }
  c(headers, meat, "\\hline", controls, "\\hline", "\\end{tabular}")
}

mergeregsumtex <- function(tab1, tab2, subheader1, subheader2,
                           tab1subfooter = NULL, tab2subfooter = NULL,
                           meatrow1 = NULL, meatrow2 = NULL, footerrow1 = NULL, footerrow2 = NULL,
                           checkheader = T, checkfooter = T) {
  if(length(meatrow1) == 0) {
    meatrow1 <- which(grepl("\\hline", tab1))[2] + 1
  }
  if(length(meatrow2) == 0) {
    meatrow2 <- which(grepl("\\hline", tab2))[2] + 1
  }
  if(length(footerrow1) == 0) {
    footerrow1 <- which(grepl("\\hline", tab1))[3]
  }
  if(length(footerrow2) == 0) {
    footerrow2 <- which(grepl("\\hline", tab2))[3]
  }
  subfooterrow1 <- NULL; subfooterrow2 <- NULL
  tabncols <- tab1[which(grepl("\\begin{tabular}", tab1, fixed = T))] %>%
    gsub(pattern = "\\begin{tabular}{", replacement = "", fixed = T) %>%
    gsub(pattern = "}", replacement = "", fixed = T) %>%
    trimws %>% nchar
  if(length(tab1subfooter) > 0 & length(tab2subfooter) > 0) {
    subfooterrow1 <- which(grepl("\\hline", tab1subfooter))[3]
    subfooterrow1 <- c(paste0("\\cmidrule(lr){1-", tabncols, "}"),
                       tab1subfooter[(subfooterrow1 + 3):(length(tab1subfooter)-2)],
                       paste0("\\cmidrule(lr){1-", tabncols, "}"))
    subfooterrow2 <- which(grepl("\\hline", tab2subfooter))[3]
    subfooterrow2 <- c(paste0("\\cmidrule(lr){1-", tabncols, "}"),
                       tab2subfooter[(subfooterrow2 + 3):(length(tab2subfooter)-2)])
  }
  header1 <- tab1[1:(meatrow1 - 1)]
  header2 <- tab2[1:(meatrow2 - 1)]
  meat1 <- tab1[meatrow1:(footerrow1 - 1)]
  meat2 <- tab2[meatrow2:(footerrow2 - 1)]
  footer1 <- tab1[footerrow1:(length(tab1))]
  footer2 <- tab2[footerrow2:(length(tab2))]
  stopifnot(identical(header1, header2) | !checkheader)
  stopifnot(identical(footer1, footer2) | !checkfooter)
  c(header1, subheader1, meat1, subfooterrow1, subheader2, meat2, subfooterrow2, footer1)
}

genmergeregsumtex <- function(regsumdf1, regsumdf2, subheader1, subheader2,
                              headertitles = NULL, footerrows = NULL, subfooterrows = NULL, jointf = F, includecontrols = T,
                              checkheader = T, checkfooter = T) {
  tab1subfooter <- NULL
  tab2subfooter <- NULL
  if(length(subfooterrows) > 0) {
    tab1subfooter <- genregsumtex(regsumdf1, headertitles = headertitles, footerrows = subfooterrows, jointf = jointf,
                                  includecontrols = F)
    tab2subfooter <- genregsumtex(regsumdf2, headertitles = headertitles, footerrows = subfooterrows, jointf = jointf,
                                  includecontrols = F)
  }
  mergeregsumtex(tab1 = genregsumtex(regsumdf1, headertitles = headertitles, footerrows = footerrows, jointf = jointf,
                                     includecontrols = includecontrols),
                 tab2 = genregsumtex(regsumdf2, headertitles = headertitles, footerrows = footerrows, jointf = jointf,
                                     includecontrols = includecontrols),
                 tab1subfooter = tab1subfooter, tab2subfooter = tab2subfooter,
                 subheader1 = subheader1, subheader2 = subheader2,
                 checkheader = checkheader, checkfooter = checkfooter)
}

genregsumtex <- function(regsumdf, headertitles = NULL, subheadertitles = NULL, footertitles = NULL,
                         footerrows = NULL, jointf = F, includecontrols = T, regsumrows = NULL,
                         regsumcontrols = NULL) {
  regsumcols <- regsumdf %>% select(c("varname", "spec", "n", "nclu", "controls", "fstat"[jointf], "fp"[jointf],
                                      unname(footerrows))) %>% unique.data.frame %>%
    dplyr::mutate(i = row_number(), vari = ifelse(i == 1, 1, varname != lag(varname)) %>% cumsum)
  stopifnot(nrow(regsumcols) == nrow(regsumcols %>% select(varname, spec) %>% distinct))
  stopifnot(nrow(regsumcols %>% select(varname, vari) %>% distinct) == length(unique(regsumcols$vari)))
  # FIGURING OUT RIGHT ORDER OF RHS VARIABLES
  if(length(regsumrows) == 0) {
    regsumrows <- regsumdf %>% group_by(varname, spec) %>% dplyr::mutate(trank = row_number()) %>%
      ungroup %>% group_by(tvarname) %>% dplyr::summarise(trank = max(trank)) %>% ungroup %>%
      arrange(trank) %>% select(tvarname) %>% unlist %>% unique
  }
  if(length(regsumcontrols) == 0) {
    regsumcontrols <- names(unlist(regsumdf$controls)) %>% unique
  }
  if(length(headertitles) == 0) {
    headertitles <- paste0(" & ",
                           sapply(unique(regsumcols$vari), function(varii) {
                             paste0("\\multicolumn{", sum(regsumcols$vari == varii), "}{c}{",
                                    unique(regsumcols$varname[which(regsumcols$vari == varii)]),
                                    "}")
                           }) %>% paste(collapse = " & "), " \\\\")
  }
  headers <- c(paste0("\\begin{tabular}{l", rep("c", nrow(regsumcols)) %>% paste(collapse = ""), "}"),
               "\\hline \\hline",
               # LHS HEADERS
               headertitles,
               # LHS CMIDRULE
               sapply(unique(regsumcols$vari), function(varii) {
                 paste0("\\cmidrule(lr){", 1 + min(regsumcols$i[which(regsumcols$vari == varii)]),
                        "-", 1 + max(regsumcols$i[which(regsumcols$vari == varii)]), "}")
               }) %>% paste(collapse = ""),
               # LHS COLUMN NUMBERS
               paste0(" & ", paste0("(", regsumcols$i, ")") %>% paste(collapse = " & "), " \\\\"),
               "\\hline",
               subheadertitles)
  meat <- lapply(regsumrows, function(rowi) {
    regsumdfi <- regsumdf %>% filter(tvarname == rowi)
    j <- lapply(1:nrow(regsumcols), function(k) which(regsumdfi$varname == regsumcols$varname[k] &
                                                        regsumdfi$spec == regsumcols$spec[k]))
    coef <- lapply(j, function(k) {
      if(length(k) == 0) return("")
      else return(ifelse(grepl("-", regsumdfi$coef[k]),
                         paste0(regsumdfi$coef[k], "\\hphantom{-}"),
                         regsumdfi$coef[k]))
    })
    se <- lapply(j, function(k) ifelse(length(regsumdfi$se[k]) == 0, "", paste0("(", regsumdfi$se[k], ")")))
    p <- lapply(j, function(k) ifelse(length(regsumdfi$p[k]) == 0, "", paste0("[", regsumdfi$p[k], "]")))
    c(paste0(rowi, " & ", coef %>% paste(collapse = " & "), " \\\\"),
      paste0(" & ", se %>% paste(collapse = " & "), " \\\\"),
      paste0(" & ", p %>% paste(collapse = " & "), " \\\\"))
  }) %>% unlist
  if(jointf) {
    meatjointf <- c(paste0("Joint F-stat [p] & ", regsumcols$fstat %>% paste(collapse = " & "), " \\\\"),
                    paste0(" & [", regsumcols$fp %>% paste(collapse = "] & ["), "] \\\\"))
  } else {
    meatjointf <- NULL
  }
  # CHANGED CODING OF INCLUDECONTROLS SO NAMES DON'T NEED TO LINE UP
  #   IF NOT INCLUDING CONTROLS
  controls <- NULL
  if(includecontrols) {
    controls <- sapply(regsumcontrols, function(i) {
      paste0(i, " & ",
             sapply(regsumcols$controls, function(j) {
               if(i %in% names(j)) ifelse(j[i], "X", "") else ""
             }) %>% paste(collapse = " & "),
             " \\\\")
    }) %>% unlist
  }
  nobs <- c(paste0("\\# of observations & ", regsumcols$n %>% paste(collapse = " & "), " \\\\"),
            paste0("\\# of clusters & ", regsumcols$nclu %>% paste(collapse = " & "), " \\\\"))
  if(length(footerrows) > 0) {
    footerrows <- lapply(1:length(footerrows), function(i) {
      paste0(names(footerrows)[i], " & ", regsumcols[,footerrows[i]] %>% unlist %>% paste(collapse = " & "), " \\\\")
    })
  }
  c(headers, meat, meatjointf, "\\hline", footertitles, controls, nobs, unlist(footerrows), "\\hline", "\\end{tabular}") %>% unlist
} 

#### STYLIZED FACTS ####

writeLines(styfactdfout, "tabfig/tabs/croptab.tex")

regout_xseccontrol <- reg_xseccontrol %>% genregsumtex
writeLines(regout_xseccontrol, "tabfig/tabs/reg_xseccontrol.tex")

#### RD ####

cleancontrols <- function(x) {
  if(class(x) == "list") {
    sapply(x, function(df) {
      df %>% dplyr::mutate(controls = lapply(controls, function(x) {
        names(x) <- names(x) %>% gsub(pattern = "MIP", replacement = "LOP") %>%
          gsub(pattern = "^Distance", replacement = "SP distance") %>%
          gsub(pattern = "^log", replacement = "SP log") %>%
          gsub(pattern = "^CA", replacement = "SP CA") %>%
          gsub(pattern = "log area", replacement = "log GPS area") %>%
          gsub(pattern = " * log GPS area", replacement = " * SP log GPS area", fixed = T) %>%
          gsub(pattern = " * Distance", replacement = " * SP distance", fixed = T)
        return(x)
      }))
    }, simplify = F, USE.NAMES = T)
  } else {
    x %>% dplyr::mutate(controls = lapply(controls, function(x) {
      names(x) <- names(x) %>% gsub(pattern = "MIP", replacement = "LOP") %>%
        gsub(pattern = "^Distance", replacement = "SP distance") %>%
        gsub(pattern = "^log", replacement = "SP log") %>%
        gsub(pattern = "^CA", replacement = "SP CA") %>%
        gsub(pattern = "log area", replacement = "log GPS area") %>%
        gsub(pattern = " * log GPS area", replacement = " * SP log GPS area", fixed = T) %>%
        gsub(pattern = " * Distance", replacement = " * SP distance", fixed = T)
      return(x)
    }))
  }
}

regout_balplotrdall_small <- list(
  "Full sample" = reg_balplotrdall$ca %>%
    filter(sample == "Zero" & specname == "Zero" & !sfe),
  "Discontinuity sample" = reg_balplotrdall$ca %>%
    filter(sample == "RD" & specname %in% c("RDD", "RDD*")) %>%
    dplyr::mutate(specno = match(specname, c("RDD", "RDD*")) + 0.5 * sfe) %>%
    arrange(specno)
) %>% cleancontrols %>%
  lapply(function(df) {
    df %>% dplyr::mutate(varname = varname %>% gsub(pattern = "log area", replacement = "log GPS area"))
  })
writeLines(regout_balplotrdall_small %>%
             genrdsumtex(depvar = c("Full sample" = F, "Discontinuity sample" = T),
                         inserts = list("10" = c(paste0("\\multicolumn{2}{l}{Dep. var. (SP, Baseline)} ",
                                                        paste(rep("& ", 6), collapse = ""), " \\\\"),
                                                 "\\cmidrule(lr){1-2}")),
                         clearnaming = T,
                         clearnamingdepvarheader = "Dep. var. (SP, Landsat)"),
           "tabfig/tabs/reg_balplotrdall_small.tex")


regout_balhrd_small <- list(
  "Full sample" = reg_balhrd$ca %>%
    filter(sample == "Zero" & specname == "Zero" & !sfe),
  "Discontinuity sample" = reg_balhrd$ca %>%
    filter(sample == "RD" & specname %in% c("RDD", "RDD*")) %>%
    dplyr::mutate(specno = match(specname, c("RDD", "RDD*")) + 0.5 * sfe) %>%
    arrange(specno)
) %>% cleancontrols
writeLines(regout_balhrd_small %>%
             genrdsumtex(depvar = c("Full sample" = F, "Discontinuity sample" = T), inserts = NULL,
                         clearnaming = T,
                         clearnamingdepvarheader = "Dep. var. (HH, Baseline)"),
           "tabfig/tabs/reg_balhrd_small.tex")

genbalmiprobtables <- function(reg_bal, specadd, filedescr,
                               depvar = c("Full sample" = F, "Discontinuity sample" = T),
                               rhs = "SP CA", inserts = NULL,
                               clearnamingdepvarheader = "Dependent variable") {
  if(!("controlmean" %in% names(reg_bal$ca))) {
    reg_bal$ca$controlmean <- NA
    reg_bal$ca$controlsd <- NA
  }
  regout_bal <- list(
    "Full sample" = reg_bal$ca %>%
      filter(sample == "Zero" & specname == paste0("Zero", specadd) & !sfe),
    "Discontinuity sample" = reg_bal$ca %>%
      filter(sample == "MIP" & specname %in% paste0(c("RDD", "RDD*"), specadd)) %>%
      dplyr::mutate(specno = match(specname, paste0(c("RDD", "RDD*"), specadd)) + 0.5 * sfe) %>%
      arrange(specno)
  ) %>% cleancontrols
  writeLines(regout_bal %>%
               genrdsumtex(depvar = depvar, inserts = inserts, clearnaming = T,
                           rhs = rhs, clearnamingdepvarheader = clearnamingdepvarheader),
             paste0("tabfig/tabs/reg_balmip_",
                    filedescr, ".tex"))
}

genbalmiprobtables(reg_balmipall %>%
                     lapply(function(df) {
                       df %>% dplyr::mutate(varname = varname %>% gsub(pattern = "log area", replacement = "log GPS area"))
                     }), "", "all_small",
                   inserts = list("10" = c(paste0("\\multicolumn{2}{l}{Dep. var. (LOP, Baseline)} ",
                                                  paste(rep("& ", 6), collapse = ""), " \\\\"),
                                           "\\cmidrule(lr){1-2}")),
                   clearnamingdepvarheader = "Dep. var. (LOP, Landsat)")

regout_hrd_small <- reg_hrd$ca %>%
  dplyr::mutate(tvarname = tvarname %>% gsub(pattern = "CA", replacement = "SP CA")) %>%
  filter(sample == "RD" & specname == "RDD*")
writeLines(genmergeregsumtex(regsumdf1 = regout_hrd_small %>% filter(!sfe),
                             regsumdf2 = regout_hrd_small %>% filter(sfe),
                             subheader1 = c("\\multicolumn{4}{l}{RDD (Site-by-survey FE, Specification \\ref{eq:specrdd})} & \\\\",
                                            "\\cmidrule(lr){1-4}"),
                             subheader2 = c("\\\\[-0.5em]",
                                            "\\multicolumn{4}{l}{SFE (Spatial FE, Specification \\ref{eq:specsfe})} & \\\\",
                                            "\\cmidrule(lr){1-4}"),
                             headertitles = c(" & \\multicolumn{4}{c}{HH, Discontinuity sample} \\\\",
                                              "\\cmidrule(lr){2-5}",
                                              " & Housing & Asset & Food & Overall \\\\",
                                              " & expenditures & index & security & index \\\\",
                                              " & & & index & \\\\"),
                             footerrows = c("Control mean" = "controlmean"),
                             includecontrols = F, checkfooter = F),
           "tabfig/tabs/reg_hrd_small.tex")

regout_balplotrdother <- reg_balplotrdother$ca %>%
  dplyr::mutate(tvarname = tvarname %>% gsub(pattern = "CA", replacement = "SP CA"))
writeLines(genmergeregsumtex(regsumdf1 = regout_balplotrdother %>% filter(!sfe),
                             regsumdf2 = regout_balplotrdother %>% filter(sfe),
                             subheader1 = c("\\multicolumn{3}{l}{RDD (Site FE, Specification \\ref{eq:specrdd})} & \\\\",
                                            "\\cmidrule(lr){1-3}"),
                             subheader2 = c("\\\\[-0.5em]",
                                            "\\multicolumn{3}{l}{SFE (Spatial FE, Specification \\ref{eq:specsfe})} & \\\\",
                                            "\\cmidrule(lr){1-3}"),
                             headertitles = c(" & \\multicolumn{3}{c}{SP, Baseline, Discontinuity sample} \\\\",
                                              "\\cmidrule(lr){2-4}",
                                              " & Terraced & Rented out, & Elevation \\\\",
                                              " & & comm. farmer & \\\\"),
                             footerrows = c("Control mean" = "controlmean"),
                             includecontrols = F, checkfooter = F),
           "tabfig/tabs/reg_balplotrdother_small.tex")

regout_plotrd_small <- list(
  "Dry season" = reg_plotrd$ca %>%
    filter(sample == "RD, Dry season" & specname == "RDD*"),
  "Rainy seasons" = reg_plotrd$ca %>%
    filter(sample == "RD, Rainy seasons" & specname == "RDD*")
) %>%
  lapply(function(df) {
    df %>% filter(var %in% c("cultivated", "irrigated", "maincrphort", "maincrpbanana", "hhlabha", "inputexpha",
                             "hiredlabexpha", "yield", "salesha", "profits", "profits800"))
  }) %>%
  lapply(function(df) {
    df %>% dplyr::mutate(tvarname = tvarname %>% gsub(pattern = "CA", replacement = "SP CA"))
  })

writeLines(genmergeregsumtex(regsumdf1 = regout_plotrd_small %>% `$`("Dry season") %>% filter(!sfe),
                             regsumdf2 = regout_plotrd_small %>% `$`("Dry season") %>% filter(sfe),
                             subheader1 = c("\\multicolumn{4}{l}{RDD (Site-by-season FE, Specification \\ref{eq:specrdd})} & & & & & & & & \\\\",
                                            "\\cmidrule(lr){1-4}"),
                             subheader2 = c("\\\\[-0.5em]",
                                            "\\multicolumn{4}{l}{SFE (Spatial FE, Specification \\ref{eq:specsfe})} & & & & & & & & \\\\",
                                            "\\cmidrule(lr){1-4}"),
                             headertitles = c(" & \\multicolumn{11}{c}{SP, Dry season, Discontinuity sample} \\\\",
                                              "\\cmidrule(lr){2-12}",
                                              paste0(" & Culti- & Irri- & Horti- & Banana & HH & Input & Hired & Yield & Sales & ",
                                                     "\\multicolumn{2}{c}{Profits/ha} \\\\"),
                                              " & vated & gated & culture & & labor/ & exp./ha & labor & & /ha & \\multicolumn{2}{c}{Shadow wage} \\\\",
                                              " & & & & & ha & & exp./ha & & & = 0 & = 800 \\\\"),
                             footerrows = c("Control mean" = "controlmean"),
                             includecontrols = F, checkfooter = F),
           "tabfig/tabs/reg_plotrd_smalldry.tex")

writeLines(genmergeregsumtex(regsumdf1 = regout_plotrd_small %>% `$`("Rainy seasons") %>% filter(!sfe),
                             regsumdf2 = regout_plotrd_small %>% `$`("Rainy seasons") %>% filter(sfe),
                             subheader1 = c("\\multicolumn{4}{l}{RDD (Site-by-season FE, Specification \\ref{eq:specrdd})} & & & & & & & & \\\\",
                                            "\\cmidrule(lr){1-4}"),
                             subheader2 = c("\\\\[-0.5em]",
                                            "\\multicolumn{4}{l}{SFE (Spatial FE, Specification \\ref{eq:specsfe})} & & & & & & & & \\\\",
                                            "\\cmidrule(lr){1-4}"),
                             headertitles = c(" & \\multicolumn{11}{c}{SP, Rainy seasons, Discontinuity sample} \\\\",
                                              "\\cmidrule(lr){2-12}",
                                              paste0(" & Culti- & Irri- & Horti- & Banana & HH & Input & Hired & Yield & Sales & ",
                                                     "\\multicolumn{2}{c}{Profits/ha} \\\\"),
                                              " & vated & gated & culture & & labor/ & exp./ha & labor & & /ha & \\multicolumn{2}{c}{Shadow wage} \\\\",
                                              " & & & & & ha & & exp./ha & & & = 0 & = 800 \\\\"),
                             footerrows = c("Control mean" = "controlmean"),
                             includecontrols = F, checkfooter = F),
           "tabfig/tabs/reg_plotrd_smallrainy.tex")


regout_plotrd <- list(
  "Dry season" = reg_plotrd$ca %>%
    filter(sample == "RD, Dry season" &
             ((specname %in% c("Base", "RDD*") & !sfe) | (specname == "RDD*" & sfe))) %>%
    dplyr::mutate(specno = match(specname, c("Base", "RDD*")) + sfe) %>% arrange(specno),
  "Rainy seasons" = reg_plotrd$ca %>%
    filter(sample == "RD, Rainy seasons" &
             ((specname %in% c("Base", "RDD*") & !sfe) | (specname == "RDD*" & sfe))) %>%
    dplyr::mutate(specno = match(specname, c("Base", "RDD*")) + sfe) %>% arrange(specno)
) %>%
  lapply(function(df) {
    df %>% dplyr::mutate(tvarname = tvarname %>% gsub(pattern = "CA", replacement = "SP CA"))
  }) %>% cleancontrols
writeLines(regout_plotrd %>%
             (function(x) { names(x) <- paste0("Discontinuity sample, ", names(x)); return(x) }) %>%
             sapply(function(x) x %>% filter(var %in% attritvar$var), USE.NAMES = T, simplify = F) %>%
             genrdsumtex(depvar = c("Discontinuity sample, Dry season" = T,
                                    "Discontinuity sample, Rainy seasons" = T),
                         inserts = list("7" = c(" & Reason data is missing & & & & & & \\\\",
                                                "\\cmidrule(lr){2-2}")),
                         clearnaming = T,
                         clearnamingdepvarheader = "Dep. var. (SP)"),
           "tabfig/tabs/reg_plotrdattrit.tex")

regout_plotmip_smallnoca <- list(
  "Dry season" = reg_plotmip$ca %>%
    filter(sample == "MIP, Dry season" & specname == "RDD*" &
             var %in% c("cultivated", "irrigated", "maincrphort", "maincrpbanana", "hhlabha", "inputexpha",
                        "hiredlabexpha")) %>%
    left_join(reg_plotrd$ca %>%
                filter(sample == "RD, Dry season" & specname == "RDD*") %>%
                select(c("var", "sfe", "sampleploteffect" = "coef"))),
  "Rainy seasons" = reg_plotmip$ca %>%
    filter(sample == "MIP, Rainy seasons" & specname == "RDD*" &
             var %in% c("cultivated", "irrigated", "maincrphort", "maincrpbanana", "hhlabha", "inputexpha",
                        "hiredlabexpha")) %>%
    left_join(reg_plotrd$ca %>%
                filter(sample == "RD, Rainy seasons" & specname == "RDD*") %>%
                select(c("var", "sfe", "sampleploteffect" = "coef")))
) %>%
  lapply(function(df) {
    df %>% dplyr::mutate(tvarname = tvarname %>% gsub(pattern = "CA", replacement = "SP CA"))
  })
writeLines(genmergeregsumtex(regsumdf1 = regout_plotmip_smallnoca %>%
                               `$`("Dry season") %>% filter(!sfe),
                             regsumdf2 = regout_plotmip_smallnoca %>%
                               `$`("Dry season") %>% filter(sfe),
                             subheader1 = c("\\multicolumn{4}{l}{RDD (Site-by-season FE, Specification \\ref{eq:specLOPrdd})} & & & & \\\\",
                                            "\\cmidrule(lr){1-4}"),
                             subheader2 = c("\\\\[-0.5em]",
                                            "\\multicolumn{4}{l}{SFE (Spatial FE, Specification \\ref{eq:specLOPsfe})} & & & & \\\\",
                                            "\\cmidrule(lr){1-4}"),
                             headertitles = c(" & \\multicolumn{7}{c}{LOP, Dry season, Discontinuity sample} \\\\",
                                              "\\cmidrule(lr){2-8}",
                                              paste0(" & Culti- & Irri- & Horti- & Banana & HH & Input & Hired \\\\"),
                                              " & vated & gated & culture & & labor/ & exp./ha & labor \\\\",
                                              " & & & & & ha & & exp./ha \\\\"),
                             subfooterrows = c("Sample plot effect" = "sampleploteffect"),
                             footerrows = c("Control mean" = "controlmean"),
                             jointf = F, includecontrols = F, checkfooter = F),
           "tabfig/tabs/reg_plotmip_smallnoca.tex")
writeLines(genmergeregsumtex(regsumdf1 = regout_plotmip_smallnoca %>%
                               `$`("Rainy seasons") %>% filter(!sfe),
                             regsumdf2 = regout_plotmip_smallnoca %>%
                               `$`("Rainy seasons") %>% filter(sfe),
                             subheader1 = c("\\multicolumn{4}{l}{RDD (Site-by-season FE, Specification \\ref{eq:specLOPrdd})} & & & & \\\\",
                                            "\\cmidrule(lr){1-4}"),
                             subheader2 = c("\\\\[-0.5em]",
                                            "\\multicolumn{4}{l}{SFE (Spatial FE, Specification \\ref{eq:specLOPsfe})} & & & & \\\\",
                                            "\\cmidrule(lr){1-4}"),
                             headertitles = c(" & \\multicolumn{7}{c}{LOP, Rainy seasons, Discontinuity sample} \\\\",
                                              "\\cmidrule(lr){2-8}",
                                              paste0(" & Culti- & Irri- & Horti- & Banana & HH & Input & Hired \\\\"),
                                              " & vated & gated & culture & & labor/ & exp./ha & labor \\\\",
                                              " & & & & & ha & & exp./ha \\\\"),
                             subfooterrows = c("Sample plot effect" = "sampleploteffect"),
                             footerrows = c("Control mean" = "controlmean"),
                             jointf = F, includecontrols = F, checkfooter = F),
           "tabfig/tabs/reg_plotmip_smallnocarainy.tex")

regout_plotmip_small <- reg_plotmiphet$ca %>%
  filter(sample == "MIP, Dry season" & specname == "RDD* CA") %>%
  left_join(reg_plotmip$ca %>%
              filter(sample == "MIP, Dry season" & specname == "RDD*") %>%
              select(c("var", "sfe", "averageeffect" = "coef"))) %>%
  left_join(reg_plotrd$ca %>%
              filter(sample == "RD, Dry season" & specname == "RDD*") %>%
              select(c("var", "sfe", "sampleploteffect" = "coef"))) %>%
  (function(df) {
    df %>% filter(var %in% c("cultivated", "irrigated", "maincrphort", "maincrpbanana", "hhlabha", "inputexpha",
                             "hiredlabexpha"))
  }) %>%
  dplyr::mutate(controlmean = zoo::na.locf(controlmean), controlsd = zoo::na.locf(controlsd)) %>%
  dplyr::mutate(tvarname = tvarname %>% gsub(pattern = "CA", replacement = "SP CA") %>%
           gsub(pattern = "MIP SP CA", replacement = "LOP CA"))

writeLines(genmergeregsumtex(regsumdf1 = regout_plotmip_small %>% filter(!sfe),
                             regsumdf2 = regout_plotmip_small %>% filter(sfe),
                             subheader1 = c("\\multicolumn{4}{l}{RDD (Site-by-season FE, Specification \\ref{eq:specLOPrdd})} & & & & \\\\",
                                            "\\cmidrule(lr){1-4}"),
                             subheader2 = c("\\\\[-0.5em]",
                                            "\\multicolumn{4}{l}{SFE (Spatial FE, Specification \\ref{eq:specLOPsfe})} & & & & \\\\",
                                            "\\cmidrule(lr){1-4}"),
                             headertitles = c(" & \\multicolumn{7}{c}{LOP, Dry season, Discontinuity sample} \\\\",
                                              "\\cmidrule(lr){2-8}",
                                              paste0(" & Culti- & Irri- & Horti- & Banana & HH & Input & Hired \\\\"),
                                              " & vated & gated & culture & & labor/ & exp./ha & labor \\\\",
                                              " & & & & & ha & & exp./ha \\\\"),
                             subfooterrows = c("Sample plot effect" = "sampleploteffect", "Average effect" = "averageeffect"),
                             footerrows = c("Control mean" = "controlmean"),
                             jointf = T, includecontrols = F, checkfooter = F),
           "tabfig/tabs/reg_plotmip_small.tex")

regout_plotmip_smallrainy <- reg_plotmiphet$ca %>%
  filter(sample == "MIP, Rainy seasons" & specname == "RDD* CA") %>%
  left_join(reg_plotmip$ca %>%
              filter(sample == "MIP, Rainy seasons" & specname == "RDD*") %>%
              select(c("var", "sfe", "averageeffect" = "coef"))) %>%
  left_join(reg_plotrd$ca %>%
              filter(sample == "RD, Rainy seasons" & specname == "RDD*") %>%
              select(c("var", "sfe", "sampleploteffect" = "coef"))) %>%
  (function(df) {
    df %>% filter(var %in% c("cultivated", "irrigated", "maincrphort", "maincrpbanana", "hhlabha", "inputexpha",
                             "hiredlabexpha"))
  }) %>%
  dplyr::mutate(controlmean = zoo::na.locf(controlmean), controlsd = zoo::na.locf(controlsd)) %>%
  dplyr::mutate(tvarname = tvarname %>% gsub(pattern = "CA", replacement = "SP CA") %>%
           gsub(pattern = "MIP SP CA", replacement = "LOP CA"))

writeLines(genmergeregsumtex(regsumdf1 = regout_plotmip_smallrainy %>% filter(!sfe),
                             regsumdf2 = regout_plotmip_smallrainy %>% filter(sfe),
                             subheader1 = c("\\multicolumn{4}{l}{RDD (Site-by-season FE, Specification \\ref{eq:specLOPrdd})} & & & & \\\\",
                                            "\\cmidrule(lr){1-4}"),
                             subheader2 = c("\\\\[-0.5em]",
                                            "\\multicolumn{4}{l}{SFE (Spatial FE, Specification \\ref{eq:specLOPsfe})} & & & & \\\\",
                                            "\\cmidrule(lr){1-4}"),
                             headertitles = c(" & \\multicolumn{7}{c}{LOP, Rainy seasons, Discontinuity sample} \\\\",
                                              "\\cmidrule(lr){2-8}",
                                              paste0(" & Culti- & Irri- & Horti- & Banana & HH & Input & Hired \\\\"),
                                              " & vated & gated & culture & & labor/ & exp./ha & labor \\\\",
                                              " & & & & & ha & & exp./ha \\\\"),
                             subfooterrows = c("Sample plot effect" = "sampleploteffect", "Average effect" = "averageeffect"),
                             footerrows = c("Control mean" = "controlmean"),
                             jointf = T, includecontrols = F, checkfooter = F),
           "tabfig/tabs/reg_plotmip_smallrainy.tex")


regout_plotmiphet_small <- reg_plotmiphet$ca %>%
  filter(sample == "MIP, Dry season" & specname == "RDD* LM") %>%
  left_join(reg_plotmip$ca %>%
              filter(sample == "MIP, Dry season" & specname == "RDD*") %>%
              select(c("var", "sfe", "averageeffect" = "coef"))) %>%
  (function(df) {
    df %>% filter(var %in% c("cultivated", "irrigated", "maincrphort", "maincrpbanana", "hhlabha", "inputexpha",
                             "hiredlabexpha"))
  }) %>%
  dplyr::mutate(controlmean = zoo::na.locf(controlmean), controlsd = zoo::na.locf(controlsd)) %>%
  dplyr::mutate(tvarname = tvarname %>% gsub(pattern = "CA", replacement = "SP CA"))

writeLines(genmergeregsumtex(regsumdf1 = regout_plotmiphet_small %>% filter(!sfe),
                             regsumdf2 = regout_plotmiphet_small %>% filter(sfe),
                             subheader1 = c("\\multicolumn{3}{l}{RDD (Site-by-season FE, Specification \\ref{eq:specLOPhet})} & & & & & \\\\",
                                            "\\cmidrule(lr){1-3}"),
                             subheader2 = c("\\\\[-0.5em]",
                                            "\\multicolumn{3}{l}{SFE (Spatial FE, Specification \\ref{eq:specLOPhet})} & & & & & \\\\",
                                            "\\cmidrule(lr){1-3}"),
                             headertitles = c(" & \\multicolumn{7}{c}{LOP, Dry season, Discontinuity sample} \\\\",
                                              "\\cmidrule(lr){2-8}",
                                              paste0(" & Culti- & Irri- & Horti- & Banana & HH & Input & Hired \\\\"),
                                              " & vated & gated & culture & & labor/ & exp./ha & labor \\\\",
                                              " & & & & & ha & & exp./ha \\\\"),
                             subfooterrows = c("Average effect" = "averageeffect"),
                             footerrows = c("Control mean" = "controlmean"),
                             jointf = T, includecontrols = F, checkfooter = F),
           "tabfig/tabs/reg_plotmiphet_small.tex")

regout_bl_plotrd_small <- list(
  "Dry season" = reg_plotrd$ca %>%
    filter(sample == "RD, Dry season, 14C only" & specname == "RDD*"),
  "Rainy seasons" = reg_plotrd$ca %>%
    filter(sample == "RD, Rainy seasons, 15AB only" & specname == "RDD*")
) %>%
  lapply(function(df) {
    df %>% filter(var %in% c("cultivated", "irrigated", "maincrphort", "maincrpbanana", "hhlabha", "inputexpha",
                             "hiredlabexpha", "yield", "salesha", "profits", "profits800"))
  }) %>%
  lapply(function(df) {
    df %>% dplyr::mutate(tvarname = tvarname %>% gsub(pattern = "CA", replacement = "SP CA"))
  })

writeLines(genmergeregsumtex(regsumdf1 = regout_bl_plotrd_small %>% `$`("Dry season") %>% filter(!sfe),
                             regsumdf2 = regout_bl_plotrd_small %>% `$`("Dry season") %>% filter(sfe),
                             subheader1 = c("\\multicolumn{4}{l}{RDD (Site-by-season FE, Specification \\ref{eq:specrdd})} & & & & & & & & \\\\",
                                            "\\cmidrule(lr){1-4}"),
                             subheader2 = c("\\\\[-0.5em]",
                                            "\\multicolumn{4}{l}{SFE (Spatial FE, Specification \\ref{eq:specsfe})} & & & & & & & & \\\\",
                                            "\\cmidrule(lr){1-4}"),
                             headertitles = c(" & \\multicolumn{11}{c}{SP, Baseline, Dry season, Discontinuity sample} \\\\",
                                              "\\cmidrule(lr){2-12}",
                                              paste0(" & Culti- & Irri- & Horti- & Banana & HH & Input & Hired & Yield & Sales & ",
                                                     "\\multicolumn{2}{c}{Profits/ha} \\\\"),
                                              " & vated & gated & culture & & labor/ & exp./ha & labor & & /ha & \\multicolumn{2}{c}{Shadow wage} \\\\",
                                              " & & & & & ha & & exp./ha & & & = 0 & = 800 \\\\"),
                             footerrows = c("Control mean" = "controlmean"),
                             includecontrols = F, checkfooter = F),
           "tabfig/tabs/reg_BL_plotrd_smalldry.tex")

writeLines(genmergeregsumtex(regsumdf1 = regout_bl_plotrd_small %>% `$`("Rainy seasons") %>% filter(!sfe),
                             regsumdf2 = regout_bl_plotrd_small %>% `$`("Rainy seasons") %>% filter(sfe),
                             subheader1 = c("\\multicolumn{4}{l}{RDD (Site-by-season FE, Specification \\ref{eq:specrdd})} & & & & & & & & \\\\",
                                            "\\cmidrule(lr){1-4}"),
                             subheader2 = c("\\\\[-0.5em]",
                                            "\\multicolumn{4}{l}{SFE (Spatial FE, Specification \\ref{eq:specsfe})} & & & & & & & & \\\\",
                                            "\\cmidrule(lr){1-4}"),
                             headertitles = c(" & \\multicolumn{11}{c}{SP, Baseline, Rainy seasons, Discontinuity sample} \\\\",
                                              "\\cmidrule(lr){2-12}",
                                              paste0(" & Culti- & Irri- & Horti- & Banana & HH & Input & Hired & Yield & Sales & ",
                                                     "\\multicolumn{2}{c}{Profits/ha} \\\\"),
                                              " & vated & gated & culture & & labor/ & exp./ha & labor & & /ha & \\multicolumn{2}{c}{Shadow wage} \\\\",
                                              " & & & & & ha & & exp./ha & & & = 0 & = 800 \\\\"),
                             footerrows = c("Control mean" = "controlmean"),
                             includecontrols = F, checkfooter = F),
           "tabfig/tabs/reg_BL_plotrd_smallrainy.tex")

regout_bl_plotmipseason_small <- reg_plotmip$ca %>%
  filter(sample == "MIP, Dry season, 14C only" & specname == "RDD*") %>%
  filter(var %in% c("cultivated", "irrigated", "maincrphort", "maincrpbanana", "hhlabha", "inputexpha",
                    "hiredlabexpha")) %>%
  dplyr::mutate(tvarname = tvarname %>% gsub(pattern = "CA", replacement = "SP CA"))

writeLines(genmergeregsumtex(regsumdf1 = regout_bl_plotmipseason_small %>% filter(!sfe),
                             regsumdf2 = regout_bl_plotmipseason_small %>% filter(sfe),
                             subheader1 = c("\\multicolumn{4}{l}{RDD (Site-by-season FE, Specification \\ref{eq:specLOPrdd})} & & & & \\\\",
                                            "\\cmidrule(lr){1-4}"),
                             subheader2 = c("\\\\[-0.5em]",
                                            "\\multicolumn{4}{l}{SFE (Spatial FE, Specification \\ref{eq:specLOPsfe})} & & & & \\\\",
                                            "\\cmidrule(lr){1-4}"),
                             headertitles = c(" & \\multicolumn{7}{c}{LOP, Baseline, Dry season, Discontinuity sample} \\\\",
                                              "\\cmidrule(lr){2-8}",
                                              paste0(" & Culti- & Irri- & Horti- & Banana & HH & Input & Hired \\\\"),
                                              " & vated & gated & culture & & labor/ & exp./ha & labor \\\\",
                                              " & & & & & ha & & exp./ha \\\\"),
                             footerrows = c("Control mean" = "controlmean"),
                             includecontrols = F, checkfooter = F),
           "tabfig/tabs/reg_BL_plotmip_smallnocadry.tex")

regout_bl_plotmip_small <- reg_plotmiphet$ca %>%
  filter(sample == "MIP, Dry season, 14C only" & specname == "RDD* CA") %>%
  left_join(reg_plotmip$ca %>%
              filter(sample == "MIP, Dry season, 14C only" & specname == "RDD*") %>%
              select(c("var", "sfe", "averageeffect" = "coef"))) %>%
  left_join(reg_plotrd$ca %>%
              filter(sample == "RD, Dry season, 14C only" & specname == "RDD*") %>%
              select(c("var", "sfe", "sampleploteffect" = "coef"))) %>%
  (function(df) {
    df %>% filter(var %in% c("cultivated", "irrigated", "maincrphort", "maincrpbanana", "hhlabha", "inputexpha",
                             "hiredlabexpha"))
  }) %>%
  dplyr::mutate(controlmean = zoo::na.locf(controlmean), controlsd = zoo::na.locf(controlsd)) %>%
  dplyr::mutate(tvarname = tvarname %>% gsub(pattern = "CA", replacement = "SP CA") %>%
           gsub(pattern = "MIP SP CA", replacement = "LOP CA"))

writeLines(genmergeregsumtex(regsumdf1 = regout_bl_plotmip_small %>% filter(!sfe),
                             regsumdf2 = regout_bl_plotmip_small %>% filter(sfe),
                             subheader1 = c("\\multicolumn{4}{l}{RDD (Site-by-season FE, Specification \\ref{eq:specLOPrdd})} & & & & \\\\",
                                            "\\cmidrule(lr){1-4}"),
                             subheader2 = c("\\\\[-0.5em]",
                                            "\\multicolumn{4}{l}{SFE (Spatial FE, Specification \\ref{eq:specLOPsfe})} & & & & \\\\",
                                            "\\cmidrule(lr){1-4}"),
                             headertitles = c(" & \\multicolumn{7}{c}{LOP, Baseline, Dry season, Discontinuity sample} \\\\",
                                              "\\cmidrule(lr){2-8}",
                                              paste0(" & Culti- & Irri- & Horti- & Banana & HH & Input & Hired \\\\"),
                                              " & vated & gated & culture & & labor/ & exp./ha & labor \\\\",
                                              " & & & & & ha & & exp./ha \\\\"),
                             subfooterrows = c("Sample plot effect" = "sampleploteffect", "Average effect" = "averageeffect"),
                             footerrows = c("Control mean" = "controlmean"),
                             jointf = T, includecontrols = F, checkfooter = F),
           "tabfig/tabs/reg_BL_plotmip_small.tex")

#### FLIP THE MIP ####

genflippedtex <- function(n, mipn, shortn) {
  refsp <- "\\ref{eq:specrddflipgeneric} \\& \\ref{eq:specLOPrddflipgeneric}"
  reflop <- "\\ref{eq:specrddflipgeneric} \\& \\ref{eq:specLOPrddflipgeneric}"
  if(n == "Sample plot") {
    refsp <- "\\ref{eq:specrdd} \\& \\ref{eq:specLOPrdd}"
    reflop <- "\\ref{eq:specsfe} \\& \\ref{eq:specLOPsfe}"
  } else if(n == "Largest other plot") {
    refsp <- "\\ref{eq:specrdd} \\& \\ref{eq:specLOPrdd}"
    reflop <- "\\ref{eq:specsfe} \\& \\ref{eq:specLOPsfe}"
  }
  genmergeregsumtex(regsumdf1 = flippedreg[[n]] %>% filter(!sfe),
                    regsumdf2 = flippedreg[[n]] %>% filter(sfe),
                    subheader1 = c(paste0("\\multicolumn{3}{l}{RDD (Site-by-season FE, Specifications ",
                                          refsp ,")} & \\\\"),
                                   "\\cmidrule(lr){1-3}"),
                    subheader2 = c("\\\\[-0.5em]",
                                   paste0("\\multicolumn{3}{l}{SFE (Spatial FE, Specifications ",
                                          reflop, ")} & \\\\"),
                                   "\\cmidrule(lr){1-3}"),
                    headertitles = c(paste0(" & \\multicolumn{3}{c}{Dry season, ", shortn, " discontinuity sample} \\\\"),
                                     "\\cmidrule(lr){2-4}",
                                     paste0(" & ", n, " & \\multicolumn{2}{c}{", mipn, "} \\\\"),
                                     "\\cmidrule(lr){2-2}\\cmidrule(lr){3-4}",
                                     " & \\multicolumn{3}{c}{Irrigated} \\\\"),
                    jointf = T, includecontrols = F, checkfooter = F)
}

writeLines(genflippedtex("Sample plot", "Largest other plot", "SP"),
           "tabfig/tabs/altsp_sp.tex")
writeLines(genflippedtex("Largest other plot", "Sample plot", "LOP"),
           "tabfig/tabs/altsp_mip.tex")


#### PRICES AND WAGES ####

pricefig <- prices %>%
  inner_join(bind_rows(prices %>% filter(hort) %>%
                         group_by(sitedist, crp_name) %>% dplyr::summarise(nobs = sum(n)) %>% ungroup %>%
                         group_by(sitedist) %>% filter(nobs >= nobs[order(-nobs)[2]]) %>% ungroup,
                       prices %>% filter(staple) %>%
                         group_by(sitedist, crp_name) %>% dplyr::summarise(nobs = sum(n)) %>% ungroup %>%
                         group_by(sitedist) %>% filter(nobs >= nobs[order(-nobs)[2]]) %>% ungroup,
                       prices %>% filter(banana) %>%
                         group_by(sitedist, crp_name) %>% dplyr::summarise(nobs = sum(n)) %>% ungroup %>%
                         group_by(sitedist) %>% filter(nobs >= nobs[order(-nobs)[1]]) %>% ungroup)) %>%
  dplyr::mutate(cropcat = case_when(hort ~ "Horticulture", staple ~ "Staple", banana ~ "Banana"))

ggsave(filename = "tabfig/figs/prices_karongi.pdf",
       width = 5, height = 4,
       pricefig %>% filter(sitedist == "k") %>%
         dplyr::mutate(season = factor(season, levels = 1:13)) %>%
         complete(season, nesting(crp_name, cropcat), fill = list(p = NA)) %>%
         dplyr::mutate(season = season %>% plyr::mapvalues(from = 1:13, to = seasonnames)) %>%
         ggplot(aes(x = season, y = p, group = crp_name, col = cropcat, linetype = crp_name,
                    shape = crp_name)) +
         scale_color_manual(values = c("Banana" = watercols5[2], "Horticulture" = watercols5[3],
                                       "Staple" = watercols5[4]), name = NULL) +
         scale_linetype_manual(values = c("Bananas for beer" = 1, "Carrots: Karoti" = 2,
                                          "Dry Beans: Ibishyimbo Byumye" = 1,
                                          "Maize: Ibigori" = 2, "Onions: Ibitunguru" = 1),
                               guide = F) +
         scale_shape_manual(values = c("Bananas for beer" = 16, "Carrots: Karoti" = 17,
                                       "Dry Beans: Ibishyimbo Byumye" = 16,
                                       "Maize: Ibigori" = 17, "Onions: Ibitunguru" = 16),
                            guide = F) +
         geom_path() + geom_point() +
         annotate(geom = "text",
                  label = c("Bananas\nfor beer", "Carrots", "Dry beans", "Maize", "Onions"),
                  x = rep(15.5, 5), y = c(100, 150, 333, 200 - 10, 212 + 10),
                  col = watercols5[c(2, 3, 4, 4, 3)], hjust = 1.1) +
         theme_bw() + xlab(NULL) +
         theme(axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1), legend.position = "top") +
         scale_y_log10(name = "RwF/kg"))

ggsave(filename = "tabfig/figs/prices_nyanza.pdf",
       width = 5, height = 4,
       pricefig %>% filter(sitedist == "n") %>%
         dplyr::mutate(season = factor(season, levels = 1:13)) %>%
         complete(season, nesting(crp_name, cropcat), fill = list(p = NA)) %>%
         dplyr::mutate(season = season %>% plyr::mapvalues(from = 1:13, to = seasonnames)) %>%
         ggplot(aes(x = season, y = p, group = crp_name, col = cropcat, linetype = crp_name,
                    shape = crp_name)) +
         scale_color_manual(values = c("Banana" = watercols5[2], "Horticulture" = watercols5[3],
                                       "Staple" = watercols5[4]), name = NULL) +
         scale_linetype_manual(values = c("Bananas for beer" = 1, "Eggplant: Intoryi" = 2,
                                          "Dry Beans: Ibishyimbo Byumye" = 1,
                                          "Sorghum: Amasaka" = 2, "Tomatoes: Inyanya" = 1),
                               guide = F) +
         scale_shape_manual(values = c("Bananas for beer" = 16, "Eggplant: Intoryi" = 17,
                                       "Dry Beans: Ibishyimbo Byumye" = 16,
                                       "Sorghum: Amasaka" = 17, "Tomatoes: Inyanya" = 16),
                            guide = F) +
         geom_path() + geom_point() +
         annotate(geom = "text",
                  label = c("Bananas\nfor beer", "Eggplant", "Dry beans", "Sorghum", "Tomatoes"),
                  x = rep(15.7, 5), y = c(87.5 - 5, 66.7, 300, 250, 100 + 5),
                  col = watercols5[c(2, 3, 4, 4, 3)], hjust = 1.1) +
         theme_bw() + xlab(NULL) +
         theme(axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1), legend.position = "top") +
         scale_y_log10(name = "RwF/kg"))

ggsave(filename = "tabfig/figs/wages.pdf",
       width = 5, height = 4,
       wages %>% 
         dplyr::mutate(season = factor(season, levels = 1:13)) %>%
         complete(season, site, fill = list(w = NA)) %>%
         dplyr::mutate(season = season %>% plyr::mapvalues(from = 1:13, to = seasonnames)) %>%
         ggplot(aes(x = season, y = w, group = site, col = site)) +
         geom_path() + geom_point() +
         annotate(geom = "text",
                  label = c("Karongi 12", "Karongi 13", "Nyanza 23"),
                  x = c(16.2, 16.2, 16.2), y = c(631, 787, 905), hjust = 1.1,
                  col = watercols5[c(2, 3, 4)]) +
         scale_color_manual(values = c("Karongi12" = watercols5[2], "Karongi13" = watercols5[3],
                                       "Nyanza23" = watercols5[4])) +
         scale_y_log10(name = "RwF/Person day") + theme_bw() + xlab(NULL) +
         theme(axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1),
               legend.position = "none"))

#### ADOPTION DYNAMICS ####

adoptiondynamicsfig <- ggplot(adoptiondynamics %>% subset(var == "irrigated")) +
  geom_point(aes(x = season, y = coef, group = loc, col = loc)) +
  geom_linerange(aes(x = season, ymin = lci, ymax = uci, col = loc)) +
  geom_vline(linetype = 2, xintercept = c(1.5, 4.5, 7.5, 10.5)) + theme_bw() +
  theme(axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1),
        legend.position = "top", legend.title = element_blank()) +
  ylab("Share irrigated") + xlab(NULL) +
  scale_color_manual(values = c("black", watercols5[3]))
ggsave(filename = "tabfig/figs/adoptiondynamics.pdf",
       plot = adoptiondynamicsfig, width = 5, height = 4, dpi = 600)
ggsave(filename = "tabfig/figs/adoptiondynamicsshort.pdf",
       plot = adoptiondynamicsfig, width = 5, height = 3, dpi = 600)

#### RD FIGURES ####

ggsave(filename = "tabfig/figs/rdfig.pdf",
       plot = grid.arrange(rdhpsfig[[1]], rdhpsfig[[2]], rdhpsfig[[3]], rdhpsfig[[4]],
                           rdhpsfig[[5]], rdhpsfig[[6]], rdhpsfig[[7]],
                           rdhpsfig[[8]], rdhpsfig[[9]], rdhpsfig[[10]], rdhpsfig[[11]],
                           layout_matrix = rbind(1:4, c(5:7, NA), c(8:11)),
                           nrow = 3),
       width = 11.25, height = 7.5, scale = 0.9)
ggsave(filename = "tabfig/figs/rdfig_tall.pdf",
       plot = grid.arrange(rdhpsfig[[1]], rdhpsfig[[2]], rdhpsfig[[3]], rdhpsfig[[4]],
                           rdhpsfig[[5]], rdhpsfig[[6]], rdhpsfig[[7]],
                           rdhpsfig[[8]], rdhpsfig[[9]], rdhpsfig[[10]], rdhpsfig[[11]],
                           layout_matrix = rbind(1:3, 4:6, 7:9, c(10:11, NA)),
                           nrow = 4),
       width = 8, height = 11.25, scale = 0.9)

ggsave(filename = "tabfig/figs/mipfig.pdf",
       plot = grid.arrange(miphpsfig[[1]], miphpsfig[[2]], miphpsfig[[3]], miphpsfig[[4]],
                           miphpsfig[[5]], miphpsfig[[6]], miphpsfig[[7]], miphpsfiglegend,
                           layout_matrix = rbind(c(NA, 8, 8, NA), 1:4, c(5:7, NA)),
                           nrow = 3, heights = c(0.25, 1, 1)),
       width = 11.25, height = 6.33, scale = 0.9)
ggsave(filename = "tabfig/figs/mipfig_tall.pdf",
       plot = grid.arrange(miphpsfig[[1]], miphpsfig[[2]], miphpsfig[[3]], miphpsfig[[4]],
                           miphpsfig[[5]], miphpsfig[[6]], miphpsfig[[7]], miphpsfiglegend,
                           layout_matrix = rbind(1:3, 4:6, c(7, 8, 8)),
                           nrow = 3, heights = c(1, 1, 1)),
       width = 8, height = 8, scale = 0.9)

#### MINIKIT ####

regout_exp <- bind_rows(
  reg_omsb %>% subset(tvar == "farmermonitor_sp" &
                        var %in% c("daysnotenoughwater", "daysirrigated", "irrigated")) %>%
    dplyr::mutate(varname = ifelse(var == "irrigated", "SP irrigated", varname)),
  reg_omsb %>% filter(tvar == "totalsubs" & var %in% c("feesowed", "paidfees", "irrigated")) %>%
    dplyr::mutate(varname = gsub(varname, pattern = " (Admin)", replacement = "", fixed = T),
           varname = ifelse(var == "irrigated", "SP irrigated", varname)),
  reg_mk %>%
    subset(spec == " ~ regminikit2 + mksat_sp | nminikitlottery + regmonitor_sp + zone_sp | 0 | wug_sp" &
             var %in% c("minikittakeup", "maincrphort", "irrigated")) %>%
    dplyr::mutate(varname = case_when(var == "irrigated" ~ "SP irrigated",
                               var == "maincrphort" ~ "SP horticulture",
                               T ~ varname))
) %>%
  dplyr::mutate(controlmean = zoo::na.locf(controlmean), controlsd = zoo::na.locf(controlsd))
regout_exp <- regout_exp %>%
  genregsumtex(footerrows = c("Control mean" = "controlmean"),
               regsumrows = c("Farmer monitor", "Subsidy", "Assigned minikit", "Minikit saturation"),
               headertitles = c(" & \\multicolumn{3}{c}{Farmer monitor} & \\multicolumn{3}{c}{Land tax subsidies} & \\multicolumn{3}{c}{Assigned minikit} \\\\",
                                "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}\\cmidrule(lr){8-10}",
                                " & Days w/o & Days & Irri- & Taxes & Taxes & Irri- & Minikit & Horti- & Irri- \\\\",
                                " & enough & irri- & gated & owed & paid & gated & takeup & culture & gated \\\\",
                                " & water & gated & & & & & & & \\\\"),
               footertitles = c("Sample (Plots) & \\multicolumn{9}{c}{SP in Command Area} \\\\",
                                "Sample (Seasons) & \\multicolumn{3}{c}{2016, 2017, \\& 2018 Dry} & \\multicolumn{2}{c}{2017 Rainy} & 2017 \\& & 2017 Rainy & \\multicolumn{2}{c}{2017 \\& 2018 Dry} \\\\",
                                " & \\multicolumn{3}{c}{} & \\multicolumn{2}{c}{1 \\& 2} &  2018 Dry & 1 \\& Dry & \\multicolumn{2}{c}{} \\\\",
                                "\\hline"),
               regsumcontrols = c("Minikit saturation", "Zone FE", "\\# of subsidy lotteries entered",
                                  "\\# of lotteries entered", "O\\&M treatment"))
writeLines(regout_exp, "tabfig/tabs/reg_exp.tex")

dfexpfig <- list(
  "mkfs1" = reg_mk %>%
    subset(spec == " ~ regminikit2 + mksat_sp | nminikitlottery + regmonitor_sp + zone_sp | 0 | wug_sp" &
             var == "minikittakeup" & tvarname == "Assigned minikit") %>%
    select(coef, se, controlmean) %>%
    dplyr::mutate(varname = "Minikit\ntakeup", tvarname = "Assigned minikit", regname = "Minikit"),
  "mkfs2" = reg_mk %>%
    subset(spec == " ~ regminikit2 + mksat_sp | nminikitlottery + regmonitor_sp + zone_sp | 0 | wug_sp" &
             var == "maincrphort" & tvarname == "Assigned minikit") %>%
    select(coef, se, controlmean) %>%
    dplyr::mutate(varname = "SP horti-\nculture", tvarname = "Assigned minikit", regname = "Minikit"),
  "mkrf" = reg_mk %>%
    subset(spec == " ~ regminikit2 + mksat_sp | nminikitlottery + regmonitor_sp + zone_sp | 0 | wug_sp" &
             var == "irrigated" & tvarname == "Assigned minikit") %>%
    select(coef, se, controlmean) %>%
    dplyr::mutate(varname = "SP irrigated", tvarname = "Assigned minikit", regname = "Minikit"),
  "sbfs1" = reg_omsb %>%
    subset(tvar == "totalsubs" & var == "feesowed") %>%
    select(coef, se, controlmean) %>%
    dplyr::mutate(varname = "Taxes\nowed", tvarname = "Subsidy", regname = "Land tax subsidies"),
  "sbfs2" = reg_omsb %>%
    subset(tvar == "totalsubs" & var == "paidfees") %>%
    select(coef, se, controlmean) %>%
    dplyr::mutate(varname = "Taxes\npaid", tvarname = "Subsidy", regname = "Land tax subsidies"),
  "sbrf" = reg_omsb %>%
    subset(tvar == "totalsubs" & var == "irrigated") %>%
    select(coef, se, controlmean) %>%
    dplyr::mutate(varname = "SP irrigated", tvarname = "Subsidy", regname = "Land tax subsidies"),
  "omfs1" = reg_omsb %>%
    subset(tvar == "farmermonitor_sp" & var == "daysnotenoughwater") %>%
    select(coef, se, controlmean, varname, tvarname) %>% dplyr::mutate(varname = "w/o enough\nwater", regname = "O&M"),
  "omfs2" = reg_omsb %>%
    subset(tvar == "farmermonitor_sp" & var == "daysirrigated") %>%
    select(coef, se, controlmean, varname, tvarname) %>% dplyr::mutate(varname = "irrigated", regname = "O&M"),
  "omrf" = reg_omsb %>%
    subset(tvar == "farmermonitor_sp" & var == "irrigated") %>%
    select(coef, se, controlmean) %>%
    dplyr::mutate(varname = "SP irrigated", tvarname = "Farmer monitor", regname = "O&M")
)
dfexpfig <- lapply(names(dfexpfig), function(n) {
  dfexpfig[[n]] %>% dplyr::mutate(coef = as.numeric(coef), se = as.numeric(se), controlmean = as.numeric(controlmean),
                           n = n)
}) %>% bind_rows()
dfexpfig <- lapply(1:nrow(dfexpfig), function(i) {
  dfexpfig[c(i, i),] %>% dplyr::mutate(x = c("Control", dfexpfig[i,"tvarname"]) %>%
                                  factor(levels = c("Control", dfexpfig[i,"tvarname"])),
                                y = ifelse(x == "Control", controlmean, controlmean + coef),
                                lci = ifelse(x == "Control", NA, y + qnorm(.025) * se),
                                uci = ifelse(x == "Control", NA, y + qnorm(.975) * se))
}) %>% bind_rows() %>%
  dplyr::mutate(regname = case_when(regname == "Minikit" ~ "Assigned minikit",
                             regname == "O&M" ~ "Farmer monitor",
                             T ~ regname),
         varname = ifelse(grepl("fs", n) & !grepl("\n", varname, fixed = T), paste0(varname, "\n"), varname))

gexpfig <- sapply(c("Assigned minikit", "Land tax subsidies", "Farmer monitor"), function(i) {
  sapply(c("fs", "rf"), function(j) {
    if(j == "rf") ymax <- max(subset(dfexpfig, grepl("rf", n))$uci, na.rm = T)
    gdf <- dfexpfig %>% filter(regname == i & grepl(j, n)) %>%
      dplyr::mutate(xerrorbar = as.integer(factor(varname)) + 0.25)
    gylab <- case_when(
      j == "rf" ~ "",
      i == "Land tax subsidies" ~ "'000 RwF",
      i == "Farmer monitor" ~ "Days",
      T ~ ""
    )
    g <- ggplot(gdf, aes(x = varname, group = x)) +
      geom_bar(aes(y = y, fill = x),
               col = "black", stat = "identity", position = position_dodge(width = 1)) +
      geom_errorbar(aes(ymin = lci, ymax = uci),
                    width = 0.25, position = position_dodge(width = 1)) +
      scale_fill_manual(values = c("black", watercols5[3])) +
      xlab(NULL) + ylab(gylab) +
      theme_bw() + theme(legend.position = "none", axis.text = element_text(size = 12))
    if(j == "rf") g <- g + ylim(0, ymax)
    if(j == "fs") g <- g + ggtitle(unique(gdf$regname))
    return(g)
  }, simplify = F, USE.NAMES = T)
}, simplify = F, USE.NAMES = T)

gexpfig1a <- grid.arrange(textGrob("Intermediate outcomes", gp=gpar(fontsize=20), rot = 90),
                          gexpfig[["Farmer monitor"]][["fs"]], gexpfig[["Land tax subsidies"]][["fs"]], 
                          gexpfig[["Assigned minikit"]][["fs"]],
                          ncol = 4, widths = c(0.7, rep(4, 3)))
gexpfig2a <- grid.arrange(textGrob("Irrigation", gp=gpar(fontsize=20), rot = 90),
                          gexpfig[["Farmer monitor"]][["rf"]], gexpfig[["Land tax subsidies"]][["rf"]],
                          gexpfig[["Assigned minikit"]][["rf"]],
                          ncol = 4, widths = c(0.7, rep(4, 3)))
gexpfiglegend <- ggpubr::get_legend(gexpfig[[1]]$fs +
                                      scale_fill_manual(values = c("black", watercols5[3]),
                                                        labels = c("Control", "Treatment")) +
                                      theme(legend.position = "top", legend.direction = "horizontal",
                                            legend.title = element_blank()))
ggsave(filename = "tabfig/figs/expfiga.pdf",
       plot = grid.arrange(ggplot() + theme_void(), gexpfiglegend, gexpfig1a, gexpfig2a,
                           layout_matrix = c(1, 2, 3, 3, 4, 4) %>% matrix(nrow = 3, byrow = T),
                           heights = c(0.7, 4, 3.5), widths = c(0.7, 12)),
       width = 8, height = 5, scale = 1)



#### LANDSAT ####

regout_landsat <- reg_landsat %>%
  dplyr::mutate(tvarname = tvarname %>% gsub(pattern = "CA", replacement = "SP CA")) %>%
  dplyr::mutate(tvarname = tvarname %>% gsub(pattern = "Banana", replacement = "SP Banana")) %>%
  cleancontrols %>%
  genregsumtex(jointf = T,
               regsumrows = c("SP CA", "Rainy seasons * SP CA", "Dry season * SP CA",
                              "SP Banana", "Rainy seasons * SP Banana", "Dry season * SP Banana"),
               headertitles = c(" & \\multicolumn{6}{c}{SP, Landsat, Discontinuity sample} \\\\",
                                "\\cmidrule(lr){2-7}",
                                " & \\multicolumn{2}{c}{Year$\\leq 2008$} & \\multicolumn{4}{c}{Year$\\geq 2015$} \\\\",
                                "\\cmidrule(lr){2-3}\\cmidrule(lr){4-7}",
                                " & \\multicolumn{6}{c}{100 * NDVI} \\\\"))
writeLines(regout_landsat, "tabfig/tabs/reg_landsat.tex")
regout_landsatmip <- reg_landsatmip %>%
  dplyr::mutate(tvarname = tvarname %>% gsub(pattern = "CA", replacement = "SP CA")) %>%
  cleancontrols %>%
  genregsumtex(jointf = T,
               regsumrows = c("SP CA", "Rainy seasons * SP CA", "Dry season * SP CA"),
               headertitles = c(" & \\multicolumn{4}{c}{LOP, Landsat, Discontinuity sample} \\\\",
                                "\\cmidrule(lr){2-5}",
                                " & \\multicolumn{2}{c}{Year$\\leq 2008$} & \\multicolumn{2}{c}{Year$\\geq 2015$} \\\\",
                                "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
                                " & \\multicolumn{4}{c}{100 * NDVI} \\\\"))
writeLines(regout_landsatmip, "tabfig/tabs/reg_landsatmip.tex")

#### 6) ADDITIONAL DISCONTINUITY FIGURES ####

profit0rdreg <- felm(profits ~ casp_sp + carv_sp + caspxcarv_sp | 0 | 0 | nearwug,
                     data = rdhpsfigdf)
profit800rdreg <- felm(profits800 ~ casp_sp + carv_sp + caspxcarv_sp | 0 | 0 | nearwug,
                       data = rdhpsfigdf)
profitfigdf <- rdhpsfigdf %>% dplyr::mutate(carv_sp = floor(carv_sp/10)*10 + 5) %>%
  group_by(carv_sp) %>% dplyr::summarise(w0 = mean(profits, na.rm = T),
                                  w800 = mean(profits800, na.rm = T)) %>% ungroup %>%
  gather(col, profits, -carv_sp) %>%
  dplyr::mutate(col = factor(col, levels = c("w0", "w800")))
profitfigdf2 <- data.frame(x = seq(-50, 0, 0.1)) %>%
  dplyr::mutate(y = matrix(c(rep(1, length(x)), rep(0, length(x)), x, rep(0, length(x))),
                    byrow = F, ncol = 4) %*% profit0rdreg$coef,
         var = (matrix(c(rep(1, length(x)), rep(0, length(x)), x, rep(0, length(x))),
                       byrow = F, ncol = 4) %*% profit0rdreg$clustervcv %*%
                  t(matrix(c(rep(1, length(x)), rep(0, length(x)), x, rep(0, length(x))),
                           byrow = F, ncol = 4))) %>% diag,
         group = "w0, outside", col = "w0") %>%
  bind_rows(data.frame(x = seq(0, 50, 0.1)) %>%
              dplyr::mutate(y = matrix(c(rep(1, length(x)), rep(1, length(x)), x, x),
                                byrow = F, ncol = 4) %*% profit0rdreg$coef,
                     var = (matrix(c(rep(1, length(x)), rep(1, length(x)), x, x),
                                   byrow = F, ncol = 4) %*% profit0rdreg$clustervcv %*%
                              t(matrix(c(rep(1, length(x)), rep(1, length(x)), x, x),
                                       byrow = F, ncol = 4))) %>% diag,
                     group = "w0, inside", col = "w0")) %>%
  bind_rows(data.frame(x = seq(-50, 0, 0.1)) %>%
              dplyr::mutate(y = matrix(c(rep(1, length(x)), rep(0, length(x)), x, rep(0, length(x))),
                                byrow = F, ncol = 4) %*% profit800rdreg$coef,
                     var = (matrix(c(rep(1, length(x)), rep(0, length(x)), x, rep(0, length(x))),
                                   byrow = F, ncol = 4) %*% profit800rdreg$clustervcv %*%
                              t(matrix(c(rep(1, length(x)), rep(0, length(x)), x, rep(0, length(x))),
                                       byrow = F, ncol = 4))) %>% diag,
                     group = "w800, outside", col = "w800")) %>%
  bind_rows(data.frame(x = seq(0, 50, 0.1)) %>%
              dplyr::mutate(y = matrix(c(rep(1, length(x)), rep(1, length(x)), x, x),
                                byrow = F, ncol = 4) %*% profit800rdreg$coef,
                     var = (matrix(c(rep(1, length(x)), rep(1, length(x)), x, x),
                                   byrow = F, ncol = 4) %*% profit800rdreg$clustervcv %*%
                              t(matrix(c(rep(1, length(x)), rep(1, length(x)), x, x),
                                       byrow = F, ncol = 4))) %>% diag,
                     group = "w800, inside", col = "w800")) %>%
  dplyr::mutate(uci = y + sqrt(var) * qnorm(0.975), lci = y + sqrt(var) * qnorm(0.025)) %>%
  dplyr::mutate(col = factor(col, levels = c("w0", "w800")))
gprofitfig <- ggplot() +
  geom_point(data = profitfigdf, aes(x = carv_sp, y = profits, col = col)) +
  geom_path(data = profitfigdf2, aes(x = x, y = y, group = group, col = col)) +
  geom_path(data = profitfigdf2, aes(x = x, y = uci, group = group, col = col), linetype = 2) +
  geom_path(data = profitfigdf2, aes(x = x, y = lci, group = group, col = col), linetype = 2) +
  geom_vline(linetype = 2, xintercept = 0) +
  scale_x_continuous(breaks = seq(-40, 40, 20), limits = c(-50, 50),
                     name = "Meters to boundary (Sample plot)") +
  scale_y_continuous(limits = c(min(profitfigdf$profits), 260),
                     name = "Profits ('000 RwF/ha)") +
  scale_color_manual(values = c("w0" = watercols5[4], "w800" = watercols5[2]),
                     labels = c("0 RwF/day", "800 RwF/day"), name = "Imputed wage\nfor HH labor") +
  theme_bw() +
  theme(legend.direction = "vertical", legend.position = c(0.25, 0.8))

gyieldfig <- rdhpsfig[[8]] +
  scale_y_continuous(limits = c(min(profitfigdf$profits), 260), name = "Yield ('000 RwF/ha)") +
  scale_x_continuous(breaks = seq(-40, 40, 20), limits = c(-50, 50),
                     name = "Meters to boundary (Sample plot)") +
  theme(legend.position = c(0.25, 0.8), legend.direction = "vertical") +
  ggtitle(NULL)

gsubstfig_irr <- miphpsfig[[2]] +
  scale_y_continuous(limits = c(0, 0.4), name = "Share irrigated") +
  scale_x_continuous(breaks = seq(-40, 40, 20), limits = c(-50, 50),
                     name = "Meters to boundary (Sample plot)") +
  theme(legend.position = c(0.25, 0.8), legend.direction = "vertical") +
  ggtitle(NULL)

ggsave(filename = "tabfig/figs/yield.pdf",
       gyieldfig, width = 4, height = 4)
ggsave(filename = "tabfig/figs/hhlab_profit.pdf",
       gprofitfig, width = 4, height = 4)
ggsave(filename = "tabfig/figs/irr_subst.pdf",
       gsubstfig_irr, width = 4, height = 4)

#### DISCONTINUITY VISUALIZATION ####

rdhpsfigalphareg <- felm(irrigated ~ casp_sp + carv_sp + caspxcarv_sp | 0 | 0 | nearwug,
                         data = rdhpsfigdf)
rdhpsfigalphadf <- rdhpsfigdf %>% dplyr::mutate(carv_sp = floor(carv_sp/10)*10 + 5) %>%
  group_by(carv_sp) %>% dplyr::summarise_at("irrigated", function(x) mean(x, na.rm = T)) %>%
  ungroup %>% dplyr::mutate(alpha = 1 - .02 * abs(carv_sp))
rdhpsfigalphadf2 <- data.frame(x = seq(-50, 0, 0.1)) %>%
  dplyr::mutate(y = matrix(c(rep(1, length(x)), rep(0, length(x)), x, rep(0, length(x))),
                    byrow = F, ncol = 4) %*% rdhpsfigalphareg$coef,
         var = (matrix(c(rep(1, length(x)), rep(0, length(x)), x, rep(0, length(x))),
                       byrow = F, ncol = 4) %*% rdhpsfigalphareg$clustervcv %*%
                  t(matrix(c(rep(1, length(x)), rep(0, length(x)), x, rep(0, length(x))),
                           byrow = F, ncol = 4))) %>% diag,
         group = "outside") %>%
  bind_rows(data.frame(x = seq(0, 50, 0.1)) %>%
              dplyr::mutate(y = matrix(c(rep(1, length(x)), rep(1, length(x)), x, x),
                                byrow = F, ncol = 4) %*% rdhpsfigalphareg$coef,
                     var = (matrix(c(rep(1, length(x)), rep(1, length(x)), x, x),
                                   byrow = F, ncol = 4) %*% rdhpsfigalphareg$clustervcv %*%
                              t(matrix(c(rep(1, length(x)), rep(1, length(x)), x, x),
                                       byrow = F, ncol = 4))) %>% diag,
                     group = "inside")) %>%
  dplyr::mutate(uci = y + sqrt(var) * qnorm(0.975), lci = y + sqrt(var) * qnorm(0.025))
rdhpsfigalphadf2 <- rdhpsfigalphadf2 %>%
  dplyr::mutate(group2 = case_when(abs(x) == 50 ~ x - 0.5 * abs(x) / x,
                            group == "outside" ~ round(x + 0.45) - 0.5,
                            group == "inside" ~ round(x - 0.45) + 0.5)) %>%
  bind_rows(rdhpsfigalphadf2 %>% filter(abs(x) %in% 1:49) %>%
              dplyr::mutate(group2 = case_when(group == "outside" ~ x + 0.5,
                                        group == "inside" ~ x - 0.5))) %>%
  arrange(group2, x) %>%
  dplyr::mutate(alpha = 1 - .02 * abs(group2))
grdhpsfigalpha <- ggplot() +
  geom_point(data = rdhpsfigalphadf, aes(x = carv_sp, y = irrigated, alpha = alpha)) +
  geom_point(data = rdhpsfigalphadf, aes(x = carv_sp, y = irrigated), shape = 1) +
  geom_path(data = rdhpsfigalphadf2, aes(x = x, y = y, group = group2, alpha = alpha)) +
  geom_path(data = rdhpsfigalphadf2, aes(x = x, y = uci, group = group2, alpha = alpha),
            linetype = 2) +
  geom_path(data = rdhpsfigalphadf2, aes(x = x, y = lci, group = group2, alpha = alpha),
            linetype = 2) +
  geom_vline(linetype = 2, xintercept = 0) +
  annotate(geom = "label", x = 25, y = 0.175, label = "Command\narea", col = "black",
           fill = watercols5[5]) +
  annotate(geom = "label", x = -25, y = 0.175, label = "Outside\ncommand area", col = "black",
           fill = watercols5[4]) +
  scale_x_continuous(breaks = seq(-40, 40, 20), limits = c(-50, 50),
                     name = "Meters to boundary (Sample plot)") +
  scale_y_continuous(limits = c(0, max(rdhpsfigalphadf$irrigated)), name = "Share irrigated") +
  theme_bw() +
  theme(legend.position = "none")

ggsave(filename = "tabfig/figs/alphardd_irr.pdf",
       grdhpsfigalpha, width = 4, height = 4)

#### TIMELINE ####

# # TIMELINE FIGURE HERE

timelineevents <- data.frame(names = c("Terracing", "Irrigation\nconstruction", "Irrigation construction\ncompleted"),
                             start = c(2009 + .5/24, 2011 + .5/24, 2015 + .5/24),
                             end = c(2010 + 23.5/24, 2014 + 23.5/24, 2015 + 23.5/24))
timelinesurveys <- data.frame(names = c("atop(underline(Baseline), atop('2014 Dry', '2015 Rainy 1, 2'))",
                                        "atop(underline(Midline~1), atop('2016 Rainy 2, Dry', '2017 Rainy 1'))",
                                        "atop(underline(Midline~2), atop('2017 Rainy 2, Dry', ' '))",
                                        "atop(underline(Midline~3), atop('2018 Rainy 1, 2, Dry'))"),
                              date = c(2015 + 15/24, 2017 + 9/24, 2017 + 21/24, 2018 + 21/24),
                              start = c(2014 + 10.5/24, 2016 + 2.5/24, 2017 + 2.5/24, 2017 + 16.5/24),
                              end = c(2015 + 9.5/24, 2017 + 1.5/24, 2017 + 15.5/24, 2018 + 15.5/24),
                              nameshift = c(-10/24, -10/24, 4/24, 2/24),
                              nameshifty = c(0, 0, -1.1, 0))
gtimeline <- ggplot() +
  annotate(geom = "segment", x = 2008.5, xend = 2019.5, y = 0, yend = 0, arrow = arrow(length = unit(1/24, "native"))) +
  annotate(geom = "segment", x = 2019.5, xend = 2008.5, y = 0, yend = 0, arrow = arrow(length = unit(1/24, "native"))) +
  geom_segment(data = data.frame(x = seq(2009, 2019, 1), xend = seq(2009, 2019, 1),
                                 y = -.1, yend = .1),
               aes(x = x, xend = xend, y = y, yend = yend)) +
  geom_text(data = data.frame(x = seq(2009.5, 2018.5, 1), y = .07, names = seq(2009, 2018, 1)),
            aes(x = x, y = y, label = names), size = 3.5, vjust = 0) +
  # TIMELINE EVENTS
  geom_text(data = timelineevents %>% dplyr::mutate(x = (start + end)/2, y = 0.62),
            aes(x = x, y = y, label = names), vjust = 0, size = 5) +
  geom_segment(data = timelineevents %>% dplyr::mutate(y = 0.3, yend = 0.3),
               aes(x = start, xend = end, y = y, yend = yend)) +
  geom_segment(data = timelineevents %>% dplyr::mutate(y = 0.3, yend = 0.6, x = (start + end)/2),
               aes(x = x, xend = x, y = y, yend = yend)) +
  geom_segment(data = timelineevents %>% select(start, end) %>% gather(var, val) %>% dplyr::mutate(y = 0.11, yend = 0.3),
               aes(x = val, xend = val, y = y, yend = yend)) +
  # TIMELINE SURVEYS
  geom_text(data = timelinesurveys %>% dplyr::mutate(y = -0.52 + nameshifty, date = date + nameshift),
            aes(x = date, y = y, label = names), vjust = 1, parse = T, size = 5) +
  geom_segment(data = timelinesurveys %>% dplyr::mutate(y = -0.5 + nameshifty, yend = -0.11),
               aes(x = date, xend = date, y = y, yend = yend)) +
  geom_segment(data = timelinesurveys %>% dplyr::mutate(xend = (start + 3*end)/4, y = -0.3),
               aes(x = date, xend = xend, y = y, yend = y), col = watercols5[3]) +
  geom_segment(data = timelinesurveys %>% dplyr::mutate(x = (start + 3*end)/4, y = -0.3, yend = -0.155),
               aes(x = x, xend = x, y = y, yend = yend), col = watercols5[3]) +
  geom_segment(data = timelinesurveys %>% dplyr::mutate(y = -0.155),
               aes(x = start, xend = end, y = y, yend = y), col = watercols5[3]) +
  geom_segment(data = timelinesurveys %>% select(start, end) %>% gather(var, val) %>% dplyr::mutate(y = -0.11, yend = -0.2),
               aes(x = val, xend = val, y = y, yend = yend), col = watercols5[3]) +
  ylim(-2.4, 1.4) + xlim(2008.5, 2019.5) + theme_void()
ggsave(file = paste0("tabfig/figs/timeline.pdf"),
       gtimeline, width = 11*0.7, height = 3.8*0.7)