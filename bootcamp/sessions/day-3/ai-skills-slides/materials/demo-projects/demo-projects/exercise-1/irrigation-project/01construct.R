library(matrixStats)
library(foreign)
library(magrittr)
library(tidyverse)
library(readstata13)
library(rgdal)
library(lfe)
library(gridExtra)
library(rgeos)
library(ggmap)
library(grid)
library(scales)
library(tikzDevice)
library(viridis)
library(ggtext)

options(stringsAsFactors = F)

watercols5 <- viridis.map %>% filter(opt == "C") %>% `[`(,1:3) %>% sapply(function(x) { x[c(1, 64, 128, 192, 256)] }) %>%
  apply(MARGIN = 1, function(x) { paste0("#", round(x*255) %>% as.hexmode %>% paste(collapse = "")) })

hortnames <- c("Onions: Ibitunguru", "Eggplant: Intoryi", "Tomatoes: Inyanya",
               "Pepper: Urusenda", "Carrots: Karoti", "Cabbage: Amashu",
               "Peas: Amashaza", "Garlic: Tungulu Sumu", "Sweet Pepper: Poivron",
               "Green Beans: Imiteja", "Strawberry: Inkeri", "Watermelon: Watermelon",
               "Chillies: Insenda", "Amaranthus: Dodo/Imbwija", "Marakuja (maracuja/passion fruit)",
               "Green Peas: Amashaza y'imiteja", "Celery: Seleri", "Pumpkin: Ibihaza",
               "Beet Root: Beterave")
banananames <- c("Bananas for beer", "Inyamunyo (bananas for cooking)",
                 "Banana fruits")
staplenames <- c("Dry Beans: Ibishyimbo Byumye", "Maize: Ibigori", "Sorghum: Amasaka",
                 "Sweet Potatoes: Ibijumba", "Irish Potatoes: Ibirayi")

#### 1) READING DATA ####

for(n in c("h", "prices", "hs", "hps", "hpsc", "hbaseline", "areas",
           "trackps", "hpbaseline", "nearwug", "hpssamp", "landsatdf", "distm")) {
  assign(n, readRDS(paste0("rds/", n, ".rds")))
}

h <- h %>%
  dplyr::mutate_at(c("houseexp"),
            function(v) { q <- quantile(v, 0.99, na.rm = T); ifelse(v >= q, q, v) })
hps <- hps %>%
  dplyr::mutate_at(c("yield", "inputexpha", "salesha", "hhlabha", "hiredlabha", "hiredlabexpha"),
            function(v) { q <- quantile(v, 0.99, na.rm = T); ifelse(v >= q, q, v) })
wages <- hps %>% select(hhcode, plot, season, hiredlabha, hiredlabexpha) %>%
  # THIS HOUSEHOLD HIRED MANY WORKERS AND MULTIPLICATION ERROR WAS MADE (WAGE = 100 INSTEAD OF WAGE = 1000)
  filter(hhcode != 4521) %>%
  left_join(hbaseline %>% select(hhcode, site)) %>% filter(hiredlabha > 0) %>%
  left_join(areas) %>%
  group_by(site, season) %>%
  dplyr::summarise(w = weighted.mean(hiredlabexpha / hiredlabha, w = hiredlabha * areasp, na.rm = T),
            n = n()) %>%
  ungroup
trackps <- trackps %>%
  dplyr::mutate(yield = ifelse(yield >= max(hps$yield, na.rm = T), max(hps$yield, na.rm = T), yield),
         inputexpha = ifelse(inputexpha >= max(hps$inputexpha, na.rm = T), max(hps$inputexpha, na.rm = T), inputexpha),
         salesha = ifelse(salesha >= max(hps$salesha, na.rm = T), max(hps$salesha, na.rm = T), salesha),
         hhlabha = ifelse(hhlabha >= max(hps$hhlabha, na.rm = T), max(hps$hhlabha, na.rm = T), hhlabha),
         hiredlabha = ifelse(hiredlabha >= max(hps$hiredlabha, na.rm = T), max(hps$hiredlabha, na.rm = T), hiredlabha),
         hiredlabexpha = ifelse(hiredlabexpha >= max(hps$hiredlabexpha, na.rm = T), max(hps$hiredlabexpha, na.rm = T), hiredlabexpha))

hps$maincrphort <- hps$anyhort
hps$maincrpbanana <- hps$anybanana
trackps$maincrphort <- trackps$anyhort
trackps$maincrpbanana <- trackps$anybanana

#### 2) ANALYSIS DATASETS ####

balanceh <- hbaseline %>%
  left_join(h %>% filter(survey == 1) %>%
              select(hhcode, hhofffarm, houseexp, assetallprc) %>%
              dplyr::rename(blhhofffarm = hhofffarm, blhouseexp = houseexp, blassetallprc = assetallprc)) %>%
  left_join(hs %>% filter(season == 8) %>% dplyr::rename(fullfees17a = fullfees) %>% select(hhcode, fullfees17a)) %>%
  left_join(hs %>% filter(season == 9) %>% dplyr::rename(fullfees17b = fullfees) %>% select(hhcode, fullfees17b)) %>%
  dplyr::mutate_at(vars(c("blhouseexp", "fullfees17a", "fullfees17b")), function(x) x / 1e3)

stopifnot(all(balanceh$reglotteryff2 + balanceh$reglotteryfn2 +
                balanceh$reglotteryhn2 + balanceh$reglotterynn2 == 1, na.rm = T))

balancehp <- hpbaseline %>%
  left_join(nearwug)

balancesphp <- balancehp %>%
  filter(sampleplot == 1) %>%
  dplyr::rename(casp_sp = casp, carv_sp = carv, caspxcarv_sp = caspxcarv) %>%
  dplyr::mutate(lareasp_sp = lareasp, elevation_sp = elevation, sitegeo_sp = sitegeo)

balancemiphp <- balancehp %>% group_by(hhcode) %>% filter(n() >= 2) %>% ungroup %>%
  filter(sampleplot == 1) %>%
  select(hhcode, plot, areasp, lareasp, casp, carv, caspxcarv,
         rdsampgeo, sitegeo, filename, nearwug, nearzone,
         elevation) %>%
  dplyr::rename_all(function(x) { paste0(x, "_sp") }) %>%
  dplyr::rename(hhcode = hhcode_sp, plot = plot_sp, filename = filename_sp, nearwug = nearwug_sp, nearzone = nearzone_sp) %>%
  left_join(balancehp %>% filter(sampleplot == 0) %>%
              select(hhcode, plot, lareasp, casp, own, terraced, owned5, rentedoutfarmer, rentedoutinvestor,
                     slope, elevation) %>%
              dplyr::rename_all(function(x) { paste0(x, "_mip") }) %>% dplyr::rename(hhcode = hhcode_mip)) %>%
  dplyr::mutate(mipsamp = 1)

balanceh <- balanceh %>%
  left_join(balancehp %>% filter(sampleplot == 1) %>% select(hhcode, filename, nearwug, nearzone)) %>%
  left_join(balancemiphp %>% select(hhcode, mipsamp) %>% distinct) %>%
  dplyr::mutate(mipsamp = ifelse(is.na(mipsamp), 0, mipsamp))

analysish <- h %>%
  complete(hhcode, survey) %>%
  dplyr::mutate_at(vars(c("houseexp")), function(x) x / 1e3) %>%
  left_join(balanceh) %>%
  dplyr::mutate(sitegeo_spxsurvey = paste0(sitegeo_sp, "x", survey))

balancehp <- balancehp %>%
  left_join(balanceh %>% select(hhcode, mipsamp))

balancesphp <- balancesphp %>%
  left_join(balanceh %>% select(hhcode, mipsamp))

analysishs <- hs %>%
  complete(hhcode, season) %>%
  dplyr::mutate_at(vars(c("fullfees", "subsfees", "paidfees")), function(x) x / 1e3)

stopifnot(nrow(hpssamp) == nrow(hpssamp %>% select(hhcode, plot, season) %>% distinct))
stopifnot(nrow(trackps) == nrow(trackps %>% select(hhcode, plot, season) %>% distinct))

analysishps <- hps %>% filter(!is.na(plot)) %>%
  complete(hhcode, plot, season) %>%
  left_join(hpssamp %>%
              dplyr::rename_at(c("blown", "rentedout", "rentedoutfarmer", "rentedoutinvestor",
                          "rentedin", "own"), function(x) { paste0("samp_", x) }) %>%
              left_join(trackps %>% dplyr::rename(samp_cultivated = cultivated) %>%
                          dplyr::mutate(samp_nacultivated = is.na(samp_cultivated)) %>%
                          select(hhcode, plot, season, samp_cultivated, samp_nacultivated)) %>%
              dplyr::mutate(samp_cultivated = ifelse(is.na(samp_cultivated), 0, samp_cultivated),
                     samp_nacultivated = ifelse(is.na(samp_nacultivated), 1, samp_nacultivated))) %>%
  dplyr::mutate(trackedsamp = (is.na(cultivated) | cultivated == 0) &
           ((samp_rentedoutfarmer == 1 & !samp_nacultivated) |
              (samp_blown == 1 & samp_own == 0 & samp_cultivated) |
              (samp_blown == 0 & samp_rentedin == 0 & samp_cultivated)),
         shouldtrackedsamp = (is.na(cultivated) | cultivated == 0) &
           ((samp_rentedoutfarmer == 1) |
              (samp_blown == 1 & samp_own == 0 & (samp_cultivated | samp_nacultivated)) |
              (samp_blown == 0 & samp_rentedin == 0 & (samp_cultivated | samp_nacultivated))))

analysishps <- bind_rows(
  analysishps %>% filter(is.na(trackedsamp) | trackedsamp == 0),
  analysishps %>% filter(trackedsamp == 1) %>%
    select(c("hhcode", "plot", "season", names(analysishps) %>% subset(grepl("samp", .)))) %>%
    left_join(trackps %>%
                select(names(analysishps) %>% subset(!. %in% c("timenotenoughwater", "daysnotenoughwater") &
                                                       !grepl("samp", .))))
) %>% arrange(hhcode, plot, season) %>%
  dplyr::mutate(attrit_any = (is.na(cultivated)) %>% as.numeric,
         attrit_hh = (is.na(cultivated) & is.na(samp_blown)) %>% as.numeric,
         attrit_investor = (is.na(cultivated) & !is.na(samp_blown) & samp_rentedoutinvestor) %>% as.numeric,
         attrit_transact = (is.na(cultivated) & !is.na(samp_blown) & !samp_rentedoutinvestor) %>% as.numeric,
         attrit_trackedsuccess = (!is.na(trackedsamp) & trackedsamp) %>% as.numeric,
         attrit_trackedfailure = (!is.na(trackedsamp) & !trackedsamp & shouldtrackedsamp) %>% as.numeric,
         attrit_shouldtracked = (!is.na(trackedsamp) & shouldtrackedsamp) %>% as.numeric)
stopifnot(identical(analysishps$attrit_hh + analysishps$attrit_investor + analysishps$attrit_transact,
                    analysishps$attrit_any))
stopifnot(identical(analysishps$attrit_trackedsuccess + analysishps$attrit_trackedfailure,
                    analysishps$attrit_shouldtracked))
analysishps <- analysishps %>%
  left_join(balanceh %>% select(-sampleplot, -filename, -casp_sp, -carv_sp, -caspxcarv_sp,
                                -omsamp_sp, -wug_sp, -zone_sp,
                                -areasp_sp, -regmonitor_sp, -mksat_sp, -farmermonitor_sp,
                                -farmermonitornoreserve_sp, -farmermonitorreserve_sp,
                                -nearwug, -nearzone, -mipsamp)) %>%
  inner_join(balancehp) %>%
  dplyr::mutate_at(c("casp", "carv", "caspxcarv", "omsamp", "wug", "zone", "areasp", "regmonitor", "mksat",
              "farmermonitor", "farmermonitornoreserve", "farmermonitorreserve"),
            list("sp" = function(x) ifelse(.$sampleplot == 1, x, NA))) %>%
  dplyr::mutate(maincrpreg = ifelse(cultivated == 0, "Fallow", maincrp),
         sitegeoxseason = paste0(sitegeo, "x", season),
         sitegeo_spxseason = paste0(sitegeo_sp, "x", season),
         dryseason = season %% 3 == 1,
         sitegeoxseasonxcrop = ifelse(cultivated == 1 & is.na(maincrp), NA,
                                      paste0(sitegeo, "x", season, "x", maincrpreg)),
         sitegeo_spxseasonxcrop = ifelse(cultivated == 1 & is.na(maincrp), NA,
                                         paste0(sitegeo_sp, "x", season, "x", maincrpreg))) %>%
  dplyr::mutate_at(vars(c("yield", "salesha", "hiredlabexpha", "inputexpha")),
            function(x) x / 1e3) %>%
  dplyr::mutate(profits = yield - hiredlabexpha - inputexpha,
         anysale = salesha > 0,
         hhlabexpha = 0.8*hhlabha, exptotha = hiredlabexpha + inputexpha,
         profits800 = yield - hiredlabexpha - inputexpha - hhlabexpha,
         profits480 = yield - hiredlabexpha - inputexpha - 0.6 * hhlabexpha)

stopifnot(nrow(analysishps %>% filter(sampleplot == 1)) ==
            nrow(analysishps %>% filter(sampleplot == 1) %>% select(hhcode, season) %>% distinct))

analysismiphps <- balancemiphp %>%
  left_join(analysishps %>% filter(sampleplot == 0) %>%
              select(-plot, -areasp, -lareasp, -casp, -carv, -caspxcarv,
                     -rdsampgeo, -sitegeo, -filename, -nearwug, -nearzone,
                     -areasp_sp, -lareasp_sp, -casp_sp, -carv_sp,
                     -caspxcarv_sp,
                     -rdsampgeo_sp, -sitegeo_sp,
                     -mipsamp))

stopifnot(nrow(analysismiphps) == (analysishps %>% group_by(hhcode, season) %>% filter(n() >= 2) %>%
                                     ungroup %>% filter(sampleplot == 1) %>% nrow))
stopifnot(nrow(analysismiphps) == (analysishps %>% group_by(hhcode, season) %>% filter(n() >= 2) %>%
                                     ungroup %>% filter(sampleplot == 0) %>% nrow))

stopifnot(nrow(analysishps %>% select(hhcode, season) %>% distinct) ==
            nrow(analysishps %>% select(hhcode, sitegeo_spxseason) %>% distinct))

analysish %<>%
  dplyr::mutate(casp_spxnmembers = casp_sp * nmembers, casp_spxblassetallprc = casp_sp * blassetallprc)
analysishps %<>%
  dplyr::mutate(casp_spxnmembers = casp_sp * nmembers, casp_spxblassetallprc = casp_sp * blassetallprc)
analysismiphps %<>%
  dplyr::mutate(casp_spxnmembers = casp_sp * nmembers, casp_spxblassetallprc = casp_sp * blassetallprc,
         casp_spxcasp_mip = casp_sp * casp_mip)

balanceh %<>% dplyr::mutate(plot = sampleplot)
analysish %<>% dplyr::mutate(plot = sampleplot)

balancemiphp <- balancemiphp %>%
  left_join(balanceh %>% select(hhcode, nmembers, blassetallprc)) %>%
  dplyr::mutate(casp_spxcasp_mip = casp_sp * casp_mip,
         casp_spxnmembers = casp_sp * nmembers,
         casp_spxblassetallprc = casp_sp * blassetallprc)

analysishps <- analysishps %>%
  left_join(balancehp %>% group_by(hhcode) %>% filter(sampleplot == 0) %>% ungroup %>%
              select(hhcode,
                     rdsampgeo_mip = rdsampgeo, sitegeo_mip = sitegeo,
                     casp_mip = casp, carv_mip = carv,
                     lareasp_mip = lareasp,
                     caspxcarv_mip = caspxcarv, nearwug_mip = nearwug)) %>%
  dplyr::mutate(casp_mipxcasp_sp = casp_mip * casp_sp,
         casp_spxcasp_mip = casp_mipxcasp_sp,
         casp_mipxnmembers = casp_mip * nmembers,
         casp_mipxblassetallprc = casp_mip * blassetallprc)

analysishps <- analysishps %>%
  dplyr::mutate(dryseasonxcasp_sp = dryseason * casp_sp,
         dryseasonxcarv_sp = dryseason * carv_sp,
         dryseasonxcaspxcarv_sp = dryseason * caspxcarv_sp,
         dryseasonxlareasp_sp = dryseason * lareasp_sp)
analysismiphps <- analysismiphps %>%
  dplyr::mutate(dryseasonxcasp_sp = dryseason * casp_sp,
         dryseasonxcarv_sp = dryseason * carv_sp,
         dryseasonxcaspxcarv_sp = dryseason * caspxcarv_sp,
         dryseasonxlareasp_sp = dryseason * lareasp_sp,
         dryseasonxlareasp_mip = dryseason * lareasp_mip,
         dryseasonxcasp_mip = dryseason * casp_mip)

# LANDSAT DATA
analysislandsat <- balancesphp %>%
  select(hhcode, plot, lareasp_sp, casp_sp, carv_sp, caspxcarv_sp, nearwug, nearzone,
         rdsampgeo, sampleplot, filename) %>%
  left_join(landsatdf) %>%
  dplyr::mutate(m = lubridate::month(t), y = lubridate::year(t),
         m = as.character(m)) %>%
  left_join(analysishps %>% filter(!is.na(anybanana) & season >= 6) %>%
              group_by(hhcode, plot) %>%
              dplyr::summarise(maincrpbanana = any(maincrpbanana == 1, na.rm = T) %>%
                          as.numeric) %>%
              ungroup) %>%
  dplyr::mutate(dry = m %in% 6:8, ndvix100 = ndvi * 100,
         dryseasonxcasp_sp = dry * casp_sp,
         rainyseasonxcasp_sp = (1 - dry) * casp_sp,
         dryseasonxmaincrpbanana = dry * maincrpbanana,
         rainyseasonxmaincrpbanana = (1 - dry) * maincrpbanana)
analysismiplandsat <- balancemiphp %>%
  select(hhcode, plot_sp = plot, plot_mip, lareasp_sp,
         casp_sp, carv_sp, caspxcarv_sp, nearwug,
         rdsampgeo_sp, lareasp_mip, casp_mip) %>%
  left_join(landsatdf %>% dplyr::rename(plot_mip = plot)) %>%
  dplyr::mutate(m = lubridate::month(t), y = lubridate::year(t),
         m = as.character(m)) %>%
  dplyr::mutate(dry = m %in% 6:8, ndvix100 = ndvi * 100,
         dryseasonxcasp_sp = dry * casp_sp,
         rainyseasonxcasp_sp = (1 - dry) * casp_sp)

balancesphp <- balancesphp %>%
  left_join(analysislandsat %>% filter(y <= 2008) %>%
              group_by(hhcode, plot) %>%
              dplyr::summarise(ndvix100meanpreconstr = mean(ndvix100, na.rm = T)) %>%
              ungroup) %>%
  left_join(analysislandsat %>% filter(y <= 2008 & m %in% 6:8) %>%
              group_by(hhcode, plot) %>%
              dplyr::summarise(ndvix100meanpreconstrdry = mean(ndvix100, na.rm = T)) %>%
              ungroup) %>%
  left_join(analysislandsat %>% filter(y <= 2008 & !m %in% 6:8) %>%
              group_by(hhcode, plot) %>%
              dplyr::summarise(ndvix100meanpreconstrrainy = mean(ndvix100, na.rm = T)) %>%
              ungroup)

balancemiphp <- balancemiphp %>%
  left_join(analysismiplandsat %>% filter(y <= 2008) %>%
              group_by(hhcode, plot_mip) %>%
              dplyr::summarise(ndvix100meanpreconstr = mean(ndvix100, na.rm = T)) %>%
              ungroup) %>%
  left_join(analysismiplandsat %>% filter(y <= 2008 & m %in% 6:8) %>%
              group_by(hhcode, plot_mip) %>%
              dplyr::summarise(ndvix100meanpreconstrdry = mean(ndvix100, na.rm = T)) %>%
              ungroup) %>%
  left_join(analysismiplandsat %>% filter(y <= 2008 & !m %in% 6:8) %>%
              group_by(hhcode, plot_mip) %>%
              dplyr::summarise(ndvix100meanpreconstrrainy = mean(ndvix100, na.rm = T)) %>%
              ungroup)