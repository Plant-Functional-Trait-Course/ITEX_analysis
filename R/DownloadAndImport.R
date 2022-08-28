##################################
#### DOWNLOAD AND IMPORT DATA ####
##################################

### Download data
DataDownloadPlan <- list(

  # not all files download automatically
  # if that is not the case, files can be downloaded from here: https://osf.io/smbqh/
  # see remote path for where the files are stored on OSF
  # and added to a folder called data

  # Temperature
  tar_target(
    name = temperature_download,
    command =  get_file(node = "smbqh",
                        file = "PFTC4_Svalbard_2005_2018_ITEX_Temperature.csv",
                        path = "data",
                        remote_path = "Climate"),
    format = "file"
  ),

  # Climate
  tar_target(
    name = climate_download,
    command =  get_file(node = "smbqh",
                        file = "PFTC4_Svalbard_2015_2018_ITEX_Climate.csv",
                        path = "data",
                        remote_path = "Climate"),
    format = "file"
  ),

  # Meta data (coordinates)
  # tar_target(
  #   name = meta_download,
  #   command = download_PFTC_data(country = "Svalbard",
  #                                datatype = "meta",
  #                                path = "data"),
  #   format = "file"
  # ),

  # Community
  tar_target(
    name = community_download,
    command = get_file(node = "smbqh",
                       file = "PFTC4_Svalbard_2003_2015_ITEX_Community.csv",
                       path = "data",
                       remote_path = "Community"),
    format = "file"
  ),

  # Height
  tar_target(
    name = height_download,
    command = get_file(node = "smbqh",
                       file = "PFTC4_Svalbard_2003_2015_ITEX_Vegetation_Structure.csv",
                       path = "data",
                       remote_path = "Community"),
    format = "file"
  ),

  # Traits
  tar_target(
    name = trait_download,
    command = get_file(node = "smbqh",
                       file = "PFTC4_Svalbard_2018_ITEX_Traits.csv",
                       path = "data",
                       remote_path = "Traits"),
    format = "file"
  ),

  # C-fluxes
  tar_target(name = cflux_download,
             command = "data/Cflux_SV_ITEX_2018.csv",
             format = "file"),

  # Soil resp
  tar_target(name = soilresp_download,
             command = "data/Endalen_FD_chambers_updatedHL.csv",
             format = "file")

)


# Import data
DataImportPlan <- list(

  # import temperature data
  tar_target(
    name = Temperature,
    command = read_csv(file = temperature_download)
  ),

  # import climate data
  tar_target(
    name = Climate,
    command = read_csv(file = climate_download)
  ),

  # import community data
  tar_target(
    name = Community,
    command = read_csv(file = community_download) %>%
      # remove iced Cassiope plots
      filter(!PlotID %in% c("CH-4", "CH-6", "CH-9", "CH-10"))
  ),

  # make meta
  tar_target(
    name = metaItex,
    command = Community %>%
      distinct(Site, Treatment, PlotID)
  ),

  # import height data
  tar_target(
    name = Height,
    command = read_csv(file = height_download) %>%
      filter(Variable == "MedianHeight_cm",
             Year == 2015) %>%
      mutate(Site = factor(Site, levels = c("SB", "CH", "DH")))
  ),

  #import trait data
  tar_target(
    name = Traits,
    command = read_csv(file = trait_download) %>%
      # remove iced Cassiope plots
      filter(!PlotID %in% c("CH-4", "CH-6", "CH-9", "CH-10")) %>%
      # remove NP ratio from data. Not part of original analysis
      filter(Trait != "NP_ratio")
  ),

  # flux data #
  tar_target(
    name = ITEX.data.pre.calcs,
    command = read_csv(file = cflux_download) |>
      ## process data
      mutate(
        #Gradient = "1",
        #BlockID = NA,
        Date = dmy(Date),     # changing Date from character to date format
        #AirTemp = NA, # need iButton data!!
        NEE = -1* NEE  ## proper terminology
      ) %>%
      select(
        #Country,
        #Year,
        #Gradient,
        #BlockID,
        #Project,
        #Site,           ## changing variable names
        PlotID,
        Treatment,
        Date,
        Time = StartTime,
        Type = Cover,
        LNrsqd = rsqd,
        nee_lm = NEE,
        SoilTemp = ST_mean,
        SoilMoist = SM_mean,
        CanTemp = IR_mean,
        #AirTemp,
        PAR = PAR_mean#,
        #Weather,
        #Notes = comment
      )
  ),

  # soil respiration data
  tar_target(
    name = soil_resp,
    command = {

      soilresp_raw <- read.csv(file = soilresp_download)

      # Renaming columns and condensing data to date means
      soilresp_raw <- soilresp_raw |>
        slice(-c(1:2)) |>
        mutate(DateTime = as.POSIXct(as.character(X)),
               Date = as.Date(DateTime),
               AirTemp = as.numeric(as.character(X.1)),
               OTC1 = as.numeric(as.character(OTC)),
               OTC2 = as.numeric(as.character(OTC.1)),
               OTC3 = as.numeric(as.character(OTC.2)),
               CTL1 = as.numeric(as.character(Control)),
               CTL2 = as.numeric(as.character(Control.1)),
               CTL3 = as.numeric(as.character(Control.2))) %>%
        select(-c(X, X.1, OTC, OTC.1, OTC.2, Control, Control.1, Control.2)) %>%
        group_by(Date) %>%
        summarize_all(mean, na.rm=TRUE)

      # Converting to long-format and calculating flux as g CO2/day
      soilresp_raw_long <- soilresp_raw %>%
        gather(Plot, Flux, OTC1:CTL3, factor_key=FALSE) %>%
        mutate(Treatment=ifelse(startsWith(Plot,"O"), "OTC", "Control") ,
               Flux_g_CO2_m2_day = (Flux*3600*24*44/(10^6))
        ) %>%
        rename(Flux_umol_CO2_m2_s = Flux)

        # Changing NaNs to NAs
        soilresp_raw_long[ is.na(soilresp_raw_long) ] <- NA

      # Summarizing for July 2015, 2016, and 2017  (apparently there are 2016 data?)
      ITEX.FD.data.GROW2015 <- soilresp_raw_long %>%
        filter(Date >= "2015-07-01" & Date <= "2015-07-31") %>%
        group_by(Plot, Treatment) %>%
        summarise_all(mean, na.rm=TRUE)

      ITEX.FD.data.GROW2016 <- soilresp_raw_long %>%
        filter(Date >= "2016-07-01" & Date <= "2016-07-31") %>%
        group_by(Plot, Treatment) %>%
        summarise_all(mean, na.rm=TRUE)

      ITEX.FD.data.GROW2017 <- soilresp_raw_long %>%
        filter(Date >= "2017-07-01" & Date <= "2017-07-31") %>%
        group_by(Plot, Treatment) %>%
        summarise_all(mean, na.rm=TRUE)

      soil_resp <- bind_rows(ITEX.FD.data.GROW2015,
                                   ITEX.FD.data.GROW2016,
                                   ITEX.FD.data.GROW2017) %>%
        mutate(Year = year(Date))


    }

  )


)
