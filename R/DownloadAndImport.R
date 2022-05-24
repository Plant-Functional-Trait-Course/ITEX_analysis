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
             format = "file")
  # tar_target(
  #   name = cflux_download,
  #   command = get_file(node = "smbqh",
  #                      file = "Cflux_SV_ITEX_2018.csv",
  #                      path = "data",
  #                      remote_path = "C-Flux"),
  #   format = "file"
  # )

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
  )

)
