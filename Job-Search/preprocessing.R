
source("functions_def.R")

states = read.csv(file.path('data', "states.csv"), stringsAsFactors = F)
files = dir('data')

# --------------------------------------------------------------------------
# I. fitting LDA model:

# check if lda_model exists and makes model if not

if (!('lda_model.RData' %in% files)) {
  # Download the data and description for "Data Analyst"
  
  job_train = data.frame()
  for (i in seq_along(states$State)) {
    location = as.character(states$State[i])
    new_jobs = JobDescript("data", 20, location)
    new_jobs$location = as.character(new_jobs$location)
    job_train = bind_rows(job_train, new_jobs)
  }
  
  save(job_train, file = file.path('data','job_train.RData'))
  
  # Clean the descriptions
  job_corpus = data2corpus(job_train)
  clean_job_corpus = clean_corpus(job_corpus)
  stem_job_corpus = clean_job_corpus %>% tm_map(stemDocument)
  
  # transform into document term matrix
  dtm = DocumentTermMatrix(stem_job_corpus)
  
  # remove terms that are in greater than 80% of documents
  dtm_reduced = removeCommonTerms(dtm, 0.8)
  dtm_dense = removeSparseTerms(dtm_reduced, 0.9)
  
  # Run the LDA model and get the 5 clusters
  set.seed(0)
  lda_model = LDA(dtm_dense, 3)
  
  # save LDA model
  save(lda_model, file = file.path('data','lda_model.RData'))
}

load(file.path('data', 'lda_model.RData'))
load(file.path('data', 'job_train.RData'))





# --------------------------------------------------------------------------
# II.Webscraping for mapping

if (!('geo.RData' %in% files)) {
  GET("http://www.mapcruzin.com/fcc-wireless-shapefiles/cities-towns.zip",
      write_disk("cities.zip", overwrite = TRUE))
  unzip("cities.zip", exdir="cities")
  
  # read in the shapefile
  shp = readOGR("cities/citiesx020.shp", "citiesx020")
  
  # extract the city centroids with name and state
  geo =
    gCentroid(shp, byid=TRUE) %>%
    data.frame() %>%
    select(lon = x, lat = y) %>%
    mutate(city=shp@data$NAME, state=shp@data$STATE)
  
  save(geo, file = file.path('data','geo.RData'))
  file.remove("cities.zip")
  unlink("cities", recursive=TRUE)
}

load(file.path('data','geo.RData'))





# --------------------------------------------------------------------------
# III. Obtain relative job title of each category

category = cbind(job_train$results.jobtitle, job_train$results.company, job_train$results.state,
                 topics(lda_model) %>%
                   matrix(ncol = 1)) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(c("JobTitle", "Company", "Abbreviation", "Cluster"))






# --------------------------------------------------------------------------
# IV. load data for shiny

job_category = setNames(seq(1,3), c('Cluster 1', 'Cluster 2', 'Cluster 3'))
states = states %>% arrange(State)
