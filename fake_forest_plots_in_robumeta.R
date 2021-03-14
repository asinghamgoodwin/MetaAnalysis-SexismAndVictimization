library(robumeta)

# Load data
data(oswald2013.ex1)

# Run intercept only model.
oswald_intercept <- robu(formula = effect.size ~ 1, data = oswald2013.ex1,
                         studynum = Study, var.eff.size = var.eff.size,
                         rho = 0.8, small = TRUE)

# Create forest plot.
forest.robu(oswald_intercept, es.lab = "Crit.Cat", study.lab = "Study",
            "Effect Size" = effect.size, # optional column
            "Weight" = r.weights) # optional column




#####

set.seed(123)
replication_counts = c(3, 2, 1, 1, 1)

fake_data = tibble(
  es_id = 1001:1008,
  study_id = rep(1:5, replication_counts),
  authors = rep(c("Author1", "Author2", "Author3", "Author4", "SameAuthor1"), replication_counts),
  pub_date = rep(2001:2005, replication_counts),
  unit_of_analysis = rep(c("City/MSA", "County", "Neighborhood", "County", "State"), replication_counts),
  setting = rep(c("Urban", "Rural", "Urban", "Rural", "USA"), replication_counts),
  outcome_data_source = rep(c("FBI", "Survey", "Survey", "Local PD", "FBI"), replication_counts),
  sample_size = rep(c(75, 100, 30, 150, 50), replication_counts),
  result = rnorm(8, mean = 1.03, sd = 0.3), #c(0.8, 0.85, 1.01, 2, 2.1, 0.95, 0.87, 1.5), #what are these??
  error = abs(rnorm(8, mean = 0.04, sd = 0.03)) #c(0.03, 0.04, 0.2, 0.2, 0.23, 0.04, 0.05, 0.01)
)

fake_model = robu(
  formula = result ~ outcome_data_source,#unit_of_analysis + setting + outcome_data_source,
  data = fake_data,
  studynum = study_id, #studynum must be a numeric or factor variable that uniquely identifies each study
  var.eff.size = error, #A vector of user-calculated effect-size variances.
  modelweights = "CORR", #a choice of corr or hier. TODO: after all data is extracted, check to make sure the balance really falls towards this side in our data.
  rho = 0.8, #this is the default value. explain what it is and why the default is appropriate
  small = TRUE #this is the default. how do we know if we want the small sample correction?
  # QUESTION: how to account for heirarchical clustering in our sample (i.e. same author1/5)?
)

fake_model_INTERCEPT_ONLY = robu(
  formula = result ~ 1,
  data = fake_data,
  studynum = study_id, #studynum must be a numeric or factor variable that uniquely identifies each study
  var.eff.size = error, #A vector of user-calculated effect-size variances.
  modelweights = "CORR", #a choice of corr or hier. TODO: after all data is extracted, check to make sure the balance really falls towards this side in our data.
  rho = 0.8, #this is the default value. explain what it is and why the default is appropriate
  small = TRUE #this is the default. how do we know if we want the small sample correction?
  # QUESTION: how to account for heirarchical clustering in our sample (i.e. same author1/5)?
)

forest_plot = forest.robu(
  fake_model_INTERCEPT_ONLY,
  es.lab = "es_id",
  study.lab = "study_id"
)
