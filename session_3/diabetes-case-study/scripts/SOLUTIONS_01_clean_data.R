library(tidyverse)
library(here)
library(readxl)
library(janitor)


# Load Data -------------------------------------------------

# ERROR - need file extension .csv (even though this isn't in the name)
# variables_raw <- read_csv(here("raw-data", 
#                      "Diabetes final_draft 2018 FINAL_FOR_REAL_draftydraft_v2.01-2019"))


variables_raw <- read_csv(here("raw-data", 
                         "Diabetes final_draft 2018 FINAL_FOR_REAL_draftydraft_v2.01-2019.csv"))


# ERROR - because it is an xls not a csv, use read_excel instead
# outcome <-  read_csv(here("raw-data", 
#                         "outcome-diab-data_05062017_19-2019.xls"))

outcome_raw <-  read_excel(here("raw-data", 
                            "outcome-diab-data_05062017_19-2019.xls"))


# Clean Data -------------------------------------------------

# rename variables --------

names(variables_raw)

# ERROR - need backticks `Stabilized Glucose` because there is a space
# variables_raw %>%
#   rename("stabilized_glucode" = Stabilized Glucose)

variables <- variables_raw %>%
  clean_names() %>%
  rename("ratio_chol_hdl" = ratio)

names(outcome_raw)

# Join the two data sets ---------
setdiff(outcome_raw$individual_id, variables$id)

df <- variables %>%
  left_join(outcome_raw, by = c("id" = "individual_id"))

# Check for dupes --------------------

# make a quick frequency table
df %>% 
  group_by(id) %>%
  count() %>% 
  arrange(desc(n))

# further inspection of other variables
df %>%
  group_by(id) %>%
  mutate(n_obs = n()) %>%
  filter(n_obs > 1) 

# get rid of duplicate rows
df <- df %>%
  distinct()

# Check Variables -------------------

# what data type is each variable?
str(df)
glimpse(df) 


# check columns that should be numeric but are being coerced 
df %>%
  mutate(stabilized_glucose_num = as.numeric(stabilized_glucose)) %>%
  select(stabilized_glucose, stabilized_glucose_num) %>%
  filter(is.na(stabilized_glucose_num) & !is.na(stabilized_glucose))
# ... and do again for hdk, ratio_chol_hdl, waist


# ~~~* ADVANCED TIP *~~~~
# let's write a function to do this 
check_coercion <- function(vec) {
  coerced_vec = as.numeric(vec)
  setdiff(vec, coerced_vec) %>%
    return()
}

# run our function
check_coercion(df$stabilized_glucose)
check_coercion(df$hdl)
check_coercion(df$ratio_chol_hdl)
check_coercion(df$waist)

# ~~~* ADVANCED TIP *~~~~
# an even better way to run our function
df %>%
  select(stabilized_glucose, hdl, ratio_chol_hdl, waist) %>%
  map(., ~check_coercion(.x))

# now that we've checked this, we can safely convert these cols to numeric
df <- df %>%
  mutate(stabilized_glucose = as.numeric(stabilized_glucose), 
         hdl = as.numeric(hdl), 
         ratio_chol_hdl = as.numeric(ratio_chol_hdl), 
         waist = as.numeric(waist))

glimpse(df)

# ~~~* ADVANCED TIP *~~~~
# an even better way to do the above code
df <- df %>%
  mutate_at(vars(stabilized_glucose, hdl, ratio_chol_hdl, waist),
            ~as.numeric(.x))


# Clean categoricals -------------------------
# inspect categoricals

# which are categoricals?
glimpse(df)

df %>%
  select_if(is.character) 

# what do the levels look like?
df %>%
  group_by(location) %>%
  count()

# another way to do this
# table(df$location)

# From the above, I can already tell that capitalizations are going to be a problem. Let's lower case all strings.
df <- df %>%
  mutate(location = tolower(location), 
         gender = tolower(gender), 
         frame = tolower(frame))

df %>%
  group_by(gender) %>%
  count()

df %>%
  group_by(frame) %>%
  count()

# let's fix all the non-matching category levels (can also use ifelse, or case_when to do this)
df <- df %>%
  mutate(location = fct_collapse(location, 
                                 buckingham = c("bckingham", "buckingham"), 
                                 louisa = c("louisa", "louiseee")), 
         gender = fct_collapse(gender, 
                               female = c("f", "female"), 
                               male = c("m", "male"))
  )

# quick look at the na's
# can do individually
sum(is.na(df$id))
sum(is.na(df$hip)) # etc....

# or all at once!
map(df, ~sum(is.na(.x)))

# Save Data -------------------------------------------------


save(df, file = here("data", "diabetes.RData"))




#------------------------------------------------------------------------------

# ~~~* ADVANCED TIP *~~~~ 
# Let's do all of the above in ONE LONG GLORIOUS PIPE!!!


variables_raw <- read_csv(here("raw-data", 
                               "Diabetes final_draft 2018 FINAL_FOR_REAL_draftydraft_v2.01-2019.csv")) 
outcome_raw <-  read_excel(here("raw-data", 
                                "outcome-diab-data_05062017_19-2019.xls"))

df <- variables_raw %>%
  clean_names() %>%
  left_join(outcome_raw, by = c("id" = "individual_id")) %>%
  rename("ratio_chol_hdl" = ratio) %>%
  
  # 2 duplicate IDs in raw data- data was same across both obs
  distinct() %>%
  
  # coerce into numeric, coerced NA's were blank or missing
  mutate_at(vars(stabilized_glucose, hdl, ratio_chol_hdl, waist),
            ~as.numeric(.x)) %>%
  
  # make character all lowercase
  mutate_at(vars(location,gender, frame),
            ~tolower(.x)) %>%
  
  # manually recode some factors
  mutate(location = fct_collapse(location, 
                                 buckingham = c("bckingham", "buckingham"), 
                                 louisa = c("louisa", "louiseee")), 
         gender = fct_collapse(gender, 
                               female = c("f", "female"), 
                               male = c("m", "male"))
  )

save(df, file = here("data", "diabetes.RData"))

