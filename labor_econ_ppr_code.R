library(data.table)
library(tidyverse)
library(dplyr)
library(plyr)
library(ggplot2)
library(tm) 
library(gsubfn)
library(koRpus)
library(textstem)
library(tidytext)

ds_jobs <- fread("~/Downloads/data_scientist_united_states_job_postings_jobspikr.csv")
se_jobs <- fread("~/Downloads/software_developer_united_states_1971_20191023_1.csv")

#STEP 1: Cleaning text to extract relevant features for education and skills
ds_jobs$state2 = substr(ds_jobs$state,  1, 2)
se_jobs$state2 = substr(se_jobs$state,  1, 2)

#preprocessing/removing stop words:
pat <- "(?<=^| )(\\S{1,6})(?=$| )"
ds_jobs$job_description2 <- gsub("[.]", " ", gsubfn(pat, ~ gsub("[.]", "", ..1), ds_jobs$job_description, perl = TRUE))
se_jobs$job_description2 <- gsub("[.]", " ", gsubfn(pat, ~ gsub("[.]", "", ..1), se_jobs$job_description, perl = TRUE))

#removing apostrophes but not inserting space:
ds_jobs$job_description2<- str_replace_all(ds_jobs$job_description2, "[\'\"]", "")
ds_jobs$job_description2 <- str_replace_all(ds_jobs$job_description2, "[[:punct:]]", " ")
ds_jobs$job_description2 <- removePunctuation(ds_jobs$job_description2)

se_jobs$job_description2<- str_replace_all(se_jobs$job_description2, "[\'\"]", "")
se_jobs$job_description2 <- str_replace_all(se_jobs$job_description2, "[[:punct:]]", " ")
se_jobs$job_description2 <- removePunctuation(se_jobs$job_description2)

ds_jobs$job_description2 <- lemmatize_strings(ds_jobs$job_description2)
se_jobs$job_description2 <- lemmatize_strings(se_jobs$job_description2)


#Finding whether all states are represented, which job boards are scraped:
se_jobs$state2 = toupper(se_jobs$state2)
ds_jobs$state2 = toupper(ds_jobs$state2)

#Dropping observations where states don't make sense:
ds_jobs$state2[ds_jobs$state2 == "HA"] <- "HI"
ds_jobs$state2[ds_jobs$state2 == "LO"] <- "LA"
ds_jobs$state2[ds_jobs$state2 == "VI"] <- "VA"
#Note "RE" stands for remote.  "UN" stands for United States. "WO"  = work at home.  recoding these all to remote
ds_jobs$state2[ds_jobs$state2 == "UN" | ds_jobs$state2 == "WO"] <- "RE"
ds_jobs = subset(ds_jobs, state2 != "EN" & state2 != "NN" &state2 != "PR" & state2 != "" )
#Missing obs from Alaska only, arkansas and wyoming. Includes DC. 
subset(se_states, !(state %in% ds_states$state))

se_jobs$state2[se_jobs$state2 == "HA"] <- "HI"
se_jobs$state2[se_jobs$state2 == "LO"] <- "LA"
se_jobs$state2[se_jobs$state2 == "VI"] <- "VA"
se_jobs$state2[se_jobs$state2 == "UN"] <- "RE"

se_jobs = subset(se_jobs, state2 != "EN"  &state2 != "PR" & state2 != "" )
se_states = as.data.frame(unique(se_jobs$state2))
ds_states = as.data.frame(unique(ds_jobs$state2))
colnames(se_states) = c("state")
colnames(ds_states) = c("state")

#Cleaning up boards:
ds_jobs = subset(ds_jobs, job_board != "")
se_boards =  as.data.frame(unique(se_jobs$job_board))
ds_boards =  as.data.frame(unique(ds_jobs$job_board))

#removing stopwords
tm_stopwords <- str_replace_all(stopwords(), "[\'\"]", "")
tm_stopwords <- c(tm_stopwords, stopwords())

#Also removing words that relate to job position (data science, software developer)
custom_stopwords <- c( "read what people say about work here", "thank you", "overview",  "contract", "sexual", "orientation", "veteran", "status","equal", "opportunity", "employer", 
                       "race", "religion", "sex", "job" ,"summary", "consideration", "national", "origin", 
                      "receive","consideration", "location", "medical", "dental", "ideal", "candidate", 
                      "affirmative", "applicant", "qualify", "qualification", "prefer", "farmer", "gender", "identity", "scientist", "relate", "software", "developer")

ds_jobs.tidy <- ds_jobs %>% 
  unnest_tokens(word, job_description2) %>% 
  filter(!word %in% custom_stopwords) %>%
  filter(!word %in% tm_stopwords)

#Find frequency of most common words:
word_freq <- count(ds_jobs.tidy, 'word')
word_freq <- word_freq[order(-word_freq$freq),]
top_word_freq <- word_freq[1:100,]
knitr::kable(top_word_freq, "markdown", digits = 4)
#also will help in cleaning data if they are not relevant

#Getting bigrams:
ds_jobs.bigrams <- ds_jobs %>% 
  unnest_tokens(word, job_description2, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% custom_stopwords)  %>%
  filter(!word2 %in% custom_stopwords)  %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(word,word1, word2, sep = " ") 

#Find frequency of most common bigrams:
bigram_freq <- count(ds_jobs.bigrams, 'word')
bigram_freq <- bigram_freq[order(-bigram_freq$freq),]

#removing "learn technique", "learn model" and  "learn algorithm" since they are usually machine-learning:
#also removing "datum science" since that is the job
bigram_freq= subset(bigram_freq, word != "learn technique" & word != "learn model" & word != "learn algorithm"  & word != "datum science" )
top_bigram_freq <- bigram_freq[1:100,]
knitr::kable(top_bigram_freq, "markdown", digits = 4)
top_bigram_freq20 <- bigram_freq[1:20,]
ggplot(top_bigram_freq20 , aes(x=reorder(word, -freq), y = freq)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(
             size=10, angle=90)) +  xlab("Bigram") + ylab("Frequency") 


#doing same for comp science job postings:
#remove software developer?  since it is title of job?
se_jobs.bigrams <- se_jobs %>% 
  unnest_tokens(word, job_description2, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% custom_stopwords)  %>%
  filter(!word2 %in% custom_stopwords)  %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(word,word1, word2, sep = " ") 

#Find frequency of most common bigrams:
se_bigram_freq <- count(se_jobs.bigrams, 'word')
se_bigram_freq <- se_bigram_freq[order(-se_bigram_freq$freq),]

#removing "learn technique", "learn model" and  "experience develop" since they are usually machine-learning or  duplicates of other bigrams:
se_bigram_freq= subset(se_bigram_freq, word != "learn technique" & word != "learn model" & word != "experience develop" )
top_se_bigram_freq <- se_bigram_freq[1:100,]
knitr::kable(top_se_bigram_freq, "markdown", digits = 4)
top_se_bigram_freq20 <- se_bigram_freq[1:20,]
ggplot(top_se_bigram_freq20 , aes(x=reorder(word, -freq), y = freq)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(
  size=10, angle=90)) +  xlab("Bigram") + ylab("Frequency") 

###PART 2: Looking at fraction of skills versus education, summmary stats by state.
#Separating top 100 bigrams into skills, education, etc.

#getting total relevant words per posting:
word_count = ds_jobs.tidy %>% group_by(uniq_id) %>% tally()


#getting number of skill bigrams per posting:

custom_skill_bigrams <- c("machine learn", "deep learn", "datum visualization", "statistical analysis",
 "statistical model",  "artificial intelligence", "natural language", "program language", "unstructured datum",
 "datum mine", "neural network", "scikit learn", "random forest", "engineer", "relational database", "software development", 
 "time series", "visualization tool", "analytic skill", "decision tree", "advance statistical", 
 "genetic information", "information technology", "professional experience", "statistical method", "advance analytics", "unstructured datum",
 "language process", "cross functional", "visualization tool", "track record", "datum collection", "product development", "real time",
 "customer experience", "project management", "write communication", "real world", 
 "team member" , "verbal communication"
)

#test with this small sample:
 
test =  ds_jobs.bigrams[grep(paste(custom_skill_bigrams, collapse="|"), ds_jobs.bigrams$word),]
#count bigrams by id:
word_count_skillbigrams = test %>% group_by(uniq_id) %>% tally()

#Need to mult bigrams by 2 to account for total number of words at end.
colnames(word_count_skillbigrams) = c("uniq_id", "num_skill_bigrams")
#merge back with data to combine data:
ds_jobs.bigrams_skill = join(ds_jobs.bigrams,word_count_skillbigrams )
#Graphing top bigrams:
ds_jobs.bigrams_wc = join(ds_jobs.bigrams_skill,word_count )
ds_jobs.bigrams_wc$term_freq =  ds_jobs.bigrams_wc$num_skill_bigrams/ds_jobs.bigrams_wc$n







bigram_tf_idf <- ds_jobs.bigrams %>%
  arrange(desc(tf_idf))
#need to remove duplicates for each posting and bigram combo...

top_words = ds_jobs.bigrams_wc[!duplicated(ds_jobs.bigrams_wc$uniq_id), ]


top_words2 <- top_words %>%
  bind_tf_idf(word, uniq_id, num_skill_bigrams)


custom_skill_unigrams <- c("python", "r", "sql","optimization")

custom_education_bigrams <- c("bachelor degree", "quantitative field", "master degree", "advance degree", 
  "education", "bs", "ms", "phd", "university", "subject matter"
)


hard_skill_bigrams <-c()
  
  
soft_skill_bigrams <-c("customer experience", "project management", "write communication", "real world", 
                    "team member" , "verbal communication" )


#bigrams that could be both?  computer science, 
  
  
#getting number of education bigrams per posting:

#Words pertaining to soft skills - create list from top 100 bigrams, or just visually analyze output?




#Words pertaining to hard skills:


#Words pertaining to education:




#Then for each listing, count words that are among the most common skill words (say top 50) and those among
#the top education words (50) and calculate fraction to see what pattern emerges. 
#skill frac = skill words/total words
#educ frac = education words/total words

#how to compare these? 

#Then look at fractions by state, or position title.



#Then combine with company data and see which companies are more skill heavy and which are more education 
#focused, or both.


###do this for comp science positions too!  Make at least 5 tables. 

desc = as.character(desc)
combdesc <- unlist(strsplit(desc,split=", "))
ASCII_index <- grep("combdesc", iconv(combdesc,"latin1","ASCII",sub="combdesc"))

subdesc <- combdesc[ - ASCII_index]

subsamp <- paste(subdesc,split=", ")


corpus <- Corpus(VectorSource(decs1))
corpus <- tm_map(corpus, removeWords, stopwords('english'))
job_desc <-data.frame(text=unlist(sapply(corpus, `[`, "content")), 
                                               stringsAsFactors=F)



#Merging with Compustat data to get company info
#check for matches, look to see if company posting job is subsidiary if not matched. 
