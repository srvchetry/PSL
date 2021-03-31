#schetry2_project3_FA20

mypackages = c("pROC", "text2vec", "glmnet")   # required packages
tmp = setdiff(mypackages, rownames(installed.packages()))  # packages need to be installed
if (length(tmp) > 0) install.packages(tmp)
lapply(mypackages, require, character.only = TRUE)


# start_time = Sys.time()
set.seed(4726)
# j = 1
# setwd(paste("split_", j, sep=""))

# creating custom vocabulary from entire dataset

# train_all = read.table("alldata.tsv",
#                    stringsAsFactors = FALSE,
#                    header = TRUE)
# train_all$review = gsub('<.*?>', ' ', train_all$review)
# stop_words = c("i", "me", "my", "myself",
#                "we", "our", "ours", "ourselves",
#                "you", "your", "yours",
#                "their", "they", "his", "her",
#                "she", "he", "a", "an", "and",
#                "is", "was", "are", "were",
#                "him", "himself", "has", "have",
#                "it", "its", "the", "us")
# it_train = itoken(train_all$review,
#                   preprocessor = tolower,
#                   tokenizer = word_tokenizer)
# tmp.vocab = create_vocabulary(it_train,
#                               stopwords = stop_words,
#                               ngram = c(1L,4L))
# tmp.vocab = prune_vocabulary(tmp.vocab, term_count_min = 10,
#                              doc_proportion_max = 0.5,
#                              doc_proportion_min = 0.001)
# dtm_train  = create_dtm(it_train, vocab_vectorizer(tmp.vocab))
# 
# set.seed(4726)
# tmpfit = glmnet(x = dtm_train,
#                 y = train_all$sentiment,
#                 alpha = 1,
#                 family='binomial')
# tmpfit$df

# myvocab = colnames(dtm_train)[which(tmpfit$beta[, 36] != 0)]
# write.table(myvocab, file = 'myvocab_new.txt',sep = ',', row.names = F, quote = F)


# reading the custom vocab file to run predictions on new test data

myvocab <- scan(file = "myvocab_new.txt", what = character())

train = read.table("train.tsv",
                   stringsAsFactors = FALSE,
                   header = TRUE)
train$review = gsub('<.*?>', ' ', train$review)

it_train = itoken(train$review,
                  preprocessor = tolower, 
                  tokenizer = word_tokenizer)
vectorizer = vocab_vectorizer(create_vocabulary(myvocab, 
                                                ngram = c(1L, 4L)))
dtm_train = create_dtm(it_train, vectorizer)

mylogit.cv = cv.glmnet(x = dtm_train, 
                       y = train$sentiment, 
                       alpha = 0,
                       family='binomial', 
                       type.measure = "auc")
mylogit.fit = glmnet(x = dtm_train, 
                     y = train$sentiment, 
                     alpha = 0,
                     lambda = mylogit.cv$lambda.min, 
                     family='binomial')

test = read.table("test.tsv",
                  stringsAsFactors = FALSE,
                  header = TRUE)
test$review <- gsub('<.*?>', ' ', test$review)
it_test = itoken(test$review,
                 preprocessor = tolower, 
                 tokenizer = word_tokenizer)
dtm_test = create_dtm(it_test, vectorizer)
mypred = predict(mylogit.fit, dtm_test, type = "response")
output = data.frame(id = test$id, prob = as.vector(mypred))
write.table(output, file = "mysubmission.txt", 
            row.names = FALSE, sep='\t')

test.y = read.table("test_y.tsv", header = TRUE)
pred = read.table("mysubmission.txt", header = TRUE)
pred = merge(pred, test.y, by="id")
roc_obj = roc(pred$sentiment, pred$prob)
tmp = pROC::auc(roc_obj)
print(tmp)

# time_diff = difftime(Sys.time(), start_time, units = 'mins')
# print(paste('Run time is:', time_diff, 'mins'))