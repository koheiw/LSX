require(quanteda)

data_dictionary_sentiment <- dictionary(
    list(
        "positive" = c("good", "nice", "excellent", "positive", "fortunate", "correct", "superior"),
        "negative" = c("bad", "nasty", "poor", "negative", "unfortunate", "wrong", "inferior")
    )
)
save(data_dictionary_sentiment, file = "data/data_dictionary_sentiment.RData")


data_dictionary_ideology <- dictionary(
    list(
        "right" = c("deficit", "austerity", "unstable", "recession", "inflation", "currency", "workforce"),
        "left" = c("poor", "poverty", "free", "benefits", "prices", "money", "workers")
    )
)
save(data_dictionary_ideology, file = "data/data_dictionary_ideology.RData")
