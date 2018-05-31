(ns conference-network.test.routes.home
  (:use midje.sweet
        conference-network.routes.home))

(facts "about validation of form inputs, given a certain request map"
       (let [hashtags-msg '("Check if all hashtags start with # and handles with @")]
         (fact "one valid hashtag doesn't return errors"
               (validate-form-fields --req-params--) =not=>  (contains {:hashtags hashtags-msg})
               (provided --req-params-- =contains=> {:params {:hashtags "#conference"}}))
         (fact "one valid handle doesn't return errors"
               (validate-form-fields --req-params--) =not=>  (contains {:hashtags hashtags-msg})
               (provided --req-params-- =contains=> {:params {:hashtags "@conf"}}))
         (fact "two valid hashtags don't return errors"
               (validate-form-fields --req-params--) =not=>  (contains {:hashtags hashtags-msg})
               (provided --req-params-- =contains=> {:params {:hashtags "#conf #again"}}))
         (fact "two valid handles don't return errors"
               (validate-form-fields --req-params--) =not=>  (contains {:hashtags hashtags-msg})
               (provided --req-params-- =contains=> {:params {:hashtags "@again @conf"}}))
         (fact "valid hashtags and handles don't return errors"
               (validate-form-fields --req-params--) =not=>  (contains {:hashtags hashtags-msg})
               (provided --req-params-- =contains=> {:params {:hashtags "#hash1 #hash2 @handle1 @handle2"}}))
         (fact "non-#@ returns errors"
               (validate-form-fields --req-params--) => (contains {:hashtags hashtags-msg})
               (provided --req-params-- =contains=> {:params {:hashtags "hash"}}))
         (fact "non-#@ followed by valid #@ returns errors"
               (validate-form-fields --req-params--) => (contains {:hashtags hashtags-msg})
               (provided --req-params-- =contains=> {:params {:hashtags "hash #hash1"}}))
         (fact "valid #@ followed by non-#@ returns errors"
               (validate-form-fields --req-params--) => (contains {:hashtags hashtags-msg})
               (provided --req-params-- =contains=> {:params {:hashtags "#hash1 hash @hash hhh"}}))
         (fact "hashtag/handle with numbers and symbols is also valid, as long as it starts with #|@"
               (validate-form-fields --req-params--) =not=>  (contains {:hashtags hashtags-msg})
               (provided --req-params-- =contains=> {:params {:hashtags "#hash1?any_way``` @hash$%^&*()+=/\\"}}))))

(facts "about validating date"
       ; validate-form-fields calls another function that requires :hashtags to be present
       ; maybe should have tried background prerequisites..
       (let [date-msg '("Check the date format.")
             order-msg '("Dates are in the wrong order.")]
         (fact "valid start-date format doesn't return errors"
               (validate-form-fields --req-params--) =not=> (contains {:startdate date-msg})
               (provided --req-params-- =contains=> {:params {:startdate "2018-05-31" :hashtags "#hash"}}))
         (fact "valid end-date format doesn't return errors; also, dates are in correct order"
               (validate-form-fields --req-params--) =not=> (contains {:enddate date-msg})
               (provided --req-params-- =contains=> {:params {:enddate "2018-05-05" :hashtags "#hash" :startdate "2018-01-01"}}))
         (fact "date is valid with leading and trailing blanks as well"
               (validate-form-fields --req-params--) =not=> (contains {:startdate date-msg})
               (provided --req-params-- =contains=> {:params {:startdate "  2018-05-31  " :hashtags "#hash"}}))
         (fact "date is not valid if contains other text"
               (validate-form-fields --req-params--) => (contains {:startdate date-msg})
               (provided --req-params-- =contains=> {:params {:startdate ":  2018-05-31" :hashtags "#hash"}}))
         (fact "dates in wrong order return errors"
               (validate-form-fields --req-params--) => (contains {:enddate order-msg})
               (provided --req-params-- =contains=> {:params {:startdate "2018-05-31" :enddate "2018-01-05" :hashtags "#hash"}}))
         (fact "same dates don't return errors"
               (validate-form-fields --req-params--) =not=> (contains {:enddate order-msg})
               (provided --req-params-- =contains=> {:params {:startdate "2018-05-05" :enddate "2018-05-05" :hashtags "#hash"}}))))
