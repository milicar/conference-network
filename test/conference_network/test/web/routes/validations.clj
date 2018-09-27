(ns conference-network.test.web.routes.validations
  (:require [conference-network.web.routes.validations :refer [validate-form-fields]]
            [midje.sweet :refer :all]))


(facts "about validation of search term"
       (let [hashtag      "#hashtag"
             handle       "@handle"
             plain-term   "query"
             non-alpha    "#hash1?any_way``` @hash$%^&*()+=/\\"
             err-msg      '("Some search terms must be supplied.")]
         (fact "hashtags, handles, or any search term don't return errors"
               (validate-form-fields {:hashtags hashtag}) =not=> (contains {:hashtags err-msg})
               (validate-form-fields {:hashtags handle}) =not=> (contains {:hashtags err-msg})
               (validate-form-fields {:hashtags plain-term}) =not=> (contains {:hashtags err-msg})
               (validate-form-fields {:hashtags non-alpha}) =not=> (contains {:hashtags err-msg})
               (validate-form-fields {:hashtags (str hashtag  " " handle)}) =not=> (contains {:hashtags err-msg})
               (validate-form-fields {:hashtags (str non-alpha  " " plain-term)}) =not=> (contains {:hashtags err-msg}))
         (fact "if no search terms are provided, error message is returned"
               (validate-form-fields {:hashtags ""}) => (contains {:hashtags err-msg})
               (validate-form-fields {:hashtags "  "}) => (contains {:hashtags err-msg}))))

(facts "about validating date"
       ; validate-form-fields calls another function that requires :hashtags to be present
       (let [params {:hashtags "#hash"}
             end-date-msg '("Check end date format.")
             start-date-msg '("Check start date format.")
             order-msg '("Dates are in the wrong order.")]
         (fact "valid start-date format doesn't return errors"
               (let [params (assoc params :startdate "2018-05-31")]
                 (validate-form-fields params) =not=> (contains {:startdate start-date-msg})))
         (fact "valid end-date format doesn't return errors; also, dates are in correct order"
               (let [params (assoc params :enddate "2018-05-25" :startdate "2018-01-01")]
                 (validate-form-fields params) =not=> (contains {:enddate end-date-msg})))
         (fact "date is valid with leading and trailing blanks as well"
               (let [params (assoc params :startdate "  2018-05-31  ")]
                 (validate-form-fields params) =not=> (contains {:startdate start-date-msg})))
         (fact "date is not valid if contains other text"
               (let [params (assoc params :startdate ":  2018-05-31")]
                 (validate-form-fields params) => (contains {:startdate start-date-msg})))
         (fact "dates in wrong order return errors"
               (let [params (assoc params :startdate "2018-05-31" :enddate "2018-01-05")]
                 (validate-form-fields params) => (contains {:enddate order-msg})))
         (fact "same dates don't return errors"
               (let [params (assoc params :startdate "2018-05-05" :enddate "2018-05-05")]
                 (validate-form-fields params) =not=> (contains {:enddate order-msg})))))
