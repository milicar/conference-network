(ns conference-network.test.routes.home
  (:use midje.sweet
        conference-network.routes.home))

(facts "about validation of form inputs, given a certain request map"
       (let [hashtags-msg '("All items in hashtags must satisfy the predicate")]
         (fact "one valid hashtag doesn't return errors"
               (validate-form-fields --req-params--) => nil
               (provided --req-params-- =contains=> {:params {:hashtags "#conference"}}))
         (fact "one valid handle doesn't return errors"
               (validate-form-fields --req-params--) => nil
               (provided --req-params-- =contains=> {:params {:hashtags "@conf"}}))
         (fact "two valid hashtags don't return errors"
               (validate-form-fields --req-params--) => nil
               (provided --req-params-- =contains=> {:params {:hashtags "#conf #again"}}))
         (fact "two valid handles don't return errors"
               (validate-form-fields --req-params--) => nil
               (provided --req-params-- =contains=> {:params {:hashtags "@again @conf"}}))
         (fact "valid hashtags and handles don't return errors"
               (validate-form-fields --req-params--) => nil
               (provided --req-params-- =contains=> {:params {:hashtags "#hash1 #hash2 @handle1 @handle2"}}))
         (fact "non-#@ returns errors"
               (validate-form-fields --req-params--) => {:hashtags hashtags-msg}
               (provided --req-params-- =contains=> {:params {:hashtags "hash"}}))
         (fact "non-#@ followed by valid #@ returns errors"
               (validate-form-fields --req-params--) => {:hashtags hashtags-msg}
               (provided --req-params-- =contains=> {:params {:hashtags "hash #hash1"}}))
         (fact "valid #@ followed by non-#@ returns errors"
               (validate-form-fields --req-params--) => {:hashtags hashtags-msg}
               (provided --req-params-- =contains=> {:params {:hashtags "#hash1 hash @hash hhh"}}))
         (fact "hashtag/handle with numbers and symbols is also valid, as long as it starts with #|@"
               (validate-form-fields --req-params--) => nil
               (provided --req-params-- =contains=> {:params {:hashtags "#hash1?any_way``` @hash$%^&*()+=/\\"}})))




)