(ns gedcom-importer.name_test
  (:use clojure.test
        gedcom-importer.names))

(deftest name-comonents-test
  (let [name-map {:first "Foo" :middle "Bar Baz" :last "Quux" :suffix "jr"}]
    (is (= name-map (name-components "Foo Bar Baz /Quux/ jr")))
    (is (= name-map (name-components "Foo Bar Baz/Quux/jr")))
    (is (= {:first nil :middle nil :last "Quux" :suffix nil}
           (name-components "/Quux/")))
    (is (= {:first nil :middle nil :last "Quux" :suffix "jr"}
           (name-components "/Quux/jr")))))
