(in-package :fileworthy)

#||
### `PAGE-ABOUT`
||#
(defun page-about ()
  "About page."
  (page-template
    "About"
    "about-page"
    (gen-html
      (:h2 "About Fileworthy")
      (:p (sf '("Fileworthy aims to be a simple solution to managing your "
                "notes and files across many devices. It is half static site "
                "generator, half file-system.")))
      (:table :class "simple-table"
       (:tr
         (:td "Version")
         (:td (str (app-version *app*))))
       (:tr
         (:td "Last Updated")
         (:td (str (pretty-time (app-last-updated *app*)))))
       (:tr
         (:td "Source Code")
         (:td (:a :href "https://github.com/thiru/fileworthy"
               "Hosted at Github")))
       (:tr
         (:td "License")
         (:td
           (:a :href "https://www.gnu.org/licenses/gpl-3.0.html" "GPL v3")))
       (:tr
         (:td "Copyright")
         (:td "2017 Thirushanth Thirunavukarasu"))))))
