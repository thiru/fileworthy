(in-package :fileworthy)

#||
### `PAGE-FS-PATH`

* This page displays a file-system path
  * i.e. a directory or file
* If the file appears to be a binary file, don't show it but provide links with options
* If the path is a directory with one non-binary file in it, just show it
||#
(defun page-fs-path ()
  "File-system path page."
  (let* ((user (empty 'user :unless (session-value 'user)))
         (path-name (script-name* *request*))
         (path-segs (split-sequence #\/ path-name :remove-empty-subseqs t))
         (last-path-seg (last1 path-segs))
         (expanded-paths (expand-sub-dirs path-name))
         (abs-fs-path (empty 'string
                             :unless (get-fs-path-from-url user path-name)))
         (path-is-dir? (if (not (empty? abs-fs-path))
                         (directory-exists-p abs-fs-path)))
         (path-is-file? (if (and (not path-is-dir?)
                                 (not (empty? abs-fs-path)))
                         (file-exists-p abs-fs-path)))
         (binary-file? (if path-is-file? (is-file-binary? abs-fs-path)))
         (curr-file-name "")
         (file-content nil)
         (file-names (get-file-names abs-fs-path))
         (dir-contains-index-file? (and file-names
                                        (find "index.md"
                                              file-names
                                              :test #'string-equal))))
    ;; Check anonymous access
    (if (and (empty? user)
             (not (config-allow-anonymous-read *config*)))
      (return-from page-fs-path (page-error-not-authorised)))
    ;; Show 404 page if dir/file not found
    (if (and (not path-is-dir?) (not path-is-file?))
      (return-from page-fs-path (page-error-not-found)))
    ;; Download file
    (if (and path-is-file?
             (or binary-file? (get-parameter "download")))
      (return-from page-fs-path (handle-static-file abs-fs-path)))
    ;; File requested
    (when path-is-file?
      (setf curr-file-name last-path-seg)
      (when (or (not binary-file?) (get-parameter "force-show"))
        (setf file-content (get-file-content abs-fs-path))))
    ;; Directory requested, but only one text file in dir so show it
    (when (and path-is-dir?
               (= 1 (length file-names)))
      (setf abs-fs-path (concatenate 'string
                                     (to-string abs-fs-path)
                                     (first file-names)))
      (if (is-file-binary? abs-fs-path)
        (setf curr-file-name (first file-names))
        (when (or (not binary-file?) (get-parameter "force-show"))
          (setf file-content (get-file-content abs-fs-path)))))
    ;; Directory requested and has "index.md" file, so show it
    (when (and path-is-dir? dir-contains-index-file?)
      (setf abs-fs-path (concatenate 'string
                                     (to-string abs-fs-path)
                                     "index.md"))
      (setf curr-file-name "index.md")
      (setf file-content (get-file-content abs-fs-path)))
    ;; TODO: fix JS injection
    (page-template
      (if (empty? last-path-seg) "Home" last-path-seg)
      "fs-path-page"
      (gen-html
        (:div :id "search-group"
          (:input
            :id "search"
            :autocomplete "off"
            :placeholder "Search page names"
            :onclick "page.onSearchTxtClick(event)"
            :onkeydown "page.onSearchTxtKeyDown(event)"
            :onkeyup "page.onSearchTxtKeyUp(event)")
          (:select
            :id "search-type"
            :onchange "page.onSearchTypeChange(event)"
            :title "Select the type of search to perform"
           (:option
             :data-long-text "Search page names"
             :title "Search names of pages"
             :value "page"
             "Pages")
           (:option
             :data-long-text "Search page content (text)"
             :title "Search within plain text pages (i.e. not binary)"
             :value "text"
             "Content")
           (:option
             :data-long-text "Search page content (text + binary)"
             :title "Searches within all pages, including binary (e.g. Word, Excel, etc.)"
             :value "text+binary"
             "Content+")))
        (:div :id "search-info" "")
        (:select
          :id "search-results"
          :class "hidden"
          :data-default-size "10"
          :onclick "page.onSearchResultsClick(event)"
          :onkeydown "page.onSearchResultsKeyDown(event)"
          :size 10)
        (if (not (empty? file-names))
          (htm
            (:div :id "file-names-nav"
             (:a
               :id "file-names-toggle"
               :href "javascript:site.toggleFilesNav()"
               :title "Hide list of pages"
               "Hide "
               (:i :class "fa fa-minus-square" " "))
             (:table :id "files" :class "file-names"
              (:tbody
                (loop
                  :for file-name :in file-names
                  :collect
                  (htm
                    (:tr
                      :class
                      (if (string= file-name curr-file-name)
                        "selected"
                        nil)
                      (:td
                        (:a
                          :href file-name
                          (str file-name)))
                      (:td
                        (:a
                          :class "download"
                          :href (sf "~A?download" file-name)
                          :title "Download file"
                          (:i :class "fa fa-download" "")))))))))))
        (if (>= (length path-segs) 2)
          (htm
            (:section :id "path-trail"
             (:span
               (:a :href "/"
                (:i :class "fa fa-folder-open" " "))
               " ")
             (loop :for path :in expanded-paths
                   :for i :from 0
                   :collect
                   (htm
                     (if (> i 0)
                       (htm
                         (:span "/")))
                     (:a
                       :href
                       (if (and path-is-file?
                                (= (1+ i) (length expanded-paths)))
                         (sf "/~A" path)
                         (sf "/~A/" path))
                       (str (nth i path-segs))))))))
        (if (not (null file-content))
          (htm
            (:section :id "file-details"
             (if (or (not binary-file?) (get-parameter "force-show"))
               (let* ((is-markdown? (cl-ppcre:scan "\\.mk?d$" abs-fs-path))
                      (file-content (cl-ppcre:regex-replace-all
                                      "~"
                                      file-content
                                      "~~")))
                 (if (not is-markdown?)
                   (setf file-content (escape-string file-content)))
                 (htm
                   (:pre :id "raw-file-content" :class "hidden"
                    (:code (write-string file-content)))
                   (:div :id "gen-file-content")))
               (htm
                 (:p "It looks like this is a binary file, so it isn't displayed.")
                 (:p
                   "You can "
                   (:a
                     :href (sf "~A?download" curr-file-name)
                     "download the file")
                   " or try to "
                   (:a
                     :href (sf "~A?force-show" curr-file-name)
                     "display it anyway.")))))))))))
