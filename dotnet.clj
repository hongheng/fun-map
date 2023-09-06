(ns dotnet
  (:require [clojure.string :as string]
            [clojure.test :refer [run-tests]])
  (:import [System.IO Directory SearchOption Path]))

(defn file-path->file-ns
  "get pure namespace part in path"
  [root file-path]
  (subs file-path
        (+ 1 (count root))
        (- (count file-path)
           (count (Path/GetExtension file-path)))))

(defn file-ns->ns
  "transfer to standard namespace string"
  [file-ns]
  (-> file-ns
      (string/replace "/" ".")
      (string/replace "_" "-")))

(defn test-cljr
  "run tests with src and test folder"
  [src test]
  (let [load-path (string/join ":" [src test])
        test-namespaces (->> (Directory/GetFiles test "*.clj*" SearchOption/AllDirectories)
                             (map file-path->file-ns (repeat test))
                             (map file-ns->ns)
                             (map symbol))]
    (Environment/SetEnvironmentVariable "CLOJURE_LOAD_PATH" load-path)
    (doseq [ns test-namespaces]
      (require ns)
      (run-tests ns))))

; run cljr with: Clojure.Main dotnet.clj
(test-cljr "src" "test")
