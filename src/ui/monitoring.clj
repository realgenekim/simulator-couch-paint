(ns ui.monitoring
  (:require
    [clojure.java.jmx :as jmx]
    [genek.sim2 :as sim]
    [taoensso.timbre :as log]
    [membrane.ui :as ui]))

; UsedMemory
; UpTime
; GcTime
; GcCount
; FreeMemory

(comment
  (jmx/mbean-names "*:*")
  (jmx/attribute-names "java.lang:type=Memory")
  (jmx/attribute-names "java.lang:type=Runtime")
  (jmx/attribute-names "java.lang:type=Threading")
  (jmx/attribute-names "com.sun.management:type=HotSpotDiagnostic")
  (jmx/attribute-names "java.lang:type=Compilation")

  (jmx/mbean "java.lang:type=Memory")
  (jmx/mbean "java.lang:type=Runtime")

    ;:Uptime
  (jmx/mbean "java.lang:type=Threading")
  (jmx/mbean "java.lang:type=Compilation")
  (jmx/mbean "java.lang:name=G1 Old Gen,type=MemoryPool")
  (jmx/mbean "java.lang:type=OperatingSystem")

  0)

(defonce *stats (atom nil))

(defn collect-stats!
  []
  ;(log/error :collect-stats! :running)
  (let [g1old (jmx/mbean "java.lang:name=G1 Old Gen,type=MemoryPool")
        os    (jmx/mbean "java.lang:type=OperatingSystem")]
    (reset! *stats {:g1old (-> g1old (select-keys [:PeakUsage :Usage]))
                    :os    (-> os (select-keys [:CpuLoad :ProcessCpuLoad :SystemLoadAverage]))})))

; {:g1old {:CollectionUsageThresholdCount 0,
;         :UsageThresholdCount 0,
;         :PeakUsage {:committed 960495616, :init 243269632, :max 4294967296, :used 849710080},
;         :UsageThreshold 0,
;         :CollectionUsageThresholdSupported true,
;         :MemoryManagerNames #object["[Ljava.lang.String;" 0x3236decf "[Ljava.lang.String;@3236decf"],
;         :CollectionUsageThreshold 0,
;         :ObjectName #object[javax.management.ObjectName 0x3157b2c2 "java.lang:type=MemoryPool,name=G1 Old Gen"],
;         :CollectionUsage {:committed 146800640, :init 243269632, :max 4294967296, :used 118680064},
;         :Usage {:committed 960495616, :init 243269632, :max 4294967296, :used 849710080},
;         :CollectionUsageThresholdExceeded false,
;         :Name "G1 Old Gen",
;         :UsageThresholdSupported true,
;         :Type "HEAP",
;         :UsageThresholdExceeded false,
;         :Valid true}}


(defn show-leaf-counter
  []
  (let [counter (-> @sim/*leaf-counter)]
    (ui/vertical-layout
      (ui/label (str "Leaf counter: " counter))
      (ui/spacer 20)
      (ui/label (str "G1 Old Usage:\n "
                  (with-out-str
                    (clojure.pprint/pprint (-> @*stats))))))))

(comment
  (swap! sim/*leaf-counter inc)
  (reset! sim/*leaf-counter 0))




(comment
  (collect-stats!)

  (def f (future
           (doall
             (repeatedly
             ;(repeatedly 5
              #(do
                 (collect-stats!)
                 (Thread/sleep 2000)
                 0)))))

  (future-cancel f)

  (for [n (range 5)]
    (apply str (repeat (* 100000 n) "*")))
  0)