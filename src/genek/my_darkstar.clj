(ns genek.my-darkstar)
;(ns applied-science.darkstar)

(def engine
  (let [engine (.getEngineByName (javax.script.ScriptEngineManager.) "graal.js")
        bindings (.getBindings engine javax.script.ScriptContext/ENGINE_SCOPE)]
    (.put bindings "polyglot.js.allowAllAccess" true)
    (.eval engine (slurp (clojure.java.io/resource "vega.js")))
    (.eval engine (slurp (clojure.java.io/resource "vega-lite.js")))
    engine))

(defn make-js-fn [js-text]
  (let [^java.util.function.Function f (.eval engine js-text)]
    (fn [& args] (.apply f (to-array args)))))

(def vega-lite->vega
  (make-js-fn "function(vlSpec) { return JSON.stringify(vegaLite.compile(JSON.parse(vlSpec)).spec);}"))

(def vega-spec->view
  (make-js-fn "function(spec) { return new vega.View(vega.parse(JSON.parse(spec)), {renderer:'svg'}).finalize();}"))

(def vega-spec->canvas
  (make-js-fn "function(spec) { return new vega.View(vega.parse(JSON.parse(spec)), {renderer:'none'});}"))

(def view->svg
  (make-js-fn "function (view) {
    var promise = Java.type('clojure.core$promise').invokeStatic();
    view.toSVG(1.0).then(function(svg) {
        Java.type('clojure.core$deliver').invokeStatic(promise,svg);
    }).catch(function(err) {
        Java.type('clojure.core$deliver').invokeStatic(promise,'<svg><text>error</text></svg>');
    });
    return promise;
}"))

; https://observablehq.com/@bmesuere/generating-images-using-vega-lite-and-node

(def view->png
  (make-js-fn "function (view) {
    var promise = Java.type('clojure.core$promise').invokeStatic();
    view.toSVG(1.0).then(function(svg) {
        Java.type('clojure.core$deliver').invokeStatic(promise,svg);
    }).catch(function(err) {
        Java.type('clojure.core$deliver').invokeStatic(promise,'<svg><text>error</text></svg>');
    });
    return promise;
}"))

(defn vega-spec->png
  "Calls Vega to render the spec in `vega-spec-json-string` to the SVG described by that spec."
  [vega-spec-json-string]
  @(view->png (vega-spec->canvas vega-spec-json-string)))

(defn vega-spec->svg
  "Calls Vega to render the spec in `vega-spec-json-string` to the SVG described by that spec."
  [vega-spec-json-string]
  @(view->svg (vega-spec->view vega-spec-json-string)))

(defn vega-lite-spec->svg
  "Converts `vega-lite-spec-json-string` to a full Vega spec, then uses Vega to render the SVG described by that spec."
  [vega-lite-spec-json-string]
  (vega-spec->svg (vega-lite->vega vega-lite-spec-json-string)))

(defn vega-lite-spec->png
  "Converts `vega-lite-spec-json-string` to a full Vega spec, then uses Vega to render the SVG described by that spec."
  [vega-lite-spec-json-string]
  (vega-spec->png (vega-lite->vega vega-lite-spec-json-string)))

(comment

  (def vgjson (-> (slurp "furniture.json")
                vega-lite->vega))
  @(view->png (vega-spec->canvas vgjson))
  @(view->svg (vega-spec->view vgjson))

  (->> (slurp "furniture.json")
    vega-lite-spec->png
    (spit "furniture.png"))

  (->> (slurp "furniture.json")
    vega-lite-spec->svg
    (spit "furniture.svg"))


  (->> (slurp "vega-lite-example.json")
    vega-lite-spec->svg
    (spit "vl-example.svg")))



