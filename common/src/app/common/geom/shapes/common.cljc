;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.geom.shapes.common
  (:require
   #?(:clj [app.common.fressian :as fres])
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.transit :as t]))

(defrecord Shape [id parent-id frame-id name type points transform transform-inverse x y selrect])
(defrecord SelRect [x y x1 x2 y1 y2 width height])

(defn center-selrect
  "Calculate the center of the selrect."
  [{:keys [x y width height]}]
  (when (d/num? x y width height)
    (gpt/point (+ x (/ width 2.0))
               (+ y (/ height 2.0)))))

(defn center-points [points]
  (let [ptx  (into [] (keep :x) points)
        pty  (into [] (keep :y) points)
        minx (reduce min ##Inf ptx)
        miny (reduce min ##Inf pty)
        maxx (reduce max ##-Inf ptx)
        maxy (reduce max ##-Inf pty)]
    (gpt/point (/ (+ minx maxx) 2.0)
               (/ (+ miny maxy) 2.0))))

(defn center-bounds [[a b c d]]
  (let [xa   (dm/get-prop a :x)
        ya   (dm/get-prop a :y)
        xb   (dm/get-prop b :x)
        yb   (dm/get-prop b :y)
        xc   (dm/get-prop c :x)
        yc   (dm/get-prop c :y)
        xd   (dm/get-prop d :x)
        yd   (dm/get-prop d :y)
        minx (min xa xb xc xd)
        miny (min ya yb yc yd)
        maxx (max xa xb xc xd)
        maxy (max ya yb yc yd)]
    (gpt/point (/ (+ minx maxx) 2.0)
               (/ (+ miny maxy) 2.0))))

(defn center-shape
  "Calculate the center of the shape."
  [shape]
  (center-selrect (:selrect shape)))

(defn transform-points
  ([points matrix]
   (transform-points points nil matrix))

  ([points center matrix]
   (if (and (d/not-empty? points) (gmt/matrix? matrix))
     (let [prev (if center (gmt/translate-matrix center) (gmt/matrix))
           post (if center (gmt/translate-matrix (gpt/negate center)) (gmt/matrix))

           tr-point (fn [point]
                      (gpt/transform point (gmt/multiply prev matrix post)))]
       (mapv tr-point points))
     points)))

(defn make-selrect
  [x y width height]
  (when (d/num? x y width height)
    (let [width (max width 0.01)
          height (max height 0.01)]
      (map->SelRect
       {:x x
        :y y
        :x1 x
        :y1 y
        :x2 (+ x width)
        :y2 (+ y height)
        :width width
        :height height}))))

(defn corners->selrect
  [p1 p2]
  (let [xp1 (:x p1)
        xp2 (:x p2)
        yp1 (:y p1)
        yp2 (:y p2)]
    (make-selrect (min xp1 xp2) (min yp1 yp2) (abs (- xp1 xp2)) (abs (- yp1 yp2)))))

(defn points->selrect
  [points]
  (when-let [points (seq points)]
    (loop [minx ##Inf
           miny ##Inf
           maxx ##-Inf
           maxy ##-Inf
           pts  points]
      (if-let [pt (first pts)]
        (let [x (dm/get-prop pt :x)
              y (dm/get-prop pt :y)]
          (recur (min minx x)
                 (min miny y)
                 (max maxx x)
                 (max maxy y)
                 (rest pts)))
        (when (d/num? minx miny maxx maxy)
          (make-selrect minx miny (- maxx minx) (- maxy miny)))))))

(defn transform-selrect
  [{:keys [x1 y1 x2 y2] :as sr} matrix]
  (let [[c1 c2] (transform-points [(gpt/point x1 y1) (gpt/point x2 y2)] matrix)]
    (corners->selrect c1 c2)))

(defn join-selrects [selrects]
  (when (d/not-empty? selrects)
    (let [minx (transduce (keep :x1) min ##Inf selrects)
          miny (transduce (keep :y1) min ##Inf selrects)
          maxx (transduce (keep :x2) max ##-Inf selrects)
          maxy (transduce (keep :y2) max ##-Inf selrects)]
      (when (d/num? minx miny maxx maxy)
        (make-selrect minx miny (- maxx minx) (- maxy miny))))))

(defn center->selrect [{:keys [x y]} width height]
  (when (d/num? x y width height)
    (make-selrect (- x (/ width 2))
                  (- y (/ height 2))
                  width
                  height)))

(defn selrect->points
  [{:keys [x y width height]}]
  (when (d/num? x y)
    (let [width  (max width 0.01)
          height (max height 0.01)]
      [(gpt/point x y)
       (gpt/point (+ x width) y)
       (gpt/point (+ x width) (+ y height))
       (gpt/point x (+ y height))])))

#?(:clj
   (fres/add-handlers!
    {:name "penpot/shape/v1"
     :class Shape
     :wfn fres/write-map-like
     :rfn (fn [r]
            (-> r fres/read-map-like map->Shape))}))

#?(:clj
   (fres/add-handlers!
    {:name "penpot/selrect/v1"
     :class SelRect
     :wfn fres/write-map-like
     :rfn (fn [r]
            (-> r fres/read-map-like map->SelRect))}))

(t/add-handlers!
 {:id "penpot/shape/v1"
  :class Shape
  :wfn #(into {} %)
  :rfn map->Shape})

(t/add-handlers!
 {:id "penpot/selrect/v1"
  :class SelRect
  :wfn #(into {} %)
  :rfn map->SelRect})
