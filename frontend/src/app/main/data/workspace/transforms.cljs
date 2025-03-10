;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.workspace.transforms
  "Events related with shapes transformations"
  (:require
   [app.common.data :as d]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as gsh]
   [app.common.geom.shapes.flex-layout :as gsl]
   [app.common.math :as mth]
   [app.common.pages.changes-builder :as pcb]
   [app.common.pages.helpers :as cph]
   [app.common.spec :as us]
   [app.common.types.modifiers :as ctm]
   [app.common.types.shape-tree :as ctst]
   [app.common.types.shape.layout :as ctl]
   [app.common.uuid :as uuid]
   [app.main.data.workspace.changes :as dch]
   [app.main.data.workspace.collapse :as dwc]
   [app.main.data.workspace.modifiers :as dwm]
   [app.main.data.workspace.selection :as dws]
   [app.main.data.workspace.state-helpers :as wsh]
   [app.main.data.workspace.undo :as dwu]
   [app.main.snap :as snap]
   [app.main.streams :as ms]
   [beicon.core :as rx]
   [cljs.spec.alpha :as s]
   [potok.core :as ptk]))

;; -- Helpers --------------------------------------------------------

;; For each of the 8 handlers gives the multiplier for resize
;; for example, right will only grow in the x coordinate and left
;; will grow in the inverse of the x coordinate
(def ^:private handler-multipliers
  {:right        [ 1  0]
   :bottom       [ 0  1]
   :left         [-1  0]
   :top          [ 0 -1]
   :top-right    [ 1 -1]
   :top-left     [-1 -1]
   :bottom-right [ 1  1]
   :bottom-left  [-1  1]})

(defn- handler-resize-origin
  "Given a handler, return the coordinate origin for resizes.
   This is the opposite of the handler so for right we want the
   left side as origin of the resize.

   sx, sy => start x/y
   mx, my => middle x/y
   ex, ey => end x/y
  "
  [{sx :x sy :y :keys [width height]} handler]
  (let [mx (+ sx (/ width 2))
        my (+ sy (/ height 2))
        ex (+ sx width)
        ey (+ sy height)

        [x y] (case handler
                :right [sx my]
                :bottom [mx sy]
                :left [ex my]
                :top [mx ey]
                :top-right [sx ey]
                :top-left [ex ey]
                :bottom-right [sx sy]
                :bottom-left [ex sy])]
    (gpt/point x y)))

(defn- fix-init-point
  "Fix the initial point so the resizes are accurate"
  [initial handler shape]
  (let [{:keys [x y width height]} (:selrect shape)]
    (cond-> initial
      (contains? #{:left :top-left :bottom-left} handler)
      (assoc :x x)

      (contains? #{:right :top-right :bottom-right} handler)
      (assoc :x (+ x width))

      (contains? #{:top :top-right :top-left} handler)
      (assoc :y y)

      (contains? #{:bottom :bottom-right :bottom-left} handler)
      (assoc :y (+ y height)))))

(defn finish-transform []
  (ptk/reify ::finish-transform
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-local dissoc :transform))))


;; -- Resize --------------------------------------------------------

(defn start-resize
  "Enter mouse resize mode, until mouse button is released."
  [handler ids shape]
  (letfn [(resize [shape objects initial layout [point lock? center? point-snap]]
            (let [{:keys [width height]} (:selrect shape)
                  {:keys [rotation]} shape

                  shape-center (gsh/center-shape shape)
                  shape-transform (:transform shape)
                  shape-transform-inverse (:transform-inverse shape)

                  rotation (or rotation 0)

                  initial (gmt/transform-point-center initial shape-center shape-transform-inverse)
                  initial (fix-init-point initial handler shape)

                  point (gmt/transform-point-center (if (= rotation 0) point-snap point)
                                                    shape-center shape-transform-inverse)

                  shapev (-> (gpt/point width height))

                  scale-text (:scale-text layout)

                  ;; Force lock if the scale text mode is active
                  lock? (or lock? scale-text)

                  ;; Vector modifiers depending on the handler
                  handler-mult (let [[x y] (handler-multipliers handler)] (gpt/point x y))

                  ;; Difference between the origin point in the coordinate system of the rotation
                  deltav (-> (gpt/to-vec initial point)
                             (gpt/multiply handler-mult))

                  ;; Resize vector
                  scalev (-> (gpt/divide (gpt/add shapev deltav) shapev)
                             (gpt/no-zeros))

                  scalev (if lock?
                           (let [v (cond
                                     (#{:right :left} handler) (:x scalev)
                                     (#{:top :bottom} handler) (:y scalev)
                                     :else (max (:x scalev) (:y scalev)))]
                             (gpt/point v v))

                           scalev)

                  ;; Resize origin point given the selected handler
                  handler-origin  (handler-resize-origin (:selrect shape) handler)


                  ;; If we want resize from center, displace the shape
                  ;; so it is still centered after resize.
                  displacement
                  (when center?
                    (-> shape-center
                        (gpt/subtract handler-origin)
                        (gpt/multiply scalev)
                        (gpt/add handler-origin)
                        (gpt/subtract shape-center)
                        (gpt/multiply (gpt/point -1 -1))
                        (gpt/transform shape-transform)))

                  resize-origin
                  (cond-> (gmt/transform-point-center handler-origin shape-center shape-transform)
                    (some? displacement)
                    (gpt/add displacement))

                  ;; When the horizontal/vertical scale a flex children with auto/fill
                  ;; we change it too fixed
                  layout? (ctl/layout? shape)
                  layout-child? (ctl/layout-child? objects shape)
                  auto-width? (ctl/auto-width? shape)
                  fill-width? (ctl/fill-width? shape)
                  auto-height? (ctl/auto-height? shape)
                  fill-height? (ctl/fill-height? shape)

                  set-fix-width?
                  (and (not (mth/close? (:x scalev) 1))
                       (or (and (or layout? layout-child?) auto-width?)
                           (and layout-child? fill-width?)))

                  set-fix-height?
                  (and (not (mth/close? (:y scalev) 1))
                       (or (and (or layout? layout-child?) auto-height?)
                           (and layout-child? fill-height?)))

                  modifiers
                  (-> (ctm/empty)
                      (cond-> displacement
                        (ctm/move displacement))
                      (ctm/resize scalev resize-origin shape-transform shape-transform-inverse)

                      (cond-> set-fix-width?
                        (ctm/change-property :layout-item-h-sizing :fix))

                      (cond-> set-fix-height?
                        (ctm/change-property :layout-item-v-sizing :fix))

                      (cond-> scale-text
                        (ctm/scale-content (:x scalev))))

                  modif-tree (dwm/create-modif-tree ids modifiers)]
              (rx/of (dwm/set-modifiers modif-tree))))

          ;; Unifies the instantaneous proportion lock modifier
          ;; activated by Shift key and the shapes own proportion
          ;; lock flag that can be activated on element options.
          (normalize-proportion-lock [[point shift? alt?]]
            (let [proportion-lock? (:proportion-lock shape)]
              [point (or proportion-lock? shift?) alt?]))]
    (reify
      ptk/UpdateEvent
      (update [_ state]
        (-> state
            (assoc-in [:workspace-local :transform] :resize)))

      ptk/WatchEvent
      (watch [_ state stream]
        (let [initial-position @ms/mouse-position
              stoper  (rx/filter ms/mouse-up? stream)
              layout  (:workspace-layout state)
              page-id (:current-page-id state)
              focus   (:workspace-focus-selected state)
              zoom    (get-in state [:workspace-local :zoom] 1)
              objects (wsh/lookup-page-objects state page-id)
              resizing-shapes (map #(get objects %) ids)]

          (rx/concat
           (->> ms/mouse-position
                (rx/with-latest-from ms/mouse-position-shift ms/mouse-position-alt)
                (rx/map normalize-proportion-lock)
                (rx/switch-map (fn [[point _ _ :as current]]
                                 (->> (snap/closest-snap-point page-id resizing-shapes objects layout zoom focus point)
                                      (rx/map #(conj current %)))))
                (rx/mapcat (partial resize shape objects initial-position layout))
                (rx/take-until stoper))
           (rx/of (dwm/apply-modifiers)
                  (finish-transform))))))))

(defn update-dimensions
  "Change size of shapes, from the sideber options form.
  Will ignore pixel snap used in the options side panel"
  [ids attr value]
  (us/verify (s/coll-of ::us/uuid) ids)
  (us/verify #{:width :height} attr)
  (us/verify ::us/number value)
  (ptk/reify ::update-dimensions
    ptk/UpdateEvent
    (update [_ state]
      (let [objects (wsh/lookup-page-objects state)
            snap-pixel? (and (contains? (:workspace-layout state) :snap-pixel-grid)
                             (int? value))
            get-modifier
            (fn [shape] (ctm/change-dimensions-modifiers shape attr value))

            modif-tree
            (-> (dwm/build-modif-tree ids objects get-modifier)
                (gsh/set-objects-modifiers objects false snap-pixel?))]

        (assoc state :workspace-modifiers modif-tree)))

    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of (dwm/apply-modifiers)))))

(defn change-orientation
  "Change orientation of shapes, from the sidebar options form.
  Will ignore pixel snap used in the options side panel"
  [ids orientation]
  (us/verify (s/coll-of ::us/uuid) ids)
  (us/verify #{:horiz :vert} orientation)
  (ptk/reify ::change-orientation
    ptk/UpdateEvent
    (update [_ state]
      (let [objects     (wsh/lookup-page-objects state)
            snap-pixel? (contains? (get state :workspace-layout) :snap-pixel-grid)

            get-modifier
            (fn [shape] (ctm/change-orientation-modifiers shape orientation))

            modif-tree
            (-> (dwm/build-modif-tree ids objects get-modifier)
                (gsh/set-objects-modifiers objects false snap-pixel?))]

        (assoc state :workspace-modifiers modif-tree)))

    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of (dwm/apply-modifiers)))))

;; -- Rotate --------------------------------------------------------

(defn start-rotate
  "Enter mouse rotate mode, until mouse button is released."
  [shapes]
  (ptk/reify ::start-rotate
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (assoc-in [:workspace-local :transform] :rotate)))

    ptk/WatchEvent
    (watch [_ _ stream]
      (let [stoper          (rx/filter ms/mouse-up? stream)
            group           (gsh/selection-rect shapes)
            group-center    (gsh/center-selrect group)
            initial-angle   (gpt/angle @ms/mouse-position group-center)

            calculate-angle
            (fn [pos mod? shift?]
              (let [angle (- (gpt/angle pos group-center) initial-angle)
                    angle (if (neg? angle) (+ 360 angle) angle)
                    angle (if (= angle 360)
                            0
                            angle)
                    angle (if mod?
                            (* (mth/floor (/ angle 45)) 45)
                            angle)
                    angle (if shift?
                            (* (mth/floor (/ angle 15)) 15)
                            angle)]
                angle))]
        (rx/concat
         (->> ms/mouse-position
              (rx/with-latest vector ms/mouse-position-mod)
              (rx/with-latest vector ms/mouse-position-shift)
              (rx/map
               (fn [[[pos mod?] shift?]]
                 (let [delta-angle (calculate-angle pos mod? shift?)]
                   (dwm/set-rotation-modifiers delta-angle shapes group-center))))
              (rx/take-until stoper))
         (rx/of (dwm/apply-modifiers)
                (finish-transform)))))))

(defn increase-rotation
  "Rotate shapes a fixed angle, from a keyboard action."
  [ids rotation]
  (ptk/reify ::increase-rotation
    ptk/WatchEvent
    (watch [_ state _]

      (let [page-id (:current-page-id state)
            objects (wsh/lookup-page-objects state page-id)
            rotate-shape (fn [shape]
                           (let [delta (- rotation (:rotation shape))]
                             (dwm/set-rotation-modifiers delta [shape])))]
        (rx/concat
         (rx/from (->> ids (map #(get objects %)) (map rotate-shape)))
         (rx/of (dwm/apply-modifiers)))))))


;; -- Move ----------------------------------------------------------

(declare start-move)
(declare start-move-duplicate)
(declare move-shapes-to-frame)
(declare get-displacement)

(defn start-move-selected
  "Enter mouse move mode, until mouse button is released."
  ([]
   (start-move-selected nil false))

  ([id shift?]
   (ptk/reify ::start-move-selected
     ptk/WatchEvent
     (watch [_ state stream]
       (let [initial  (deref ms/mouse-position)

             stopper  (rx/filter ms/mouse-up? stream)
             zoom    (get-in state [:workspace-local :zoom] 1)

             ;; We toggle the selection so we don't have to wait for the event
             selected
             (cond-> (wsh/lookup-selected state {:omit-blocked? true})
               (some? id)
               (d/toggle-selection id shift?))]

         (when (or (d/not-empty? selected) (some? id))
           (->> ms/mouse-position
                (rx/map #(gpt/to-vec initial %))
                (rx/map #(gpt/length %))
                (rx/filter #(> % (/ 10 zoom)))
                (rx/take 1)
                (rx/with-latest vector ms/mouse-position-alt)
                (rx/mapcat
                 (fn [[_ alt?]]
                   (rx/concat
                    (if (some? id)
                      (rx/of (dws/select-shape id shift?))
                      (rx/empty))

                    (if alt?
                      ;; When alt is down we start a duplicate+move
                      (rx/of (start-move-duplicate initial)
                             (dws/duplicate-selected false))

                      ;; Otherwise just plain old move
                      (rx/of (start-move initial selected))))))
                (rx/take-until stopper))))))))

(defn- start-move-duplicate
  [from-position]
  (ptk/reify ::start-move-duplicate
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (assoc-in [:workspace-local :transform] :move)))

    ptk/WatchEvent
    (watch [_ _ stream]
      (->> stream
           (rx/filter (ptk/type? ::dws/duplicate-selected))
           (rx/take 1)
           (rx/map #(start-move from-position))))))

(defn- start-move
  ([from-position] (start-move from-position nil))
  ([from-position ids]
   (ptk/reify ::start-move
     ptk/UpdateEvent
     (update [_ state]
       (-> state
           (assoc-in [:workspace-local :transform] :move)))

     ptk/WatchEvent
     (watch [_ state stream]
       (let [page-id (:current-page-id state)
             objects (wsh/lookup-page-objects state page-id)
             selected (wsh/lookup-selected state {:omit-blocked? true})
             ids     (if (nil? ids) selected ids)
             shapes  (mapv #(get objects %) ids)
             stopper (rx/filter ms/mouse-up? stream)
             layout  (get state :workspace-layout)
             zoom    (get-in state [:workspace-local :zoom] 1)
             focus   (:workspace-focus-selected state)

             exclude-frames
             (into #{}
                   (filter (partial cph/frame-shape? objects))
                   (cph/selected-with-children objects selected))

             exclude-frames-siblings
             (into exclude-frames
                   (mapcat (partial cph/get-siblings-ids objects))
                   selected)

             fix-axis
             (fn [[position shift?]]
               (let [delta (gpt/to-vec from-position position)]
                 (if shift?
                   (if (> (mth/abs (:x delta)) (mth/abs (:y delta)))
                     (gpt/point (:x delta) 0)
                     (gpt/point 0 (:y delta)))
                   delta)))

             position (->> ms/mouse-position
                           (rx/with-latest-from ms/mouse-position-shift)
                           (rx/map #(fix-axis %)))

             snap-delta (rx/concat
                         ;; We send the nil first so the stream is not waiting for the first value
                         (rx/of nil)
                         (->> position
                              (rx/throttle 20)
                              (rx/switch-map
                               (fn [pos]
                                 (->> (snap/closest-snap-move page-id shapes objects layout zoom focus pos)
                                      (rx/map #(vector pos %)))))))]
         (if (empty? shapes)
           (rx/of (finish-transform))
           (let [move-stream
                 (->> position
                      ;; We ask for the snap position but we continue even if the result is not available
                      (rx/with-latest vector snap-delta)

                      ;; We try to use the previous snap so we don't have to wait for the result of the new
                      (rx/map snap/correct-snap-point)

                      (rx/with-latest vector ms/mouse-position-mod)

                      (rx/map
                       (fn [[move-vector mod?]]
                         (let [position     (gpt/add from-position move-vector)
                               exclude-frames (if mod? exclude-frames exclude-frames-siblings)
                               target-frame (ctst/top-nested-frame objects position exclude-frames)
                               layout?      (ctl/layout? objects target-frame)
                               drop-index   (when layout? (gsl/get-drop-index target-frame objects position))]
                           [move-vector target-frame drop-index])))

                      (rx/take-until stopper))]

             (rx/merge
              ;; Temporary modifiers stream
              (->> move-stream
                   (rx/map
                    (fn [[move-vector target-frame drop-index]]
                      (-> (dwm/create-modif-tree ids (ctm/move-modifiers move-vector))
                          (dwm/build-change-frame-modifiers objects selected target-frame drop-index)
                          (dwm/set-modifiers)))))

              ;; Last event will write the modifiers creating the changes
              (->> move-stream
                   (rx/last)
                   (rx/mapcat
                    (fn [[_ target-frame drop-index]]
                      (let [undo-id (uuid/next)]
                        (rx/of (dwu/start-undo-transaction undo-id)
                             (move-shapes-to-frame ids target-frame drop-index)
                             (dwm/apply-modifiers {:undo-transation? false})
                             (finish-transform)
                             (dwu/commit-undo-transaction undo-id))))))))))))))

(s/def ::direction #{:up :down :right :left})

(defn move-selected
  "Move shapes a fixed increment in one direction, from a keyboard action."
  [direction shift?]
  (us/verify ::direction direction)
  (us/verify boolean? shift?)

  (let [same-event (js/Symbol "same-event")]
    (ptk/reify ::move-selected
      IDeref
      (-deref [_] direction)

      ptk/UpdateEvent
      (update [_ state]
        (if (nil? (get state ::current-move-selected))
          (-> state
              (assoc-in [:workspace-local :transform] :move)
              (assoc ::current-move-selected same-event))
          state))

      ptk/WatchEvent
      (watch [_ state stream]
        (if (= same-event (get state ::current-move-selected))
          (let [selected (wsh/lookup-selected state {:omit-blocked? true})
                nudge (get-in state [:profile :props :nudge] {:big 10 :small 1})
                move-events (->> stream
                                 (rx/filter (ptk/type? ::move-selected))
                                 (rx/filter #(= direction (deref %))))
                stopper (->> move-events
                             (rx/debounce 100)
                             (rx/take 1))
                scale (if shift? (gpt/point (or (:big nudge) 10)) (gpt/point (or (:small nudge) 1)))
                mov-vec (gpt/multiply (get-displacement direction) scale)]

            (rx/concat
             (rx/merge
              (->> move-events
                   (rx/scan #(gpt/add %1 mov-vec) (gpt/point 0 0))
                   (rx/map #(dwm/create-modif-tree selected (ctm/move-modifiers %)))
                   (rx/map (partial dwm/set-modifiers))
                   (rx/take-until stopper))
              (rx/of (move-selected direction shift?)))

             (rx/of (dwm/apply-modifiers)
                    (finish-transform))))
          (rx/empty))))))

(s/def ::x number?)
(s/def ::y number?)
(s/def ::position
  (s/keys :opt-un [::x ::y]))

(defn update-position
  "Move shapes to a new position, from the sidebar options form."
  [id position]
  (us/verify ::us/uuid id)
  (us/verify ::position position)
  (ptk/reify ::update-position
    ptk/WatchEvent
    (watch [_ state _]
      (let [page-id (:current-page-id state)
            objects (wsh/lookup-page-objects state page-id)
            shape   (get objects id)

            bbox (-> shape :points gsh/points->selrect)

            cpos (gpt/point (:x bbox) (:y bbox))
            pos  (gpt/point (or (:x position) (:x bbox))
                            (or (:y position) (:y bbox)))
            delta (gpt/subtract pos cpos)

            modif-tree (dwm/create-modif-tree [id] (ctm/move-modifiers delta))]

        (rx/of (dwm/set-modifiers modif-tree)
               (dwm/apply-modifiers))))))

(defn- move-shapes-to-frame
  [ids frame-id drop-index]
  (ptk/reify ::move-shapes-to-frame
    ptk/WatchEvent
    (watch [it state _]
      (let [page-id  (:current-page-id state)
            objects  (wsh/lookup-page-objects state page-id)
            layout?  (get-in objects [frame-id :layout])
            lookup   (d/getf objects)

            shapes (->> ids (cph/clean-loops objects) (keep lookup))

            moving-shapes
            (cond->> shapes
              (not layout?)
              (remove #(= (:frame-id %) frame-id))

              layout?
              (remove #(and (= (:frame-id %) frame-id)
                            (not= (:parent-id %) frame-id))))

            all-parents
            (reduce (fn [res id]
                      (into res (cph/get-parent-ids objects id)))
                    (d/ordered-set)
                    ids)

            find-all-empty-parents
            (fn recursive-find-empty-parents [empty-parents]
              (let [all-ids   (into empty-parents ids)
                    contains? (partial contains? all-ids)
                    xform     (comp (map lookup)
                                    (filter cph/group-shape?)
                                    (remove #(->> (:shapes %) (remove contains?) seq))
                                    (map :id))
                    parents   (into #{} xform all-parents)]
                (if (= empty-parents parents)
                  empty-parents
                  (recursive-find-empty-parents parents))))

            empty-parents
            ;; Any parent whose children are moved should be deleted
            (into (d/ordered-set) (find-all-empty-parents #{}))

            changes
            (-> (pcb/empty-changes it page-id)
                (pcb/with-objects objects)
                (pcb/change-parent frame-id moving-shapes drop-index)
                (pcb/remove-objects empty-parents))]

        (when (and (some? frame-id) (d/not-empty? changes))
          (rx/of (dch/commit-changes changes)
                 (dwc/expand-collapse frame-id)))))))

(defn- get-displacement
  "Retrieve the correct displacement delta point for the
  provided direction speed and distances thresholds."
  [direction]
  (case direction
    :up (gpt/point 0 (- 1))
    :down (gpt/point 0 1)
    :left (gpt/point (- 1) 0)
    :right (gpt/point 1 0)))


;; -- Flip ----------------------------------------------------------

(defn flip-horizontal-selected []
  (ptk/reify ::flip-horizontal-selected
    ptk/WatchEvent
    (watch [_ state _]
      (let [objects  (wsh/lookup-page-objects state)
            selected (wsh/lookup-selected state {:omit-blocked? true})
            shapes   (map #(get objects %) selected)
            selrect  (gsh/selection-rect shapes)
            origin   (gpt/point (:x selrect) (+ (:y selrect) (/ (:height selrect) 2)))

            modif-tree (dwm/create-modif-tree
                        selected
                        (-> (ctm/empty)
                            (ctm/resize (gpt/point -1.0 1.0) origin)
                            (ctm/move (gpt/point (:width selrect) 0))))]

        (rx/of (dwm/set-modifiers modif-tree true)
               (dwm/apply-modifiers))))))

(defn flip-vertical-selected []
  (ptk/reify ::flip-vertical-selected
    ptk/WatchEvent
    (watch [_ state _]
      (let [objects  (wsh/lookup-page-objects state)
            selected (wsh/lookup-selected state {:omit-blocked? true})
            shapes   (map #(get objects %) selected)
            selrect  (gsh/selection-rect shapes)
            origin   (gpt/point (+ (:x selrect) (/ (:width selrect) 2)) (:y selrect))

            modif-tree (dwm/create-modif-tree
                        selected
                        (-> (ctm/empty)
                            (ctm/resize (gpt/point 1.0 -1.0) origin)
                            (ctm/move (gpt/point 0 (:height selrect)))))]

        (rx/of (dwm/set-modifiers modif-tree true)
               (dwm/apply-modifiers))))))
