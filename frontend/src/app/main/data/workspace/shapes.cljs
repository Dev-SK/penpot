;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.workspace.shapes
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.geom.proportions :as gpp]
   [app.common.geom.shapes :as gsh]
   [app.common.pages.changes-builder :as pcb]
   [app.common.pages.helpers :as cph]
   [app.common.spec :as us]
   [app.common.types.component :as ctk]
   [app.common.types.page :as ctp]
   [app.common.types.shape :as cts]
   [app.common.types.shape-tree :as ctst]
   [app.common.types.shape.interactions :as ctsi]
   [app.common.types.shape.layout :as ctl]
   [app.common.uuid :as uuid]
   [app.main.data.comments :as dc]
   [app.main.data.workspace.changes :as dch]
   [app.main.data.workspace.edition :as dwe]
   [app.main.data.workspace.selection :as dws]
   [app.main.data.workspace.shapes-update-layout :as dwsul]
   [app.main.data.workspace.state-helpers :as wsh]
   [app.main.data.workspace.undo :as dwu]
   [app.main.features :as features]
   [app.main.streams :as ms]
   [beicon.core :as rx]
   [cljs.spec.alpha :as s]
   [potok.core :as ptk]))

(s/def ::shape-attrs ::cts/shape-attrs)

(defn get-shape-layer-position
  [objects selected attrs]

  ;; Calculate the frame over which we're drawing
  (let [position @ms/mouse-position
        frame-id (:frame-id attrs (ctst/top-nested-frame objects position))
        shape    (when-not (empty? selected)
                   (cph/get-base-shape objects selected))]

    ;; When no shapes has been selected or we're over a different frame
    ;; we add it as the latest shape of that frame
    (if (or (not shape) (not= (:frame-id shape) frame-id))
      [frame-id frame-id nil]

      ;; Otherwise, we add it to next to the selected shape
      (let [index (cph/get-position-on-parent objects (:id shape))
            {:keys [frame-id parent-id]} shape]
        [frame-id parent-id (inc index)]))))

(defn make-new-shape
  [attrs objects selected]
  (let [default-attrs (if (= :frame (:type attrs))
                        cts/default-frame-attrs
                        cts/default-shape-attrs)

        selected-non-frames
        (into #{} (comp (map (d/getf objects))
                        (remove cph/frame-shape?))
              selected)

        [frame-id parent-id index]
        (get-shape-layer-position objects selected-non-frames attrs)]

    (-> (merge default-attrs attrs)
        (gpp/setup-proportions)
        (assoc :frame-id frame-id
               :parent-id parent-id
               :index index))))

(defn add-shape
  ([attrs]
   (add-shape attrs {}))

  ([attrs {:keys [no-select?]}]
   (us/verify ::shape-attrs attrs)
   (ptk/reify ::add-shape
     ptk/WatchEvent
     (watch [it state _]
       (let [page-id  (:current-page-id state)
             objects  (wsh/lookup-page-objects state page-id)
             selected (wsh/lookup-selected state)

             id       (or (:id attrs) (uuid/next))
             name     (-> objects
                          (ctst/retrieve-used-names)
                          (ctst/generate-unique-name (:name attrs)))

             shape (make-new-shape
                     (assoc attrs :id id :name name)
                     objects
                     selected)

             changes  (-> (pcb/empty-changes it page-id)
                          (pcb/with-objects objects)
                          (pcb/add-object shape)
                          (cond-> (some? (:parent-id attrs))
                            (pcb/change-parent (:parent-id attrs) [shape])))]

         (rx/concat
          (rx/of (dch/commit-changes changes)
                 (dwsul/update-layout-positions [(:parent-id shape)])
                 (when-not no-select?
                   (dws/select-shapes (d/ordered-set id))))
          (when (= :text (:type attrs))
            (->> (rx/of (dwe/start-edition-mode id))
                 (rx/observe-on :async)))))))))

(defn move-shapes-into-frame [frame-id shapes]
  (ptk/reify ::move-shapes-into-frame
    ptk/WatchEvent
    (watch [it state _]
      (let [page-id  (:current-page-id state)
            objects (wsh/lookup-page-objects state page-id)

            to-move-shapes
            (into []
                  (map (d/getf objects))
                  (reverse (ctst/sort-z-index objects shapes)))

            changes
            (when (d/not-empty? to-move-shapes)
              (-> (pcb/empty-changes it page-id)
                  (pcb/with-objects objects)
                  (pcb/change-parent frame-id to-move-shapes 0)))]

        (if (some? changes)
          (rx/of (dch/commit-changes changes))
          (rx/empty))))))

(declare real-delete-shapes)
(declare update-shape-flags)

(defn delete-shapes
  ([ids] (delete-shapes nil ids))
  ([page-id ids]
   (us/assert ::us/set-of-uuid ids)
   (ptk/reify ::delete-shapes
     ptk/WatchEvent
     (watch [it state _]
       (let [file-id       (:current-file-id state)
             page-id       (or page-id (:current-page-id state))
             file          (wsh/get-file state file-id)
             page          (wsh/lookup-page state page-id)
             objects       (wsh/lookup-page-objects state page-id)

             components-v2 (features/active-feature? state :components-v2)

             ids     (cph/clean-loops objects ids)

             in-component-copy?
             (fn [shape-id]
               ;; Look for shapes that are inside a component copy, but are
               ;; not the root. In this case, they must not be deleted,
               ;; but hidden (to be able to recover them more easily).
               (let [shape           (get objects shape-id)
                     component-shape (cph/get-component-shape objects shape)]
                 (and (ctk/in-component-instance? shape)
                      (not= shape component-shape)
                      (not (ctk/main-instance? component-shape)))))

             [ids-to-delete ids-to-hide]
             (if components-v2
               (loop [ids-seq       (seq ids)
                      ids-to-delete []
                      ids-to-hide   []]
                 (let [id (first ids-seq)]
                   (if (nil? id)
                     [ids-to-delete ids-to-hide]
                     (if (in-component-copy? id)
                       (recur (rest ids-seq)
                              ids-to-delete
                              (conj ids-to-hide id))
                       (recur (rest ids-seq)
                              (conj ids-to-delete id)
                              ids-to-hide)))))
               [ids []])]

         (rx/concat (rx/of (update-shape-flags ids-to-hide {:hidden true}))
                    (real-delete-shapes file page objects ids-to-delete it components-v2)))))))

(defn- real-delete-shapes
  [file page objects ids it components-v2]
  (let [lookup  (d/getf objects)

        layout-ids (->> ids
                        (mapcat (partial cph/get-parent-ids objects))
                        (filter (partial ctl/layout? objects)))

        groups-to-unmask
        (reduce (fn [group-ids id]
                  ;; When the shape to delete is the mask of a masked group,
                  ;; the mask condition must be removed, and it must be
                  ;; converted to a normal group.
                  (let [obj    (lookup id)
                        parent (lookup (:parent-id obj))]
                    (if (and (:masked-group? parent)
                             (= id (first (:shapes parent))))
                      (conj group-ids (:id parent))
                      group-ids)))
                #{}
                ids)

        interacting-shapes
        (filter (fn [shape]
                  ;; If any of the deleted shapes is the destination of
                  ;; some interaction, this must be deleted, too.
                  (let [interactions (:interactions shape)]
                    (some #(and (ctsi/has-destination %)
                                (contains? ids (:destination %)))
                          interactions)))
                (vals objects))

        ;; If any of the deleted shapes is a frame with guides
        guides (into {}
                     (comp (map second)
                           (remove #(contains? ids (:frame-id %)))
                           (map (juxt :id identity)))
                     (dm/get-in page [:options :guides]))

        starting-flows
        (filter (fn [flow]
                  ;; If any of the deleted is a frame that starts a flow,
                  ;; this must be deleted, too.
                  (contains? ids (:starting-frame flow)))
                (-> page :options :flows))

        all-parents
        (reduce (fn [res id]
                  ;; All parents of any deleted shape must be resized.
                  (into res (cph/get-parent-ids objects id)))
                (d/ordered-set)
                ids)

        all-children
        (->> ids ;; Children of deleted shapes must be also deleted.
             (reduce (fn [res id]
                       (into res (cph/get-children-ids objects id)))
                     [])
             (reverse)
             (into (d/ordered-set)))

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
        ;; Any parent whose children are all deleted, must be deleted too.
        (into (d/ordered-set) (find-all-empty-parents #{}))

        components-to-delete
        (if components-v2
          (reduce (fn [components id]
                    (let [shape (get objects id)]
                      (if (and (= (:component-file shape) (:id file)) ;; Main instances should exist only in local file
                               (:main-instance? shape))               ;; but check anyway
                        (conj components (:component-id shape))
                        components)))
                  []
                  (into ids all-children))
          [])

        changes (-> (pcb/empty-changes it (:id page))
                    (pcb/with-page page)
                    (pcb/with-objects objects)
                    (pcb/with-library-data file)
                    (pcb/set-page-option :guides guides))

        changes (reduce (fn [changes component-id]
                          ;; It's important to delete the component before the main instance, because we
                          ;; need to store the instance position if we want to restore it later.
                          (pcb/delete-component changes component-id components-v2))
                        changes
                        components-to-delete)

        changes (-> changes
                    (pcb/remove-objects all-children)
                    (pcb/remove-objects ids)
                    (pcb/remove-objects empty-parents)
                    (pcb/resize-parents all-parents)
                    (pcb/update-shapes groups-to-unmask
                                       (fn [shape]
                                         (assoc shape :masked-group? false)))
                    (pcb/update-shapes (map :id interacting-shapes)
                                       (fn [shape]
                                         (d/update-when shape :interactions
                                                        (fn [interactions]
                                                          (into []
                                                                (remove #(and (ctsi/has-destination %)
                                                                              (contains? ids (:destination %))))
                                                                interactions)))))
                    (cond-> (seq starting-flows)
                      (pcb/update-page-option :flows (fn [flows]
                                                       (->> (map :id starting-flows)
                                                            (reduce ctp/remove-flow flows))))))]

    (rx/of (dc/detach-comment-thread ids)
           (dwsul/update-layout-positions all-parents)
           (dch/commit-changes changes)
           (dwsul/update-layout-positions layout-ids))))

(defn create-and-add-shape
  [type frame-x frame-y data]
  (ptk/reify ::create-and-add-shape
    ptk/WatchEvent
    (watch [_ state _]
      (let [{:keys [width height]} data

            vbc (wsh/viewport-center state)
            x (:x data (- (:x vbc) (/ width 2)))
            y (:y data (- (:y vbc) (/ height 2)))
            page-id (:current-page-id state)
            frame-id (-> (wsh/lookup-page-objects state page-id)
                         (ctst/top-nested-frame {:x frame-x :y frame-y}))
            shape (-> (cts/make-minimal-shape type)
                      (merge data)
                      (merge {:x x :y y})
                      (assoc :frame-id frame-id)
                      (cts/setup-rect-selrect))]
        (rx/of (add-shape shape))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Artboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-artboard-from-selection
  ([]
   (create-artboard-from-selection nil))
  ([id]
   (create-artboard-from-selection id nil))
  ([id parent-id]
   (ptk/reify ::create-artboard-from-selection
     ptk/WatchEvent
     (watch [_ state _]
       (let [page-id       (:current-page-id state)
             objects       (wsh/lookup-page-objects state page-id)
             selected      (wsh/lookup-selected state)
             selected-objs (map #(get objects %) selected)]
         (when (d/not-empty? selected)
           (let [srect     (gsh/selection-rect selected-objs)
                 frame-id  (get-in objects [(first selected) :frame-id])
                 parent-id (or parent-id (get-in objects [(first selected) :parent-id]))
                 shape     (-> (cts/make-minimal-shape :frame)
                               (merge {:x (:x srect) :y (:y srect) :width (:width srect) :height (:height srect)})
                               (cond-> id
                                 (assoc :id id))
                               (assoc :frame-id frame-id :parent-id parent-id)
                               (cond-> (not= frame-id uuid/zero)
                                 (assoc :fills [] :hide-in-viewer true))
                               (cts/setup-rect-selrect))
                 undo-id (uuid/next)]
             (rx/of
              (dwu/start-undo-transaction undo-id)
              (add-shape shape)
              (move-shapes-into-frame (:id shape) selected)
              (dwu/commit-undo-transaction undo-id)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shape Flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn update-shape-flags
  [ids {:keys [blocked hidden] :as flags}]
  (us/verify (s/coll-of ::us/uuid) ids)
  (us/assert ::shape-attrs flags)
  (ptk/reify ::update-shape-flags
    ptk/WatchEvent
    (watch [_ state _]
      (let [update-fn
            (fn [obj]
              (cond-> obj
                (boolean? blocked) (assoc :blocked blocked)
                (boolean? hidden) (assoc :hidden hidden)))
            objects (wsh/lookup-page-objects state)
            ids     (into ids (->> ids (mapcat #(cph/get-children-ids objects %))))]
        (rx/of (dch/update-shapes ids update-fn))))))

(defn toggle-visibility-selected
  []
  (ptk/reify ::toggle-visibility-selected
    ptk/WatchEvent
    (watch [_ state _]
      (let [selected (wsh/lookup-selected state)]
        (rx/of (dch/update-shapes selected #(update % :hidden not)))))))

(defn toggle-lock-selected
  []
  (ptk/reify ::toggle-lock-selected
    ptk/WatchEvent
    (watch [_ state _]
      (let [selected (wsh/lookup-selected state)]
        (rx/of (dch/update-shapes selected #(update % :blocked not)))))))

(defn toggle-file-thumbnail-selected
  []
  (ptk/reify ::toggle-file-thumbnail-selected
    ptk/WatchEvent
    (watch [_ state _]
      (let [selected   (wsh/lookup-selected state)
            pages      (-> state :workspace-data :pages-index vals)
            get-frames (fn [{:keys [objects id] :as page}]
                         (->> (ctst/get-frames objects)
                              (sequence
                               (comp (filter :use-for-thumbnail?)
                                     (map :id)
                                     (remove selected)
                                     (map (partial vector id))))))]

        (rx/concat
         (rx/from
          (->> (mapcat get-frames pages)
               (d/group-by first second)
               (map (fn [[page-id frame-ids]]
                      (dch/update-shapes frame-ids #(dissoc % :use-for-thumbnail?) {:page-id page-id})))))
         (rx/of (dch/update-shapes selected #(update % :use-for-thumbnail? not))))))))

