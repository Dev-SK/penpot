;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.viewer.handoff.attributes
  (:require
   [app.common.geom.shapes :as gsh]
   [app.main.ui.hooks :as hooks]
   [app.main.ui.viewer.handoff.attributes.blur :refer [blur-panel]]
   [app.main.ui.viewer.handoff.attributes.fill :refer [fill-panel]]
   [app.main.ui.viewer.handoff.attributes.image :refer [image-panel]]
   [app.main.ui.viewer.handoff.attributes.layout :refer [layout-panel]]
   [app.main.ui.viewer.handoff.attributes.layout-flex :refer [layout-flex-panel]]
   [app.main.ui.viewer.handoff.attributes.layout-flex-element :refer [layout-flex-element-panel]]
   [app.main.ui.viewer.handoff.attributes.shadow :refer [shadow-panel]]
   [app.main.ui.viewer.handoff.attributes.stroke :refer [stroke-panel]]
   [app.main.ui.viewer.handoff.attributes.svg :refer [svg-panel]]
   [app.main.ui.viewer.handoff.attributes.text :refer [text-panel]]
   [app.main.ui.viewer.handoff.exports :refer [exports]]
   [rumext.v2 :as mf]))

(def type->options
  {:multiple [:fill :stroke :image :text :shadow :blur :layout-flex-item]
   :frame    [:layout :fill :stroke :shadow :blur :layout-flex :layout-flex-item]
   :group    [:layout :svg :layout-flex-item]
   :rect     [:layout :fill :stroke :shadow :blur :svg :layout-flex-item]
   :circle   [:layout :fill :stroke :shadow :blur :svg :layout-flex-item]
   :path     [:layout :fill :stroke :shadow :blur :svg]
   :image    [:image :layout :fill :stroke :shadow :blur :svg :layout-flex-item]
   :text     [:layout :text :shadow :blur :stroke :layout-flex-item]})

(mf/defc attributes
  [{:keys [page-id file-id shapes frame]}]
  (let [shapes  (hooks/use-equal-memo shapes)
        shapes  (mf/with-memo [shapes]
                  (mapv #(gsh/translate-to-frame % frame) shapes))
        type    (if (= (count shapes) 1) (-> shapes first :type) :multiple)
        options (type->options type)]
    [:div.element-options
     (for [option options]
       [:> (case option
             :layout           layout-panel
             :layout-flex      layout-flex-panel
             :layout-flex-item layout-flex-element-panel
             :fill             fill-panel
             :stroke           stroke-panel
             :shadow           shadow-panel
             :blur             blur-panel
             :image            image-panel
             :text             text-panel
             :svg              svg-panel)
        {:shapes shapes
         :frame frame}])
     [:& exports
      {:shapes shapes
       :type type
       :page-id page-id
       :file-id file-id}]]))
