;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.pages.common
  (:require
   [app.common.colors :as clr]
   [app.common.uuid :as uuid]))

(def file-version 19)
(def default-color clr/gray-20)
(def root uuid/zero)

(def component-sync-attrs
  {:name                  :name-group
   :fills                 :fill-group
   :fill-color            :fill-group
   :fill-opacity          :fill-group
   :fill-color-gradient   :fill-group
   :fill-color-ref-file   :fill-group
   :fill-color-ref-id     :fill-group
   :hide-fill-on-export   :fill-group
   :content               :content-group
   :hidden                :visibility-group
   :blocked               :modifiable-group
   :grow-type             :text-font-group
   :font-family           :text-font-group
   :font-size             :text-font-group
   :font-style            :text-font-group
   :font-weight           :text-font-group
   :letter-spacing        :text-display-group
   :line-height           :text-display-group
   :text-align            :text-display-group
   :strokes               :stroke-group
   :stroke-color          :stroke-group
   :stroke-color-gradient :stroke-group
   :stroke-color-ref-file :stroke-group
   :stroke-color-ref-id   :stroke-group
   :stroke-opacity        :stroke-group
   :stroke-style          :stroke-group
   :stroke-width          :stroke-group
   :stroke-alignment      :stroke-group
   :stroke-cap-start      :stroke-group
   :stroke-cap-end        :stroke-group
   :rx                    :radius-group
   :ry                    :radius-group
   :r1                    :radius-group
   :r2                    :radius-group
   :r3                    :radius-group
   :r4                    :radius-group
   :selrect               :geometry-group
   :points                :geometry-group
   :locked                :geometry-group
   :proportion            :geometry-group
   :proportion-lock       :geometry-group
   :x                     :geometry-group
   :y                     :geometry-group
   :width                 :geometry-group
   :height                :geometry-group
   :rotation              :geometry-group
   :transform             :geometry-group
   :transform-inverse     :geometry-group
   :position-data         :geometry-group
   :opacity               :layer-effects-group
   :blend-mode            :layer-effects-group
   :shadow                :shadow-group
   :blur                  :blur-group
   :masked-group?         :mask-group
   :constraints-h         :constraints-group
   :constraints-v         :constraints-group
   :fixed-scroll          :constraints-group
   :exports               :exports-group

   :layout                :layout-container
   :layout-dir            :layout-container
   :layout-gap            :layout-container
   :layout-type           :layout-container
   :layout-wrap-type      :layout-container
   :layout-padding-type   :layout-container
   :layout-padding        :layout-container
   :layout-h-orientation  :layout-container
   :layout-v-orientation  :layout-container

   :layout-item-margin         :layout-item
   :layout-item-margin-type    :layout-item
   :layout-item-h-sizing       :layout-item
   :layout-item-v-sizing       :layout-item
   :layout-item-max-h          :layout-item
   :layout-item-min-h          :layout-item
   :layout-item-max-w          :layout-item
   :layout-item-min-w          :layout-item
   :layout-item-align-self     :layout-item
   })

;; Attributes that may directly be edited by the user with forms
(def editable-attrs
  {:frame #{:proportion-lock
            :width :height
            :x :y
            :rx :ry
            :r1 :r2 :r3 :r4
            :rotation
            :selrect
            :points
            :show-content
            :hide-in-viewer

            :opacity
            :blend-mode
            :blocked
            :hidden

            :fills
            :fill-color
            :fill-opacity
            :fill-color-ref-id
            :fill-color-ref-file
            :fill-color-gradient
            :hide-fill-on-export

            :strokes
            :stroke-style
            :stroke-alignment
            :stroke-width
            :stroke-color
            :stroke-color-ref-id
            :stroke-color-ref-file
            :stroke-opacity
            :stroke-color-gradient
            :stroke-cap-start
            :stroke-cap-end

            :exports

            :layout
            :layout-flex-dir
            :layout-gap
            :layout-gap-type
            :layout-align-items
            :layout-justify-content
            :layout-align-content
            :layout-wrap-type
            :layout-padding-type
            :layout-padding

            :layout-item-margin
            :layout-item-margin-type
            :layout-item-h-sizing
            :layout-item-v-sizing
            :layout-item-max-h
            :layout-item-min-h
            :layout-item-max-w
            :layout-item-min-w
            :layout-item-align-self}

   :group #{:proportion-lock
            :width :height
            :x :y
            :rotation
            :selrect
            :points

            :constraints-h
            :constraints-v
            :fixed-scroll
            :parent-id
            :frame-id

            :opacity
            :blend-mode
            :blocked
            :hidden

            :shadow

            :blur

            :exports

            :layout-item-margin
            :layout-item-margin-type
            :layout-item-h-sizing
            :layout-item-v-sizing
            :layout-item-max-h
            :layout-item-min-h
            :layout-item-max-w
            :layout-item-min-w
            :layout-item-align-self}

   :rect #{:proportion-lock
           :width :height
           :x :y
           :rotation
           :rx :ry
           :r1 :r2 :r3 :r4
           :selrect
           :points

           :constraints-h
           :constraints-v
           :fixed-scroll
           :parent-id
           :frame-id

           :opacity
           :blend-mode
           :blocked
           :hidden

           :fills
           :fill-color
           :fill-opacity
           :fill-color-ref-id
           :fill-color-ref-file
           :fill-color-gradient

           :strokes
           :stroke-style
           :stroke-alignment
           :stroke-width
           :stroke-color
           :stroke-color-ref-id
           :stroke-color-ref-file
           :stroke-opacity
           :stroke-color-gradient
           :stroke-cap-start
           :stroke-cap-end

           :shadow

           :blur

           :exports

           :layout-item-margin
           :layout-item-margin-type
           :layout-item-h-sizing
           :layout-item-v-sizing
           :layout-item-max-h
           :layout-item-min-h
           :layout-item-max-w
           :layout-item-min-w
           :layout-item-align-self}

   :circle #{:proportion-lock
             :width :height
             :x :y
             :rotation
             :selrect
             :points

             :constraints-h
             :constraints-v
             :fixed-scroll
             :parent-id
             :frame-id

             :opacity
             :blend-mode
             :blocked
             :hidden

             :fills
             :fill-color
             :fill-opacity
             :fill-color-ref-id
             :fill-color-ref-file
             :fill-color-gradient

             :strokes
             :stroke-style
             :stroke-alignment
             :stroke-width
             :stroke-color
             :stroke-color-ref-id
             :stroke-color-ref-file
             :stroke-opacity
             :stroke-color-gradient
             :stroke-cap-start
             :stroke-cap-end

             :shadow

             :blur

             :exports

             :layout-item-margin
             :layout-item-margin-type
             :layout-item-h-sizing
             :layout-item-v-sizing
             :layout-item-max-h
             :layout-item-min-h
             :layout-item-max-w
             :layout-item-min-w
             :layout-item-align-self}

   :path #{:proportion-lock
           :width :height
           :x :y
           :rotation
           :selrect
           :points

           :constraints-h
           :constraints-v
           :fixed-scroll
           :parent-id
           :frame-id

           :opacity
           :blend-mode
           :blocked
           :hidden

           :fills
           :fill-color
           :fill-opacity
           :fill-color-ref-id
           :fill-color-ref-file
           :fill-color-gradient

           :strokes
           :stroke-style
           :stroke-alignment
           :stroke-width
           :stroke-color
           :stroke-color-ref-id
           :stroke-color-ref-file
           :stroke-opacity
           :stroke-color-gradient
           :stroke-cap-start
           :stroke-cap-end

           :shadow

           :blur

           :exports

           :layout-item-margin
           :layout-item-margin-type
           :layout-item-h-sizing
           :layout-item-v-sizing
           :layout-item-max-h
           :layout-item-min-h
           :layout-item-max-w
           :layout-item-min-w
           :layout-item-align-self}

   :text #{:proportion-lock
           :width :height
           :x :y
           :rotation
           :selrect
           :points

           :constraints-h
           :constraints-v
           :fixed-scroll
           :parent-id
           :frame-id

           :opacity
           :blend-mode
           :blocked
           :hidden

           :fill-color
           :fill-opacity
           :fill-color-ref-id
           :fill-color-ref-file
           :fill-color-gradient

           :stroke-style
           :stroke-alignment
           :stroke-width
           :stroke-color
           :stroke-color-ref-id
           :stroke-color-ref-file
           :stroke-opacity
           :stroke-color-gradient
           :stroke-cap-start
           :stroke-cap-end

           :shadow

           :blur

           :typography-ref-id
           :typography-ref-file

           :font-id
           :font-family
           :font-variant-id
           :font-size
           :font-weight
           :font-style

           :text-align

           :text-direction

           :line-height
           :letter-spacing

           :vertical-align

           :text-decoration

           :text-transform

           :grow-type

           :exports

           :layout-item-margin
           :layout-item-margin-type
           :layout-item-h-sizing
           :layout-item-v-sizing
           :layout-item-max-h
           :layout-item-min-h
           :layout-item-max-w
           :layout-item-min-w
           :layout-item-align-self}

   :image #{:proportion-lock
            :width :height
            :x :y
            :rotation
            :rx :ry
            :r1 :r2 :r3 :r4
            :selrect
            :points

            :constraints-h
            :constraints-v
            :fixed-scroll
            :parent-id
            :frame-id

            :opacity
            :blend-mode
            :blocked
            :hidden

            :shadow

            :blur

            :exports

            :layout-item-margin
            :layout-item-margin-type
            :layout-item-h-sizing
            :layout-item-v-sizing
            :layout-item-max-h
            :layout-item-min-h
            :layout-item-max-w
            :layout-item-min-w
            :layout-item-align-self}

   :svg-raw #{:proportion-lock
              :width :height
              :x :y
              :rotation
              :rx :ry
              :r1 :r2 :r3 :r4
              :selrect
              :points

              :constraints-h
              :constraints-v
              :fixed-scroll
              :parent-id
              :frame-id

              :opacity
              :blend-mode
              :blocked
              :hidden

              :fills
              :fill-color
              :fill-opacity
              :fill-color-ref-id
              :fill-color-ref-file
              :fill-color-gradient

              :strokes
              :stroke-style
              :stroke-alignment
              :stroke-width
              :stroke-color
              :stroke-color-ref-id
              :stroke-color-ref-file
              :stroke-opacity
              :stroke-color-gradient
              :stroke-cap-start
              :stroke-cap-end

              :shadow

              :blur

              :exports

              :layout-item-margin
              :layout-item-margin-type
              :layout-item-h-sizing
              :layout-item-v-sizing
              :layout-item-max-h
              :layout-item-min-h
              :layout-item-max-w
              :layout-item-min-w
              :layout-item-align-self}

   :bool #{:proportion-lock
           :width :height
           :x :y
           :rotation
           :rx :ry
           :r1 :r2 :r3 :r4
           :selrect
           :points

           :constraints-h
           :constraints-v
           :fixed-scroll
           :parent-id
           :frame-id

           :opacity
           :blend-mode
           :blocked
           :hidden

           :fill-color
           :fill-opacity
           :fill-color-ref-id
           :fill-color-ref-file
           :fill-color-gradient

           :stroke-style
           :stroke-alignment
           :stroke-width
           :stroke-color
           :stroke-color-ref-id
           :stroke-color-ref-file
           :stroke-opacity
           :stroke-color-gradient
           :stroke-cap-start
           :stroke-cap-end

           :shadow

           :blur

           :exports

           :layout-item-margin
           :layout-item-margin-type
           :layout-item-h-sizing
           :layout-item-v-sizing
           :layout-item-max-h
           :layout-item-min-h
           :layout-item-max-w
           :layout-item-min-w
           :layout-item-align-self}})

