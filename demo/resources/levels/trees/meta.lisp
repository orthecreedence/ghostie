(:name "trees"
 ;:actors ((:type "ghostie" :start-pos (-480 -450) :main t :scale (.7 .7 .7)))
 :actors ((:id :pilly :type "pill" :start-position (-480 -400) :scale (.7 .7 .7)))
 :objects ((:id :boxy :type "platform" :start-position (-320 -440) :scale (.7 .7 .7) :limit-x (-300 1100)))
 :scale (1 1 1)
 :camera (99 205 -400)
 :background "#262524"
 :fog-color "#262524"
 :fog-amt 1.0
 :fog-start 800
 :fog-end 1800
 :gravity -700
 :physics-iterations 20
 :object-properties ((:name "ground"
                      :layer-depth -.01)
                     (:name "ground-background"
                      :layer-depth -160)
                     (:name "tree1"
                      :layer-depth -120)
                     (:name "tree2"
                      :layer-depth -240)
                     (:name "tree3"
                      :layer-depth -320)
                     (:name "tree4"
                      :layer-depth -420)))
