(:name "trees"
 ;:actors ((:actor "ghostie" :start-pos (-480 -450 0) :main t :scale (.7 .7 .7)))
 :actors ((:actor "pill" :start-pos (-480 -470 0) :main t :scale (.7 .7 .7)))
 :scale (1 1 1)
 :camera (99 205 -400)
 :background "#262524"
 :fog-color "#262524"
 :fog-amt 1.0
 :fog-start 800
 :fog-end 1800
 :gravity -700
 :physics-iterations 10
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
