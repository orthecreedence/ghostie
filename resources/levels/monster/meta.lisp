(:name "monster"
 ;:actors ((:actor "ghostie" :start-pos (-480 -450 0) :main t :scale (.5 .5 .5)))
 ;:actors ((:actor "pill" :start-pos (480 470 0) :main t :scale (1 1 1)))
 :scale (1 1 1)
 :camera (99 205 -440)
 :background "#534741"
 :fog-color "#534741"
 :fog-amt 0.0
 :fog-start 600
 :fog-end 1600
 :gravity -700
 :physics-iterations 10
 :object-properties ((:name "ground"
                      :layer-depth 10)
                     (:name "monster"
                      :layer-depth -30)
                     (:name "cave"
                      :layer-depth -120)
                     (:name "midground"
                      :layer-depth -240)
                     (:name "background"
                      :layer-depth -320)))
