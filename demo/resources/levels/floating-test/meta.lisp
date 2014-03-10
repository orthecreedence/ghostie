(:name "floating-test"
 ;:actors ((:type "ghostie" :start-pos (-480 -450) :main t :scale (.7 .7 .7)))
 :actors ((:id :pilly :type "pill" :start-position (-850.480 -400) :scale (.7 .7 .7)))
 :objects ((:id :floater1 :type "floater" :start-position (-200 -400) :scale (.7 .7 .7) :limit (-200 -100)))
 :scale (1 1 1)
 :camera (99 205 -410)
 :background "#262524"
 :fog-color "#262524"
 :fog-amt 1.0
 :fog-start 800
 :fog-end 1800
 :gravity -700
 :physics-iterations 20
 :object-properties ((:name "ground"
                      :layer-depth -.01)))
