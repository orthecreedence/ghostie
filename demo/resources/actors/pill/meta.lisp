(:max-velocity 1000
 :friction 0.9
 ;:physics ((:type :circle :mass 12.5 :position (0 .73275) :radius 0.25960576565)
 ;          (:type :circle :mass 25.0 :position (0 0) :radius 0.4655d0)
 ;          (:type :circle :mass 12.5 :position (0 -.73275) :radius 0.25960576565))
 :physics ((:type :segment :mass 50d0 :position (0 0) :endpoints ((0 1) (0 -1)) :radius .4))
 )

