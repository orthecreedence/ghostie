(in-package :ghostie)

;; define an "infinity" value
(defconstant +infinity-f+ most-positive-single-float)
(defconstant +neg-infinity-f+ most-negative-single-float)

;; --------- graphics config
(defconstant +config-graphics-window-x+ 800)
(defconstant +config-graphics-window-y+ 640)
(defconstant +config-graphics-pixels-per-meter+ 4)

;; --------- physics
(defconstant +object-thickness+ 5.0)
;; global world settings
(defconstant +physics-step-num-iterations+ 1)
(defconstant +physics-steps+ 0.005)
(defconstant +physics-speed+ 1)
(defconstant +physics-gravity+ -9.8)
(defconstant +physics-linear-damping+ 0.001)
(defconstant +physics-linear-damping-threshold+ 0.001)
(defconstant +physics-angular-damping+ 0.001)
(defconstant +physics-angular-damping-threshold+ 0.001)
(defconstant +physics-auto-disable-bodies+ 1)
(defconstant +physics-auto-disable-linear-threshold+ 0.4)
(defconstant +physics-auto-disable-angular-threshold+ 0.01)
(defconstant +physics-global-cfm+ 0.001)
(defconstant +physics-global-erp+ 0.4)
(defconstant +physics-contact-surface-layer+ 0.001)
(defconstant +physics-contact-max-correcting-vel+ 1.0)
(defconstant +max-collision-objects+ 64)
(defconstant +physics-step-w+ 1.3)   ; probably don't fuck with this
;; default joint settings (can be overridden per joint)
(defconstant +physics-contact-mode+ (bit-or ode:+contact-slip-1+ ode:+contact-slip-2+
                                             ;ode:+contact-soft-erp+ ode:+contact-soft-cfm+
                                             ode:+contact-approx-1+))
(defconstant +physics-contact-mu+ 5.1)
(defconstant +physics-contact-mu2+ +physics-contact-mu+)
(defconstant +physics-contact-slip1+ 0.0)
(defconstant +physics-contact-slip2+ +physics-contact-slip1+)
(defconstant +physics-contact-cfm+ +physics-global-cfm+)
(defconstant +physics-contact-erp+ +physics-global-erp+)
(defconstant +physics-contact-motion1+ 0.0)
(defconstant +physics-contact-motion2+ 0.0)
(defconstant +physics-contact-motion-n+ 0.0)
(defconstant +physics-contact-bounce+ 0.1)
(defconstant +physics-contact-bounce-min-vel+ 0.001)

