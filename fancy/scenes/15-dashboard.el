;;; 15-dashboard -- startup dashboard
(require 'dashboard)
(dashboard-open)
(delete-other-windows)
(fancy:shoot (getenv "FANCY_OUT"))
