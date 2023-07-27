;; #!/bin/env sbcl

(ql:quickload :pichunter/tests)
(unless (rove:run :pichunter/tests)
  (sb-ext:exit :code 666))
