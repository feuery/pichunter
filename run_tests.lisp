(ql:quickload :rove)
(unless (rove:run :pichunter/tests)
  (sb-ext:exit :code 666))
