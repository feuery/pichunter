(ql:quickload :pichunter/tests)
(unless (and (fiveam:run! 'pichunter/tests/main:main-suite)
	     (fiveam:run! 'pichunter/tests/game:game-suite))
  (sb-ext:exit :code 666))
