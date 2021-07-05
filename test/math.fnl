(import-macros {: assert= : is} :test.macros)
(local B (require :bulb))

(local tests {})

(fn tests.inc []
  (assert= 1 (B.inc 0))
  (assert= 1.5 (B.inc 0.5))
  (assert= 0 (B.inc -1)))

(fn tests.dec []
  (assert= -1 (B.dec 0))
  (assert= -0.5 (B.dec 0.5))
  (assert=  0 (B.dec 1)))

(fn tests.clamp []
  (assert= 0 (B.clamp 5 0 0))
  (assert= 0 (B.clamp -1 0 0))
  (assert= 5 (B.clamp 5 0 5))
  (assert= 5 (B.clamp 5 5 10))
  (assert= 15 (B.clamp 15 15 15)))

tests
