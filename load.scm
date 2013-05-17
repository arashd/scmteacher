;;; This loader constructs a new user-initial-environment.

(load "utils" user-initial-environment)
(load "ghelper" user-initial-environment)
(load "syntax" user-initial-environment)
(load "rtdata" user-initial-environment)

(define generic-evaluation-environment
  (extend-top-level-environment user-initial-environment))

(load "string-utils" generic-evaluation-environment)

(load "graph" generic-evaluation-environment)
(load "eq-properties" generic-evaluation-environment)
(load "advice" generic-evaluation-environment)
(load "linalg" generic-evaluation-environment)
(load "steps" generic-evaluation-environment)
(load "linalg-advice" generic-evaluation-environment)
(load "linalg-generics" generic-evaluation-environment)

(load "interp" generic-evaluation-environment)
(load "repl" generic-evaluation-environment)

(ge generic-evaluation-environment)
