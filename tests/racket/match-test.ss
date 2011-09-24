#lang planet masm/sines

(require
 (rename-in "../../test.ss"
            [check-expect ->e]
            [check-expect-one-of ->e/one-of]
            [check-quoted-expect ->]
            [check-quoted-expect-one-of ->/one-of]))

[(match '(1 2 3)
   [(list a b c)
    (list a b c)])          . -> . (1 2 3)]
