(push "../" asdf:*central-registry*)
(require 'peercoin-vote)

(peercoin-vote:set-testnet t)
(peercoin-vote:vote "b883a8983f9f8776a8353d81518bc6aaed6b157f7bc0475e5b7d01ce87efb099" "candidates.txt" "votes.txt" "balances.txt")
(quit)
