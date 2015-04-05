#|
Copyright 2015 Guillaume LE VAILLANT

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#

(push "../../peercoin-blockchain-parser/" asdf:*central-registry*)
(require 'peercoin-blockchain-parser)

(load "../../peercoin-blockchain-parser/my-testnet-config.lisp")
(peercoin-blockchain-parser:set-testnet t)
;;(load "../../peercoin-blockchain-parser/my-config.lisp")

(defun make-balance-file (block-number output-file)
  (let ((balance-list (peercoin-blockchain-parser:rdbms-get-balances-at-block block-number)))
    (with-open-file (f output-file :direction :output :if-exists :supersede)
      (dolist (e balance-list)
        (format f "~a ~d~%" (first e) (round (* 1000000 (second e))))))))
