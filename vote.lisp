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

(in-package peercoin-vote)


(defparameter +message-header+ (format nil "Peercoin Signed Message:~c" #\Newline))


(defmacro for-each-line ((line file) &rest body)
  (let ((f (gensym)))
    `(with-open-file (,f ,file)
       (do ((,line (read-line ,f nil nil) (read-line ,f nil nil)))
           ((null ,line))
         ,@body))))

(defun verify-signature (address vote signature)
  "Check whether the SIGNATURE was made on the hash of the VOTE by ADDRESS."
  (let* ((msg (concatenate 'string
                           (string (code-char (length +message-header+)))
                           +message-header+
                           (string (code-char (length vote)))
                           vote))
         (hash (sha256d (map '(vector (unsigned-byte 8)) #'char-code msg)))
         (sig (cl-base64:base64-string-to-usb8-array signature)))
    (verify-compact-signature hash sig address)))

(defun vote (motion-hash file-candidates file-votes file-balances)
  "Compute the results of the vote for a specific MOTION-HASH, reading the lists of candidates, votes and address balances from their respective files."
  (let (weights results valid-votes invalid-votes total-weight)
    ;; Initialize weight table
    (setf weights (make-hash-table :test #'equal))
    (for-each-line (line file-balances)
      (destructuring-bind (address balance) (split-sequence:split-sequence #\Space line :count 2)
        (let ((v (read-from-string balance)))
          (when (and (numberp v) (plusp v))
            (setf (gethash address weights) v)))))

    ;; Get candidates from file and prepare result table
    (setf results (make-hash-table :test #'equalp))
    (for-each-line (candidate file-candidates)
      (setf (gethash candidate results) 0))

    ;; Process votes
    (setf valid-votes 0)
    (setf invalid-votes 0)
    (setf total-weight 0)
    (for-each-line (line file-votes)
      (destructuring-bind (&optional (address "") (vote "") (signature ""))
          (split-sequence:split-sequence #\Space line :count 3)
        (destructuring-bind (&optional (motion "") (candidate ""))
            (split-sequence:split-sequence #\: vote :count 2)
          (let ((weight (gethash address weights))
                (result (gethash candidate results)))
            (if (and (numberp weight)
                     (plusp weight)
                     (numberp result)
                     (string= motion motion-hash)
                     (verify-signature address vote signature))
                (progn
                  (setf (gethash candidate results) (+ result weight))
                  (incf total-weight weight)
                  (incf valid-votes)
                  ;; Remove the address from the table to prevent it from voting several times
                  (remhash address weights))
                (incf invalid-votes))))))

    ;; Print results
    (format t "~%Results of the vote for the motion ~a~%~%" motion-hash)
    (format t "Invalid votes: ~d~%" invalid-votes)
    (format t "Valid votes: ~d~%~%" valid-votes)
    (when (plusp valid-votes)
      (loop
         for candidate being the hash-keys in results using (hash-value result)
         do (format t "~a:~7t~7,3f% (~d)~%" candidate (* 100 (/ result total-weight)) result)))))
