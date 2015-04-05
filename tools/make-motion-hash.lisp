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

(require 'ironclad)

(defun sha256d (file)
  "Compute the double sha256 hash of the FILE."
  (ironclad:digest-sequence :sha256 (ironclad:digest-file :sha256 file)))

(defun make-motion-hash (motion-file output-file)
  (let ((hash (ironclad:byte-array-to-hex-string (sha256d motion-file))))
    (with-open-file (f output-file :direction :output :if-exists :supersede)
      (format f "~a~%" hash))))
