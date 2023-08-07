(defun org-babel-execute:kcats (body params)
  "Execute a block of kcats code with org-babel."
  (org-babel-eval
   kcats-babel-executable
   body))

(defcustom kcats-babel-executable "kcats"
  "Location of the kcats binary"
  :type 'string
  :group 'kcats-babel)
