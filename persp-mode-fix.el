;;; persp-mode-fix.el -*- lexical-binding: t; -*-
;; from https://github.com/Bad-ptr/persp-mode.el/issues/104

(with-eval-after-load "persp-mode"
  (defun persp--wc-to-writable (wc)
    (cl-labels
        ((wc-to-writable
          (it)
          (cond
           ((and it (listp it))
            (cl-destructuring-bind (head . tail) it
              (cond
               ;; ((listp head)
               ;;  (cons (wc-to-writable head)
               ;;        (wc-to-writable tail)))
               ((eq 'parameters head)
                (let ((rw-params
                       (delq nil
                             (mapcar
                              #'(lambda (pc)
                                  (when
                                      (and
                                       (alist-get (car pc) window-persistent-parameters)
                                       (persp-elisp-object-readable-p (cdr pc)))
                                    pc))
                              tail))))
                  (if rw-params
                      `(parameters
                        ,@rw-params)
                    :delete)))
               (t
                (let ((new-head (wc-to-writable head))
                      (new-tail (wc-to-writable tail)))
                  (when (eq :delete new-tail)
                    (setq new-tail nil))
                  (if (eq :delete new-head)
                      new-tail
                    (cons new-head
                          new-tail)))))))
           ((bufferp it)
            (if (buffer-live-p it)
                (buffer-name it)
              "*Messages*"))
           ((markerp it)
            (marker-position it))
           (t it))))
      (wc-to-writable wc)))

  (setq persp-window-state-get-function
        #'(lambda (&optional frame rwin)
            (when (or rwin (setq rwin (frame-root-window
                                       (or frame (selected-frame)))))
              (window-state-get rwin nil))))

  (add-hook 'persp-before-save-state-to-file-functions
            #'(lambda (_fname phash _rpfp)
                (mapc
                 #'(lambda (persp)
                     (if persp
                         (setf (persp-window-conf persp)
                               (persp--wc-to-writable (persp-window-conf persp)))
                       (setq persp-nil-wconf
                             (persp--wc-to-writable persp-nil-wconf))))
                 (persp-persps phash)))))
