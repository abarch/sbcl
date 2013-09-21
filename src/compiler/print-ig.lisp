
(defpackage :sb-graph (:use :cl :cl-dot))
(in-package :sb-graph)

(setf (sb-c::event-action 'sb-c::pack-done) nil)

(defparameter graph '())

  (setf (sb-c::event-action 'sb-c::pack-done)

        (lambda (node vertices)

          (declare (ignore node))
          (print (nth-value 1
                            (ignore-errors
                              ;; (let ((*print-circle* t))
                              ;; (print vertices))


          (let ((context (sb-c::find-error-context '())))
            (when context
              (format t "~&~A:~A~%"
                      (sb-c::compiler-error-context-file-name context)
                      (sb-c::compiler-error-context-context context))))

          (let ((vars (make-hash-table))
                (result '()))
            (dolist (v vertices)
              (setf (gethash v vars) (list (princ-to-string  (sb-c::vertex-tn v))
                                           (car (sb-c::vertex-color v)))))
            (dolist (v vertices)
              (let ((output (gethash v vars)))
                (dolist (n (sb-c::vertex-incidence v))
                  (let ((neighbor (gethash n vars)))
                    (when (string< (first output) (first neighbor))
                      (push neighbor (cddr output)))))))
            (maphash (lambda (k v) (push v result)) vars)
            (setf graph result)

            ;; (let ((*print-circle* t)) (print result ))
            ))))))



(graph-print graph)


(defmethod graph-object-points-to ((graph (eql :allocation)) (object list))
  (cddr object))

(defmethod graph-object-node ((graph (eql :allocation)) (object list))
  (make-instance 'node
                 :attributes `(:label ,(first object)
                                      :style :filled
                               :fillcolor ,(format nil "#~6,'0X"
                                               (mod (* (second object) 435345353)
                                                    (ash 1 24))))))

(defun allocation-graph (graph)
  (generate-graph-from-roots :allocation graph))

(defun  graph-print (vertices)
(cl-dot:dot-graph (allocation-graph vertices)
                  "/homes/abarch/google/sbcl/bla.pdf"
                  :format :pdf
                  :directed t))
