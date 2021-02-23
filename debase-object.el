(defclass debase-object* ()
  ((bus :initarg :bus
        :type symbol
        :documentation "Bus the D-Bus service is on.")
   (service :initarg :service
            :type string
            :documentation "D-Bus service.")
   (path :initarg :path
         :type string
         :documentation "Path to D-Bus object.")
   (interface :initarg :interface
              :type string
              :accessor debase-object--interfaces
              :documentation "Interface this object binds to, if any.")
   (xml :type cons
        :accessor debase-object--xml
        :documentation "XML representation of the D-Bus object."))
  :abstract t
  :documentation "Base class for D-Bus objects.")

(cl-defmethod debase-object--xml ((this debase-object*))
  "Return XML representation of D-Bus object THIS."
  (debase-bind this
      (if (boundp 'xml) xml
        (setf xml (dbus-introspect-xml)))))

(cl-defmethod debase-object--interfaces ((this debase-object*) &optional interfaces)
  "Return D-Bus interface definitions INTERFACES from XML.

If INTERFACES is nil, returns all interfaces except those in
`debase--ignore-interfaces'.

If INTERFACES is :all, returns all interfaces, even those in
`debase--ignore-interfaces'.

If INTERFACES is a list of strings, return interfaces matching them.
"
  (let ((xml (debase-object--xml this)))
    (debase--assert xml 'node)
    (cl-loop for child in (dom-non-text-children xml)
             when (eq 'interface (dom-tag child))
             when (cond
                   ((eq interfaces :all) t)
                   ((consp interfaces) (member (debase-interface-name child) interfaces))
                   (t (not (member (debase-interface-name child) debase--ignore-interfaces))))
             collect child)))
