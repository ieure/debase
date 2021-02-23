
 ;; Binding helpers



(cl-defmacro debase-flet-partial* ((func &rest args) &rest body)
  "Like FLET, but binds a partial of ARGS applied FUNC around BODY."
  (declare (indent 2))
  `(cl-flet ((,func (apply-partially #',func ,@args)))
     ,@body))



(cl-defmacro debase-bind* ((bus service path &optional interface) &rest forms)
  "Bind D-Bus functions around FORMS, targeting BUS SERVICE PATH INTERFACE

Inside FORMS, calls to DBUS-INTROSPECT-XML, DBUS-CALL-METHOD,
DBUS-GET-PROPERTY, AND DBUS-SET-PROPERTY take their bus, service,
path, from this function's arguments.

\(fn (BUS SERVICE PATH &OPTIONAL :INTERFACE INTERFACE) &REST BODY)"
  (declare (indent 2))
  (let ((oargs (list bus service path))
        (iargs `(,bus ,service ,path ,(when interface interface))))
    `(thread-last ,@forms
       (debase-flet-partial* (dbus-introspect-xml ,@oargs))
       (debase-flet-partial* (dbus-get-property ,@iargs))
       (debase-flet-partial* (dbus-set-property ,@iargs))
       (debase-flet-partial* (dbus-call-method ,@iargs)))))

(cl-defmacro debase-bind (debase-object &rest forms)
  "Bind FORMS so D-Bus methods implicitly target DEBASE-OBJECT.

Inside FORMS, calls to DBUS-INTROSPECT-XML, DBUS-CALL-METHOD,
DBUS-GET-PROPERTY, AND DBUS-SET-PROPERTY take their bus, service,
path, and interface arguments from DBUS-OBJECT, and don't require them
to be set.

\(fn (BUS SERVICE PATH &OPTIONAL :INTERFACE INTERFACE) &REST BODY)"
  (declare (indent 2))
  `(with-slots (bus service path interface) ,debase-object
     (debase-bind* (bus service path interface)
         ,@forms)))

