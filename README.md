# Debase, the D-Bus convenience layer for Emacs

![img](sorry.jpg)

D-Bus is an [IPC system](https://en.wikipedia.org/wiki/Inter-process_communication) which is ubiquitous on Linux, and (in this author’s opinion) not very good. Emacs has bindings for interfacing with it (see the former point), which are annoying to use (see the latter point).

These days, numerous common system management tasks are implemented as D-Bus services rather than tradidional executables, and many of the command-line tools themselves are now front-ends which communicate via D-Bus. Mounting and unmounting disks, monitoring battery status, controlling display brightness, connecting to wireless networks and more are now handled with D-Bus services.

It makes no sense to shell out to the tools when one could interact with them directly via D-Bus, if only it was less annoying to do so.

Debase frees you from writing repetitive, annoying boilerplate code to drive D-Bus services by throwing another pile of abstraction at the problem, in the form of unreadably dense, macro-heavy, profoundly cursed Lisp.


## A crash course in D-Bus

-   Bus. A bus contains services; D-Bus can manage many different busses, but the two standard ones are:
    -   System bus. This generally has hardware-interfacing and system management services.
    -   Session bus. This is private to the current session, i.e. a logged-in user.

-   Services. A service exists on a bus, and is a set of information and operations offered by a program. Example: `org.bluez` on the system bus is the service which manages Bluetooth.

-   Objects. An object exists within a service, and typically represents a resource it manages. Objects are identified by paths; paths are namespaced under the service. Example: `/org/bluez/hci0/dev_01_23_45_67_89_AB` is the path to an object representing a specific Bluetooth device. Because this is part of the service, that path doesn’t represent anything in a different service, like `org.freedesktop.fwupd`.

-   Interfaces. An interface is a view into the capabilities of an object. Objects can (and almost always do) support multiple interfaces. Example: `org.bluez.Device1` is a general interface for managing pairing/unpairing/connecting/disconnecting from Bluetooth devices; `org.bluez.MediaControl1` is an interface for media devices, such as speakers or speakerphones. Since `/org/bluez/hci0/dev_01_23_45_67_89_AB` is a media device, it supports both interfaces.

-   Properties. A property is a value attached to an interface, which exposes information about an object. For example, the `Name` property in the `org.bluez.Device1` interface of `/org/bluez/hci0/dev_01_23_45_67_89_AB` is "Bluetooth Speaker" — the name of the device. Properties can be read/write, read-only, or write-only.

-   Methods. A method is a remote function call attached to an interface. For example, the `VolumeUp()` method in the `org.bluez.MediaControl1` interface of object `/org/bluez/hci0/dev_01_23_45_67_89_AB` in the `org.bluez` service of the system bus increases the volume of "Bluetooth Speaker." Methods can take arguments and return values.

-   Signals. D-Bus enabled applications can generate and respond to signals. A signal represents some kind of event, such as hardware being plugged in or unplugged.

-   Common interfaces. *Most* D-Bus objects support some common interfaces:
    -   [`org.freedesktop.DBus.Introspectable`](https://dbus.freedesktop.org/doc/dbus-java/api/org/freedesktop/DBus.Introspectable.html). Allows retrieving the schema for the object as XML. It has all the interfaces it supports, as well as their properties and methods.
    -   [`org.freedesktop.DBus.Peer`](https://dbus.freedesktop.org/doc/dbus-java/api/org/freedesktop/DBus.Peer.html). Provides a `Ping` method.
    -   [`org.freedesktop.DBus.Properties`](https://dbus.freedesktop.org/doc/dbus-java/api/org/freedesktop/DBus.Properties.html). An interface which exposes object properties, and provides signals so other D-Bus applications receive notifications of changes to them.
    -   [`org.freedesktop.DBus.ObjectManager`](https://dbus.freedesktop.org/doc/dbus-specification.html#standard-interfaces-objectmanager). Used by D-Bus applications which manage other D-Bus objects. For example, the `org.bluez` service’s `/` object implements `ObjectManager`, which can be used to enumerate connected Bluetooth devices. It also provides signals when managed objects are added or removed.


## Building Blocks FIXME

Even though Debase makes this easier, many D-Bus methods require additional type wrangling or conversion to be used comfortably. For these cases, you should subclass `DEBASE-OBJECT` and write more specialized methods.

If you need to target the same object repeatedly, you can subclass `EIEIO-SINGLETON` as well. Calls to the constructor function will always return the same object.

```emacs-lisp
(defclass udisks2-block (debase-object) ())

(cl-defmethod initialize-instance :after ((this udisks2-manager) &rest ignore)
  ;; (with-slots (bus service interface) this
  ;;   (setf bus :system
  ;;         service "org.freedesktop.UDisks2"
  ;;         interface  "org.freedesktop.UDisks2.Block"))
  )


(cl-defmethod udisks2-block-preferred-device ((this udisks2-block))
  (apply #'str (debase-object-get this "PreferredDevice")))

(let ((block (udisks2-block :path "/org/freedesktop/UDisks2/block_devices/nvme0n1")))
  (debase-object-target block)
  ;; (udisks2-block-preferred-device block)
  )


```


### Object Manager

Debase provides a `DEBASE-OBJECTMANAGER` class which interacts with the `org.freedesktop.DBus.ObjectManager` interface. It maintains a local cache of managed objects, which is populated on instantiation and automatically updated when one is added or removed.

If a class inherits from it, accessing the `MANAGED-OBJECTS` slot will return the currently managed objects.

It can also dispatch notifications when the list of managed objects changes.


## Code Generation (Experimental)

Debase also offers a code generation facility, which turns the XML D-Bus interface descriptions into EIEIO classes. It’s based on `DEBASE-GEN` classes, which are themselves `DEBASE-OBJECT` subclasses, and the `DEBASE-GEN-CODE` generic function.

This code is experimental and subject to changes. Feedback is welcome. I’m not sure it’s useful enough.

Basic example:

```emacs-lisp
(thread-first
    (debase-gen-class :bus :system
                      :service "org.freedesktop.UDisks2"
                      :interface "org.freedesktop.UDisks2.Manager"
                      :class-name udisks2-manager)
  (debase-gen-code))
```


### Name Mangling

To make generated code more pleasant, `DEBASE-GEN` supports name manglers. These are functions which take a string of a D-Bus name, and return a friendlier one. When generating a class, you can specify a mangler for property, method, and argument names.

```emacs-lisp
(debase-gen-class :bus :system
                  :service "org.freedesktop.UDisks2"
                  :interface "org.freedesktop.UDisks2.Manager"
                  :class-name udisks2-manager
                  :property-mangle (lambda (name) (concat "prop-" name))
                  :method-mangle #'identity)
```

This will leave method and argument names untouched, and prefix properties with "prop-".
