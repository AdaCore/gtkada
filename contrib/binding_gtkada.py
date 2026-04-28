"""
See annotations documentation at:
    https://live.gnome.org/GObjectIntrospection/Annotations

Parses toml files describing bindings, which are used to override some aspects of
the automatically generated code.
The syntax of these files is as follows:

    # Each package is a top-level table section
    [PackageId]
      obsolescent = true   # optional, whether this package is obsolete
      bindtype = false     # optional, false if the type should not be bound,
                           #   only its methods
      into = "..."         # optional, the package to bind types and methods
                           #   into
      ada = "..."          # optional, the package's Ada name
      parent = "..."       # override the parent type for the widget type in
                           #   this package

      [PackageId.doc]
        screenshot = "..."   # optional
        group = "..."        # optional
        testgtk = "..."      # optional
        see = "..."          # optional
        text = '''...'''     # package-level documentation

      [[PackageId.parameter]]  # repeated as often as needed
        name = "..."
        ada = "..."       # override default naming for all methods;
                          #   in particular used for "Self"
        type = "..."      # override Ada types for all methods
        ctype = "..."     # override C type (to better qualify it)
        direction = "..."  # override direction (see [[...parameter]] below)

      # By default, virtual methods are bound for interfaces, but not for
      # other classes. Use the following to control which methods are bound.
      [[PackageId.virtual_method]]
        id = "..."         # name of the virtual method, or "*"
        bind = false       # by default, all methods bound for interfaces

      # The following can be used to override various properties from the
      # GIR file. It is also possible to indicate that a method should not
      # be bound (or that a method inherited from an interface should not be
      # repeated in the binding) by setting bind = false.
      [[PackageId.method]]   # repeated as needed
        id = "..."           # mandatory, name of the C method
                             # fields are not bound by default, but are
                             #   associated with
                             #     "gtkada_%s_get_%s" % (adapkg, field_name)
                             #   methods
                             #   for signals, use "::signal-name"
        ada = "..."          # optional, name of the Ada subprogram
        bind = false         # optional, if false no binding generated
        obsolescent = true   # optional, whether this method is obsolete
        transfer_ownership = "full"  # set to "full" to indicate the return
                             #   value must be freed by the caller
        return_as_param = "..."  # optional, replace return parameter with an
                             #   out parameter with this name; used to avoid
                             #   functions with out parameters
        return = "..."       # override C type for the returned value, or
                             #   "void" to change into procedure
        classwide = true     # classwide, not a primitive operation
        body = '''...'''     # optional body of subprogram (after "is");
                             #   use "%(auto)s" to use the automatic body
                             #   and append to it

        [PackageId.method.doc]
          extend = true      # if true, append to doc from GIR
          text = '''...'''   # "\n" forces a newline; paragraphs are created
                             #   on empty lines

        [[PackageId.method.parameter]]  # repeated as needed
          name = "..."       # mandatory, lower-cased name of param;
                             #   use "varargs" to replace the varargs parameter
          ada = "..."        # optional, name to use in Ada; if empty, the
                             #   parameter will be omitted in the Ada profile
                             #   but kept in the C profile
          type = "..."       # optional, override Ada type; the value will be
                             #   passed as is to C, unless it is
                             #   "Glib.Object.GObject", in which case the
                             #   value is processed as a GObject
          ctype = "..."      # override C type (to better qualify it)
          default = "..."    # optional, the default value for the param
          direction = "..."  # optional, "in", "out", "access" or "inout"
          allow_none = "1"   # if C accepts a NULL value (an empty string
                             #   is mapped to the null pointer)

      [[PackageId.function]]  # repeated as needed, for global functions
                              # (same content as [[...method]])

      [[PackageId.callback]]  # repeated as needed (same content as method)

      [[PackageId.constant]]
        prefix_regexp = "..."  # maps all constants whose C type matches
                               #   the regexp
        prefix = "..."         # omit this prefix from the C name, to
                               #   generate the Ada name

      # The following indicates that the binding for the enumeration should
      # be added in the current package. This automatically generates the
      # naming exceptions for the type and its values, but you can override
      # the mapping by adding entries in data.py (cname_to_adaname) or by
      # setting the prefix attribute.
      [[PackageId.enum]]
        ctype = "..."
        ada = "..."        # optional Ada name (no package info needed)
        prefix = "GTK_"   # remove prefix from values to get Ada name
        asbitfield = false # forces a bitfield
        ignore = "value1 value2..."  # values that should not be bound

      [[PackageId.record]]
        ctype = "..."
        ada = "..."        # optional Ada name (no package info needed)
        private = true     # whether to make it "private"

        [[PackageId.record.field]]
          name = "..."
          ctype = "..."    # override ctype

        [[PackageId.record.union]]  # once for each value of discriminant,
                                    #   mapping to the relevant field
          value = "..."
          field = "..."

      # Instantiates a list of elements. These automatically define new
      # ctypes which are the concatenation of the ctype given as attribute
      # and a suffix (resp. "List" and "SList"), so that you can then use
      # these lists as parameter types in methods.
      [[PackageId.list]]    # double-linked list
        ada = "..."         # Ada name for the list type
        ctype = "..."       # name of the element contained in the list
        section = "..."     # optional, where should the list be declared

      [[PackageId.slist]]   # single-linked list
        ada = "..."
        ctype = "..."
        section = "..."     # optional

      [[PackageId.type]]
        name = "..."
        subtype = true      # optional, if true generate a subtype

      [PackageId.extra]
        body = '''...'''    # code inserted in body (before generated
                            #   subprograms by default)

        [[PackageId.extra.with_spec]]
          pkg = "..."       # extra with clause for spec
          use = false       # default true

        [[PackageId.extra.with_body]]
          pkg = "..."       # extra with clause for body
          use = false       # default true

        [[PackageId.extra.spec]]
          text = '''...'''  # code to insert in spec
          private = true    # optional, whether to add to private part

        [[PackageId.extra.type]]   # maps a C type to an Ada type
          ctype = "..."     # mandatory: C type name
          ada = "..."       # mandatory: Ada type name
          text = '''...'''  # optional, type declaration code (placed after
                            #   generated types but before subprograms)

        [[PackageId.extra.gir_method]]
          xml = "..."       # XML string of the GIR method element
"""

try:
    import tomllib
except ImportError:
    import tomli as tomllib

import xml.etree.ElementTree as ET

from adaformat import AdaType, GObject, List, naming, Enum, Record


class GtkAda(object):

    def __init__(self, location):
        from os import listdir
        from os.path import join
        from pathlib import Path

        self.packages = {}
        tomls = filter(lambda f: f.endswith('.toml'), listdir(Path(location)))
        # Build GtkAda package from each toml file
        for t in tomls:
            filepath = join(location, t)
            with open(filepath, 'rb') as f:
                data = tomllib.load(f)
            for id, node in data.items():
                self.packages[id] = GtkAdaPackage(id, node)

    def get_pkg(self, pkg):
        """Return the GtkAdaPackage for a given package"""
        return self.packages.get(pkg, GtkAdaPackage(None, None))


_CONTENT_KEYS = frozenset({
    'doc', 'parameter', 'type', 'enum', 'record', 'list', 'slist',
    'constant', 'method', 'function', 'virtual_method', 'callback', 'extra',
})
"""Keys in a package dict that correspond to child elements in the XML.

In the original XML reader, an ElementTree element with no children evaluated
as falsy (``bool(elem) == False``).  Methods that used ``if self.node:`` would
therefore be skipped for packages that had only attributes (e.g. just
``into="..."``) and no child elements.  We replicate this behaviour by
checking for the presence of at least one content key.
"""


class GtkAdaPackage(object):

    """A package in a toml file"""

    def __init__(self, pkg_id, node):
        self.pkg_id = pkg_id
        self.node = node

        if node is not None:
            self.bindtype = node.get("bindtype", True)
        else:
            self.bindtype = True

    def __repr__(self):
        return "<GtkAdaPackage name=%s>" % (self.pkg_id or "")

    def _has_children(self):
        """True if the package has content beyond direct attributes.

        Replicates the XML ElementTree falsy-element behaviour: a node with no
        child elements (even if it has attributes) evaluated as False.
        """
        return self.node is not None and bool(self.node.keys() & _CONTENT_KEYS)

    def register_types(self, adapkg):
        """If we are going to generate some enumerations in the package, we
           need to register them now, so that all places where the enumeration
           is referenced have the proper full name.

           adapkg is the name of the Ada package.
        """

        if self._has_children():
            for enum in self.node.get("enum", []):
                Enum.register_ada_decl(pkg=adapkg,
                                       ctype=enum["ctype"],
                                       ada=enum.get("ada"))
            for rec in self.node.get("record", []):
                Record.register_ada_record(
                    pkg=adapkg,
                    ctype=rec["ctype"],
                    ada=rec.get("ada"))
            for lst in self.node.get("list", []):
                List.register_ada_list(
                    pkg=adapkg,
                    ctype=lst["ctype"],
                    ada=lst.get("ada"))
            for slst in self.node.get("slist", []):
                List.register_ada_list(
                    pkg=adapkg,
                    ctype=slst["ctype"],
                    ada=slst.get("ada"),
                    single=True)

    def parent_type(self):
        """Override the parent type for the main widget type in this package"""
        if self._has_children():
            return self.node.get("parent")
        return None

    def enumerations(self):
        """List of all enumeration types that need to be declared in the
           package. The result is a list of tuples.
        """
        result = []
        if self._has_children():
            for enum in self.node.get("enum", []):
                result.append((enum["ctype"],
                               naming.type(name="", cname=enum["ctype"]),
                               enum.get("prefix", "GTK_"),
                               enum.get("asbitfield", False),
                               enum.get("ignore", "")
                               ))
        return result

    def constants(self):
        """Return the list of constants that should be bound as part
           of this package.
        """
        if self.node is not None:
            return [(c.get("prefix_regexp", ""), c.get("prefix", ""))
                    for c in self.node.get("constant", [])]
        return []

    def lists(self):
        """Return the list of list instantiations we need to add to the
           package. Returns a list of tuples:
              [(adaname, CType for element,
                  true for a single-linked list, section),
                ...]
        """
        result = []
        if self._has_children():
            for lst in self.node.get("list", []):
                result.append((lst.get("ada"),
                               naming.type(name="", cname=lst["ctype"]),
                               False,
                               lst.get("section", "")))
            for slst in self.node.get("slist", []):
                result.append((slst.get("ada"),
                               naming.type(name="", cname=slst["ctype"]),
                               True,
                               slst.get("section", "")))

        return result

    def add_record_type(self, ctype):
        """Add explicit record to bind, unless it is already mentioned
           explicitly in the [[...record]] entries.
        """

        if self._has_children():
            for rec in self.node.get("record", []):
                if rec.get("ctype") == ctype:
                    return

            self.node.setdefault("record", []).append({"ctype": ctype})

    def records(self):
        """Returns the list of record types, as a list of tuples:
               [ (ctype name,  corresponding CType, ada name, [fields],
                 [union], [private=False]) ...]
           Where fields is a dict for each field whose type is
           overridden:
               { name: CType, ... }
           and union is a list of tuples (value, field) associating
           a field of a [[...record.union]] with the corresponding value of
           the discriminant.

           The returned list includes records added via add_record_type
        """

        result = []
        if self._has_children():
            for rec in self.node.get("record", []):
                override_fields = {}

                for field in rec.get("field", []):
                    override_fields[field["name"]] = \
                        naming.type(name="", cname=field["ctype"])

                unions = []
                for union in rec.get("union", []):
                    unions.append((union["value"], union["field"]))

                result.append((rec["ctype"],
                               naming.type(name="",
                                           cname=rec["ctype"]),
                               rec.get("ada"),
                               override_fields, unions,
                               rec.get("private", False)))

        return result

    def get_doc(self):
        """Return the overridden doc for the package, as a list of
           strings. Each string is a paragraph.
        """
        if self.node is None:
            return []

        docnode = self.node.get("doc")
        if docnode is None:
            return []

        doc = []

        txt = docnode.get("text", "")
        if txt:
            doc = [txt]

        n = docnode.get("screenshot")
        if n is not None:
            doc.append("<screenshot>%s</screenshot>" % n)

        n = docnode.get("group")
        if n is not None:
            doc.append("<group>%s</group>" % n)

        n = docnode.get("testgtk")
        if n is not None:
            doc.append("<testgtk>%s</testgtk>" % n)

        n = docnode.get("see")
        if n is not None:
            doc.append("<see>%s</see>" % n)

        return doc

    def get_method(self, cname):
        if self.node is not None:
            for f in self.node.get("method", []):
                if f.get("id") == cname:
                    return GtkAdaMethod(f, self)
            for f in self.node.get("virtual_method", []):
                if f.get("id") == cname:
                    return GtkAdaMethod(f, self)
            for cb in self.node.get("callback", []):
                if cb.get("id") == cname:
                    return GtkAdaMethod(cb, self)

        return GtkAdaMethod(None, self)

    def get_type(self, name):
        if self.node is not None:
            name = name.lower()
            for f in self.node.get("type", []):
                if f.get("name", "").lower() == name:
                    return GtkAdaType(f)
        return GtkAdaType(None)

    def into(self):
        if self.node is not None:
            return self.node.get("into")
        return None

    def ada_name(self):
        if self.node is not None:
            return self.node.get("ada")
        return None

    def is_obsolete(self):
        if self.node is not None:
            return self.node.get("obsolescent", False)
        return False

    def extra(self):
        if self.node is not None:
            extra_dict = self.node.get("extra")
            if extra_dict is not None:
                return self._extra_to_elements(extra_dict)
        return None

    def _extra_to_elements(self, extra_dict):
        """Convert the TOML extra dict to XML elements.

           The result is extended into a GIR XML node by callers, so the
           elements must use the same tags and attributes that binding.py
           expects to find via ElementTree searches.
        """
        GIR_NS = "http://www.gtk.org/introspection/core/1.0"
        elements = []

        body_text = extra_dict.get("body")
        if body_text:
            elem = ET.Element("body")
            elem.text = body_text
            elements.append(elem)

        for ws in extra_dict.get("with_spec", []):
            elem = ET.Element("with_spec")
            elem.set("pkg", ws.get("pkg", ""))
            use = ws.get("use", True)
            elem.set("use", "true" if use else "false")
            elements.append(elem)

        for wb in extra_dict.get("with_body", []):
            elem = ET.Element("with_body")
            elem.set("pkg", wb.get("pkg", ""))
            use = wb.get("use", True)
            elem.set("use", "true" if use else "false")
            elements.append(elem)

        for spec in extra_dict.get("spec", []):
            elem = ET.Element("spec")
            elem.text = spec.get("text", "")
            if spec.get("private", False):
                elem.set("private", "True")
            elements.append(elem)

        for type_entry in extra_dict.get("type", []):
            elem = ET.Element("type")
            elem.set("ctype", type_entry.get("ctype", ""))
            ada = type_entry.get("ada")
            if ada:
                elem.set("ada", ada)
            elem.text = type_entry.get("text", "")
            elements.append(elem)

        for gir_entry in extra_dict.get("gir_element", []):
            xml_str = gir_entry.get("xml", "")
            if xml_str:
                ET.register_namespace("gir", GIR_NS)
                gir_elem = ET.fromstring(xml_str)
                elements.append(gir_elem)

        return elements or None

    def get_default_param_node(self, name):
        if name and self.node is not None:
            name = name.lower()
            for p in self.node.get("parameter", []):
                if p.get("name") == name:
                    return p
        return None

    def get_global_functions(self):
        """Return the list of global functions that should be bound as part
           of this package.
        """
        if self.node is not None:
            return [GtkAdaMethod(c, self)
                    for c in self.node.get("function", [])
                    if c.get("bind", True)]
        return []

    def bind_virtual_method(self, name, default):
        """
        Whether to bind the given virtual method
        """
        if not hasattr(self, 'virtual_methods'):
            self.virtual_methods = {'*': default}
            if self.node is not None:
                for c in self.node.get("virtual_method", []):
                    self.virtual_methods[c.get('id')] = c.get('bind', True)

        v = self.virtual_methods.get(name, None)
        if v is None:
            v = self.virtual_methods['*']
        return v


class GtkAdaMethod(object):

    def __init__(self, node, pkg):
        self.node = node
        self.pkg = pkg

    def cname(self):
        """Return the name of the C function"""
        return self.node.get("id")

    def get_param(self, name):
        default = self.pkg.get_default_param_node(name)
        if self.node is not None:
            name = name.lower()
            for p in self.node.get("parameter", []):
                if p.get("name", "").lower() == name:
                    return GtkAdaParameter(p, default=default)

        return GtkAdaParameter(None, default=default)

    def is_class_wide(self):
        return self.node is not None and self.node.get("classwide", False)

    def bind(self, default="true"):
        """Whether to bind"""
        if self.node is not None:
            return self.node.get("bind", default != "false")
        return default != "false"

    def ada_name(self):
        if self.node is not None:
            return self.node.get("ada")
        return None

    def returned_c_type(self):
        if self.node is not None:
            return self.node.get("return")
        return None

    def is_obsolete(self):
        if self.node is not None:
            return self.node.get("obsolescent", False)
        return False

    def convention(self):
        if self.node is not None:
            return self.node.get("convention")
        return None

    def return_as_param(self):
        if self.node is not None:
            return self.node.get("return_as_param")
        return None

    def transfer_ownership(self, return_girnode):
        """Whether the value returned by this method needs to be freed by the
           caller.
           return_girnode is the XML node from the gir file for the return
           value of the method.
        """
        default = return_girnode.get('transfer-ownership', 'none')
        if self.node is not None:
            return self.node.get('transfer_ownership', default) != 'none'
        else:
            return default != 'none'

    def get_body(self):
        if self.node is not None:
            return self.node.get("body")
        return None

    def get_doc(self, default):
        """Return the doc, as a list of lines"""
        if self.node is not None:
            d = self.node.get("doc")
            if d is not None:
                txt = d.get("text", "")
                doc = []
                for paragraph in txt.split("\n\n"):
                    for p in paragraph.split("\\n\n"):
                        doc.append(p)
                    doc.append("")

                if d.get("extend", False):
                    return [default, ""] + doc
                return doc
        return [default]


class GtkAdaParameter(object):

    def __init__(self, node, default):
        self.node = node
        self.default = default

    def get_default(self):
        if self.node is not None:
            return self.node.get("default")
        return None

    def get_direction(self):
        if self.node is not None:
            return self.node.get("direction")
        if self.default is not None:
            return self.default.get("direction")
        return None

    def get_caller_allocates(self):
        value = None
        if self.node is not None:
            value = self.node.get("caller_allocates")
        if self.default is not None:
            value = self.default.get("caller_allocates")
        return value

    def get_transfer_ownership(self):
        value = None
        if self.node is not None:
            value = self.node.get("transfer_ownership")
        if self.default is not None:
            value = self.default.get("transfer_ownership")
        return value

    def ada_name(self):
        name = None
        if self.node is not None:
            name = self.node.get("ada")
        if name is None and self.default is not None:
            name = self.default.get("ada")
        return name

    def get_type(self, pkg):
        """pkg is used to set the with statements.
           This returns the locally overridden type, or the one from the
           default node for this parameter, or None if the type isn't
           overridden.
        """

        if self.node is not None:
            t = self.node.get("type")
            if t:
                if t == "Glib.Object.GObject":
                    return GObject(t, userecord=False)

                return AdaType(t, pkg=pkg)  # An instance of CType

            t = self.node.get("ctype")
            if t:
                return t   # The C type string

        if self.default is not None:
            t = self.default.get("type")
            if t:
                return AdaType(t, pkg=pkg)

        return None

    def allow_none(self, girnode):
        default = girnode.get('allow-none', '0')
        if self.node is not None:
            return self.node.get('allow_none', default) == '1'
        else:
            return default == '1'


class GtkAdaType(object):

    def __init__(self, node):
        self.node = node

    def is_subtype(self):
        if self.node is not None:
            return self.node.get("subtype", False)
        return False
