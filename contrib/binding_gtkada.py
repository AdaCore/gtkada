"""
See annotations documentation at:
    https://live.gnome.org/GObjectIntrospection/Annotations

Parses the file binding.xml, which is used to override some aspects of
the automatically generated code.
The syntax of that file is as follows:
    <?xml version="1.0"?>
    <GIR>
       <package />   <!--  repeated as often as needed
    </GIR>

Where the package node is defined as follows:
    <package id="..."       <!-- mandatory, ctype -->
             obsolescent="..." <!--  Whether this package is obsolete -->
             bindtype="..."  <!-- False if the type should not be bound, only
                                  its methods -->
             into="..."      <!-- optional, the package to bind types and
                                  methods into -->
             ada="..."       <!-- optional, the package's ada name -->
             parent="..."    <!-- Override the parent type for the widget type
                                  in this package -->
    >
       <doc screenshot="..." <!-- optional -->
            group="..."      <!-- optional -->
            testgtk="..."    <!-- optional -->
            see="..."        <!-- optional -->
       >
       package-level documentation
       </doc>

       <type               <!-- repeated as needed ->
           name="..."      <!-- mandatory, Ada type name -->
           subtype="True"  <!-- optional, if True generate a subtype -->
       />

       <parameter
           name="..."
           ada="..."       <!-- Override default naming for all methods.
                                In particular used for "Self" -->
           type="..."      <!-- Override Ada types for all methods -->
           ctype="..."     <!-- Override C type (to better qualify it) -->
           direction="..." <!-- Override direction (see <parameter> below) -->
       />

       <!-- By default, virtual methods are bound for interfaces, but not for
            other classes. You should use the following to control which
            methods should be bound.
       -->
       <virtual-method  <!--  list of virtual methods to bind -->
           id='...'     <!--  the name of the virtual method, or '*'  -->
           bind=True    <!--  by default, all methods bound for interfaces -->
       />

       <!-- The following tag can be used to override various properties from
            the GIR file.

            It is also possible to indicate that a method should not be bound
            (or that a method inherited from an interface should not be
            repeated in the binding, by setting the "bind" attribute to False.
       -->

       <method             <!-- repeated as needed -->
           id="..."        <!-- mandatory, name of the C method -->
                           <!-- fields are not bound by default, but are
                                associated with
                                    "gtkada_%s_get_%s" % (adapkg, field_name)
                                methods
                                For signals, we use "::signal-name"
                           -->
           ada="..."       <!-- optional, name of the Ada subprogram -->
           bind="true"     <!-- optional, if false no binding generated -->
           obsolescent=".." <!--  Whether this method is obsolete" -->
           transfer-ownership='none'  <!-- set to 'full' to indicate the return
                                value must be freed by the caller -->
           return_as_param="..." <!-- optional, replace return parameter with
                                an out parameter with this name. Used to avoid
                                functions with out parameters. -->
           return="..."    <!-- Override C type for the returned value, or
                                "void" to change into procedure. -->
           classwide="False"  <!-- classwide, not a primitive operation -->
       >
         <doc extend="..."> <!-- if extend is true, append to doc from GIR -->
            ...            <!-- "\n" forces a newline, paragraphs are created
                                on empty lines. A paragraph that starts with
                                '__PRE__' will be displayed exactly as is, no
                                line wrapping is done.-->
         </doc>
         <parameter        <!-- repeated as needed -->
            name="..."     <!-- mandatory, lower-cased name of param,
                             use "varargs" to replace the varargs parameter -->
            ada="..."      <!-- optional, name to use in Ada. If empty, the
                                parameter will be omitted in the Ada profile,
                                but kept in the C profile.
                           -->
            type="..."     <!-- optional, override Ada type.
                                The value will be passed as is to C, unless
                                it is "Glib.Object.GObject", in which
                                case the value is processed as a GObject -->
            ctype="..."    <!-- Override C type (to better qualify it) -->
            default="..."  <!-- optional, the default value for the param-->
            direction=".." <!-- optional, "in", "out", "access" or "inout" -->
            allow-none="0" <!-- If C accepts a NULL value (an empty string
                                is mapped to the null pointer -->
         />
         <body>...</body>  <!-- optional, body of subprogram (after "is")
                             If the subprogram is defined for an interface,
                             it will automatically be duplicated on each type
                             that implements that interface. As such, you
                             should use "+Self" to pass the parameter to
                             C, where the type of the C parameter should be
                             the interface type itself. See GtkTreeModel.

                             You can use "%(auto)s" to use the automatic
                             body and append to it.  -->
         />

       <function id="...">   <!--  repeated as needed, for global functions -->
                             <!--  content is same as <method> -->
       </function>

       <constant
             prefix_regexp="..."  <!--  maps all constants whose C type matches
                                        the regexp -->
             prefix="..."         <!--  omit this prefix from the C name, to
                                        generate the Ada name -->
       />

       <!-- The following statement indicates that the binding for the
            enumeration should be added in the current package.
            This automatically generates the naming exceptions for the type
            and its values, but you can override the mapping by:
              * adding entries in data.py (cname_to_adaname)
                For instance:    "GTK_SIZE_GROUP_NONE": "None"
              * or setting the prefix attribute, for instance
                 prefix="GTK_SIZE_GROUP_"
       -->
       <enum ctype="..."
             ada="..."     <!-- optional Ada name (no package info needed) -->
             prefix="GTK_" <!-- remove prefix from values to get Ada name -->
             asbitfield="false" <!--  forces a bitfield -->
             ignore="value1 value2..."  <!-- values that should not be bound-->
       />

       <!-- Support for <record> types -->
       <record ctype="..."
             ada="..."     <!-- optional Ada name (no package info needed) -->
             private="False"  <!--  whether to make it "private" -->
       >
          <field name="..." type="..."/>   <!--  override ctype -->
          <union value="..." field="..."/>  <!--  once for each value of
                                                  discriminant, mapping to
                                                  relevant field --
       </record>

       <!-- Instantiates a list of elements. These statements automatically
            define new ctypes which are the concatenation of the ctype given
            as attribute and a suffix (resp. "List" and "SList"), so that you
            can then use these lists as parameter types in methods.
       -->

       <list ada="Ada name for the list type"
             section="..."  <!--  Where should the list be declared -->
             ctype="..."/>  <!--  Name of the element contained in the list -->
       <slist ada="Ada name for the list type"  <!-- single-linked list -->
             section="..."  <!--  Where should the list be declared -->
             ctype="..."/>  <!--  Name of the element contained in the list -->

       <extra>
          <gir:method>...  <!-- optional, same nodes as in the .gir file -->
          <with_spec pkg="..." use="true"/>
                           <!-- extra with clauses for spec -->
          <with_body pkg="..." use="true"/>
                           <!-- extra with clauses for body -->

          <!-- Code will be put after generated subprograms-->
          <spec>...     <!-- optional, code to insert in spec -->
          <body before="true">...     <!-- optional, code to insert in body
                By default, it is inserted before the generated code,
                unless 'before' is set to "false" -->

          <type            <!-- Maps a C type to an Ada type -->
             ctype="..."   <!-- Mandatory: c type name -->
             ada="..."     <!-- Mandatory: ada type name -->
          >
             code          <!-- Optional, the type declaration, will be put
                                after generated types but before subprograms-->
          </type>
       </extra>
    </package>
"""

from xml.etree.cElementTree import parse, SubElement
from adaformat import AdaType, GObject, List, naming, Enum, Record


class GtkAda(object):

    def __init__(self, filename):
        self._tree = parse(filename)
        self.root = self._tree.getroot()
        self.packages = dict()
        for node in self.root:
            if node.tag == "package":
                self.packages[node.get("id")] = GtkAdaPackage(node)

    def get_pkg(self, pkg):
        """Return the GtkAdaPackage for a given package"""
        return self.packages.get(pkg, GtkAdaPackage(None))


class GtkAdaPackage(object):

    """A <package> node in the binding.xml file"""

    def __init__(self, node):
        self.node = node
        self.doc = []

        if node:
            self.bindtype = node.get("bindtype", "t").lower() != "false"
        else:
            self.bindtype = True

    def __repr__(self):
        return "<GtkAdaPackage name=%s>" % (
            self.node.get("id") if self.node else "")

    def register_types(self, adapkg):
        """If we are going to generate some enumerations in the package, we
           need to register them now, so that all places where the enumeration
           is referenced have the proper full name.

           adapkg is the name of the Ada package.
        """

        if self.node:
            for enum in self.node.findall("enum"):
                Enum.register_ada_decl(pkg=adapkg,
                                       ctype=enum.get("ctype"),
                                       ada=enum.get("ada", None))
            for rec in self.node.findall("record"):
                Record.register_ada_record(
                    pkg=adapkg,
                    ctype=rec.get("ctype"),
                    ada=rec.get("ada", None))
            for rec in self.node.findall("list"):
                List.register_ada_list(
                    pkg=adapkg,
                    ctype=rec.get("ctype"),
                    ada=rec.get("ada", None))
            for rec in self.node.findall("slist"):
                List.register_ada_list(
                    pkg=adapkg,
                    ctype=rec.get("ctype"),
                    ada=rec.get("ada", None),
                    single=True)

    def parent_type(self):
        """Override the parent type for the main widget type in this package"""
        if self.node:
            return self.node.get("parent", None)
        return None

    def enumerations(self):
        """List of all enumeration types that need to be declared in the
           package. The result is a list of tuples.
        """
        result = []
        if self.node:
            for enum in self.node.findall("enum"):
                result.append((enum.get("ctype"),
                               naming.type(name="", cname=enum.get("ctype")),
                               enum.get("prefix", "GTK_"),
                               enum.get(
                                   "asbitfield", "false").lower() == "true",
                               enum.get("ignore", "")
                               ))
        return result

    def constants(self):
        """Return the list of constants that should be bound as part
           of this package.
        """
        if self.node is not None:
            return [(c.get("prefix_regexp", ""), c.get("prefix", ""))
                    for c in self.node.findall("constant")]
        return []

    def lists(self):
        """Return the list of list instantiations we need to add to the
           package. Returns a list of tuples:
              [(adaname, CType for element,
                  true for a single-linked list, section),
                ...]
        """
        result = []
        if self.node:
            for l_inst in self.node.findall("list"):
                result.append((l_inst.get("ada"),
                               naming.type(name="", cname=l_inst.get("ctype")),
                               False,
                               l_inst.get("section", "")))
            for s_l in self.node.findall("slist"):
                result.append((s_l.get("ada"),
                               naming.type(name="", cname=s_l.get("ctype")),
                               True,
                               s_l.get("section", "")))

        return result

    def add_record_type(self, ctype):
        """Add explicit record to bind, unless it is already mentioned
           explictly in the <record> nodes.
        """

        if self.node:
            for rec in self.node.findall("record"):
                if rec.get("ctype") == ctype:
                    return

            SubElement(self.node, "record", {"ctype": ctype})

    def records(self):
        """Returns the list of record types, as a list of tuples:
               [ (ctype name,  corresponding CType, ada name, [fields],
                 [union], [private=False]) ...]
           Where fields is a dict for each field whose type is
           overridden:
               { name: CType, ... }
           and union is a list of tuples (value, field) associating
           a field of a <union> with the corresponding value of the
           discriminant.

           The returned list includes records added via add_record_type
        """

        result = []
        if self.node:
            for rec in self.node.findall("record"):
                override_fields = {}

                for field in rec.findall("field"):
                    override_fields[field.get("name")] = \
                        naming.type(name="", cname=field.get("ctype"))

                unions = []
                for field in rec.findall("union"):
                    unions.append((field.get("value"), field.get("field")))

                result.append((rec.get("ctype"),
                               naming.type(name="",
                                           cname=rec.get("ctype")),
                               rec.get("ada"),
                               override_fields, unions,
                               rec.get("private", "false").lower() == "true"))

        return result

    def get_doc(self):
        """Return the overridden doc for for the package, as a list of
           string. Each string is a paragraph
        """
        if self.node is None:
            return []

        docnode = self.node.find("doc")
        if docnode is None:
            return []

        doc = []

        txt = docnode.text or ""
        if txt:
            doc = ["<description>", txt, "</description>"]

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
            for f in self.node.findall("method"):
                if f.get("id") == cname:
                    return GtkAdaMethod(f, self)
            for f in self.node.findall("virtual-method"):
                if f.get("id") == cname:
                    return GtkAdaMethod(f, self)
            for cb in self.node.findall("callback"):
                if cb.get("id") == cname:
                    return GtkAdaMethod(cb, self)

        return GtkAdaMethod(None, self)

    def get_type(self, name):
        if self.node is not None:
            name = name.lower()
            for f in self.node.findall("type"):
                if f.get("name").lower() == name:
                    return GtkAdaType(f)
        return GtkAdaType(None)

    def into(self):
        if self.node is not None:
            return self.node.get("into", None)
        return None

    def ada_name(self):
        if self.node is not None:
            return self.node.get("ada", None)
        return None

    def is_obsolete(self):
        if self.node is not None:
            return self.node.get("obsolescent", "False").lower() == "true"
        return False

    def extra(self):
        if self.node is not None:
            extra = self.node.find("extra")
            if extra is not None:
                return list(extra)
        return None

    def get_default_param_node(self, name):
        if name and self.node is not None:
            name = name.lower()
            for p in self.node.findall("parameter"):
                if p.get("name") == name:
                    return p
        return None

    def get_global_functions(self):
        """Return the list of global functions that should be bound as part
           of this package.
        """
        if self.node is not None:
            return [GtkAdaMethod(c, self)
                    for c in self.node.findall("function")
                    if c.get("bind", "true").lower() != "false"]
        return []

    def bind_virtual_method(self, name, default):
        """
        Whether to bind the given virtual method
        """
        if not hasattr(self, 'virtual_methods'):
            self.virtual_methods = {'*': default}
            if self.node is not None:
                for c in self.node.findall("virtual-method"):
                    self.virtual_methods[c.get('id')] = (
                        c.get("bind", "true").lower() == "true")

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
            for p in self.node.findall("parameter"):
                if p.get("name").lower() == name:
                    return GtkAdaParameter(p, default=default)

        return GtkAdaParameter(None, default=default)

    def is_class_wide(self):
        return self.node is not None \
            and self.node.get("classwide", "False").lower() != "false"

    def bind(self, default="true"):
        """Whether to bind"""
        if self.node is not None:
            return self.node.get("bind", default).lower() != "false"
        return default != "false"

    def ada_name(self):
        if self.node is not None:
            return self.node.get("ada", None)
        return None

    def returned_c_type(self):
        if self.node is not None:
            return self.node.get("return", None)
        return None

    def is_obsolete(self):
        if self.node is not None:
            return self.node.get("obsolescent", "False").lower() == "true"
        return False

    def convention(self):
        if self.node is not None:
            return self.node.get("convention", None)
        return None

    def return_as_param(self):
        if self.node is not None:
            return self.node.get("return_as_param", None)
        return None

    def transfer_ownership(self, return_girnode):
        """Whether the value returned by this method needs to be free by the
           caller.
           return_girnode is the XML node from the gir file for the return
           value of the method.
        """
        default = return_girnode.get('transfer-ownership', 'none')
        if self.node is not None:
            return self.node.get('transfer-ownership', default) != 'none'
        else:
            return default != 'none'

    def get_body(self):
        if self.node is not None:
            return self.node.findtext("body")
        return None

    def get_doc(self, default):
        """Return the doc, as a list of lines"""
        if self.node is not None:
            d = self.node.find("doc")
            if d is not None:
                txt = d.text
                doc = []
                for paragraph in txt.split("\n\n"):
                    for p in paragraph.split("\\n\n"):
                        doc.append(p)
                    doc.append("")

                if d.get("extend", "false").lower() == "true":
                    return [default, ""] + doc
                return doc
        return [default]


class GtkAdaParameter(object):

    def __init__(self, node, default):
        self.node = node
        self.default = default

    def get_default(self):
        if self.node is not None:
            return self.node.get("default", None)
        return None

    def get_direction(self):
        if self.node is not None:
            return self.node.get("direction", None)
        if self.default is not None:
            return self.default.get("direction", None)
        return None

    def get_caller_allocates(self):
        value = None
        if self.node is not None:
            value = self.node.get("caller-allocates", None)
        if self.default is not None:
            value = self.default.get("caller-allocates", None)
        return value

    def get_transfer_ownership(self):
        value = None
        if self.node is not None:
            value = self.node.get("transfer-ownership", None)
        if self.default is not None:
            value = self.default.get("transfer-ownership", None)
        return value

    def ada_name(self):
        name = None
        if self.node is not None:
            name = self.node.get("ada", None)
        if name is None and self.default is not None:
            name = self.default.get("ada", None)
        return name

    def get_type(self, pkg):
        """pkg is used to set the with statements.
           This returned the locally overridden type, or the one from the
           default node for this parameter, or None if the type isn't
           overridden.
        """

        if self.node is not None:
            t = self.node.get("type", None)
            if t:
                if t == "Glib.Object.GObject":
                    return GObject(t, userecord=False)

                return AdaType(t, pkg=pkg)  # An instance of CType

            t = self.node.get("ctype", None)
            if t:
                return t   # An XML node

        if self.default is not None:
            t = self.default.get("type", None)
            if t:
                return AdaType(t, pkg=pkg)

        return None

    def allow_none(self, girnode):
        default = girnode.get('allow-none', '0')
        if self.node is not None:
            return self.node.get('allow-none', default) == '1'
        else:
            return default == '1'


class GtkAdaType(object):

    def __init__(self, node):
        self.node = node

    def is_subtype(self):
        if self.node is not None:
            return self.node.get("subtype", "false").lower() == "true"
        return False
