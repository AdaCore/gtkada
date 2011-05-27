"""
Parses the file binding.xml, which is used to override some aspects of
the automatically generated code.
The syntax of that file is as follows:
    <?xml version="1.0"?>
    <GIR>
       <package />   <!--  repeated as often as needed
    </GIR>

Where the package node is defined as follows:
    <package id="..."       <!-- mandatory -->
             obsolescent="..." <!--  Whether this package is obsolete -->

    >
       <doc screenshot="..." <!-- optional -->
            group="..."      <!-- optional -->
            testgtk="..."    <!-- optional -->
       >
       package-level documentation
       </doc>

       <type               <!-- repeated as needed ->
           name="..."      <!-- mandatory, Ada type name -->
           subtype="True"  <!-- optional, if True generate a subtype -->
       />

       <parameter
           name="..."
           ada="..."/>     <!-- Override default naming for all methods.
                                In particular used for "Self" -->

       <method             <!-- repeated as needed -->
           id="..."        <!-- mandatory, name of the C method -->
                           <!-- fields are not bound by default, but are
                                associated with
                                    "gtkada_%s_get_%s" % (adapkg, field_name)
                                methods -->
           ada="..."       <!-- optional, name of the Ada subprogram -->
           bind="true"     <!-- optional, if false no binding generated -->
           obsolescent=".." <!--  Whether this method is obsolete" -->
           into="..."      <!-- optional, name of C class in which to
                                add the bindings -->
           return_as_param="..." <!-- optional, relace return parameter with
                                an out parameter with this name -->
       >
         <parameter        <!-- repeated as needed -->
            name="..."     <!-- mandatory, lower-cased name of param -->
            ada="..."      <!-- optional, name to use in Ada -->
            type="..."     <!-- optional, override Ada type -->
            default="..."  <!-- optional, the default value for the param-->
            empty_maps_to_null="False"  <!--  If true, an empty string is
                           mapped to a null pointer in C, rather than an empty
                           C string -->
         />
       />

       <extra>
          <gir:method>...  <!-- optional, same nodes as in the .gir file -->
          <with_spec pkg="..."/>  <!-- extra with clauses for spec -->
          <with_body pkg="..."/>  <!-- extra with clauses for body -->
          <method>         <!-- optional, pure Ada methods
                           <!-- Code will be put after generated subprograms-->
             <spec>...     <!-- optional, code to insert in spec -->
             <body>...     <!-- optional, code to insert in body -->
          </method>
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

from xml.etree.cElementTree import parse, QName, tostring
from adaformat import AdaType


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

    def get_doc(self):
        """Return the overridden doc for for the package, as a list of
           string. Each string is a paragraph
        """
        if self.node is None:
            return ""

        docnode = self.node.find("doc")
        if docnode is None:
            return ""

        text = docnode.text or ""
        doc = ["<description>\n"]

        for paragraph in text.split("\n\n"):
            doc.append(paragraph)
            doc.append("")

        doc.append("</description>")

        n = docnode.get("screenshot")
        if n is not None:
            doc.append("<screenshot>%s</screenshot>" % n)

        n = docnode.get("group")
        if n is not None:
            doc.append("<group>%s</group>" % n)

        n = docnode.get("testgtk")
        if n is not None:
            doc.append("<testgtk>%s</testgtk>" % n)

        return doc

    def get_method(self, cname):
        if self.node is not None:
            for f in self.node.findall("method"):
                if f.get("id") == cname:
                    return GtkAdaMethod(f, self)
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

    def is_obsolete(self):
        if self.node is not None:
            return self.node.get("obsolescent", "False").lower() == "true"
        return False

    def extra(self):
        if self.node is not None:
            extra = self.node.find("extra")
            if extra is not None:
                return extra.getchildren()
        return None

    def get_default_param_node(self, name):
        if self.node is not None:
            name = name.lower()
            for p in self.node.findall("parameter"):
                if p.get("name") == name:
                    return p
        return None


class GtkAdaMethod(object):
    def __init__(self, node, pkg):
        self.node = node
        self.pkg  = pkg

    def get_param(self, name):
        default = self.pkg.get_default_param_node(name)
        if self.node is not None:
            name = name.lower()
            for p in self.node.findall("parameter"):
                if p.get("name") == name:
                    return GtkAdaParameter(p, default=default)
        return GtkAdaParameter(None, default=default)

    def bind(self, default="true"):
        """Whether to bind"""
        if self.node is not None:
            return self.node.get("bind", default).lower() != "false"
        return default != "false"

    def ada_name(self):
        if self.node is not None:
            return self.node.get("ada", None)
        return None

    def is_obsolete(self):
        if self.node is not None:
            return self.node.get("obsolescent", "False").lower() == "true"
        return False

    def return_as_param(self):
        if self.node is not None:
            return self.node.get("return_as_param", None)
        return None

    def get_doc(self):
        if self.node is not None:
            txt = self.node.findtext("doc")
            if txt:
                doc = []
                for paragraph in txt.split("\n\n"):
                    doc.append(paragraph)
                    doc.append("")
                return doc
        return None


class GtkAdaParameter(object):
    def __init__(self, node, default):
        self.node = node
        self.default = default

    def get_default(self):
        if self.node is not None:
            return self.node.get("default", None)
        return None

    def ada_name(self):
        name = None
        if self.node is not None:
            name = self.node.get("ada", None)
        if name is None and self.default is not None:
            name = self.default.get("ada", None)
        return name

    def get_type(self):
        if self.node is not None:
            t = self.node.get("type", None)
            if t:
                return AdaType(t)
        return None

    def empty_maps_to_null(self):
        if self.node is not None:
            return self.node.get("empty_maps_to_null", "F").lower() == "true"
        return False

class GtkAdaType(object):
    def __init__(self, node):
        self.node = node

    def is_subtype(self):
        if self.node is not None:
            return self.node.get("subtype", "false").lower() == "true"
        return False
