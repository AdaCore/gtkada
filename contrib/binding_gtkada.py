"""
Parses the file binding.xml, which is used to override some aspects of
the automatically generated code.
The syntax of that file is as follows:
    <?xml version="1.0"?>
    <GIR>
       <package />   <!--  repeated as often as needed
    </GIR>

Where the package node is defined as follows:
    <package id="...">       <!-- mandatory -->
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

       <method             <!-- repeated as needed -->
           id="..."        <!-- mandatory, name of the C method -->
           ada="..."       <!-- optional, name of the Ada subprogram -->
           binding="false" <!-- optional, if false no binding generated -->
           into="..."      <!-- optional, name of Ada package in which to
                                add the bindings -->
           return_as_param="..." <!-- optional, relace return parameter with
                                an out parameter with this name -->
       >
         <parameter        <!-- repeated as needed -->
            name="..."     <!-- mandatory, lower-cased name of param -->
            ada="..."      <!-- optional, name to use in Ada -->
            default="..."  <!-- optional, the default value for the param-->
         />
       />

       <extra>
          ...              <!-- optional, same nodes as in the .gir file -->
       </extra>
    </package>
"""

from xml.etree.cElementTree import parse, QName, tostring


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
                    return GtkAdaMethod(f)
        return GtkAdaMethod(None)

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

    def extra(self):
        if self.node is not None:
            extra = self.node.find("extra", None)
            if extra:
                return extra.getchildren()
        return None


class GtkAdaMethod(object):
    def __init__(self, node):
        self.node = node

    def get_param(self, name):
        if self.node is not None:
            name = name.lower()
            for p in self.node.findall("parameter"):
                if p.get("name") == name:
                    return GtkAdaParameter(p)
        return GtkAdaParameter(None)

    def bind(self):
        """Whether to bind"""
        return self.node is None \
            or self.node.get("binding", "true").lower() != "false"

    def ada_name(self):
        if self.node is not None:
            return self.node.get("ada", None)
        return None

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
    def __init__(self, node):
        self.node = node

    def get_default(self):
        if self.node is not None:
            return self.node.get("default", None)
        return None

    def ada_name(self):
        if self.node is not None:
            return self.node.get("ada", None)
        return None

class GtkAdaType(object):
    def __init__(self, node):
        self.node = node

    def is_subtype(self):
        if self.node is not None:
            return self.node.get("subtype", "false").lower() == "true"
        return False
