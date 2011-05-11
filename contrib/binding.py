#!/usr/bin/env python

"""Parse a .gir file for any of the gtk+ libraries (gtk+, glib,...)
   and generate Ada bindings.
"""

# Issues:
#   - Missing withs, and access to gtk+ enum types
#   - Missing handling of interfaces
#   - No full view of the *_Record types in the private section
#   - Missing body
#   - Some comments contain xref like "#GtkMisc". Not sure what to do with
#     those. Likewise for names of subprograms in comments.
#   - Property types not handled correctly.
#
# Backward incompatibility:
#   - Missing documentation for some properties.
#     SOLVE: we could point to the corresponding Set_* and Get_* subprograms,
#            or simply ignore the missing doc
#
#   - Gtk.Button.Gtk_New used to have a "Label" parameter
#     Users must now use Gtk_New_With_Label.
#     SOLVE: we could have special cases

from xml.etree.cElementTree import parse, QName
from adaformat import *

uri = "http://www.gtk.org/introspection/core/1.0"
glib_uri = "http://www.gtk.org/introspection/glib/1.0"
c_uri = "http://www.gtk.org/introspection/c/1.0"

namespace = QName(uri, "namespace").text
ntype = QName(uri, "type").text
ctype = QName(c_uri, "type").text
narray = QName(uri, "array").text
ndoc = QName(uri, "doc").text
nparam = QName(uri, "parameter").text
nparams = QName(uri, "parameters").text
nreturn = QName(uri, "return-value").text

class GIR(object):
    def __init__(self, filename):
        """Parse filename and initializes SELF"""
        self._tree = parse(filename)
        self.root = self._tree.getroot()
        self.namespace = self.root.find(namespace).get("name")

        self._classes = dict()
        k = "{%(uri)s}namespace/{%(uri)s}class" % {"uri":uri}
        for cl in self.root.findall(k):
            self._classes[cl.get("name")] = GIRClass(self, cl)

    def getClass(self, className):
        """Return the Element corresponding to the given class"""
        return self._classes[className]


def to_ada_name(cname):
    c = cname.replace("-", "_")
    return c.title()


class GIRClass(object):
    """Represents a gtk class"""

    def __init__(self, gir, node):
        self.gir = gir
        self.node = node

        self._subst = {
            "ns": self.gir.namespace,
            "parent": self.node.get("parent"),
            "name": self.node.get("name")}

        self._private = ""

    def _parameters(self, c, isfunc):
        """Format the parameters for the node C by looking at the <parameters>
           child.
        """
        params = c.find(nparams)
        if not params:
            return []
        result = []

        for p in params.findall(nparam):
            type = self._get_type(p)

            if type.is_ptr:
                mode = "out"
            else:
                mode = "in"

            doc = p.find(ndoc)
            if doc is not None:
                doc = '"%s": %s' % (p.get("name"), doc.text)
            else:
                doc = ""

            result.append(
                Parameter(name=to_ada_name(p.get("name")),
                          type=type.as_param(self.pkg),
                          mode=mode,
                          doc=doc))

        return result

    def _format_subprogram(
        self, node, name, plist=[], returns=None, doc=""):
        """print a subprogram declaration"""

        if isinstance(returns, CType):
            returns = returns.as_return(self.pkg)

        sub = Subprogram(name=name,
                         plist=plist + self._parameters(
                             node, isfunc=returns is not None),
                         returns=returns,
                         doc=doc)
        self.section.add (sub)

    def _constructors(self):
        n = QName(uri, "constructor").text
        for c in self.node.findall(n):
            name = c.get("name").title()
            self._format_subprogram(
                c, "Gtk_%s" % name,
                plist=[Parameter(
                    name="Self",
                    type="%(ns)s_%(name)s" % self._subst,
                    mode="out",
                    doc="")],
                doc=c.findtext(ndoc, ""))
            self._format_subprogram(
                c, ("Initialize_%s" % name).replace("_New", ""),
                plist=[Parameter(
                    name="Self",
                    type="%(ns)s_%(name)s_Record'Class" % self._subst,
                    mode="access",
                    doc="")])

    def _methods(self):
        n = self.node.findall(QName(uri, "method").text)
        for c in n:
            self._format_subprogram(
              c, c.get("name").title(),
              plist=[Parameter(
                  name="Self",
                  type="%(ns)s_%(name)s_Record" % self._subst,
                  mode="access",
                  doc="")],
              returns=self._get_type(c.find(nreturn)),
              doc=c.findtext(ndoc, ""))

    def _get_type(self, node):
        """Return the type of the node"""
        t = node.find(ntype)
        if t is not None:
            return self._to_ada_type(t)
        a = node.find(narray)
        if a is not None:
            t = a.find(ntype)
            if a:
                return "array of " + self._to_ada_type(t)
        return "void"

    def _to_ada_type(self, node):
        """Converts a type described in a node into an Ada type"""
        return CType(name=node.get("name"), cname=node.get(ctype))

    def _properties(self):
        n = QName(uri, "property")

        props = list(self.node.findall(n.text))
        if props:
            adaprops = []
            section = self.pkg.section("Properties")
            section.add_comment(
                """The following properties are defined for this widget.
See Glib.Properties for more information on properties)""")

            for p in props:
                flags = []
                if p.get("readable", "1") != "0":
                    flags.append("read")
                if p.get("writable", "1") != "0":
                    flags.append("write")

                type = self._get_type(p).as_param(self.pkg)
                pkg  = "Glib.Properties"

                if type == "UTF8_String":
                    type = "String"
                elif type == "Widget":
                    type = "Object"
                elif type.startswith("Gtk_"):
                    self.pkg.add_with("Gtk.Enums")
                    pkg = "Gtk.Enums"

                adaprops.append({
                    "cname": p.get("name"),
                    "name": to_ada_name(p.get("name")) + "_Property",
                    "flags": "-".join(flags),
                    "doc": p.findtext(ndoc, ""),
                    "pkg": pkg,
                    "type":  type})

            adaprops.sort(lambda x,y: x["name"] <> y["name"])

            for p in adaprops:
                section.add_comment("")
                section.add_comment("Name:  %(name)s" % p)
                section.add_comment("Type:  %(type)s" % p)
                section.add_comment("Flags: %(flags)s" % p)
                if p["doc"]:
                    section.add_comment("%s\n" % p["doc"])

            self.pkg.add_with("Glib.Properties")

            for p in adaprops:
                d = '   %(name)s : constant %(pkg)s.Property_%(type)s' % p
                section.add (d + ";")
                self.pkg.add_private(
                    d + ':=\n     %(pkg)s.Build ("%(name)s");' % p)

    def _signals(self):
        n = QName(glib_uri, "signal").text

        signals = list(self.node.findall(n))
        if signals:
            adasignals = []
            section = self.pkg.section("Signals")
            section.add_comment(
                "The following new signals are defined for this widget:")

            for s in signals:
                sub = Subprogram(
                    name="Handler",
                    plist=[
                      Parameter(
                          name="Self",
                          type="%(ns)s_%(name)s_Record'Class" % self._subst,
                          mode="access",
                          doc="")],
                    returns=s.find(nreturn).find(ntype).get("name"))
                adasignals.append({
                    "name": s.get("name"),
                    "profile": fill_text(sub.spec(), "   --      ", 79, 69),
                    "doc": s.findtext(ndoc, "")})

            adasignals.sort(lambda x,y: x["name"] <> y["name"])

            for s in adasignals:
                section.add_comment("")
                section.add_comment('"%(name)s"' % s)
                section.add_comment(" %(profile)s""" % s)
                if s["doc"]:
                    section.add_comment("  %s""" % s["doc"])

            for s in adasignals:
                section.add(
                    '   Signal_%s : constant Glib.Signal_Name := "%s";' % (
                    to_ada_name(s["name"]), s["name"]))

    def generate(self):
        self.pkg = Package(name="%(ns)s.%(name)s" % self._subst)
        self.pkg.add_with("%(ns)s.%(parent)s" % self._subst)

        self.section = self.pkg.section("")

        self.section.add(
            Type("""   type %(ns)s_%(name)s_Record is
      new %(parent)s.%(ns)s_%(parent)s_Record with null record;"""
                 % self._subst),
            Type("""   type %(ns)s_%(name)s is access all %(ns)s_%(name)s_Record'Class;""" % self._subst))

        self._constructors()
        self._methods()
        self._properties()
        self._signals()

        self.pkg.spec()

p = GIR("/usr/share/gir-1.0/Gtk-2.0.gir")
cl = p.getClass("Button")
cl.generate()
