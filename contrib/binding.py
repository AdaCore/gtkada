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
cidentifier = QName(c_uri, "identifier").text
ggettype = QName(glib_uri, "get-type").text
gsignal = QName(glib_uri, "signal").text
narray = QName(uri, "array").text
ndoc = QName(uri, "doc").text
nmethod = QName(uri, "method").text
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


class GtkAda(object):
    """The overrides hard-coded for GtkAda"""

    def __init__(self, filename):
        self._tree = parse(filename)
        self.root = self._tree.getroot()
        self.packages = dict()

        for node in self.root:
            self.packages[node.get("id")] = node

    def get_doc(self, pkg):
        """Return the overridden doc for for the package, as a list of
           string. Each string is a paragraph
        """
        if pkg not in self.packages:
            return ""

        node = self.packages[pkg]
        docnode = node.find("doc")

        text = docnode.text
        doc = ["<description>\n"]

        for paragraph in docnode.text.split("\n\n"):
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

    def _parameters(self, c):
        """Format the parameters for the node C by looking at the <parameters>
           child.
        """
        if c is None:
            return []

        params = c.find(nparams)
        if params is None:
            return []
        result = []

        for p in params.findall(nparam):
            type = self._get_type(p)

            if type.is_ptr:
                mode = "out"
            else:
                mode = "in"

            doc = p.findtext(ndoc, "")
            if doc:
                doc = '"%s": %s' % (p.get("name"), doc)

            result.append(
                Parameter(name=to_ada_name(p.get("name")),
                          type=type,
                          mode=mode,
                          doc=doc))

        return result

    def _c_plist(self, plist):
        """Converts a list of parameters from Ada to C types.
           This also removes the documentation for the parameters
        """
        result = []
        for p in plist:
            result.append(
                Parameter(
                    name=p.name,
                    mode=p.mode,
                    type=p.type))

        return result;

    def _constructors(self):
        n = QName(uri, "constructor").text
        for c in self.node.findall(n):
            name = c.get("name").title()
            params = self._parameters(c)

            format_params = ", ".join(p.name for p in params)
            if format_params:
                self._subst["internal_params"] = " (%s)" % format_params
                format_params = ", " + format_params
                self._subst["params"] = format_params
            else:
                self._subst["params"] = ""
                self._subst["internal_params"] = ""

            internal = Subprogram(
                name="Internal",
                plist=self._c_plist(params),
                returns="System.Address").import_c(c.get(cidentifier))
            call = internal.call(add_return=False)  # A VariableCall

            initialize_params = [Parameter(
                name="Self",
                type="%(ns)s_%(name)s_Record'Class" % self._subst,
                mode="access")] + params
            initialize = Subprogram(
                name=("Initialize_" + name).replace("_New", ""),
                plist=initialize_params,
                local_vars=call.tmpvars,
                code="%sSet_Object (Self, %s);%s" %
                    (call.precall, call.call, call.postcall)
                ).add_nested(internal)
            call = initialize.call(in_pkg=self.pkg)

            gtk_new = Subprogram(
                name="Gtk_%s" % name,
                plist=[Parameter(
                    name="Self",
                    type="%(ns)s_%(name)s" % self._subst,
                    mode="out")] + params,
                local_vars=call.tmpvars,
                code="Self := new %(ns)s_%(name)s_Record;" % self._subst
                   + call.precall + call.call + call.postcall,
                doc=c.findtext(ndoc, ""))

            self.section.add(gtk_new, initialize)

    def _methods(self):
        n = self.node.findall(nmethod)
        for c in n:
            returns = self._get_type(c.find(nreturn)).as_return()
            params = [
                Parameter(
                    name="Self",
                    type=AdaType(
                        adatype="access %(ns)s_%(name)s_Record" % self._subst,
                        ctype="System.Address",
                        convert="Get_Object (%s)"))
                ] + self._parameters(c)

            internal=Subprogram(
                name="Internal",
                returns=returns,
                plist=self._c_plist(params)).import_c(c.get(cidentifier))

            code = internal.call()  # A VariableCall

            self.section.add(
                Subprogram(
                    name=c.get("name").title(),
                    plist=params,
                    returns=returns,
                    doc=c.findtext(ndoc, ""),
                    local_vars=code.tmpvars,
                    code="%s%s%s" % (code.precall, code.call, code.postcall)
                ).add_nested(internal))

    def _method_get_type(self):
        n = self.node.get(ggettype)
        if n is not None:
            self.pkg.add_with("Glib")
            self.section.add(
                Subprogram(
                    name="Get_Type",
                    returns="Glib.GType")
                .import_c(n))

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
        return CType(name=node.get("name"), cname=node.get(ctype),
                     pkg=self.pkg)

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

                type = self._get_type(p).as_ada_param()
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
        signals = list(self.node.findall(gsignal))
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
                    code="null",
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
        name = "%(ns)s.%(name)s" % self._subst
        self.pkg = Package(name=name, doc=gtkada.get_doc(name))
        self.pkg.add_with("%(ns)s.%(parent)s" % self._subst)

        self.section = self.pkg.section("")

        self.section.add(
            """
type %(ns)s_%(name)s_Record is new %(ns)s_%(parent)s_Record with null record;
type %(ns)s_%(name)s is access all %(ns)s_%(name)s_Record'Class;"""
            % self._subst)

        self._constructors()
        self._method_get_type()
        self._methods()
        self._properties()
        self._signals()

        self.pkg.spec(sys.stdout)
        sys.stdout.write("\n")
        self.pkg.body(sys.stdout)

Package.copyright_header="""-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2011, AdaCore                   --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------
"""

p = GIR(sys.argv[1])
gtkada = GtkAda(sys.argv[2])

cl = p.getClass("Button")
cl.generate()
