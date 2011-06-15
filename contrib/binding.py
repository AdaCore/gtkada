#!/usr/bin/env python

"""Parse a .gir file for any of the gtk+ libraries (gtk+, glib,...)
   and generate Ada bindings.
"""

# Issues:
#   - Missing access to gtk+ enum types
#   - Missing handling of <field> nodes (see GtkArrow for instance)
#   - Missing handling of interfaces
#   - Some comments contain xref like "#GtkMisc". Not sure what to do with
#     those. Likewise for names of subprograms in comments.
#
# Backward incompatibility:
#   - Missing documentation for some properties.
#     SOLVE: we could point to the corresponding Set_* and Get_* subprograms,
#            or simply ignore the missing doc
#
#   - Gtk.Button.Gtk_New used to have a "Label" parameter
#     Users must now use Gtk_New_With_Label.
#     SOLVE: we could have special cases

from xml.etree.cElementTree import parse, QName, tostring
from adaformat import *
from binding_gtkada import GtkAda

uri = "http://www.gtk.org/introspection/core/1.0"
glib_uri = "http://www.gtk.org/introspection/glib/1.0"
c_uri = "http://www.gtk.org/introspection/c/1.0"

namespace = QName(uri, "namespace").text
nvarargs = QName(uri, "varargs").text
ntype = QName(uri, "type").text
ctype = QName(c_uri, "type").text
cidentifier = QName(c_uri, "identifier").text
ggettype = QName(glib_uri, "get-type").text
gsignal = QName(glib_uri, "signal").text
narray = QName(uri, "array").text
ndoc = QName(uri, "doc").text
nmethod = QName(uri, "method").text
nfunction = QName(uri, "function").text
nparam = QName(uri, "parameter").text
nparams = QName(uri, "parameters").text
nreturn = QName(uri, "return-value").text
nfield = QName(uri, "field").text

class GIR(object):
    def __init__(self, filename):
        """Parse filename and initializes SELF"""
        self._tree = parse(filename)
        self.root = self._tree.getroot()
        self.namespace = self.root.find(namespace).get("name")

        self.packages = dict() # Ada name (lower case) -> Package instance
        self.ccode = ""
        self.classes = dict()  # Maps GIR's "name" to a GIRClass instance
        k = "{%(uri)s}namespace/{%(uri)s}class" % {"uri":uri}

        # Register all standard gobject types found in the GIR file
        for cl in self.root.findall(k):
            naming.add_type_exception(
                cname=cl.get(ctype),
                type=GObject(
                    "Gtk.%(name)s.Gtk_%(name)s" % {
                        "name":naming.case(cl.get("name"))}))
            naming.add_girname(girname=cl.get("name"), ctype=cl.get(ctype))
            self.classes[cl.get("name")] = GIRClass(self, cl)

        self.globals = GlobalsBinder(self, self.root) # global vars

    def debug(self, element):
        """A debug form of element"""
        return tostring(element)

    def get_package(self, name):
        """Return a handle to an Ada package"""
        if not name.lower() in self.packages:
            self.packages[name.lower()] = Package(
                name=name,
                doc=gtkada.get_pkg(name).get_doc())
        return self.packages[name.lower()]

    def generate(self, out, cout):
        """Generate Ada code for all packages"""
        for pkg in self.packages.itervalues():
            pkg.spec(out)
            out.write("\n")
            pkg.body(out)
            out.write("\n")

        cout.write(self.ccode)


class GlobalsBinder(object):
    def __init__(self, gir, node):
        self.gir = gir
        self.globals = dict()

        k = "{%(uri)s}namespace/{%(uri)s}function" % {"uri":uri}
        all = node.findall(k)
        if all is not None:
            for c in all:
                id = c.get(cidentifier)
                self.globals[id] = c

    def get_function(self, id):
        """Return the XML node corresponding to a global function"""
        return self.globals[id]


class GIRClass(object):
    """Represents a gtk class"""

    def __init__(self, gir, node):
        self.gir = gir
        self.node = node
        self.ctype = self.node.get(ctype)
        self._subst = dict()  # for substitution in string templates
        self._private = ""
        self._generated = False

    def _parameters(self, c, gtkmethod):
        """Format the parameters for the node C by looking at the <parameters>
           child.
           Returns None if the parameter list could not be parsed.
           gtkmethod is the GtkAdaMethod that contains the overriding for the
           various method attributes.
        """
        if c is None:
            return []

        params = c.find(nparams)
        if params is None:
            return []
        result = []

        for p in params.findall(nparam):
            name = p.get("name")
            gtkparam = gtkmethod.get_param(name=name)
            name = gtkparam.ada_name() or name  # override

            default = gtkparam.get_default()

            type = gtkparam.get_type(pkg=self.pkg) \
                or self._get_type(
                    p,
                    empty_maps_to_null=gtkparam.empty_maps_to_null(),
                    allow_access=not default)

            if type is None:
                return None

            if p.get("direction", "in") == "out":
                mode = "out"
            elif type.is_ptr:
                mode = "out"
            else:
                mode = "in"

            doc = p.findtext(ndoc, "")
            if doc:
                doc = '"%s": %s' % (name, doc)

            result.append(
                Parameter(name=naming.case(name),
                          type=type,
                          mode=mode,
                          default=default,
                          doc=doc))

        return result

    def _c_plist(self, plist, returns, localvars, code):
        """Converts a list of parameters from Ada to C types.
           This also removes the documentation for the parameters
        """
        result = []
        for p in plist:
            n = p.name
            m = p.mode
            t = p.type

            if returns is not None and m != "in":
                m = "access_c"
                n = "Acc_%s" % p.name
                localvars.append(Local_Var(
                    name="Acc_%s" % p.name,
                    aliased=True,
                    type=t))
                code.append("%s := Acc_%s;" % (p.name, p.name))

            result.append(Parameter(name=n, mode=m, type=t))

        return result;

    def _getdoc(self, gtkmethod, node):
        doc = gtkmethod.get_doc(default=node.findtext(ndoc, ""))
        if node.get("version"):
            doc.append("Since: gtk+ %s" % node.get("version"))
        return doc

    def _get_method_returns(self, gtkmethod, node):
        """The method's return type. `node' is the XML node that describes the
           return type in the GIR file"""
        returns = gtkmethod.returned_c_type()
        if returns is None:
            return self._get_type(node, allow_access=False)
        else:
            return CType(name=None, cname=returns, pkg=self.pkg)

    def _handle_function(self, section, c, ismethod=False, gtkmethod=None):
        cname = c.get(cidentifier)

        if gtkmethod is None:
            gtkmethod = self.gtkpkg.get_method(cname=cname)

        if gtkmethod.bind("true"):
            self._handle_function_internal(
                section, node=c, cname=cname,
                gtkmethod=gtkmethod,
                returns=self._get_method_returns(gtkmethod, c.find(nreturn)),
                ismethod=ismethod)

    def _handle_function_internal(self, section, node, cname,
                                  gtkmethod,
                                  returns=None,
                                  adaname=None,
                                  ismethod=False, params=None):
        """Generate a binding for a function.
           This returns None if no binding was made, an instance of Subprogram
           otherwise.
           `adaname' is the name of the generated Ada subprograms. By default,
           it is computed automatically from either binding.xml or the "name"
           attribute of `node'.
           `params' is the list of parameters. If set to None, it is computed
           automatically from `node'.
        """

        adaname = adaname or gtkmethod.ada_name() \
                or node.get("name").title()

        if params is None:
            params = self._parameters(node, gtkmethod)
            if params is None:
                naming.add_cmethod(cname, cname)  # Avoid warning later on.
                print "No binding for %s: varargs" % cname
                return None

        if ismethod:
            if adaname.startswith("Gtk_New"):
                pname = gtkmethod.get_param("param1").ada_name() or "Param1"
                ptype = "%(typename)s" % self._subst
            else:
                pname = gtkmethod.get_param("self").ada_name() or "Self"
                ptype = "access %(typename)s_Record" % self._subst

            params.insert(0 , Parameter(
                name=pname,
                type=AdaType(
                    adatype=ptype,
                    pkg=self.pkg,
                    in_spec=True,
                    ctype="System.Address",
                    convert="Get_Object (%(var)s)")))

        if adaname.startswith("Gtk_New"):
            self._handle_constructor(
                node, gtkmethod=gtkmethod, cname=cname, params=params)
            return

        local_vars = []
        call = []

        body = gtkmethod.get_body()
        if not body:
            plist = self._c_plist(params, returns, local_vars, call)
            internal=Subprogram(
                name="Internal",
                returns=returns,
                plist=plist).import_c(cname)

            ret = gtkmethod.return_as_param()
            execute = internal.call(extra_postcall="".join(call))

            if ret is not None:
                # Ada return value goes into an "out" parameter
                params += [Parameter(name=ret, type=returns, mode="out")]
                returns = None

                assert(execute[1] is not None)  # We must have a C function
                call = "%s%s := %s;" % (execute[0], ret, execute[1])

            else:
                if execute[1]: # A function, with a standard "return"
                    call = "%sreturn %s;" % (execute[0], execute[1])
                else:
                    call = execute[0]

            local_vars += execute[2]

        doc = self._getdoc(gtkmethod, node)
        naming.add_cmethod(cname, "%s.%s" % (self.pkg.name, adaname))

        subp = Subprogram(
                name=adaname,
                plist=params,
                returns=returns,
                doc=doc,
                local_vars=local_vars,
                code=call)

        if not body:
            subp.add_nested(internal)
        else:
            subp.set_body(body)

        depr = node.get("deprecated")
        if depr is not None:
            subp.mark_deprecated(
                "\nDeprecated since %s, %s"
                % (node.get("deprecated-version"), depr))
        elif gtkmethod.is_obsolete():
            subp.mark_deprecated("Deprecated")

        section.add(subp)
        return subp

    def _constructors(self):
        n = QName(uri, "constructor").text
        for c in self.node.findall(n):
            cname = c.get(cidentifier)
            gtkmethod = self.gtkpkg.get_method(cname=cname)
            if not gtkmethod.bind():
                continue

            params = self._parameters(c, gtkmethod)
            if params is None:
                naming.add_cmethod(cname, cname)  # Avoid warning later on.
                print "No binding for %s: varargs" % cname
                continue

            self._handle_constructor(
                c, gtkmethod=gtkmethod, cname=cname, params=params)

    def _handle_constructor(self, c, cname, gtkmethod, params=None):
        section = self.pkg.section("Constructors")
        name = c.get("name").title()

        format_params = ", ".join(p.name for p in params)
        if format_params:
            self._subst["internal_params"] = " (%s)" % format_params
            format_params = ", " + format_params
            self._subst["params"] = format_params
        else:
            self._subst["params"] = ""
            self._subst["internal_params"] = ""

        returns = AdaType("System.Address", pkg=self.pkg, in_spec=False)
        local_vars = []
        code = []
        plist = self._c_plist(params, returns, local_vars, code)
        internal = Subprogram(
            name="Internal",
            plist=plist,
            returns=returns).import_c(cname)

        call = internal.call()
        assert(call[1] is not None)   # A function

        adaname = gtkmethod.ada_name() or "Gtk_%s" % name

        doc = self._getdoc(gtkmethod, c)

        selfname = gtkmethod.get_param("self").ada_name() or "Self"

        initialize_params = [Parameter(
            name=selfname,
            type=AdaType("%(typename)s_Record'Class" % self._subst,
                         pkg=self.pkg, in_spec=True),
            mode="access")] + params
        initialize = Subprogram(
            name=adaname.replace("Gtk_New", "Initialize"),
            plist=initialize_params,
            local_vars=local_vars + call[2],
            doc=doc,
            code="%sSet_Object (%s, %s)" % (call[0], selfname, call[1]),
            ).add_nested(internal)

        call = initialize.call(in_pkg=self.pkg)
        assert(call[1] is None)  # This is a procedure

        naming.add_cmethod(cname, "%s.%s" % (self.pkg.name, adaname))
        gtk_new = Subprogram(
            name=adaname,
            plist=[Parameter(
                name=selfname,
                type=AdaType("%(typename)s" % self._subst,
                             pkg=self.pkg, in_spec=True),
                mode="out")] + params,
            local_vars=call[2],
            code=selfname + " := new %(typename)s_Record;" % self._subst
               + call[0],
            doc=doc)

        section.add(gtk_new, initialize)

    def _methods(self):
        all = self.node.findall(nmethod)
        if all is not None:
            section = self.pkg.section("Methods")
            for c in all:
                self._handle_function(section, c, ismethod=True)

    def _functions(self):
        all = self.node.findall(nfunction)
        if all is not None:
            section = self.pkg.section("Functions")
            for c in all:
                self._handle_function(section, c)

    def _globals(self):
        funcs = self.gtkpkg.get_global_functions() # List of binding.xml nodes
        if funcs:
            section = self.pkg.section("Functions") # Make sure section exists
            for f in funcs:
                c = self.gir.globals.get_function(f.cname()) # Node in gir file
                self._handle_function(section, c, gtkmethod=f)

    def _method_get_type(self):
        """Generate the Get_Type subprogram"""

        n = self.node.get(ggettype)
        if n is not None:
            section = self.pkg.section("Constructors")

            gtkmethod = self.gtkpkg.get_method(cname=n)
            if not gtkmethod.bind():
                return

            self.pkg.add_with("Glib")
            section.add(
                Subprogram(
                    name=gtkmethod.ada_name() or "Get_Type",
                    returns=AdaType("Glib.GType", pkg=self.pkg, in_spec=True))
                .import_c(n))

            if not self.gtktype.is_subtype():
                self.pkg.add_with("Glib.Type_Conversion_Hooks", specs=False);
                section.add_code("""
package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
   (Get_Type'Access, %(typename)s_Record);
pragma Unreferenced (Type_Conversion);""" % self._subst, specs=False)

    def _get_c_type(self, node):
        t = node.find(ntype)
        if t is not None:
            return t.get(ctype)
        return None

    def _get_type(self, node, allow_access=True, empty_maps_to_null=False):
        """Return the type of the node
           `allow_access' should be False if "access Type" parameters should
           not be allowed, and an explicit type is needed instead.
           `empty_maps_to_null': see doc for CType
        """
        t = node.find(ntype)
        if t is not None:
            if t.get("name") == "none":
                return None
            return CType(name=t.get("name"),
                         cname=t.get(ctype), allow_access=allow_access,
                         empty_maps_to_null=empty_maps_to_null,
                         pkg=self.pkg)

        a = node.find(narray)
        if a is not None:
            t = a.find(ntype)
            if a:
                type = t.get(ctype)
                name = t.get("name") or type  # Sometimes name is not set
                if type:
                    type = "array_of_%s" % type

                return CType(name="array_of_%s" % name,
                             cname=type,
                             pkg=self.pkg, isArray=True,
                             empty_maps_to_null=empty_maps_to_null,
                             allow_access=allow_access)

        a = node.find(nvarargs)
        if a is not None:
            # A function with multiple arguments cannot be bound
            # No need for an error message, we will already let the user know
            # that the function is not bound.
            return None

        print "Error: XML Node has unknown type\n", self.gir.debug(node)
        return None

    def _fields(self):
        fields = self.node.findall(nfield)
        if fields:
            section = self.pkg.section("Fields")
            for f in fields:
                if f.get("private", "0") == "1":
                    continue

                name = f.get("name")

                # Getter

                if f.get("readable", "1") != "0":
                    cname = "gtkada_%(cname)s_get_%(name)s" % {
                        "cname":self._subst["cname"], "name":name}
                    gtkmethod = self.gtkpkg.get_method(cname=cname)
                    if gtkmethod.bind(default="false"):
                        returns = self._get_method_returns(gtkmethod, f)

                        func = self._handle_function_internal(
                            section,
                            node=f,
                            cname=cname,
                            adaname="Get_%s" % name,
                            ismethod=True,
                            params=[],
                            gtkmethod=gtkmethod,
                            returns=returns)

                        if func is not None:
                            ctype = self._get_c_type(f)
                            if ctype is None:
                                continue

                            self.gir.ccode += """
%(ctype)s %(cname)s (%(self)s* self) {
    return self->%(name)s;
}
""" % {"ctype":ctype, "cname":cname, "self":self.ctype, "name":name}

                # Setter

                if f.get("writable", "1") != "0":
                    cname = "gtkada_%(cname)s_set_%(name)s" % {
                        "cname":self._subst["cname"], "name":name}
                    gtkmethod = self.gtkpkg.get_method(cname=cname)
                    if gtkmethod.bind("false"):
                        func = self._handle_function_internal(
                            section,
                            node=f,
                            cname=cname,
                            adaname="Set_%s" % name,
                            ismethod=True,
                            gtkmethod=gtkmethod,
                            params=[Parameter("Value", self._get_type(f))])

                        if func is not None:
                            ctype = self._get_c_type(f)
                            if ctype is None:
                                continue

                            self.gir.ccode += """
void %(cname)s (%(self)s* self, %(ctype)s val) {
    self->%(name)s = val;
}
""" % {"ctype":ctype, "cname":cname, "self":self.ctype, "name":name}


    def _properties(self):
        n = QName(uri, "property")

        props = list(self.node.findall(n.text))
        if props is not None:
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

                tp = self._get_type(p)
                ptype = tp.as_property()
                if ptype:
                    pkg = ptype[:ptype.rfind(".")]
                    if pkg:
                        self.pkg.add_with(pkg)
                else:
                    continue

                adaprops.append({
                    "cname": p.get("name"),
                    "name": naming.case(p.get("name")),
                    "flags": "-".join(flags),
                    "doc": p.findtext(ndoc, ""),
                    "pkg": pkg,
                    "ptype": ptype,
                    "type": tp.as_ada_param()})

            adaprops.sort(lambda x,y: x["name"] <> y["name"])

            for p in adaprops:
                section.add_comment("")
                section.add_comment("Name:  %(name)s_Property" % p)
                section.add_comment("Type:  %(type)s" % p)
                section.add_comment("Flags: %(flags)s" % p)
                if p["doc"]:
                    section.add_comment("%s\n" % p["doc"])

            for p in adaprops:
                d = '   %(name)s_Property : constant %(ptype)s' % p
                section.add (d + ";")
                self.pkg.add_private(
                    d + ':=\n     %(pkg)s.Build ("%(cname)s");' % p)

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
                          type="%(typename)s_Record'Class" % self._subst,
                          mode="access",
                          doc="")],
                    code="null",
                    returns=AdaType(s.find(nreturn).find(ntype).get("name"),
                                    pkg=self.pkg))
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
                section.add_code(
                    '   Signal_%s : constant Glib.Signal_Name := "%s";' % (
                    naming.case(s["name"]), s["name"]))

    def generate(self, gir):
        if self._generated:
            return

        typename = naming.full_type(self.ctype)[0]
        self.name = typename[:typename.rfind(".")]    # Only package part
        self._subst["name"] = self.name
        self._subst["typename"] = typename[typename.rfind(".") + 1:]
        self._subst["cname"] = self.ctype

        parent_ctype = naming.ctype_from_girname(self.node.get("parent"))
        parent = naming.full_type(parent_ctype)[0]
        if parent.rfind(".") != -1:
            self._subst["parent_pkg"] = parent[:parent.rfind(".")]
            self._subst["parent"] = parent[parent.rfind(".") + 1:]
        else:
            self._subst["parent_pkg"] = None
            self._subst["parent"] = parent

        self._generated = True

        self.gtkpkg = gtkada.get_pkg(self.name)
        extra = self.gtkpkg.extra()
        if extra:
            self.node.extend(extra)

        into = self.gtkpkg.into()
        if into:
            # Make sure we have already generated the other package, so that
            # its types go first.
            klass = gir.classes[into]
            klass.generate(gir)
            into = klass.name  # from now on, we want the Ada name

            # Override the type exception. For instance, from now on we
            # want to use "Gtk.Box.Gtk_HBox" rather than "Gtk.HBox.Gtk_HBox"
            # which doesn't exist
            typename = "%s.%s" % (into, self._subst["typename"])
            naming.add_type_exception(
                override=True,
                cname=self.ctype,
                type=GObject(typename))

        self.pkg = gir.get_package(into or self.name)

        if self._subst["parent_pkg"]:
            self.pkg.add_with("%(parent_pkg)s" % self._subst)

        self.gtktype = self.gtkpkg.get_type(self._subst["typename"])

        section = self.pkg.section("")

        if self.gtkpkg.is_obsolete():
            section.add_code("pragma Obsolescent;")

        if self.gtktype.is_subtype():
            section.add_code(
            """
subtype %(typename)s_Record is %(parent)s_Record;
subtype %(typename)s is %(parent)s;""" % self._subst);

        else:
            section.add_code("""
type %(typename)s_Record is new %(parent)s_Record with null record;
type %(typename)s is access all %(typename)s_Record'Class;"""
            % self._subst)

        if extra:
            for p in extra:
                if p.tag == "with_spec":
                    self.pkg.add_with(
                        p.get("pkg", "Missing package name in <extra>"),
                        do_use=p.get("use", "true").lower() == "true")
                elif p.tag == "with_body":
                    self.pkg.add_with(
                        p.get("pkg"), specs=False,
                        do_use=p.get("use", "true").lower() == "true")
                elif p.tag == "type":
                    naming.add_type_exception(
                        cname=p.get("ctype"),
                        type=Proxy(p.get("ada"), p.get("properties", None)))
                    section.add_code(p.text)
                elif p.tag == "body":
                    # Go before the body of generated subprograms, so that
                    # we can add type definition
                    section.add_code(p.text, specs=False)

        self._constructors()
        self._method_get_type()
        self._methods()

        if extra:
            s = None
            for p in extra:
                if p.tag == "spec":
                    s = s or self.pkg.section("GtkAda additions")
                    s.add_code(p.text)

        self._functions()
        self._globals()
        self._fields()
        self._properties()
        self._signals()


gir = GIR(sys.argv[1])

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

gir.ccode = \
"""/*********************************************************************
 *               GtkAda - Ada95 binding for Gtk+/Gnome               *
 *                                                                   *
 *   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   *
 *                Copyright (C) 2000-2011, AdaCore                   *
 *                                                                   *
 * This library is free software; you can redistribute it and/or     *
 * modify it under the terms of the GNU General Public               *
 * License as published by the Free Software Foundation; either      *
 * version 2 of the License, or (at your option) any later version.  *
 *                                                                   *
 * This library is distributed in the hope that it will be useful,   *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of    *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU *
 * General Public License for more details.                          *
 *                                                                   *
 * You should have received a copy of the GNU General Public         *
 * License along with this library; if not, write to the             *
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,      *
 * Boston, MA 02111-1307, USA.                                       *
 *                                                                   *
 * As a special exception, if other files instantiate generics from  *
 * this unit, or you link this unit with other files to produce an   *
 * executable, this  unit  does not  by itself cause  the resulting  *
 * executable to be covered by the GNU General Public License. This  *
 * exception does not however invalidate any other reasons why the   *
 * executable file  might be covered by the  GNU Public License.     *
 *********************************************************************

This file is automatically generated from the .gir files
*/
#include <gtk/gtk.h>
"""

gtkada = GtkAda(sys.argv[2])

# Contains "GIR names"
binding = ("AboutDialog", "Arrow", "AspectFrame",
           "Bin", "Box", "Button", "ButtonBox",
           "EventBox",
           "Frame", "Range", "HBox", "HButtonBox", "HPaned", "HScale",
           "Label", "Layout", "List",
           "Misc", "Paned", "Pixmap", "Progress", "Scale",
           "VBox", "VButtonBox", "Viewport", "VPaned",
           "VScale", "VolumeButton",
           # "Entry",   # partially done only
           "AccelGroup",
           "Adjustment",
           "Alignment",
           "Calendar",
           # "ComboBox",  # Needs .gir for gtk 2.24 for backward compatibility
                          # Also needs support for interfaces
           "CheckButton",
           "Curve",
           "Dialog",
           "DrawingArea",
           "Expander",
           "Image",
           "RadioAction",
           "RadioButton",
           "SizeGroup",
           "Statusbar",
           "ToggleButton",
           "Table",
           "Combo",  # Needs HBox, so must come after it
          )

for name in binding:
    gir.classes[name].generate(gir)

out = file("src/generated/tmp.ada", "w")
cout = file("src/genmisc.c", "w")
gir.generate(out, cout)
