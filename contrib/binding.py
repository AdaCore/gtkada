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
nimplements = QName(uri, "implements").text

class GIR(object):
    def __init__(self, filename):
        """Parse filename and initializes SELF"""
        self._tree = parse(filename)
        self.root = self._tree.getroot()
        self.namespace = self.root.find(namespace).get("name")

        self.packages = dict() # Ada name (lower case) -> Package instance
        self.ccode = ""
        self.classes = dict()  # Maps GIR's "name" to a GIRClass instance
        self.interfaces = dict() # Maps GIR's "name" to an interface
        self.callbacks = dict()

        k = "{%(uri)s}namespace/{%(uri)s}callback" % {"uri":uri}
        for cl in self.root.findall(k):
            self.callbacks[cl.get("name")] = cl

        k = "{%(uri)s}namespace/{%(uri)s}interface" % {"uri":uri}
        for cl in self.root.findall(k):
            self.interfaces[cl.get("name")] = self._create_class(
                cl, is_interface=True)

        k = "{%(uri)s}namespace/{%(uri)s}class" % {"uri":uri}
        for cl in self.root.findall(k):
            self.classes[cl.get("name")] = self._create_class(
                cl, is_interface=False)

        self.globals = GlobalsBinder(self, self.root) # global vars

    def _create_class(self, node, is_interface):
        n = node.get("name")
        naming.add_type_exception(
            cname=node.get(ctype),
            type=GObject(
                "Gtk.%(name)s.Gtk_%(name)s" % {"name":naming.case(n)}))
        naming.add_girname(girname=n, ctype=node.get(ctype))
        return GIRClass(self, node, is_interface)

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


def _get_type(node, allow_access=True, empty_maps_to_null=False, pkg=None):
    """Return the type of the GIR XML node.
       `allow_access' should be False if "access Type" parameters should
       not be allowed, and an explicit type is needed instead.
       `empty_maps_to_null': see doc for CType.
       `pkg' is used to add with statements, if specified
    """
    t = node.find(ntype)
    if t is not None:
        if t.get("name") == "none":
            return None
        return CType(name=t.get("name"),
                     cname=t.get(ctype), allow_access=allow_access,
                     empty_maps_to_null=empty_maps_to_null,
                     pkg=pkg)

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
                         pkg=pkg, isArray=True,
                         empty_maps_to_null=empty_maps_to_null,
                         allow_access=allow_access)

    a = node.find(nvarargs)
    if a is not None:
        # A function with multiple arguments cannot be bound
        # No need for an error message, we will already let the user know
        # that the function is not bound.
        return None

    print "Error: XML Node has unknown type\n", node
    return None



class SubprogramProfile(object):
    """A class that groups info on the parameters of a function and
       its return type.
    """

    def __init__(self):
        self.params = None  # list of parameters (None if we have varargs)
        self.returns = None # return value (None for a procedure)
        self.returns_doc = "" # documentation for returned value

    @staticmethod
    def parse(node, gtkmethod, pkg=None):
        """Parse the parameter info and return type info from the XML
           GIR node, overriding with binding.xml.
           gtkmethod is the GtkAdaMethod that contains the overriding for the
           various method attributes.
           If pkg is specified, with statements are added as necessary.
        """
        profile = SubprogramProfile()
        profile.params = profile._parameters(node, gtkmethod, pkg=pkg)
        profile.returns = profile._returns(node, gtkmethod, pkg=pkg)
        return profile

    @staticmethod
    def setter(node, pkg=None):
        """Create a new SubprogramProfile for a getter"""
        profile = SubprogramProfile()
        profile.params = [Parameter("Value", _get_type(node, pkg))]
        return profile

    def has_varargs(self):
        return self.params is None

    def direct_c_map(self):
        """Wether all parameters and return value can be mapped directly from
           C to Ada.
        """
        for p in self.params:
            if not p.direct_cmap():
                return False
        return self.returns is None or self.returns.direct_cmap()

    def c_params(self, localvars, code):
        """Returns the list of parameters for an Ada function that would be
           a direct pragma Import. local variables or additional code will
           be added as needed to handle conversions.
        """
        assert(isinstance(localvars, list))
        assert(isinstance(code, list))

        result = []
        for p in self.params:
            n = p.name
            m = p.mode
            t = p.type

            if self.returns is not None and m != "in":
                m = "access_c"
                n = "Acc_%s" % p.name
                localvars.append(Local_Var(
                    name="Acc_%s" % p.name,
                    aliased=True,
                    type=t))
                code.append("%s := Acc_%s;" % (p.name, p.name))

            result.append(Parameter(name=n, mode=m, type=t))

        return result;

    def _parameters(self, c, gtkmethod, pkg):
        """Parse the <parameters> child node of c"""
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

            type = gtkparam.get_type(pkg=pkg) \
                or _get_type(
                    p,
                    empty_maps_to_null=gtkparam.empty_maps_to_null(),
                    allow_access=not default,
                    pkg=pkg)

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

    def _returns(self, node, gtkmethod, pkg):
        """Parse the method's return type"""

        returns = gtkmethod.returned_c_type()
        if returns is None:
            ret = node.find(nreturn)
            if ret is None:
                # For a <field>, the method's return value will be the type
                # of the field itself
                ret = node
            else:
                self.returns_doc = ret.findtext(ndoc, "")
                if self.returns_doc:
                    self.returns_doc = "Returns %s" % self.returns_doc
            return _get_type(ret, allow_access=False, pkg=pkg)
        else:
            return CType(name=None, cname=returns, pkg=pkg)


class GIRClass(object):
    """Represents a gtk class"""

    def __init__(self, gir, node, is_interface):
        self.gir = gir
        self.node = node
        self.ctype = self.node.get(ctype)
        self._subst = dict()  # for substitution in string templates
        self._private = ""
        self._generated = False
        self.implements = dict() # Implemented interfaces
        self.is_interface = is_interface

    def _getdoc(self, gtkmethod, node):
        doc = gtkmethod.get_doc(default=node.findtext(ndoc, ""))
        if node.get("version"):
            doc.append("Since: gtk+ %s" % node.get("version"))
        return doc

    def _handle_function(self, section, c, ismethod=False, gtkmethod=None,
                         showdoc=True):
        cname = c.get(cidentifier)

        if gtkmethod is None:
            gtkmethod = self.gtkpkg.get_method(cname=cname)

        if gtkmethod.bind():
            profile = SubprogramProfile.parse(
                node=c, gtkmethod=gtkmethod, pkg=self.pkg)
            self._handle_function_internal(
                section, node=c, cname=cname,
                gtkmethod=gtkmethod,
                profile=profile,
                showdoc=showdoc,
                ismethod=ismethod)

    def _func_is_direct_import(self, profile):
        """Whether a function with this profile
           should be implemented directly as a pragma Import, rather than
           require its own body.
        """
        return not self.is_gobject and profile.direct_c_map()

    def _add_self_param(self, adaname, gtkmethod, profile, is_import):
        """Add a Self parameter to the list of parameters in profile.
           The exact type of the parameter depends on several criteria.
           'is_import' should be true if the function will be implemented as
           a pragma Import, with no body.
        """

        getobject = "Get_Object (%(var)s)"

        if not self.is_gobject:
            # An interface
            pname = gtkmethod.get_param("self").ada_name() or "Self"
            ptype = "%(typename)s" % self._subst
            ctype = ptype
            getobject = "%(var)s"

        elif adaname.startswith("Gtk_New"):
            # Implement as a constructor, even if the GIR file reported this
            # as a function or method
            pname = gtkmethod.get_param("param1").ada_name() or "Param1"
            ptype = "%(typename)s" % self._subst
            ctype = "System.Address"

        else:
            pname = gtkmethod.get_param("self").ada_name() or "Self"
            ptype = "access %(typename)s_Record" % self._subst
            ctype = "System.Address"

        if is_import:
            ctype = ptype

        profile.params.insert(0, Parameter(
            name=pname,
            type=AdaType(
                adatype=ptype, pkg=self.pkg, in_spec=True, ctype=ctype,
                convert=getobject)))

    def _handle_function_internal(self, section, node, cname,
                                  gtkmethod,
                                  profile=None,
                                  showdoc=True,
                                  adaname=None,
                                  ismethod=False):
        """Generate a binding for a function.
           This returns None if no binding was made, an instance of Subprogram
           otherwise.
           `adaname' is the name of the generated Ada subprograms. By default,
           it is computed automatically from either binding.xml or the "name"
           attribute of `node'.
           `profile' is an instance of SubprogramProfile
        """
        assert(profile is None or isinstance(profile, SubprogramProfile))

        if profile.has_varargs():
            naming.add_cmethod(cname, cname)  # Avoid warning later on.
            print "No binding for %s: varargs" % cname
            return None

        is_import = self._func_is_direct_import(profile)
        adaname = adaname or gtkmethod.ada_name() or node.get("name").title()
        doc = self._getdoc(gtkmethod, node)
        if profile.returns_doc:
            doc.append(profile.returns_doc)

        naming.add_cmethod(cname, "%s.%s" % (self.pkg.name, adaname))

        if ismethod:
            self._add_self_param(
                adaname, gtkmethod, profile, is_import=is_import)

        if adaname.startswith("Gtk_New"):
            # Overrides the GIR file even if it reported a function or method
            self._handle_constructor(
                node, gtkmethod=gtkmethod, cname=cname, profile=profile)
            return

        local_vars = []
        call = []

        body = gtkmethod.get_body()
        if not body and not is_import:
            internal=Subprogram(
                name="Internal",
                returns=profile.returns,
                plist=profile.c_params(local_vars, call)).import_c(cname)
            internal.set_param_lang("c")

            ret = gtkmethod.return_as_param()
            execute = internal.call(extra_postcall="".join(call))

            if ret is not None:
                # Ada return value goes into an "out" parameter
                profile.params.append(
                    Parameter(name=ret, type=profile.returns, mode="out"))
                profile.returns = None

                assert(execute[1] is not None)  # We must have a C function
                call = "%s%s := %s;" % (execute[0], ret, execute[1])

            else:
                if execute[1]: # A function, with a standard "return"
                    call = "%sreturn %s;" % (execute[0], execute[1])
                else:
                    call = execute[0]

            local_vars += execute[2]

        subp = Subprogram(
                name=adaname,
                plist=profile.params,
                returns=profile.returns,
                showdoc=showdoc,
                doc=doc,
                local_vars=local_vars,
                code=call)

        if is_import:
            subp.import_c(cname)
        elif not body:
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

            profile = SubprogramProfile.parse(
                node=c, gtkmethod=gtkmethod, pkg=self.pkg)
            if profile.has_varargs():
                naming.add_cmethod(cname, cname)  # Avoid warning later on.
                print "No binding for %s: varargs" % cname
                continue

            self._handle_constructor(
                c, gtkmethod=gtkmethod, cname=cname, profile=profile)

    def _handle_constructor(self, c, cname, gtkmethod, profile=None):
        assert(profile is None or isinstance(profile, SubprogramProfile))

        section = self.pkg.section("Constructors")
        name = c.get("name").title()

        format_params = ", ".join(p.name for p in profile.params)
        if format_params:
            self._subst["internal_params"] = " (%s)" % format_params
            format_params = ", " + format_params
            self._subst["params"] = format_params
        else:
            self._subst["params"] = ""
            self._subst["internal_params"] = ""

        profile.returns = AdaType(
            "System.Address", pkg=self.pkg, in_spec=False)
        local_vars = []
        code = []
        internal = Subprogram(
            name="Internal",
            plist=profile.c_params(local_vars, code),
            returns=profile.returns).import_c(cname)
        internal.set_param_lang("c")

        call = internal.call()
        assert(call[1] is not None)   # A function

        adaname = gtkmethod.ada_name() or "Gtk_%s" % name
        doc = self._getdoc(gtkmethod, c)
        selfname = gtkmethod.get_param("self").ada_name() or "Self"

        if self.is_gobject:
            selftype = "%(typename)s_Record'Class" % self._subst
        else:
            selftype = "%(typename)s" % self._subst

        initialize_params = [Parameter(
            name=selfname,
            type=AdaType(selftype, pkg=self.pkg, in_spec=True),
            mode="access")] + profile.params
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
                mode="out")] + profile.params,
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

            if not self.gtktype.is_subtype() and not self.is_interface:
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
                        profile = SubprogramProfile.parse(
                            node=f, gtkmethod=gtkmethod, pkg=self.pkg)
                        func = self._handle_function_internal(
                            section,
                            node=f,
                            cname=cname,
                            adaname="Get_%s" % name,
                            ismethod=True,
                            profile=profile,
                            gtkmethod=gtkmethod)

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
                        profile = SubprogramProfile.setter(
                            node=f, pkg=self.pkg)
                        func = self._handle_function_internal(
                            section,
                            node=f,
                            cname=cname,
                            adaname="Set_%s" % name,
                            ismethod=True,
                            gtkmethod=gtkmethod,
                            profile=profile)

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

                tp = _get_type(p, pkg=self.pkg)
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
                    d + ' :=\n     %(pkg)s.Build ("%(cname)s");' % p)

    def _signals(self):
        signals = list(self.node.findall(gsignal))
        if signals:
            adasignals = []
            section = self.pkg.section("Signals")
            section.add_comment(
                "The following new signals are defined for this widget:")

            for s in signals:
                gtkmethod = self.gtkpkg.get_method(
                    cname="%s::%s" % (self.name, s.get("name")))
                profile = SubprogramProfile.parse(
                    node=s, gtkmethod=gtkmethod, pkg=None)

                if self.is_gobject:
                    selftype = "%(typename)s_Record'Class" % self._subst
                else:
                    selftype = "%(typename)s" % self._subst

                sub = Subprogram(
                    name="Handler",
                    plist=[
                      Parameter(
                          name="Self", type=selftype, mode="access", doc="")]
                      + profile.params,
                    code="null",
                    returns=profile.returns)

                spec = sub.spec(maxlen=69)
                doc = s.findtext(ndoc, "")
                if profile.returns_doc:
                    doc += "\n\n%s" % profile.returns_doc

                adasignals.append({
                    "name": s.get("name"),
                    "profile": spec,
                    "doc": doc})

            adasignals.sort(lambda x,y: x["name"] <> y["name"])

            for s in adasignals:
                section.add_comment("")
                section.add_comment('"%(name)s"' % s)
                section.add_comment(" %(profile)s""" % s, fill=False)
                if s["doc"]:
                    section.add_comment("  %s""" % s["doc"])

            for s in adasignals:
                section.add_code(
                    '   Signal_%s : constant Glib.Signal_Name := "%s";' % (
                    naming.case(s["name"]), s["name"]))

    def _implements(self):
        """Bind the interfaces that a class implements"""

        implements = list(self.node.findall(nimplements))
        if not implements:
            return

        for impl in implements:
            name = impl.get("name")
            if name in ("Atk.ImplementorIface",):
                continue

            type = naming.full_type_from_girname(girname=name)
            if "." in type.ada:
                self.pkg.add_with(type.ada[:type.ada.rfind(".")])
                self.pkg.add_with("Glib.Types")

            impl = dict(
                name=name,
                impl=type.ada,
                interface=self.gir.interfaces[name],
                pkg="%(typename)s" % self._subst)
            impl["code"] = \
                """package Implements_%(name)s is new Glib.Types.Implements
       (%(impl)s, %(pkg)s_Record, %(pkg)s);
   function "+"
      (Widget : access %(pkg)s_Record'Class)
      return %(impl)s
      renames Implements_%(name)s.To_Interface;
   function "-"
      (Interf : %(impl)s)
      return %(pkg)s
      renames Implements_%(name)s.To_Object;
""" % impl

            self.implements[name] = impl

        if self.implements:

            # Duplicate the subprograms from the interfaces. This doesn't
            # quite work: for instance, Gtk.About_Dialog already has a
            # Get_Name, so we can't redefine the one inherited from Buildable.
            section = self.pkg.section("Interfaces_Impl")
            for impl in sorted(self.implements.iterkeys()):
                impl = self.implements[impl]
                if impl["name"] == "Buildable":
                    # Do not repeat for buildable, that's rarely used
                    continue

                interf = impl["interface"]

                # Ignore interfaces that we haven't bound
                if not hasattr(interf, "gtkpkg"):
                    print "%s: methods for interface %s were not bound" % (
                        self.name, impl["name"])
                else:
                    all = interf.node.findall(nmethod)
                    for c in all:
                        cname = c.get(cidentifier)
                        gtkmethod = interf.gtkpkg.get_method(cname)
                        self._handle_function(
                            section, c, showdoc=False, gtkmethod=gtkmethod,
                            ismethod=True)

            section = self.pkg.section("Interfaces")
            section.add_comment(
                "This class implements several interfaces. See Glib.Types")

            for impl in sorted(self.implements.iterkeys()):
                impl = self.implements[impl]
                section.add_comment("")
                section.add_comment('- "%(name)s"' % impl)
                section.add_code(impl["code"])


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

        self.is_gobject = True

        if self.is_interface:
            self.pkg.add_with("Glib.Types")
            section.add_code(
"type %(typename)s is new Glib.Types.GType_Interface;" % self._subst)
            self.is_gobject = False

        elif self.gtktype.is_subtype():
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

        if not into:
            self._implements()
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

interfaces = ("Activatable",
              "Buildable",
              "CellEditable",
              #"CellLayout",
              #"FileChooser",
              #"RecentChooser",
              "Orientable",
              #"TreeSortable"
             )

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
           "CheckButton",
           # "ComboBox",  # Needs .gir for gtk 2.24 for backward compatibility
                          # Also needs support for interfaces
           "Curve",
           "Dialog",
           "DrawingArea",
           "EntryCompletion",
           "Expander",
           "Fixed",
           "Image",
           "RadioAction",
           "RadioButton",
           "Ruler", "VRuler", "HRuler",
           "Separator", "VSeparator", "HSeparator",
           "SeparatorMenuItem",
           "SeparatorToolItem",
           "SizeGroup",
           "Spinner",
           "Statusbar",
           "ToggleButton",
           "Table",
           "Combo",  # Needs HBox, so must come after it
          )

for name in interfaces:
    gir.interfaces[name].generate(gir)

for name in binding:
    gir.classes[name].generate(gir)

out = file("src/generated/tmp.ada", "w")
cout = file("src/misc_generated.c", "w")
gir.generate(out, cout)
