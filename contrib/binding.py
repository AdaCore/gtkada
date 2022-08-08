#!/usr/bin/env python2

"""
Parse a .gir file for any of the gtk+ libraries (gtk+, glib,...)
and generate Ada bindings.
"""

# Issues:
#   - Missing handling of <field> nodes (see GtkArrow for instance)
# - Some comments contain xref like "#GtkMisc". Not sure what to do with
#     those. Likewise for names of subprograms in comments.
#
# Backward incompatibility:
#   - Missing documentation for some properties.
#     SOLVE: we could point to the corresponding Set_* and Get_* subprograms,
#            or simply ignore the missing doc

from xml.etree.cElementTree import parse, Element, QName, tostring, fromstring
from adaformat import *
import copy
from binding_gtkada import GtkAda
from data import enums, interfaces, binding, user_data_params
from data import destroy_data_params
import sys

# Unfortunately, generating the slot marshallers in a separate package
# does not work since we end up with circularities in a number of
# cases. For now, we simply duplicate them as needed
SHARED_SLOT_MARSHALLERS = False

# For parsing command line options
from optparse import OptionParser

# Python interpreter version check: this script does not work with Python
# version 3.7 or earlier!
from sys import version_info
version_string = '.'.join(map(str, version_info[0:3]))
if version_info[0] < 3:
    print(('Need at least Python 3.7, got version ' + version_string))
    quit(1)
if version_info[0] == 3 and version_info[1] < 7:
    print(('Need at least Python 3.7, got version ' + version_string))
    quit(1)

uri = "http://www.gtk.org/introspection/core/1.0"
glib_uri = "http://www.gtk.org/introspection/glib/1.0"
c_uri = "http://www.gtk.org/introspection/c/1.0"

cidentifier = QName(c_uri, "identifier").text
cidentifier_prefix = QName(c_uri, "identifier-prefixes").text
ctype_qname = QName(c_uri, "type").text
ggettype = QName(glib_uri, "get-type").text
gsignal = QName(glib_uri, "signal").text
glib_type_struct = QName(glib_uri, "type-struct").text
glib_type_name = QName(glib_uri, "type-name").text
namespace = QName(uri, "namespace").text
narray = QName(uri, "array").text
nbitfield = QName(uri, "bitfield").text
ncallback = QName(uri, "callback").text
nclass = QName(uri, "class").text
ndoc = QName(uri, "doc").text
nenumeration = QName(uri, "enumeration").text
nfield = QName(uri, "field").text
nfunction = QName(uri, "function").text
nimplements = QName(uri, "implements").text
ninterface = QName(uri, "interface").text
nmember = QName(uri, "member").text
nmethod = QName(uri, "method").text
nvirtualmethod = QName(uri, "virtual-method").text
nparam = QName(uri, "parameter").text
nparams = QName(uri, "parameters").text
nrecord = QName(uri, "record").text
nunion = QName(uri, "union").text
nreturn = QName(uri, "return-value").text
ntype = QName(uri, "type").text
nvalue = QName(uri, "value").text
nvarargs = QName(uri, "varargs").text
nconstant = QName(uri, "constant").text
ninstanceparam = QName(uri, "instance-parameter").text


class GIR(object):

    def __init__(self, files):
        """Parse filename and initializes SELF"""

        self.packages = dict()  # Ada name (lower case) -> Package instance
        self.ccode = ""
        self.classes = dict()  # Maps C name to a GIRClass instance
        self.interfaces = dict()  # Maps GIR's "name" to an interface
        self.ctype_interfaces = dict()  # Maps GIR's c:type to an interface
        self.callbacks = dict()  # Ada name to GIR XML node
        self.enums = dict()  # Maps C "name" to a GIR XML node
        self.globals = GlobalsBinder(self)  # global vars
        self.records = dict()  # Maps C "name" to a GIR XML node
        # Maps C "name" to a GIR XML node for constants
        self.constants = dict()

        self.bound = set()  # C names for the entities that have an Ada binding

        # The marshallers that have been generated when we use a slot object.
        # These can be shared among all packages, since the profiles of the
        # handlers are the same.
        if SHARED_SLOT_MARSHALLERS:
            self.slot_marshallers = set()
            self.slot_marshaller_pkg = self.get_package(
                name="Gtkada.Marshallers",
                ctype=None,
                doc="Automatically generated, used internally by GtkAda")
            self.slot_marshaller_section = self.slot_marshaller_pkg.section("")

        for filename in files:
            _tree = parse(filename)
            root = _tree.getroot()

            identifier_prefix = root.find(namespace).get(cidentifier_prefix)

            k = "%s/%s" % (namespace, ncallback)
            for cl in root.findall(k):
                ct = cl.get(ctype_qname)
                type = Callback(naming.case(ct))
                naming.add_type_exception(cname=ct, type=type)
                ct = naming.type(ct).ada
                self.callbacks[ct] = cl

            k = "%s/%s" % (namespace, ninterface)
            for cl in root.findall(k):
                self.ctype_interfaces[cl.get(ctype_qname)] = \
                    self.interfaces[cl.get("name")] = \
                    self._create_class(
                    root, cl, is_interface=True, is_gobject=False,
                    identifier_prefix=identifier_prefix)

            k = "%s/%s" % (namespace, nclass)
            for cl in root.findall(k):
                if cl.get(ctype_qname) is not None:
                    self.classes[cl.get(ctype_qname)] = self._create_class(
                        root, cl, is_interface=False,
                        identifier_prefix=identifier_prefix)

            k = "%s/%s" % (namespace, nconstant)
            for cl in root.findall(k):
                self.constants[cl.get(ctype_qname)] = cl

            # Some <record> are defined with methods. They are bound the same
            # way in GtkAda, except that they do not derive from GObject

            k = "%s/%s" % (namespace, nrecord)
            for cl in root.findall(k):
                if cl.findall(nmethod):
                    self.classes[cl.get(ctype_qname)] = self._create_class(
                        root, cl, is_interface=False, is_gobject=False,
                        identifier_prefix=identifier_prefix)
                self.records[cl.get(ctype_qname)] = cl

            k = "%s/%s" % (namespace, nunion)
            for cl in root.findall(k):
                if cl.findall(nmethod):
                    self.classes[cl.get(ctype_qname)] = self._create_class(
                        root, cl, is_interface=False, is_gobject=False,
                        identifier_prefix=identifier_prefix)
                self.records[cl.get(ctype_qname)] = cl

            for enums in (nenumeration, nbitfield):
                k = "%s/%s" % (namespace, enums)
                for cl in root.findall(k):
                    self.enums[cl.get(ctype_qname)] = cl

            self.globals.add(root)

    def show_unbound(self):
        """Display the list of entities known in the GIR files, but that have
           no Ada binding.
        """

        print("Missing bindings:")
        count = 0
        for name in sorted(gir.interfaces.keys()):
            if name not in self.bound:
                sys.stdout.write("%-28s" % (name + "(intf)", ))
                count += 1
                if (count % 4) == 0:
                    sys.stdout.write("\n")

        for name in sorted(gir.classes.keys()):
            if name not in self.bound:
                sys.stdout.write("%-28s" % name)
                # print '    "--%s", # Not tested yet, from Gio' % name
                count += 1
                if (count % 4) == 0:
                    sys.stdout.write("\n")

        print()

    def _get_class_node(self, rootNode, girname):
        """Find the <class> node in the same XML document as node that matches
           [girname].
        """
        k = "{%(uri)s}namespace/{%(uri)s}class" % {"uri": uri}
        for cl in rootNode.findall(k):
            if cl.get("name") == girname:
                return cl
        return None

    def _create_class(self, rootNode, node, is_interface,
                      identifier_prefix, is_gobject=True,
                      has_toplevel_type=True):
        return GIRClass(self, rootNode=rootNode, node=node,
                        is_interface=is_interface,
                        is_gobject=is_gobject,
                        identifier_prefix=identifier_prefix,
                        has_toplevel_type=has_toplevel_type)

    def debug(self, element):
        """A debug form of element"""
        return tostring(element)

    def get_package(self, name, ctype, doc=""):
        """Return a handle to an Ada package"""
        if not name.lower() in self.packages:
            pkg = self.packages[name.lower()] = Package(
                name=name,
                doc=gtkada.get_pkg(ctype).get_doc())
        else:
            pkg = self.packages[name.lower()]

        if doc:
            pkg.doc = ["<description>", doc, "</description>"] + pkg.doc

        return pkg

    def generate(self, out, cout):
        """Generate Ada code for all packages"""
        for pkg in self.packages.values():
            out.write(pkg.spec().encode('UTF-8'))
            out.write(b"\n")
            out.write(pkg.body().encode('UTF-8'))
            out.write(b"\n")

        cout.write(self.ccode.encode("UTF-8"))


class GlobalsBinder(object):

    def __init__(self, gir):
        self.gir = gir
        self.globals = dict()

    def add(self, node):
        k = "{%(uri)s}namespace/{%(uri)s}function" % {"uri": uri}
        all = node.findall(k)
        if all is not None:
            for c in all:
                id = c.get(cidentifier)
                self.globals[id] = c

    def get_function(self, id):
        """Return the XML node corresponding to a global function"""
        return self.globals[id]


def _get_clean_doc(node):
    """
    Get the <doc> child node, and replace common unicode characters by their
    ASCII equivalent to keep the Ada specs more readable in a terminal.
    """

    doc = node.findtext(ndoc, "")
    if doc:
        doc = doc.replace("\u2019", "'").replace(
            "\u201c", '"').replace("\u201d", '"')
    return doc


def _get_type(nodeOrType, allow_access=True, allow_none=False,
              transfer_ownership=False, userecord=True, pkg=None):
    """Return the type of the GIR XML node.
       nodeOrType can be one of:
          * a string, given the name of the C type
          * an instance of CType, which is returned as is.
          * An XML node from which the information is gathered.

       `allow_access' should be False if "access Type" parameters should
       not be allowed, and an explicit type is needed instead.
       `allow_none': see doc for CType.
       `pkg' is used to add with statements, if specified
    """
    if isinstance(nodeOrType, CType):
        return nodeOrType

    elif isinstance(nodeOrType, str):
        return naming.type(name=nodeOrType,
                           cname=nodeOrType,
                           userecord=userecord,
                           transfer_ownership=transfer_ownership,
                           allow_access=allow_access,
                           allow_none=allow_none, pkg=pkg)

    else:
        t = nodeOrType.find(ntype)
        if t is not None:
            if t.get("name") == "none":
                return None

            ctype_name = t.get(ctype_qname)
            if ctype_name:
                ctype_name = ctype_name.replace("const ", "")
            return naming.type(name=t.get("name"),
                               cname=ctype_name,
                               userecord=userecord,
                               transfer_ownership=transfer_ownership,
                               allow_access=allow_access,
                               allow_none=allow_none, pkg=pkg)

        a = nodeOrType.find(narray)
        if a is not None:
            t = a.find(ntype)
            if a:
                type = t.get(ctype_qname)
                name = t.get("name") or type  # Sometimes name is not set

                size = a.get("fixed-size", None)

                if type:
                    type = "array_of_%s" % (type, )

                return naming.type(name="array_of_%s" % name,
                                   cname=type,
                                   pkg=pkg, isArray=True,
                                   array_fixed_size=size,
                                   transfer_ownership=transfer_ownership,
                                   allow_none=allow_none,
                                   userecord=userecord,
                                   allow_access=allow_access)

        a = nodeOrType.find(nvarargs)
        if a is not None:
            # A function with multiple arguments cannot be bound
            # No need for an error message, we will already let the user know
            # that the function is not bound.
            return None

    print("Error: XML Node has unknown type: %s (%s)" % (nodeOrType, nodeOrType.attrib))
    return None


class SubprogramProfile(object):

    """A class that groups info on the parameters of a function and
       its return type.
    """

    def __init__(self):
        self.node = None    # the XML node for this profile
        self.gtkmethod = None
        self.params = None  # list of parameters (None if we have varargs)
        self.returns = None  # return value (None for a procedure)
        self.returns_doc = ""  # documentation for returned value
        self.doc = ""       # documentation for the subprogram

        # The following fields are used to handle callback parameters
        # and generate an Ada generic

        self.callback_param = []  # indexes of the callback parameter
        self.user_data_param = -1  # index of the "user data" parameter
        self.destroy_param = -1   # index of the parameter to destroy data

    def __repr__(self):
        return "<SubprogramProfile %s>" % self.node.get('name')

    @staticmethod
    def parse(node, gtkmethod, pkg=None, ignore_return=False):
        """Parse the parameter info and return type info from the XML
           GIR node, overriding with binding.xml.
           gtkmethod is the GtkAdaMethod that contains the overriding for the
           various method attributes.
           If pkg is specified, with statements are added as necessary.

           If ignore_return is True, the return type is not parsed. This is
           used for constructors, so that we do not end up adding extra 'with'
           statements in the generated package.
        """
        profile = SubprogramProfile()
        profile.node = node
        profile.gtkmethod = gtkmethod

        # make sure to init the 'returns' field before the parameters, to be
        # able to correctly set the parameters direction ('in out' or 'out'
        # case)
        if not ignore_return:
            profile.returns = profile._returns(node, gtkmethod, pkg=pkg)

        profile.params = profile._parameters(node, gtkmethod, pkg=pkg)

        profile.doc = profile._getdoc(gtkmethod, node)
        return profile

    @staticmethod
    def setter(node, pkg=None):
        """Create a new SubprogramProfile for a getter"""
        profile = SubprogramProfile()
        profile.node = node
        profile.params = [Parameter("Value", _get_type(node, pkg))]
        return profile

    def callback_param_info(self):
        """If there is one or more callback parameters in this profile, return
           them so that we can generate the appropriate function.  Returns None
           if there are no such parameters.
        """
        if not self.callback_param:
            return None
        return [self.params[p] for p in self.callback_param]

    def callback_destroy(self):
        if self.destroy_param < 0:
            return None
        return self.params[self.destroy_param]

    def callback_user_data(self):
        """Returns the name of the "user_data" parameter"""

        if self.user_data_param == -1:
            return None
        return self.params[self.user_data_param].name

    def has_varargs(self):
        return self.params is None

    def direct_c_map(self):
        """Wether all parameters and return value can be mapped directly from
           C to Ada.
        """
        for p in self.params:
            # If a parameter is not mapped in Ada, we need an actual body
            if not p.direct_c_map() or not p.ada_binding:
                return False
        return self.returns is None or self.returns.direct_c_map()

    def c_params(self, localvars, code):
        """Returns the list of parameters for an Ada function that would be
           a direct pragma Import. local variables or additional code will
           be extended as needed to handle conversions.
        """
        assert(isinstance(localvars, list))
        assert(isinstance(code, list))

        result = []
        for p in self.params:
            n = p.name
            is_temporary = False

            # Restore original mode only for arrays now
            # because generator is not adopted for c_mode = p.c_mod and
            # generates incorrect code for example for instance parameters
            c_mode = p.mode
            if p.type.isArray and p.c_mode == "out" \
               and p.is_caller_allocates:
                # C expects an allocated array like for "in out" mode
                # so we will pass an address of buffer's first element
                # and do not expect allocation in C
                c_mode = "in"

            # Pass the array as is, without creating temporary variable
            as_array = p.type.isArray and p.is_caller_allocates \
                and p.c_mode in ("in", "in out", "out")

            if self.returns is not None and p.mode != "in" and p.ada_binding \
               and as_array is False:
                n = "Acc_%s" % p.name
                var = Local_Var(
                    name=n,
                    aliased=True,
                    default="" if p.mode != "in out" else p.name,
                    type=p.type)
                var.type.userecord = False
                localvars.append(var)

                if p.mode == "access":
                    if p.type.allow_none:
                        code.append(
                            "if %s /= null then %s.all := %s; end if;"
                            % (p.name, p.name, var.name))
                    else:
                        code.append("%s.all := %s;" % (p.name, var.name))
                else:
                    is_temporary = p.mode != "out"
                    code.append("%s := %s;" % (p.name, var.name))

            # If we do not bind the parameter in the Ada profile, we will need
            # to substitute its default value instead. But we don't want to
            # systematically put the default value, which is in Ada. We would
            # end up with Interfaces.C.Strings.chars_ptr=""

            result.append(Parameter(
                name=n, mode=c_mode, type=p.type,
                for_function=self.returns is not None,
                default=p.default if not p.ada_binding else None,
                is_temporary_variable=is_temporary,
                ada_binding=p.ada_binding,
                c_mode=p.c_mode,
                ownership=p.ownership,
                is_caller_allocates=p.is_caller_allocates))

        return result

    def set_class_wide(self):
        """This profile is not for a primitive operation, but for a class-wide
           operation.
        """
        if isinstance(self.params[0].type, GObject):
            self.params[0].type.classwide = True

    def add_param(self, pos, param):
        """Add a new parameter in the list, at the given position"""
        self.params.insert(pos, param)
        if self.callback_param:
            self.callback_param = [p + 1 if p >= pos else p
                                   for p in self.callback_param]
        if self.user_data_param >= 0 and self.user_data_param >= pos:
            self.user_data_param += 1
        if self.destroy_param >= 0 and self.destroy_param >= pos:
            self.destroy_param += 1

    def replace_param(self, name_or_index, type):
        """Overrides the type of a parameter"""
        if name_or_index is None:
            return

        if isinstance(name_or_index, int):
            self.params[name_or_index].set_type(type)
        else:
            for idx, p in enumerate(self.params):
                if p.name.lower() == name_or_index.lower():
                    self.params[idx].set_type(type)
                    return

    def remove_param(self, names):
        """Remove the parameter with the given names from the list"""
        assert(isinstance(names, list))

        for n in names:
            if n is not None:
                n = n.lower()
                for p in self.params:
                    if p.name.lower() == n:
                        self.params.remove(p)
                        break

    def find_param(self, names):
        """Return the first name for which there is a parameter"""
        for n in names:
            lo = n.lower()
            for p in self.params:
                if p.name.lower() == lo:
                    return n
        return None

    def unset_default_values(self):
        """Remove the default values for the parameters"""
        for p in self.params:
            p.default = None

    def subprogram(self, name, showdoc=True, local_vars=[], code=[],
                   convention=None, lang="ada"):
        """Return an instance of Subprogram with the corresponding profile.
           lang is one of "ada", "c->ada" or "ada->c".
        """

        params = self.params
        if lang == "ada":
            params = [p for p in self.params if p.ada_binding]

        subp = Subprogram(
            name=name,
            plist=params,
            returns=self.returns,
            showdoc=showdoc,
            doc=self.doc,
            lang=lang,
            local_vars=local_vars,
            code=code)
        subp.convention = convention or self.gtkmethod.convention()

        if name != "":
            depr = self.node.get("deprecated")
            if depr is not None:
                subp.mark_deprecated(
                    "\nDeprecated since %s, %s"
                    % (self.node.get("deprecated-version"), depr))
            elif self.gtkmethod and self.gtkmethod.is_obsolete():
                subp.mark_deprecated("Deprecated")

        return subp

    def _getdoc(self, gtkmethod, node):
        doc = gtkmethod.get_doc(default=_get_clean_doc(node))
        if node.get("version"):
            doc.append("Since: gtk+ %s" % node.get("version"))
        return doc

    def _parameters(self, c, gtkmethod, pkg):
        """Parse the <parameters> child node of c"""
        if c is None:
            return []

        params = c.find(nparams)
        if params is None:
            return []

        # Check whether we'll bind to a procedure or to a function
        is_function = self.returns and not gtkmethod.return_as_param()

        result = []

        for p_index, p in enumerate(params.findall(nparam)):
            name = p.get("name")
            if name == "...":
                name = "varargs"

            gtkparam = gtkmethod.get_param(name=name)
            adan = gtkparam.ada_name()

            ada_binding = adan is None or adan != ""
            name = adan or name  # override default computed name

            default = gtkparam.get_default()
            allow_access = not default
            allow_none = gtkparam.allow_none(girnode=p) or default == 'null'
            nodeOrType = gtkparam.get_type(pkg=pkg) or p

            type = _get_type(
                nodeOrType=nodeOrType,
                allow_none=allow_none,
                userecord=default != 'null',
                allow_access=allow_access,
                pkg=pkg)

            if type is None:
                if nodeOrType.find(nvarargs) is not None:
                    type = gtkmethod.get_param("varargs").get_type(pkg=pkg)
                else:
                    type = gtkmethod.get_param(name).get_type(pkg=pkg)
                if type is None:
                    return None
                type = _get_type(type)

            if not default and allow_none and isinstance(type, UTF8):
                default = '""'

            if (p.get("scope", "") in ("notified", "call", "async")
                    or p.get("closure", "") != ""):

                # "async" means a callback with no closure. As a special case,
                # we ignore it for destroy callbacks, since they are already
                # handled specially.

                if p.get("scope") != "async" \
                        or type.ada != "Glib.G_Destroy_Notify_Address":
                    self.callback_param.append(p_index)
                    self.user_data_param = int(p.get("closure", "-1"))
                    self.destroy_param = int(p.get("destroy", "-1")) - 1

            direction = gtkparam.get_direction() or p.get("direction", "in")
            assert direction in ("in", "out", "inout", "access"), \
                "Invalid value for direction: '%s'" % direction

            is_allocated = (gtkparam.get_caller_allocates() or
                            p.get("caller-allocates", None)) == "1"

            ownership = (gtkparam.get_transfer_ownership() or
                         p.get("transfer-ownership", "none")) == "full"

            if direction == "inout":
                c_mode = "in out"
            elif direction in ("out", "access"):
                c_mode = direction
            elif type.is_ptr:
                c_mode = "in out"
            else:
                c_mode = "in"

            if is_function and direction not in ("in", "access"):
                mode = "access"
            else:
                mode = c_mode

            doc = _get_clean_doc(p)
            if doc:
                doc = '"%s": %s' % (name, doc)

            result.append(
                Parameter(name=naming.case(name),
                          type=type,
                          mode=mode,
                          default=default,
                          ada_binding=ada_binding,
                          doc=doc,
                          c_mode=c_mode,
                          ownership=ownership,
                          is_caller_allocates=is_allocated))

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
                self.returns_doc = _get_clean_doc(ret)
                if self.returns_doc:
                    self.returns_doc = "Returns %s" % self.returns_doc

            return _get_type(
                ret, allow_access=False, pkg=pkg,
                transfer_ownership=gtkmethod.transfer_ownership(ret))
        else:
            return naming.type(name=None, cname=returns, pkg=pkg)


class GIRClass(object):

    """Represents a gtk class"""

    def __init__(self, gir, rootNode, node, identifier_prefix,
                 is_interface=False, is_gobject=True, has_toplevel_type=True):
        """If has_toplevel_type is False, no widget type is generated"""

        self.gir = gir
        self.node = node
        self.rootNode = rootNode
        self.ctype = self.node.get(ctype_qname)
        if not self.ctype:
            print("no c:type defined for %s" % (self.node.get(glib_type_name, )))
            return

        self._private = ""
        self._generated = False
        self.identifier_prefix = identifier_prefix
        self.implements = dict()  # Implemented interfaces
        self.is_gobject = is_gobject
        self.is_interface = is_interface
        self.has_toplevel_type = has_toplevel_type
        self.callbacks = set()  # The callback functions
        self.pkg = None  # Instance of Package(), that we are generating

        # List of Convert(...) functions that were implemented
        self.conversions = dict()

        self.__marshallers = set()  # The generated marshallers

        # Search for the GtkAda binding information

        self.gtkpkg = gtkada.get_pkg(self.ctype)
        if not self.gtkpkg.bindtype:
            self.has_toplevel_type = False
            self.is_gobject = False

        # Is this a binding for an opaque C record (not a GObject). In this
        # case, we bind it as an Ada tagged type so that we can use the
        # convenient dot notation for primitive operations. This is only doable
        # if there is no public field that should be user visible in the
        # record. Otherwise, we'll map to a standard Ada record
        # (self.is_record)

        self.is_proxy = False
        self.is_boxed = (self.node.tag == nrecord
                         and (not self.node.findall(nfield)
                              or self.node.get("introspectable", "1") == "0"))
        self.is_record = self.node.tag == nrecord and not self.is_boxed

        if (self.is_boxed
                and naming.type_exceptions.get(self.ctype) is not None
                and isinstance(naming.type_exceptions.get(self.ctype), Proxy)):
            self.is_proxy = True
            self.is_boxed = False

        # Register naming exceptions for this class

        n = naming.case(self.ctype)

        into = self.gtkpkg.into()
        ada = self.gtkpkg.ada_name()
        if ada:
            pkg = ada
        elif into:
            into = naming.case(into)
            pkg = naming.protect_keywords(into.replace("_", ".", 1))
        else:
            pkg = naming.protect_keywords(n.replace("_", ".", 1))

        pkg = "%s.%s" % (pkg, n)
        naming.add_girname(girname=n, ctype=self.ctype)

        if has_toplevel_type:
            ctype = node.get(ctype_qname)
            if is_interface:
                t = Interface(pkg)
            elif is_gobject:
                t = GObject(pkg)
            elif self.is_proxy:
                t = Proxy(pkg)
            elif self.is_boxed:
                t = Tagged(pkg)
            else:
                t = Record(pkg)

            naming.add_type_exception(cname=ctype, type=t)
            classtype = naming.type(name=self.ctype)
            typename = classtype.ada
            self.name = package_name(typename)
            if ada is not None:
                self.name = ada

            self.ada_package_name = self.name

            if not self.has_toplevel_type:
                # Compute the package name ignoring the type_exceptions. For
                # instance, we have defined that GdkWindow is mapped to
                # Gdk.Gdk_Window, but the operations should go into the package
                # Gdk.Window.Gdk_Window.
                self.ada_package_name = package_name(pkg)

        else:
            typename = ""
            self.name = package_name(pkg)
            self.ada_package_name = self.name

        self.gtkpkg.register_types(adapkg=self.ada_package_name)

        # Compute information that will be used for the binding

        self._subst = {  # for substitution in string templates
            "name": self.name,
            "typename": base_name(typename),
            "cname": self.ctype or ""}

    def _handle_function(self, section, c, ismethod=False, gtkmethod=None,
                         showdoc=True, isinherited=False):
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
                ismethod=ismethod,
                isinherited=isinherited)
        else:
            naming.add_cmethod(
                cname, gtkmethod.ada_name() or cname)  # Avoid warning later on

    def _func_is_direct_import(self, profile):
        """Whether a function with this profile
           should be implemented directly as a pragma Import, rather than
           require its own body.
        """
        return not self.is_gobject \
            and not self.is_boxed \
            and profile.direct_c_map()

    def _add_self_param(self, adaname, node, gtkmethod, profile, inherited):
        """Add a Self parameter to the list of parameters in profile.
           The exact type of the parameter depends on several criteria.

           :param bool inherited: should be true if this is for a subprogram
             inherited from an interface (in which case we force the type of
             Self to be that of the child, not the interface type as described
             in the gir file)
        """

        # Try to extract the type of the parameter from the instance-parameter
        # node.
        t = None
        if not inherited:
            try:
                ip = next(node.iter(ninstanceparam))
                ipt = ip.find(ntype)
                if ipt is not None:
                    ctype_name = ipt.get(ctype_qname)
                    if ctype_name:
                        ctype_name = ctype_name.replace('const ', '')
                    t = naming.type(name=ipt.get('name'),
                                    cname=ctype_name,
                                    useclass=gtkmethod.is_class_wide())
            except StopIteration:
                t = None

        # There was no instance-parameter node, guess the type from the
        # package name

        if t is None:
            t = naming.type(self._subst["cname"],
                            cname=self._subst["cname"],
                            useclass=gtkmethod.is_class_wide())

        gtkparam = gtkmethod.get_param("self")
        pname = gtkparam.ada_name() or "Self"

        direction = gtkparam.get_direction() or "in"
        if direction in ("out", "access"):
            mode = direction
        elif direction == "inout":
            mode = "in out"
        else:
            mode = "in"

        profile.add_param(0, Parameter(name=pname, type=t, mode=mode))

    def _handle_function_internal(self, section, node, cname,
                                  gtkmethod,
                                  profile=None,
                                  showdoc=True,
                                  adaname=None,
                                  ismethod=False,
                                  isinherited=False):
        """Generate a binding for a function.,
           This returns None if no binding was made, an instance of Subprogram
           otherwise.
           `adaname' is the name of the generated Ada subprograms. By default,
           it is computed automatically from either binding.xml or the "name"
           attribute of `node'.
           `profile' is an instance of SubprogramProfile
        """
        assert(profile is None or isinstance(profile, SubprogramProfile))

        if profile.has_varargs() \
                and gtkmethod.get_param("varargs").node is None:
            naming.add_cmethod(cname, cname)  # Avoid warning later on.
            print("No binding for %s: varargs" % cname)
            return None

        is_import = self._func_is_direct_import(profile) \
            and not gtkmethod.get_body() \
            and not gtkmethod.return_as_param()
        adaname = adaname or gtkmethod.ada_name() or node.get("name").title()
        adaname = naming.protect_keywords(adaname)

        if not isinherited:
            naming.add_cmethod(cname, "%s.%s" % (self.pkg.name, adaname))

        if ismethod:
            self._add_self_param(
                adaname, node, gtkmethod, profile, inherited=isinherited)

        if adaname.startswith("Gtk_New"):
            # Overrides the GIR file even if it reported a function or method
            self._handle_constructor(
                node, gtkmethod=gtkmethod, cname=cname, profile=profile)
            return

        local_vars = []
        call = ""

        body = gtkmethod.get_body()
        if not is_import:
            # Prepare the Internal C function

            internal_call = []
            internal = Subprogram(
                name="Internal",
                returns=profile.returns,
                lang="ada->c",
                plist=profile.c_params(local_vars, internal_call)
            ).import_c(cname)

            # Should we transform the return value into a parameter ?

            ret_as_param = gtkmethod.return_as_param()
            if ret_as_param is not None:
                profile.params.append(
                    Parameter(name=ret_as_param,
                              type=profile.returns, mode="out"))
                profile.returns = None

            # Is this a function that takes a callback parameter ?

            cb = profile.callback_param_info()
            if cb is not None:
                if ret_as_param:
                    # ??? We would need to change _callback_support to
                    # have additional code to set the return value. One
                    # issue is that the profile has already been changed
                    raise Exception("Cannot bind function with callback"
                                    + " and return value as parameter: %s"
                                    % cname)

                return self._callback_support(adaname, cname, profile, cb)

            execute = internal.call(
                in_pkg=self.pkg, extra_postcall="".join(internal_call))

            if ret_as_param is not None:
                assert execute[1] is not None, \
                    "Must have a return value in %s => %s" % (cname, execute)
                call = "%s%s := %s;" % (execute[0], ret_as_param, execute[1])

            else:
                if execute[1]:  # A function, with a standard "return"
                    call = "%sreturn %s;" % (execute[0], execute[1])
                else:
                    call = execute[0]

            local_vars += execute[2]

        subp = profile.subprogram(name=adaname, showdoc=showdoc,
                                  local_vars=local_vars, code=call)

        if is_import:
            subp.import_c(cname)
        else:
            subp.add_nested(internal)
            if body:
                subp.set_body("   " + body.strip() + "\n")

        section.add(subp)
        return subp

    def _callback_support(self, adaname, cname, profile, cb):
        """Add support for a function with a callback parameter and user data.
           We generate multiple bindings for such a function:
           * One version that doesn't take a user_data. This looks like:
                type My_Callback is access function (Self, other_params);
                procedure Gtk_Func (Self : ...; Cb : My_Callback);

             since My_Callback doesn't have exactly the same profile as
             required by gtk+, we in fact go through an intermediate function
             in the body, to which we pass, as user_data, a pointer to the
             user's callback:

                 function Internal_Callback (same_profile_as_c, user_data) is
                    User_Func : My_Callback := convert (user_data);
                 begin
                    return User_Func (...);
                 end Internal_Callback;
                 pragma Convention (C, Internal_Callback);

            * Ideally we want to generate a generic package to which users
              can pass their own user data type. We then need to generate the
              proper Destroy callback that C will call to free that user data.

           :profile: is an instance of SubprogramProfile.
           :cname: is the name of the gtk+ C function.
           :adaname: is the name of the corresponding Ada function.
           :cb: is a list of Parameter instances representing the callback
              parameters.
        """

        if len(cb) > 1:
            print("No binding for %s: multiple callback parameters" % cname)
            return
        cb = cb[0]

        def call_to_c(gtk_func, values,
                      user_data_setup='', user_data_cleanup=''):
            """Implement the call to the C function.
               If the user passes a null callback, we always want to pass null
               to C rather than passing our Internal_Callback'Address.

               :param values: a dictionary of the parameters to pass to call().
               :return: the code for the Ada function's body
            """

            values_if_null = copy.deepcopy(values)
            values_if_null[cb.name.lower()] = "System.Null_Address"

            if user_data is not None:
                values_if_null[user_data.lower()] = "System.Null_Address"

            exec1 = gtk_func.call(
                in_pkg=self.pkg,
                extra_postcall="".join(call), values=values_if_null)

            call1 = gtk_func.call_to_string(exec1, lang="ada->c")
            if not call1.endswith(";"):
                call1 += ";"

            exec2 = gtk_func.call(
                in_pkg=self.pkg,
                extra_postcall="".join(call), values=values)

            call2 = gtk_func.call_to_string(exec2, lang="ada->c")
            if not call2.endswith(";"):
                call2 += ";"
            if user_data_setup:
                call2 = user_data_setup + '\n' + call2
            if user_data_cleanup:
                call2 += '\n' + user_data_cleanup

            return ("""if %s = null then
   %s
else
   %s
end if;""" % (cb.name, call1, call2), exec2[2])

        cbname = cb.type.param

        # Compute the name of the Ada type representing the user callback

        cb_type_name = naming.type(name=cb.type.ada, cname=cbname).ada
        funcname = base_name(cb_type_name)

        destroy = profile.find_param(destroy_data_params)

        # Compute the profile of the callback (will all its arguments)

        gtkmethod = self.gtkpkg.get_method(cname=cname)

        try:
            cb_gir_node = self.gir.callbacks[cb.type.ada]
        except:
            raise Exception(
                "No GIR node for %s in callback %s" % (cb.type.ada, cname))

        cbgtk = self.gtkpkg.get_method(cbname)

        cb_profile = SubprogramProfile.parse(
            cb_gir_node, gtkmethod=cbgtk, pkg=self.pkg)

        user_data = profile.callback_user_data()
        cb_user_data = cb_profile.find_param(user_data_params)
        if user_data is None and cb_user_data is not None:
            user_data = cb_user_data

        # Generate the access-to-subprogram type for the user callback, unless
        # we have already done so. This is the type that doesn't receive
        # user data.

        if cbname not in self.callbacks:
            self.callbacks.add(cbname)   # Prevent multiple generations

            section = self.pkg.section("Callbacks")

            if cb_user_data is None:
                print("callback has no user data: %s" % cbname)
                # If the C function has no user data, we do not know how to
                # generate a high-level binding, since we cannot go through an
                # intermediate C function that transforms the parameters into
                # their Ada equivalent.
                #
                # Instead, we just generate a low-level C callback passing
                # System.Address for widgets.

                nouser_cb_profile = copy.deepcopy(cb_profile)
                subp = nouser_cb_profile.subprogram(name="", lang="ada->c")
                section.add(
                    "\ntype %s is %s" % (funcname, subp.spec(pkg=self.pkg)))
                section.add(
                    "\npragma Convention (C, %s);" % funcname)
                section.add(
                    ("function To_Address is new Ada.Unchecked_Conversion\n"
                     + "   (%s, System.Address);\n") % (cb_type_name,),
                    in_spec=False)

            else:
                # Generate a simpler version of the callback, without
                # user data, that the Ada applications can use

                nouser_cb_profile = copy.deepcopy(cb_profile)
                nouser_cb_profile.remove_param(
                    destroy_data_params + [cb_user_data])
                subp = nouser_cb_profile.subprogram(name="")
                section.add(
                    "\ntype %s is %s" % (funcname, subp.spec(pkg=self.pkg)))

                # Generate a subprogram in the body to act as the C callback.
                # This subprogram is responsible for calling the user's
                # callback. In the call to the user's callback, we need to
                # convert the parameters from the C values to the
                # corresponding Ada values.

                self.pkg.add_with(
                    "Ada.Unchecked_Conversion", do_use=False, specs=False)
                section.add(
                    ("function To_%s is new Ada.Unchecked_Conversion\n"
                     + "   (System.Address, %s);\n") % (funcname, funcname),
                    in_spec=False)
                section.add(
                    ("function To_Address is new Ada.Unchecked_Conversion\n"
                     + "   (%s, System.Address);\n") % (cb_type_name,),
                    in_spec=False)

                ada_func = copy.deepcopy(subp)

                if ada_func.plist:
                    ada_func.name = "Func"
                else:
                    ada_func.name = "Func.all"

                ada_func_call = ada_func.call(in_pkg=self.pkg, lang="c->ada")
                body_cb = cb_profile.subprogram(
                    name="Internal_%s" % funcname,
                    local_vars=[Local_Var(
                        "Func", "constant %s" % funcname,
                        "To_%s (%s)" % (funcname, cb_user_data))]
                    + ada_func_call[2],
                    lang="c->ada",
                    code=ada_func.call_to_string(ada_func_call, lang="c->ada"))
                body_cb.convention = "C"
                body_cb.doc = []
                section.add(body_cb, in_spec=False)

        # The gtk C function, will all parameters.
        # This will be used to generate the "Internal" nested subprogram.

        local_vars = []
        call = []
        gtk_func_profile = copy.deepcopy(profile)

        if cb is not None:
            gtk_func_profile.replace_param(cb.name, "System.Address")

        if cb_user_data is not None:
            gtk_func_profile.replace_param(destroy, "System.Address")

        gtk_func = gtk_func_profile.subprogram(
            name=naming.case("C_%s" % cname), lang="ada->c").import_c(cname)

        # This function is shared both by the version without user_data and by
        # the generic package, so we need to put it directly in the package,
        # not a nested subprogram.

        self.pkg.section("").add(gtk_func, in_spec=False)

        # Create a version of the function without a user data.

        section = self.pkg.section("Methods")

        nouser_profile = copy.deepcopy(profile)

        if user_data is None:
            values = {destroy: "System.Null_Address",
                      cb.name.lower(): "To_Address (%s)" % cb.name}
        elif cb_user_data is None:
            values = {destroy: "System.Null_Address",
                      cb.name.lower(): "Internal_%s'Address" % funcname,
                      user_data.lower(): "To_Address (%s)" % cb.name}
        else:
            nouser_profile.remove_param(destroy_data_params + [user_data])
            values = {destroy: "System.Null_Address",
                      cb.name.lower(): "Internal_%s'Address" % funcname,
                      user_data.lower(): "To_Address (%s)" % cb.name}

        c_call = call_to_c(gtk_func, values)

        subp = nouser_profile.subprogram(
            name=adaname, local_vars=c_call[1],
            code=c_call[0])
        section.add(subp)

        # Now create a generic package that will provide access to
        # user_data. The function can no longer be a primitive operation of the
        # object, since it is in a nested package.
        # It is possible that the function doesn't accept a user data in fact
        # (for instance when scope="async"). In this case, no need for a
        # generic package.

        user_data2 = cb_profile.find_param(user_data_params)

        if user_data2 is not None:
            # If we have no "destroy" callback, so any memory we allocate via
            # User.Build would be leaked. Better not to provide a binding
            # in such a case.
            # As a special case, some functions do not need to keep the data
            # after their execution, so we can still bind those.

            if profile.callback_destroy() is None \
               and '_for' not in cname \
               and cname not in ('gtk_tree_view_map_expanded_rows',
                                 'pango_attributes_filter',
                                 'gdk_window_invalidate_maybe_recurse',
                                 'gtk_menu_popup'):
                pass

            else:
                self.pkg.add_with("Glib.Object", do_use=False, specs=False)

                pkg2 = Package(name="%s_User_Data" % adaname)
                section.add(pkg2)
                pkg2.formal_params = """type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;"""

                sect2 = pkg2.section("")
                sect2.add("""package Users is new Glib.Object.User_Data_Closure
         (User_Data_Type, Destroy);""", in_spec=False)

                sect2.add(
                    ("function To_%s is new Ada.Unchecked_Conversion\n"
                     + "   (System.Address, %s);\n") % (funcname, funcname),
                    in_spec=False)
                sect2.add(
                    ("function To_Address is new Ada.Unchecked_Conversion\n"
                     + "   (%s, System.Address);\n") % (funcname,),
                    in_spec=False)

                cb_profile2 = copy.deepcopy(cb_profile)
                cb_profile2.replace_param(user_data2, "User_Data_Type")
                cb2 = cb_profile2.subprogram(name="")
                sect2.add(
                    "\ntype %s is %s" % (funcname, cb2.spec(pkg=pkg2)))

                values = {user_data2.lower(): "D.Data.all"}
                user_cb = cb_profile2.subprogram(
                    name="To_%s (D.Func)" % funcname)
                user_cb_call = user_cb.call(
                    in_pkg=self.pkg,
                    lang="c->ada",
                    extra_postcall="".join(call), values=values)

                internal_cb = cb_profile.subprogram(
                    name="Internal_Cb",
                    local_vars=[
                        Local_Var("D", "constant Users.Internal_Data_Access",
                                  "Users.Convert (%s)" % user_data2)]
                    + user_cb_call[2],
                    convention="C",
                    lang="c->ada",
                    code=user_cb.call_to_string(user_cb_call, lang="c->ada"))
                sect2.add(internal_cb, in_spec=False)

                values = {destroy: "Users.Free_Data'Address",
                          cb.name.lower(): "%s'Address" % internal_cb.name,
                          user_data.lower(): "D"}

                full_profile = copy.deepcopy(profile)
                full_profile.set_class_wide()
                full_profile.remove_param(destroy_data_params)
                full_profile.replace_param(cb.name, funcname)
                full_profile.replace_param(user_data, "User_Data_Type")

                if profile.callback_destroy() is None:
                    c_call = call_to_c(
                        gtk_func, values,
                        user_data_setup=
                            "D := Users.Build (To_Address (%s), %s);" %
                            (cb.name, user_data),
                        user_data_cleanup="Users.Free_Data (D);")
                else:
                    c_call = call_to_c(
                        gtk_func, values,
                        user_data_setup=
                            "D := Users.Build (To_Address (%s), %s);" %
                            (cb.name, user_data))

                subp2 = full_profile.subprogram(
                    name=adaname,
                    local_vars=c_call[1] + [Local_Var("D", "System.Address")],
                    code=c_call[0])
                sect2.add(subp2)

        return subp

    def _constructors(self):
        n = QName(uri, "constructor").text
        for c in self.node.findall(n):
            cname = c.get(cidentifier)
            gtkmethod = self.gtkpkg.get_method(cname=cname)
            if not gtkmethod.bind():
                naming.add_cmethod(
                    cname, gtkmethod.ada_name() or cname)  # Avoid warning
                continue

            profile = SubprogramProfile.parse(
                node=c, gtkmethod=gtkmethod, pkg=self.pkg,
                ignore_return=True)
            if profile.has_varargs() \
                    and gtkmethod.get_param("varargs").node is None:

                naming.add_cmethod(cname, cname)  # Avoid warning later on.
                print("No binding for %s: varargs" % cname)
                continue

            self._handle_constructor(
                c, gtkmethod=gtkmethod, cname=cname, profile=profile)

    def _handle_constructor(self, c, cname, gtkmethod, profile=None):
        assert(profile is None or isinstance(profile, SubprogramProfile))

        section = self.pkg.section("Constructors")
        name = c.get("name").title()

        assert profile.params is not None, "No profile defined for %s" % cname

        format_params = ", ".join(p.name for p in profile.params)
        if format_params:
            self._subst["internal_params"] = " (%s)" % format_params
            format_params = ", " + format_params
            self._subst["params"] = format_params
        else:
            self._subst["params"] = ""
            self._subst["internal_params"] = ""

        if self.is_gobject or self.is_boxed:
            profile.returns = AdaType(
                "System.Address", pkg=self.pkg, in_spec=False)
        else:
            profile.returns = AdaType(
                "%(typename)s" % self._subst,
                pkg=self.pkg, in_spec=False)

        local_vars = []
        code = []
        internal = Subprogram(
            name="Internal",
            lang="ada->c",
            plist=profile.c_params(local_vars, code),
            returns=profile.returns).import_c(cname)

        call = internal.call(in_pkg=self.pkg)
        assert(call[1] is not None)   # A function

        gtk_new_prefix = "Gtk_New"

        adaname = gtkmethod.ada_name()
        if not adaname:
            if cname.startswith("gdk_") or cname.startswith("pango_"):
                gtk_new_prefix = "Gdk_New"
                adaname = "Gdk_%s" % name    # e.g.  Gdk_New
            elif cname.startswith("g_"):
                gtk_new_prefix = "G_New"
                adaname = "G_%s" % name      # e.g.  G_New
            else:
                adaname = "Gtk_%s" % name    # e.g.  Gtk_New

        selfname = gtkmethod.get_param("self").ada_name() or "Self"

        if self.is_gobject:
            selftype = "%(typename)s_Record'Class" % self._subst
        else:
            selftype = "%(typename)s" % self._subst

        if self.is_gobject:
            filtered_params = [p for p in profile.params if p.ada_binding]

            initialize_name=adaname.replace(
                gtk_new_prefix, "%s.Initialize" % self.pkg.name)

            initialize_params = [Parameter(
                name=selfname,
                type=AdaType(selftype, pkg=self.pkg, in_spec=True),
                mode="not null access")] + filtered_params
            initialize = Subprogram(
                name=initialize_name,
                plist=initialize_params,
                local_vars=local_vars + call[2],
                doc=profile.doc +
                    [('\n%s does nothing if the object was already' +
                     ' created with another call to Initialize* or G_New.') % (
                         base_name(initialize_name), )],
                code="if not %s.Is_Created then %sSet_Object (%s, %s); end if" % (
                    selfname, call[0], selfname, call[1]),
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
                    mode="out")] + filtered_params,
                local_vars=call[2],
                code=selfname + " := new %(typename)s_Record;" % self._subst
                + call[0],
                doc=profile.doc)

            section.add(gtk_new)
            section.add(initialize)

            # Gtk_New as a function
            gtk_new = Subprogram(
                name="%s_%s" % (self._subst["typename"], name),
                returns=AdaType("%(typename)s" % self._subst,
                                pkg=self.pkg, in_spec=True),
                plist=filtered_params,
                local_vars=call[2] +
                [Local_Var(selfname,
                           "constant %(typename)s" % self._subst,
                           "new %(typename)s_Record" % self._subst)],
                code=call[0] + "return %s;" % selfname,
                doc=profile.doc)
            section.add(gtk_new)

        elif self.is_boxed:
            gtk_new = Subprogram(
                name=adaname,
                plist=[Parameter(
                    name=selfname,
                    type=AdaType("%(typename)s" % self._subst,
                                 pkg=self.pkg, in_spec=True),
                    mode="out")] + profile.params,
                local_vars=local_vars + call[2],
                code="%s%s.Set_Object (%s)" % (call[0], selfname, call[1]),
                doc=profile.doc)

            gtk_new.add_nested(internal)
            section.add(gtk_new)

            # Now as a function
            gtk_new = Subprogram(
                name="%s_%s" % (self._subst["typename"], name),
                returns=AdaType("%(typename)s" % self._subst,
                                pkg=self.pkg, in_spec=True),
                plist=profile.params,
                local_vars=local_vars + call[2] +
                [Local_Var(selfname,
                           "%(typename)s" % self._subst)],
                code="%s%s.Set_Object (%s); return %s" % (
                    call[0], selfname, call[1], selfname),
                doc=profile.doc)
            gtk_new.add_nested(internal)
            section.add(gtk_new)

        else:
            # likely a Proxy
            gtk_new = Subprogram(
                name=adaname,
                plist=[Parameter(
                    name=selfname,
                    type=AdaType("%(typename)s" % self._subst,
                                 pkg=self.pkg, in_spec=True),
                    mode="out")] + profile.params,
                local_vars=local_vars + call[2],
                code="%s%s := %s" % (call[0], selfname, call[1]),
                doc=profile.doc)
            gtk_new.add_nested(internal)
            section.add(gtk_new)

            # Now as a function
            gtk_new = Subprogram(
                name="%s_%s" % (self._subst["typename"], name),
                returns=AdaType("%(typename)s" % self._subst,
                                pkg=self.pkg, in_spec=True),
                plist=profile.params,
                local_vars=local_vars + call[2] +
                [Local_Var(selfname,
                           "%(typename)s" % self._subst)],
                code="%s%s := %s; return %s;" % (
                    call[0], selfname, call[1], selfname),
                doc=profile.doc)
            gtk_new.add_nested(internal)
            section.add(gtk_new)

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

    def _virtual_methods(self):
        all = self.node.findall(nvirtualmethod)
        has_iface = False

        if all is not None:
            ifacename = base_name(self.name)
            info = ''

            for c in all:
                if not self.gtkpkg.bind_virtual_method(
                   c.get('name'), default=self.is_interface):
                    continue

                gtkmethod = self.gtkpkg.get_method(cname=c.get('name'))
                basename = gtkmethod.ada_name() or c.get('name').title()
                adaname = "Virtual_%s" % basename

                if not has_iface:
                    has_iface = True
                    section = self.pkg.section("Virtual Methods")

                info += """
   procedure Set_%(basename)s
      (Self    : %(type)s;
       Handler : %(adaname)s);
   pragma Import (C, Set_%(basename)s, "gtkada_%(ifacename)s_set_%(method)s");
""" % {"basename": basename,
       "type": "%s_Interface_Descr" % ifacename
               if self.is_interface
               else "Glib.Object.GObject_Class",
       "adaname": adaname,
       "ifacename": ifacename,
       "method": basename.lower()}

                c_iface = '%s%s' % (
                    self.identifier_prefix,
                    self.node.get(glib_type_struct))

                self.gir.ccode += """
void gtkada_%(type)s_set_%(method)s(%(iface)s* iface, void* handler) {
    iface->%(field)s = handler;
}
""" % {"type": ifacename,
       "method": basename.lower(),
       "iface": c_iface,
       "field": c.get('name')}

                profile = SubprogramProfile.parse(
                    node=c,
                    gtkmethod=gtkmethod,
                    pkg=self.pkg)
                self._add_self_param(
                    adaname=adaname, node=c, gtkmethod=gtkmethod, profile=profile,
                    inherited=False)
                subp = profile.subprogram(
                    name=adaname,
                    lang="ada->c",
                    convention="C",
                    showdoc=True)
                section.add(
                    '\ntype %s is %s' % (
                        adaname,
                        subp.spec(pkg=self.pkg, as_type=True)))
                subp.add_withs_for_subprogram(pkg=self.pkg, in_specs=True)

        if has_iface:
            info = ('subtype %s_Interface_Descr is ' % ifacename
                    + 'Glib.Object.Interface_Description;\n'
                    + info
                    + '--  See Glib.Object.Add_Interface\n')
            section.add(info)
            self.pkg.add_with('Glib.Object')

    def _globals(self):
        funcs = self.gtkpkg.get_global_functions()  # List of binding.xml nodes
        if funcs:
            section = self.pkg.section("Functions")  # Make sure section exists
            for f in funcs:
                c = self.gir.globals.get_function(
                    f.cname())  # Node in gir file
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
            get_type_name = gtkmethod.ada_name() or "Get_Type"
            section.add(
                Subprogram(
                    name=get_type_name,
                    doc=gtkmethod.get_doc(default=''),
                    returns=AdaType("Glib.GType", pkg=self.pkg, in_spec=True))
                .import_c(n))

            if not self.gtktype.is_subtype() \
                    and not self.is_interface \
                    and self.is_gobject \
                    and self._subst["parent"] is not None:

                self.pkg.add_with("Glib.Type_Conversion_Hooks", specs=False)

                self._subst["get_type"] = get_type_name

                section.add(
                    ("package Type_Conversion_%(typename)s is new"
                     + " Glib.Type_Conversion_Hooks.Hook_Registrator\n"
                     + "   (%(get_type)s'Access, %(typename)s_Record);\n"
                     + "pragma Unreferenced (Type_Conversion_%(typename)s);""")
                    % self._subst, in_spec=False)

    def _get_c_type(self, node):
        t = node.find(ntype)
        if t is not None:
            return t.get(ctype_qname)
        return None

    def _fields(self):
        fields = self.node.findall(nfield)
        if fields:
            section = self.pkg.section("Fields")
            for f in fields:
                name = f.get("name")

                # Getter

                if f.get("readable", "1") != "0":
                    cname = "gtkada_%(cname)s_get_%(name)s" % {
                        "cname": self._subst["cname"], "name": name}
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
""" % {"ctype": ctype, "cname": cname, "self": self.ctype, "name": name}

                # Setter

                if f.get("writable", "1") != "0":
                    cname = "gtkada_%(cname)s_set_%(name)s" % {
                        "cname": self._subst["cname"], "name": name}
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
""" % {"ctype": ctype, "cname": cname, "self": self.ctype, "name": name}

    def _properties(self):
        n = QName(uri, "property")

        props = list(self.node.findall(n.text))
        if props is not None:
            adaprops = []
            for p in props:
                flags = []
                if p.get("readable", "1") != "0":
                    flags.append("read")
                if p.get("writable", "1") != "0":
                    flags.append("write")

                # Do not provide a "pkg=self.pkg" parameter, since we do
                # not want to generate a with for the actual property type
                # (for instance a Gtk_Cell_Area), just for the actual type
                # (Property_Object).

                tp = _get_type(p)
                tp.userecord = False
                ptype = tp.as_property()
                if ptype:
                    pkg = ptype[:ptype.rfind(".")]
                    if pkg:
                        self.pkg.add_with(pkg)
                else:
                    section = self.pkg.section("Properties")
                    section.add(
                        ('   %(name)s_Property : constant '
                         + 'Glib.Properties.Property_String :=\n'
                         + '      Glib.Properties.Build ("%(cname)s");'
                         + '  --  Unknown type: %(type)s') % {
                            "name": naming.case(p.get("name")),
                            "type": (p.find(ntype).get("name")
                                     if p.find(ntype) is not None
                                     else "unspecified"),
                            "cname": p.get("name")})
                    self.pkg.add_with("Glib.Properties")
                    continue

                adaprops.append({
                    "cname": p.get("name"),
                    "name": naming.case(p.get("name")),
                    "flags": "-".join(flags),
                    "doc": _get_clean_doc(p),
                    "pkg": pkg,
                    "ptype": ptype,
                    "type": tp.as_ada_param(self.pkg)})

            if adaprops:
                section = self.pkg.section("Properties")
                section.add_comment(
                    """The following properties are defined for this widget.
See Glib.Properties for more information on properties)""")

                adaprops.sort(key=lambda x: x["name"])

                for p in adaprops:
                    prop_str = '   %(name)s_Property : constant %(ptype)s;' % p
                    if not section.add(prop_str):
                        continue

                    # Only display the type if it isn't obvious from the actual
                    # type of the property.
                    if p["type"] not in ("Boolean", "UTF8_String", "Gfloat",
                                         "Glib.Gint", "Guint") \
                       and not p["type"].startswith("Gtk.Enums."):
                        section.add(
                            Code("Type:  %(type)s" % p, iscomment=True))

                    if p["flags"] != "read-write":
                        section.add(
                            Code("Flags: %(flags)s" % p, iscomment=True))
                    if p["doc"]:
                        section.add(Code("%s" % p["doc"], iscomment=True))

                    d = '   %(name)s_Property : constant %(ptype)s' % p
                    self.pkg.add_private(
                        d + ' :=\n     %(pkg)s.Build ("%(cname)s");' % p)

    def _compute_marshaller_suffix(self, selftype, profile):
        """Computes the suffix for the connect and marshallers types based on
           the profile of the signal.
        """
        return "_".join(
            [base_name(selftype.ada)]
            + [base_name(p.type.ada) for p in profile.params]
            + [(base_name(profile.returns.ada)
                if profile.returns else "Void")])

    def _marshall_gvalue(self, profile):
        """Return the arguments to parse to an Ada callback, after extracting
           them from a GValue. Returns a list of local variables for the
           marshaller, and its body
        """
        call_params = []
        for index, p in enumerate(profile.params):
            if isinstance(p.type, Tagged):
                call_params.append(
                    "%s.From_Object (Unchecked_To_Address (Params, %d))" %
                    (package_name(p.type.ada), index + 1))
            elif isinstance(p.type, Interface):
                call_params.append(
                    "%s (Unchecked_To_Interface (Params, %d))" %
                    (p.type.ada, index + 1))
            elif isinstance(p.type, GObject):
                if p.type.ada != "Glib.Object.GObject":
                    call_params.append(
                        "%s (Unchecked_To_Object (Params, %d))" % (
                            p.type.ada, index + 1))
                else:
                    call_params.append(
                        "Unchecked_To_Object (Params, %d)" % (index + 1, ))
            else:
                if p.mode != "in":
                    call_params.append(
                        "Unchecked_To_%s_Access (Params, %d)" % (
                            base_name(p.type.ada), index + 1))
                else:
                    call_params.append(
                        "Unchecked_To_%s (Params, %d)" % (
                            base_name(p.type.ada), index + 1))

        if call_params:
            call = "H (Obj, %s)" % ", ".join(call_params)
        else:
            call = "H (Obj)"

        if profile.returns:
            marsh_local = [
                Local_Var("V", profile.returns, aliased=True, default=call)]
            marsh_body = "Set_Value (Return_Value, V'Address);"
        else:
            marsh_local = []
            marsh_body = "%s;" % call

        marsh_body += "exception when E : others => Process_Exception (E);"

        return marsh_local, marsh_body

    def _generate_slot_marshaller(self, section, selftype, node, gtkmethod):
        """Generate connect+marshaller when connect takes a slot object. These
           procedure are independent of the specific widget, and can be shared.
           Returns the name for the hander type.
        """

        if SHARED_SLOT_MARSHALLERS:
            pkg = gir.slot_marshaller_pkg
            section = gir.slot_marshaller_section
            existing_marshallers = gir.slot_marshallers
        else:
            pkg = self.pkg
            section = section
            existing_marshallers = self.__marshallers

        profile = SubprogramProfile.parse(
            node=node, gtkmethod=gtkmethod, pkg=pkg)

        callback_selftype = GObject(
            "Glib.Object.GObject", classwide=True, allow_none=True)

        name_suffix = self._compute_marshaller_suffix(
            callback_selftype, profile)
        slot_name = "Cb_%s" % name_suffix
        marshname = "Marsh_%s" % name_suffix

        callback = Subprogram(
            name="",
            plist=[Parameter(name="Self", type=callback_selftype)]
            + profile.params,
            code="null",
            allow_none=False,
            returns=profile.returns)

        if slot_name not in existing_marshallers:
            existing_marshallers.add(slot_name)

            slot_handler_type = "type %s is %s;" % (
                slot_name, callback.profile(
                    pkg=pkg,
                    maxlen=69, indent="      "))
            section.add(Code(slot_handler_type), in_spec=True)

            section.add(Code(
                """function Cb_To_Address is new Ada.Unchecked_Conversion
   (%s, System.Address);
function Address_To_Cb is new Ada.Unchecked_Conversion
   (System.Address, %s);
""" % (slot_name, slot_name)),
                in_spec=False)

            if isinstance(selftype, Interface):
                obj_in_body = "Glib.Types.GType_Interface (Object)"
            else:
                obj_in_body = "Object"

            connect_slot_body = """
      Unchecked_Do_Signal_Connect
        (Object      => %s,
         C_Name      => C_Name,
         Marshaller  => %s'Access,
         Handler     => Cb_To_Address (Handler),  --  Set in the closure
         Slot_Object => Slot,
         After       => After)""" % (obj_in_body, marshname)

            marsh_local, marsh_body = self._marshall_gvalue(profile)

            connect_slot = Subprogram(
                name="Connect_Slot",
                plist=[Parameter("Object", selftype),
                       Parameter("C_Name", "Glib.Signal_Name"),
                       Parameter("Handler", slot_name),
                       Parameter("After", "Boolean"),
                       Parameter("Slot", GObject(
                           "Glib.Object.GObject", allow_none=True,
                           classwide=True), default="null")
                       ],
                code=connect_slot_body)
            section.add(connect_slot, in_spec=False)

            addr_to_obj = "Glib.Object.Convert (Get_Data (Closure))"

            marsh = Subprogram(
                name=marshname,
                plist=[Parameter("Closure", "GClosure"),
                       Parameter("Return_Value", "Glib.Values.GValue"),
                       Parameter("N_Params", "Glib.Guint"),
                       Parameter("Params", "Glib.Values.C_GValues"),
                       Parameter("Invocation_Hint", "System.Address"),
                       Parameter("User_Data", "System.Address")],
                local_vars=[
                    Local_Var("H", "constant %s" % slot_name,
                              "Address_To_Cb (Get_Callback (Closure))"),
                    Local_Var("Obj", "Glib.Object.GObject", constant=True,
                              default=addr_to_obj)
                ] + marsh_local,
                convention="C",
                code=marsh_body)
            section.add(marsh, in_spec=False)

        return slot_name, callback

    def _generate_marshaller(self, section, selftype, node, gtkmethod):
        """Generate, if needed, a connect+marshaller for signals with this
           profile.
           Returns the name of the type that contains the subprogram profile
        """

        profile = SubprogramProfile.parse(
            node=node, gtkmethod=gtkmethod,
            pkg=self.pkg if gtkmethod.bind() else None)

        name_suffix = self._compute_marshaller_suffix(selftype, profile)
        marshname = "Marsh_%s" % name_suffix
        name = "Cb_%s" % name_suffix

        callback = Subprogram(
            name="",
            plist=[Parameter(name="Self", type=selftype)] + profile.params,
            code="null",
            allow_none=False,
            returns=profile.returns)

        if gtkmethod.bind() and name not in self.__marshallers:
            self.__marshallers.add(name)

            handler_type = "type %s is %s;" % (
                name, callback.profile(
                    pkg=self.pkg, maxlen=69, indent="      "))
            section.add(Code(handler_type), in_spec=True)

            section.add(Code(
                """function Cb_To_Address is new Ada.Unchecked_Conversion
   (%s, System.Address);
function Address_To_Cb is new Ada.Unchecked_Conversion
   (System.Address, %s);
""" % (name, name)),
                in_spec=False)

            if isinstance(selftype, Interface):
                obj_in_body = "Glib.Types.GType_Interface (Object)"
            else:
                obj_in_body = "Object"

            connect_body = """
      Unchecked_Do_Signal_Connect
        (Object      => %s,
         C_Name      => C_Name,
         Marshaller  => %s'Access,
         Handler     => Cb_To_Address (Handler),  --  Set in the closure
         After       => After)""" % (obj_in_body, marshname)

            marsh_local, marsh_body = self._marshall_gvalue(profile)

            connect = Subprogram(
                name="Connect",
                plist=[Parameter("Object", selftype),
                       Parameter("C_Name", "Glib.Signal_Name"),
                       Parameter("Handler", name),
                       Parameter("After", "Boolean"),
                       ],
                code=connect_body)
            section.add(connect, in_spec=False)

            if isinstance(selftype, Interface):
                addr_to_obj = \
                    "%s (Unchecked_To_Interface (Params, 0))" % selftype.ada
            else:
                addr_to_obj = \
                    "%s (Unchecked_To_Object (Params, 0))" % selftype.ada

            marsh = Subprogram(
                name=marshname,
                plist=[Parameter("Closure", "GClosure"),
                       Parameter("Return_Value", "Glib.Values.GValue"),
                       Parameter("N_Params", "Glib.Guint"),
                       Parameter("Params", "Glib.Values.C_GValues"),
                       Parameter("Invocation_Hint", "System.Address"),
                       Parameter("User_Data", "System.Address")],
                local_vars=[
                    Local_Var("H", "constant %s" % name,
                              "Address_To_Cb (Get_Callback (Closure))"),
                    Local_Var("Obj", selftype.ada, constant=True,
                              default=addr_to_obj)
                ] + marsh_local,
                convention="C",
                code=marsh_body)
            section.add(marsh, in_spec=False)

        return name, callback, profile

    def _signals(self):
        signals = list(self.node.findall(gsignal))
        if signals:
            adasignals = []
            section = self.pkg.section("Signals")
            section.sort_objects = False  # preserve insertion order

            # If at least one signal gets a On_* procedure
            for s in signals:
                if self.gtkpkg.get_method(
                   cname="::%s" % s.get("name")).bind():
                    section.add(
                        Code("use type System.Address;"), in_spec=False)
                    self.pkg.add_with("Gtkada.Bindings", specs=False)
                    self.pkg.add_with("Glib.Values", specs=False)
                    self.pkg.add_with("Gtk.Arguments", specs=False)

                    if SHARED_SLOT_MARSHALLERS:
                        self.pkg.add_with("Gtkada.Marshallers")
                    self.pkg.add_with(
                        "Ada.Unchecked_Conversion", specs=False, do_use=False)
                    break

            signals.sort(key=lambda x: x.get("name"))

            for s in signals:
                gtkmethod = self.gtkpkg.get_method(
                    cname="::%s" % (s.get("name")))
                bind = gtkmethod.bind()

                name = s.get("name")

                if self.is_gobject:
                    selftype = GObject("%(typename)s" % self._subst,
                                       allow_none=True, classwide=True)
                    on_selftype = GObject("%(typename)s" % self._subst,
                                          allow_none=False, classwide=False)
                elif self.is_interface:
                    on_selftype = selftype = Interface(
                        "%(typename)s" % self._subst)
                else:
                    on_selftype = selftype = Proxy(
                        "%(typename)s" % self._subst)

                # We need to parse the profile twice, so that the with
                # statements are set both in self.pkg and in
                # gtkada.marshallers
                handler_type_name, sub, profile = \
                    self._generate_marshaller(
                        section, selftype, node=s, gtkmethod=gtkmethod)

                if bind:
                    slot_handler_type_name, obj_sub = \
                        self._generate_slot_marshaller(
                            section, selftype, node=s, gtkmethod=gtkmethod)

                section.add(
                    Code('   Signal_%s : constant Glib.Signal_Name := "%s";' %
                         (naming.case(name, protect=False), name)),
                    add_newline=False)

                if bind:
                    connect = Subprogram(
                        name="On_%s" % naming.case(name, protect=False),
                        plist=[Parameter(name="Self", type=on_selftype),
                               Parameter(
                            name="Call",
                            type=Proxy(handler_type_name)),
                            Parameter(
                            name="After",
                            type="Boolean",
                            default="False")],
                        code='Connect (Self, "%s" & ASCII.NUL, Call, After);' %
                        name)
                    section.add(connect, add_newline=False)

                    self.pkg.add_with("Glib.Object")
                    obj_connect = Subprogram(
                        name="On_%s" % naming.case(name, protect=False),
                        plist=[Parameter(name="Self", type=on_selftype),
                               Parameter(
                            name="Call",
                            type=Proxy(slot_handler_type_name)),
                            Parameter(
                            name="Slot",
                            type=GObject("Glib.Object.GObject",
                                         allow_none=False,
                                         classwide=True)),
                            Parameter(
                            name="After",
                            type="Boolean",
                            default="False")],
                        code=('Connect_Slot (Self, "%s" & ASCII.NUL,'
                              + ' Call, After, Slot);') % name)
                    section.add(obj_connect)

                doc = _get_clean_doc(s)
                if doc:
                    section.add(Code("  %s""" % doc, iscomment=True))

                if not bind:
                    sub.name = "Handler"
                    section.add(
                        Code(
                            sub.profile(pkg=self.pkg, maxlen=69),
                            fill=False, iscomment=True))

                if profile.returns_doc or len(profile.params) > 1:
                    doc = "\n Callback parameters:" + sub.formatted_doc()
                    if profile.returns_doc:
                        doc += "\n   --  %s" % profile.returns_doc
                    section.add(Code(doc, fill=False, iscomment=True))

    def _implements(self):
        """Bind the interfaces that a class implements"""

        implements = list(self.node.findall(nimplements)) or []

        for impl in implements:
            name = impl.get("name")

            # Special case, because the GIR file are inconsistent
            if name == "Gio.Icon":
                name = "Icon"

            # If the interface is purposefully not bound
            if "--%s" % name in interfaces:
                continue

            try:
                intf = self.gir.interfaces[name]
            except KeyError:
                intf = self.gir.interfaces[naming.girname_to_ctype[name]]

            type = naming.type(intf.ctype)
            if "." in type.ada:
                self.pkg.add_with(type.ada[:type.ada.rfind(".")])
                self.pkg.add_with("Glib.Types")

            impl = dict(
                name=name,
                adatype=base_name(type.ada),
                impl=type.ada,
                interface=intf,
                body="",
                pkg="%(typename)s" % self._subst)
            impl["code"] = \
                """package Implements_%(adatype)s is new Glib.Types.Implements
       (%(impl)s, %(pkg)s_Record, %(pkg)s);
   function "+"
      (Widget : access %(pkg)s_Record'Class)
      return %(impl)s
      renames Implements_%(adatype)s.To_Interface;
   function "-"
      (Interf : %(impl)s)
      return %(pkg)s
      renames Implements_%(adatype)s.To_Object;
""" % impl

            self.implements[name] = impl

        # For an interface, we also define "+" to cast to itself. This is used
        # when writting custom bodies for the methods of the interface, so that
        # those bodies can be reused for the types that implement the
        # interface. See GtkTreeModel.

        if self.is_interface:
            self.implements[""] = {
                "name": self._subst["typename"],
                "interface": None,
                "code": """function "+" (W : %(typename)s) return %(typename)s;
    pragma Inline ("+"); """ % self._subst,
                "body": """function "+" (W : %(typename)s) return %(typename)s is
begin
   return W;
end "+";""" % self._subst,
            }

        if self.implements:

            # Duplicate the subprograms from the interfaces. This doesn't
            # quite work: for instance, Gtk.About_Dialog already has a
            # Get_Name, so we can't redefine the one inherited from Buildable.
            section = self.pkg.section(
                "Inherited subprograms (from interfaces)")

            for impl in sorted(self.implements.keys()):
                impl = self.implements[impl]
                if impl["name"] == "Buildable":
                    # Do not repeat for buildable, that's rarely used

                    section.add_comment(
                        "Methods inherited from the Buildable interface are"
                        + " not duplicated here since they are meant to be"
                        + " used by tools, mostly. If you need to call them,"
                        + ' use an explicit cast through the "-" operator'
                        + " below.")

                    continue

                interf = impl["interface"]

                # Ignore interfaces that we haven't bound
                if interf is not None and not hasattr(interf, "gtkpkg"):
                    print("%s: methods for interface %s were not bound" % (
                        self.name, impl["name"]))
                elif interf is not None:
                    all = interf.node.findall(nmethod)
                    for c in all:
                        cname = c.get(cidentifier)
                        gtkmethod = interf.gtkpkg.get_method(cname)
                        interfmethod = self.gtkpkg.get_method(cname)

                        if interfmethod.bind():
                            self._handle_function(
                                section, c, showdoc=False, gtkmethod=gtkmethod,
                                ismethod=True, isinherited=True)

            section = self.pkg.section("Interfaces")
            section.add_comment(
                "This class implements several interfaces. See Glib.Types")

            for impl in sorted(self.implements.keys()):
                impl = self.implements[impl]
                section.add_comment("")
                section.add_comment('- "%(name)s"' % impl)
                section.add(impl["code"])
                if impl["body"]:
                    section.add(impl["body"], in_spec=False)

    def add_list_binding(self, section, adaname, ctype, singleList):
        """Generate a list instance"""

        conv = "%s->Address" % ctype.ada
        decl = ""
        body = ""

        if conv not in self.conversions:
            self.conversions[conv] = True

            base = "function Convert (R : %s) return System.Address" % (
                ctype.ada)

            decl += base + ';\n'
            body += base + " is\nbegin\n"

            if isinstance(ctype, Proxy):
                body += "return Glib.To_Address (Glib.C_Proxy (R));"
            else:
                body += "return Get_Object (R);"

            body += "\nend Convert;\n\n"

        conv = "Address->%s" % ctype.ada
        if conv not in self.conversions:
            self.conversions[conv] = True

            decl += "function Convert (R : System.Address) return %s;\n" % (
                ctype.ada)
            body += "function Convert (R : System.Address) return %s is\n" % (
                ctype.ada)

            if isinstance(ctype, Proxy):
                body += "begin\nreturn %s" % ctype.ada \
                    + "(Glib.C_Proxy'(Glib.To_Proxy (R)));"

            elif isinstance(ctype, Tagged):
                # Not a GObject ?
                body += "begin\nreturn From_Object(R);"

            else:
                body += "Stub : %s_Record;" % ctype.ada
                body += "begin\n"
                body += "return %s (Glib.Object.Get_User_Data (R, Stub));" % (
                    ctype.ada)

            body += "\nend Convert;"

        if singleList:
            pkg = "GSlist"
            generic = "Generic_SList"
        else:
            pkg = "Glist"
            generic = "Generic_List"

        self.pkg.add_with("Glib.%s" % pkg)

        decl += "package %s is new %s (%s);\n" % (adaname, generic, ctype.ada)
        section.add(decl)
        section.add(body, in_spec=False)

    def record_binding(
            self, section, ctype, adaname, type, override_fields,
            unions, private):
        """Create the binding for a <record> or <union> type.
           override_fields has the same format as returned by
           GtkAdaPackage.records()
        """
        base = adaname or base_name(type.ada)

        try:
            node = gir.records[ctype]
        except KeyError:
            # The package doesn't contain a type (for instance GtkMain)
            return

        gir.bound.add(ctype)

        is_union = node.tag == nunion
        first_field_ctype = None

        fields = []   # array of (name,type,default) tuples

        # Check if we have forced the mapping as a C proxy ?

        if (naming.type_exceptions.get(ctype, None) is None
                or not isinstance(naming.type_exceptions[ctype], Proxy)):

            for field in node.findall(nfield):
                name = field.get("name")

                ftype = None
                type = _get_type(field)
                cb = field.findall(ncallback)

                if type:
                    ftype = override_fields.get(name, None)
                    if ftype is None:

                        if not first_field_ctype:
                            t = field.findall(ntype)
                            assert t, "No type info for %s.%s" % (ctype, name)

                            ctype = t[0].get(ctype_qname)
                            if not ctype:
                                # <type name="..."> has no c:type attribute,
                                # so we try to map the name to a Girname
                                ctype = naming.girname_to_ctype[
                                    t[0].get("name")]

                            first_field_ctype = ctype

                        ftype = type

                elif cb:
                    # ??? JL: Should properly bind the callback here.
                    # For now, we just use a System.Address to maintain the
                    # record integrity
                    ftype = override_fields.get(name, None)
                    if ftype is None:
                        ftype = AdaType(
                            "System.Address", pkg=self.pkg)

                else:
                    print("WARNING: Field '%s.%s' has no type" % (name, base))
                    print(" generated record is most certainly incorrect")

                if ftype is not None:
                    if ftype.ada in ("GSList", "GList") and private:
                        ftype = "System.Address"
                        default_value = "System.Null_Address"
                    else:
                        default_value = ftype.default_record_field_value
                        ftype = ftype.record_field_type(pkg=self.pkg)

                    self.pkg.add_with(package_name(ftype))
                    fields.append((naming.case(name), ftype, default_value))

        if not fields:
            section.add(
                ("\ntype %(typename)s is new Glib.C_Proxy;\n"
                 + "function From_Object_Free (B : access %(typename)s) "
                 + "return %(typename)s;\npragma Inline (From_Object_Free);")
                 % {"typename": base})
            section.add("""
function From_Object_Free (B : access %(typename)s) return %(typename)s is
   Result : constant %(typename)s := B.all;
begin
   Glib.g_free (B.all'Address);
   return Result;
end From_Object_Free;""" % {"typename": base}, in_spec=False)

        else:
            if private:
                section.add(
                    ("\ntype %(typename)s is private;\n"
                     + "function From_Object_Free (B : access %(typename)s)"
                     + " return %(typename)s;\n"
                     + "pragma Inline (From_Object_Free);")
                    % {"typename": base})
                adder = self.pkg.add_private
            else:
                adder = section.add

            if is_union:
                enums = self.get_enumeration_values(first_field_ctype)
                enums_dict = {ctype: adatype for ctype, adatype in enums}
                text = "\ntype %s (%s : %s := %s) is record\n" % (
                    base, fields[0][0], fields[0][1],
                    enums[0][1]) + \
                    "    case %s is\n" % fields[0][0]
                for index, f in enumerate(fields):
                    if index != 0:
                        when_stmt = []
                        if unions:
                            for v, key in unions:
                                # applies to field
                                if key.lower() == f[0].lower():
                                    when_stmt.append(enums_dict[v])
                        else:
                            when_stmt = [enums[index][1]]

                        if not when_stmt:
                            print("ERROR: no discrimant value for field %s"
                                  % f[0])

                        text += "\n      when %s =>\n %s : %s;\n" % (
                            "\n          | ".join(when_stmt), f[0], f[1])

                text += "   end case;\nend record;\n"
                text += "pragma Convention (C, %s);\n" % base
                text += "pragma Unchecked_Union(%s);\n" % base
                adder("\n" + Code(text).format())

            else:
                c = []
                for f in fields:
                    if f[2]:
                        c.append('%s : %s := %s;' % (f[0], f[1], f[2]))
                    else:
                        c.append('%s : %s;' % (f[0], f[1]))

                c = Code('\ntype %s is record\n' % base
                         + '\n'.join(c)
                         + '\nend record;\npragma Convention (C, %s);\n' % base)
                adder(c.format())

            if not private:
                section.add(
                    ("\nfunction From_Object_Free (B : access %(type)s)"
                     + " return %(type)s;\n"
                     + "pragma Inline (From_Object_Free);") % {"type": base})

            section.add("""
function From_Object_Free (B : access %(typename)s) return %(typename)s is
   Result : constant %(typename)s := B.all;
begin
   Glib.g_free (B.all'Address);
   return Result;
end From_Object_Free;""" % {"typename": base}, in_spec=False)

        section.add(Code(_get_clean_doc(node), iscomment=True))

    def get_enumeration_values(sef, enum_ctype):
        """Return the list of enumeration values for the given enum, as a list
           of tuples  (C identifier, ada identifier)"""

        node = gir.enums[enum_ctype]
        return [(m.get(cidentifier), naming.adamethod_name(m.get(cidentifier)))
                for m in node.findall(nmember)]

    def constants_binding(self, section, regexp, prefix):
        constants = []
        r = re.compile(regexp)

        for name, node in gir.constants.items():
            if r.match(name):
                name = name.replace(prefix, "").title()

                gir.bound.add(node.get(ctype_qname))

                type = node.findall(ntype)
                ctype = type[0].get(ctype_qname)
                ftype = naming.type(name="", cname=ctype)

                constants.append(
                    '%s : constant %s := "%s";' %
                    (name, ftype.ada, node.get("value")))

        constants.sort()
        section.add("\n".join(constants))

    def enumeration_binding(self, section, ctype, type, prefix,
                            asbitfield=False, ignore=""):
        """Add to the section the Ada type definition for the <enumeration>
           ctype. type is the corresponding instance of CType().
           This function also handles <bitfield> types.

           :param prefix: is removed from the values to get the default Ada name,
              which can be overridden in data.cname_to_adaname.
           :param ignore: space-separated list of values that should not be
              bound.
        """
        base = base_name(type.ada)
        node = gir.enums[ctype]

        current = 0
        is_default_representation = True

        gir.bound.add(ctype)

        members = []
        ignore = set(ignore.split())

        for member in node.findall(nmember):
            cname = member.get(cidentifier)

            if cname in ignore:
                continue

            m = naming.adamethod_name(cname, warning_if_not_found=False)
            if m is None:
                continue

            if cname == m:
                # No special case ? Attempt a simple rule (remove leading
                # Gtk prefix, and capitalize the value)
                m = cname.replace(prefix, "").title()

            else:
                m = base_name(m)

            # For proper substitution in docs
            naming.add_cmethod(
                cname=cname,
                adaname="%s.%s" % (self.name, m))

            value = int(member.get("value"))
            if value != current:
                is_default_representation = False
            current += 1

            members.append((m, value))

        decl = ""

        if node.tag == nenumeration and not asbitfield:
            section.add(
                "type %s is " % base
                + "(\n" + ",\n".join(m[0]
                                     for m in sorted(members, key=lambda m: m[1]))
                + ");\n"
                + "pragma Convention (C, %s);\n" % base)
            section.add(Code(_get_clean_doc(node), iscomment=True))

            if not is_default_representation:
                repr = ("   for %s use (\n" % base
                        + ",\n".join("      %s => %s" % m
                                     for m in sorted(members, key=lambda m: m[1]))
                        + ");\n")
                section.add(repr)

        elif node.tag == nbitfield or asbitfield:
            section.add(
                "\ntype %s is " % base
                + "mod 2 ** Integer'Size;\n"
                + "pragma Convention (C, %s);\n" % base)
            section.add(Code(_get_clean_doc(node), iscomment=True))

            for m, value in members:
                decl += "%s : constant %s := %s;\n" % (m, base, value)
            section.add(decl)

        section.pkg.section("Enumeration Properties").add(
            "package %s_Properties is\n" % base
            + "   new Generic_Internal_Discrete_Property (%s);\n" % base
            + "type Property_%s is new %s_Properties.Property;\n\n" % (
                base, base))

        self.pkg.add_with("Glib.Generic_Properties")

    def generate(self, gir):
        if self._generated:
            return

        extra = self.gtkpkg.extra()
        if extra:
            self.node.extend(extra)

        # The parent is unfortunately specified as a GIR name. But that creates
        # ambiguities when loading both Gtk and Gdk, which for instance both
        # define "Window". So we first look in the same file as the current
        # Class.

        parent = gir._get_class_node(
            self.rootNode,
            girname=self.gtkpkg.parent_type() or self.node.get("parent"))
        parent = naming.type(
            name=self.gtkpkg.parent_type() or self.node.get(
                "parent"),  # GIRName
            cname=parent and parent.get(ctype_qname)).ada

        if parent and parent.rfind(".") != -1:
            self._subst["parent_pkg"] = package_name(parent)
            self._subst["parent"] = base_name(parent)
        else:
            self._subst["parent_pkg"] = None
            self._subst["parent"] = parent

        self._generated = True

        girdoc = _get_clean_doc(self.node)

        into = self.gtkpkg.into()
        if into:
            # Make sure we have already generated the other package, so that
            # its types go first.
            klass = gir.classes.get(into, None) or gir.ctype_interfaces[into]
            klass.generate(gir)
            into = klass.name  # from now on, we want the Ada name

            # Do not integrate the documentation from the other package, it
            # is likely useless anyway

            girdoc = ""

            # Override the type exception. For instance, from now on we
            # want to use "Gtk.Box.Gtk_HBox" rather than "Gtk.HBox.Gtk_HBox"
            # which doesn't exist
            typename = "%s.%s" % (into, self._subst["typename"])

            # retrieve the old type
            oldtype = naming.type(cname=self.ctype)
            newtype = None
            # and create the new one accordingly
            if isinstance(oldtype, Tagged):
                newtype = Tagged(typename)
            elif isinstance(oldtype, GObject):
                newtype = GObject(typename)
            elif isinstance(oldtype, Proxy):
                newtype = Proxy(typename)
            elif isinstance(oldtype, Record):
                newtype = Record(typename)
            else:
                raise Exception("Can't override %s for a package with 'into'"
                                % oldtype)
            naming.add_type_exception(
                override=True,
                cname=self.ctype,
                type=newtype)

        self.pkg = gir.get_package(into or self.ada_package_name,
                                   ctype=self.ctype,
                                   doc=girdoc)

        if self._subst["parent_pkg"]:
            self.pkg.add_with("%(parent_pkg)s" % self._subst)

        self.gtktype = self.gtkpkg.get_type(self._subst["typename"])

        section = self.pkg.section("")

        if self.gtkpkg.is_obsolete():
            section.add("pragma Obsolescent;")

        if not self.has_toplevel_type:
            pass

        elif self.is_interface:
            self.pkg.add_with("Glib.Types")
            section.add(
                """type %(typename)s is new Glib.Types.GType_Interface;
Null_%(typename)s : constant %(typename)s;""" % self._subst)

            self.pkg.add_private("""
Null_%(typename)s : constant %(typename)s :=
   %(typename)s (Glib.Types.Null_Interface);""" %
                                 self._subst)

        elif self.gtktype.is_subtype():
            section.add(
                """
subtype %(typename)s_Record is %(parent)s_Record;
subtype %(typename)s is %(parent)s;""" % self._subst)

        elif self.is_proxy:
            section.add("""
   type %(typename)s is new Glib.C_Proxy;""" % self._subst)

        elif self.is_boxed:
            # The type is not private so that we can directly instantiate
            # generic packages for lists of this type.

            section.add("""
   type %(typename)s is new Glib.C_Boxed with null record;
   Null_%(typename)s : constant %(typename)s;

   function From_Object (Object : System.Address) return %(typename)s;
   function From_Object_Free (B : access %(typename)s'Class) return %(typename)s;
   pragma Inline (From_Object_Free, From_Object);
""" % self._subst)

            # Insert constant declaration at the end of the package, to avoid
            # freezing issues
            self.pkg.add_private(
                ("\n   Null_%(typename)s : constant %(typename)s :="
                + " (Glib.C_Boxed with null record);\n") %
                self._subst, at_end=True)

            section.add("""
   function From_Object_Free
      (B : access %(typename)s'Class) return %(typename)s
   is
      Result : constant %(typename)s := %(typename)s (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return %(typename)s is
      S : %(typename)s;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;
""" % self._subst, in_spec=False)

        elif self._subst["parent"] is None:
            # Likely a public record type (ie with visible fields).
            # Automatically add it to the list of records to bind.

            self.gtkpkg.add_record_type(self.ctype)

        else:
            section.add("""
type %(typename)s_Record is new %(parent)s_Record with null record;
type %(typename)s is access all %(typename)s_Record'Class;"""
                        % self._subst)

        for ctype, enum, prefix, asbitfield, ignore in \
             self.gtkpkg.enumerations():

            self.enumeration_binding(
                section, ctype, enum, prefix, asbitfield=asbitfield,
                ignore=ignore)

        for regexp, prefix in self.gtkpkg.constants():
            self.constants_binding(section, regexp, prefix)

        for ctype, enum, adaname, fields, unions, private in \
             self.gtkpkg.records():

            self.record_binding(
                section, ctype, adaname, enum, fields, unions, private)

        for ada, ctype, single, sect_name in self.gtkpkg.lists():
            sect = self.pkg.section(sect_name)
            self.add_list_binding(sect, ada, ctype, single)

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
                    section.add(p.text)
                elif p.tag == "body":
                    if p.get("before", "true").lower() == "true":
                        # Go before the body of generated subprograms, so that
                        # we can add type definition
                        section.add(p.text, in_spec=False)

        self._constructors()
        self._method_get_type()
        self._methods()

        if extra:
            s = None
            for p in extra:
                if p.tag == "spec":
                    if p.get("private", "False").lower() == "true":
                        self.pkg.add_private(p.text, at_end=True)
                    else:
                        s = s or self.pkg.section("GtkAda additions")
                        s.add(p.text)
                elif p.tag == "body" \
                        and p.get("before", "true").lower() != "true":
                    s.add(p.text, in_spec=False)

        self._functions()
        self._globals()
        self._fields()

        self._virtual_methods()

        if not into and self.name != "Gtk.Widget":
            self._implements()
        self._properties()
        self._signals()


# Set up and invoke our command line parser
parser = OptionParser()
parser.add_option(
    "--gir-file",
    help="input GTK .gir file",
    action="append",
    dest="gir_file",
    metavar="FILE")
parser.add_option(
    "--xml-file",
    help="input GtkAda binding.xml file",
    dest="xml_file",
    metavar="FILE")
parser.add_option(
    "--ada-output",
    help="Ada language output file",
    dest="ada_outfile",
    metavar="FILE")
parser.add_option(
    "--c-output",
    help="C language output file",
    dest="c_outfile",
    metavar="FILE")
(options, args) = parser.parse_args()

# Command line argument sanity checking: make sure that all of our
# inputs and outputs are specified.
missing_files = []
if options.gir_file is None:
    missing_files.append("GIR file")
if options.xml_file is None:
    missing_files.append("binding.xml file")
if options.ada_outfile is None:
    missing_files.append("Ada output file")
if options.c_outfile is None:
    missing_files.append("C output file")
if missing_files:
    parser.error('Must specify files:\n\t' + ', '.join(missing_files))

gtkada = GtkAda(options.xml_file)
gir = GIR(options.gir_file)

Package.copyright_header = \
    """------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------
"""

gir.ccode = \
    """/*****************************************************************************
 *               GtkAda - Ada95 binding for Gtk+/Gnome                       *
 *                                                                           *
 *   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet           *
 *                     Copyright (C) 2000-2022, AdaCore                      *
 *                                                                           *
 * This library is free software;  you can redistribute it and/or modify it  *
 * under terms of the  GNU General Public License  as published by the Free  *
 * Software  Foundation;  either version 3,  or (at your  option) any later  *
 * version. This library is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-  *
 * TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                             *
 *                                                                           *
 * As a special exception under Section 7 of GPL version 3, you are granted  *
 * additional permissions described in the GCC Runtime Library Exception,    *
 * version 3.1, as published by the Free Software Foundation.                *
 *                                                                           *
 * You should have received a copy of the GNU General Public License and     *
 * a copy of the GCC Runtime Library Exception along with this program;      *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see     *
 * <http://www.gnu.org/licenses/>.                                           *
 *
 *****************************************************************************

This file is automatically generated from the .gir files
*/
#include <gtk/gtk.h>
"""


for the_ctype in enums:
    node = Element(
        nclass,
        {ctype_qname: the_ctype})
    root = Element(nclass)

    cl = gir._create_class(rootNode=root, node=node,
                           identifier_prefix='',
                           is_interface=False,
                           is_gobject=False,
                           has_toplevel_type=False)
    cl.generate(gir)

for name in interfaces:
    if name.startswith("--"):
        gir.bound.add(name[2:])
        continue

    gir.interfaces[name].generate(gir)
    gir.bound.add(name)

for the_ctype in binding:
    if the_ctype.startswith("--"):
        gir.bound.add(the_ctype[2:])
        continue

    try:
        e = gir.classes[the_ctype]
    except KeyError:
        cl = gtkada.get_pkg(the_ctype)
        if not cl:
            raise
        node = Element(nclass, {ctype_qname: the_ctype})
        e = gir.classes[the_ctype] = gir._create_class(
            rootNode=root, node=node, is_interface=False,
            identifier_prefix='')

    e.generate(gir)
    gir.bound.add(the_ctype)


with open(options.ada_outfile, "wb") as out:
    with open(options.c_outfile, "wb") as cout:
        gir.generate(out, cout)

gir.show_unbound()
