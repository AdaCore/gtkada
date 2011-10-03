#!/usr/bin/env python

"""
Various formatting classes for Ada code
"""

import sys
import re
import copy
from collections import namedtuple, defaultdict


class CType(object):
    """Describes the types in the various cases where they can be used.

       A type applies either to an Ada subprogram written in Ada, or to an
       Ada subprogram implemented via a pragma Import. The latter case is
       abbreviated to a "c" subprogram below.

       For returned values, various pieces of information are needed:
              (adatype, ctype, converter, tmpvars=[])
       They are used as:
              function ... (...) return adatype is
                  function ... (...) return ctype;
                  pragma Import (C, ..., "...");
                  Tmp : ctype;
                  tmpvars;
              begin
                  ...;   --  pre-call code
                  Tmp := ... (...);
                  ...;   --  post-call code
                  return <converter % Tmp>
              end;
       In the example above, "Tmp" is only created if there is some post-call
       code, otherwise we avoid the temporary variable.
       The variable "Tmp" is automatically added, and does not need to
       be specified manually in tmpvars.

       if converted contains the string "%(tmp)s", then we always use a
       temporary variable of type adatype. This is used for instance when the
       variable is initialized through a procedure call rather than a function
       call.
              function ... (...) return adatype is
                  function ... (...) return ctype;
                  pragma Import (C, ..., "...")
                  Tmp_Result : adatype;
                  tmpvars;
              begin
                  ...   --  pre-call code
                  convert % {"var":..., "tmp":"Tmp_Result"};  -- proc call
                  ...   --  post-call code
                  return Tmp_Result;
              end;
       The variable "Tmp_Result" is automatically added, and does not need to
       be specified manually in tmpvars.

       Converter will contain a single %s which will be replaced by the
       name of the temporary variable that holds the result of the call
       to the function.
    """

    def __init__(self, ada, property):
        self.ada = ada       # Fully qualified Ada type
        self.property = property

        self.is_ptr = False
        self.param = ada     # type as parameter
        self.cparam = ada    # type for Ada subprograms binding to C
        self.returns = (self.param, self.cparam, "%(var)s", [])
        self.convert = "%(var)s"  # Convert from Ada parameter to C parameter
                             # If it used %(tmp)s, we assume the converter sets
                             # the value of the temporary variable itself.
                             # Otherwise, it is used as " Tmp := <convert>"
        self.postconvert = "%(var)s" # Convert from C to Ada value
                             # %(var)s is the value to convert
        self.cleanup = None  # If set, a tmp variable is created to hold the
                             # result of convert during the call, and is then
                             # free by calling this cleanup. Use "%s" as the
                             # name of the variable.
        self.isArray = False

    def as_property(self):
        """The type to use for the property"""
        return self.property

    def as_return(self, lang="ada"):
        """See CType documentation for a description of the returned tuple"""
        return self.returns

    def as_ada_param(self):
        """Converts self to a description for an Ada parameter to a
           subprogram.
        """
        return self.param

    def as_c_param(self):
        """Returns the C type (as a parameter to a subprogram that imports
           a C function)
        """
        return self.cparam

    def as_call(self, name, wrapper="%s", lang="ada", mode="in"):
        """'name' represents a parameter of type 'self'.
           'wrapper' is used in the call itself, and %s is replaced by the
              name of the variable (or the temporary variable).
           Returns an instance of VariableCall.
        """
        if lang == "ada":
            return VariableCall(
                call=wrapper % name, precall='', postcall='', tmpvars=[])

        elif self.postconvert != "%(var)s" and mode != "in":
            # An "out" parameter for an enumeration requires a temporary
            # variable: Internal(Enum'Pos (Param)) is invalid
            tmp = "Tmp_%s" % name
            return VariableCall(
                call=wrapper % tmp,
                precall="",
                postcall='%s := %s;' % (name, self.postconvert % {"var":tmp}),
                tmpvars=[Local_Var(name=tmp, type=self.cparam)])

        elif "%(tmp)" in self.convert:
            # The conversion sets the temporary variable itself
            tmp = "Tmp_%s" % name
            return VariableCall(
                call=wrapper % tmp,
                precall=self.convert % {"var":name, "tmp":tmp},
                postcall=self.cleanup % tmp,
                tmpvars=[Local_Var(name=tmp, type=self.cparam)])

        elif self.cleanup:
            tmp = "Tmp_%s" % name
            conv = self.convert % {"var":name}

            # Initialize the temporary variable with a default value, in case
            # it is an unconstrained type (a chars_ptr_array for instance)
            return VariableCall(
                call=wrapper % tmp,
                precall='',
                postcall=self.cleanup % tmp,
                tmpvars=[Local_Var(
                    name=tmp, type=AdaType(self.cparam), default=conv)])

        else:
            conv = self.convert % {"var":name}
            return VariableCall(
                call=wrapper % conv,
                precall='', postcall='', tmpvars=[])

    def direct_cmap(self):
        """Whether the parameter can be passed as is to C"""
        return self.convert == "%(var)s"

    def add_with(self, pkg=None, specs=True):
        """Add required withs for this type"""
        if pkg and "." in self.ada:
            pkg.add_with(self.ada[0:self.ada.rfind(".")], specs=specs)

    def copy(self, **kwargs):
        """Return a copy of self, possibly modifying some properties.
           kwargs is interpreted by each child class.
        """
        return copy.deepcopy(self)


class Enum(CType):
    def __init__(self, ada, property=None):
        base = ada[ada.rfind(".") + 1:] or ada
        if property is None:
            CType.__init__(self, ada, "Gtk.Enums.Property_%s" % base)
        else:
            CType.__init__(self, ada, property)

        self.cparam = "Integer"
        self.convert = "%s'Pos (%%(var)s)" % ada
        self.postconvert = "%s'Val (%%(var)s)" % ada
        self.returns = (ada, self.cparam, self.postconvert, [])


class GObject(CType):
    def __init__(self, ada):
        CType.__init__(self, ada, "Glib.Properties.Property_Object")
        self.cparam = "System.Address"
        self.convert = "Get_Object_Or_Null (GObject (%(var)s))"
        self.is_ptr = False

        if ada == "Glib.Object.GObject":
            convert = "Get_User_Data (%(var)s, Stub)"
        else:
            convert = "%s (Get_User_Data (%%(var)s, Stub))" % ada

        self.returns = (
            self.param, self.cparam, convert,
            [Local_Var("Stub", AdaType("%s_Record" % ada, in_spec=False))])

    def copy(self, **kwargs):
        result = CType.copy(self)
        if "userecord" in kwargs and kwargs["userecord"]:
            result.param = "access %s_Record'Class" % self.ada
        return result


class UTF8(CType):
    def __init__(self, empty_maps_to_null):
        CType.__init__(self, "UTF8_String", "Glib.Properties.Property_String")
        self.cparam = "Interfaces.C.Strings.chars_ptr"

        if empty_maps_to_null:
            self.convert = 'if %(var)s = "" then %(tmp)s := Interfaces.C.Strings.Null_Ptr; else %(tmp)s := New_String (%(var)s); end if;'
        else:
            self.convert = "New_String (%(var)s)"

        self.cleanup = "Free (%s);"
        self.returns = (
            self.param, self.cparam,
            "Interfaces.C.Strings.Value (%(var)s)", [])

    def add_with(self, pkg=None, specs=True):
        if pkg:
            pkg.add_with("Interfaces.C.Strings", specs=False)


class UTF8_List(CType):
    def __init__(self):
        CType.__init__(self, "GNAT.Strings.String_List", "")
        self.cparam = "Interfaces.C.Strings.chars_ptr_array"
        self.convert = "From_String_List (%(var)s)"
        self.cleanup = "GtkAda.Types.Free (%s)"
        self.returns = (
            self.param, "chars_ptr_array_access",
            "To_String_List (%(var)s.all)", [])

    def add_with(self, pkg=None, specs=True):
        if pkg:
            pkg.add_with("GNAT.Strings", specs=specs)
            pkg.add_with("Gtkada.Types", specs=False)
            pkg.add_with("Interfaces.C.Strings", specs=False)
            pkg.add_with("Gtkada.Bindings", specs=False)


class Proxy(CType):
    def __init__(self, ada, property=None):
        if property is None:
            CType.__init__(self, ada, "Glib.Properties.Property_Boxed")
        else:
            CType.__init__(self, ada, property)


class Interface(CType):
    def __init__(self, ada):
        CType.__init__(self, ada, "")
        self.cparam = ada
        self.convert = "%(var)s"
        self.is_ptr = False
        self.returns = (self.param, self.cparam, "%(var)s", [])


class List(CType):
    def __init__(self, ada):
        CType.__init__(self, ada, "Glib.Properties.Property_Object")
        adapkg = ada[:ada.rfind(".")]
        self.cparam = "System.Address"
        self.convert = "%s.Get_Object (%%(var)s)" % adapkg
        self.is_ptr = False
        self.returns = (   # Use %(tmp)s so forces the use of temporary var.
           self.param, self.cparam,
            "%s.Set_Object (%%(tmp)s, %%(var)s)" % adapkg, [])

    def add_with(self, pkg=None, specs=True):
        # A list comes from an instantiation (pkg.instance.glist), so we need
        # to skip backward two "."
        if pkg:
            p = self.ada.rfind(".")
            if p != -1:
                p = self.ada[:p].rfind(".")
                if p != -1:
                    pkg.add_with(self.ada[:p], specs=specs)


class AdaType(CType):
    def __init__(self, adatype, pkg=None, in_spec=True, ctype="",
                 convert="%(var)s"):
        """The 'adatype' type is represented as 'ctype' for subprograms
           that import C functions. The parameters of that type are converted
           from Ada to C by using 'convert'. 'convert' must use '%s' once
           to indicate where the name of the parameter should go
        """
        CType.__init__(self, adatype, "")
        self.param   = adatype
        self.cparam  = ctype or adatype
        self.returns = (self.param, self.cparam, "%(var)s", [])
        self.convert = convert
        self.postconvert = "%(var)s"
        self.cleanup = None
        self.is_ptr  = adatype.startswith("access ")

        if pkg:
            self.add_with(pkg, specs=in_spec)


class AdaNaming(object):
    def __init__(self):
        self.cname_to_adaname = {}  # c methods to Ada subprograms
        self.girname_to_ctype = {}  # gir names to C types
        self.exceptions = {}        # naming exceptions
        self.type_exceptions = {}   # C types to CType instances

    def add_type_exception(self, cname, type, override=False):
        """Declares a new type exception, unless there already existed
           one for that cname.
        """
        assert(isinstance(type, CType))
        if override or cname not in self.type_exceptions:
            self.type_exceptions[cname] = type

    def add_cmethod(self, cname, adaname):
        """Register the mapping from c method's name to Ada subprogram.
           This is used to replace C function names in the documentation
           with their Ada equivalent"""
        self.cname_to_adaname[cname] = adaname

    def add_girname(self, girname, ctype):
        """Maps a GIR's "name" attribute to its matching C type.
           This is used to resolve such names in the documentation and in
           properties types.
        """
        self.girname_to_ctype[girname] = ctype

    def ctype_from_girname(self, girname):
        """Return the C type corresponding to a GIR name"""
        return self.girname_to_ctype.get(girname, "Gtk%s" % girname)

    def adamethod_name(self, cname):
        """Return the ada name corresponding to the C method's name"""
        try:
            return self.cname_to_adaname[cname]
        except KeyError:
            if cname.startswith("gtk_"):
                print "Function quoted in doc has no Ada binding: %s" % cname
            self.cname_to_adaname[cname] = cname  # Display warning once only
            return cname

    def case(self, name):
        """Return the proper casing to use for 'name', taking keywords
           into account. This is for packages.
        """
        name = name.replace("-", "_").title()
        if name.endswith("_"):
            name = name[:-1]

        return self.exceptions.get(name, name)

    def __full_type_from_girname(self, girname):
        """Return the type description from a GIR name"""
        return self.type_exceptions.get(
            girname,  # First try GIR name as is in the table (gint, ...)
            self.type_exceptions.get(
                self.ctype_from_girname(girname), # Else the C type
                Proxy(girname)))  # Else return the GIR name itself

    def type(self, name, cname=None, pkg=None, isArray=False,
             allow_access=True, empty_maps_to_null=False):
        """Build an instance of CType for the corresponding cname.
           A type a described in a .gir file
           'pkg' is an instance of Package, to which extra
           with clauses will be added if needed.
           'isArray' should be true for an array of the simple type 'name'.
           'allow_access' should be True if the parameter can be represented
           as 'access Type', rather than an explicit type, in the case of
           GObject descendants.
           If `empty_maps_to_null' is True, then an empty string maps to a
           NULL pointer in C, rather than an empty C string. For a GObject,
           this means the parameter should not be passed with mode="access",
           since the user is allowed to pass null.
        """

        if cname == "gchar**" or name == "array_of_utf8":
            t = UTF8_List()
        elif name == "utf8":
            t = UTF8(empty_maps_to_null=empty_maps_to_null)
        elif cname:
            # Check whether the C type, including trailing "*", maps
            # directly to an Ada type.
            t = self.__full_type_from_girname(cname)
            is_ptr = False

            if t.ada[-1] == "*":
                # No, try without the trailing "*"
                t = self.__full_type_from_girname(cname[0:-1])

                if t.ada[-1] != "*":
                    is_ptr = True    # Yes, so we had a pointer
                else:
                    basename = cname[0:-1] # Remove all "*"
                    if basename[-1] == "*":
                        basename = basename[0:-1]
                    t = self.__full_type_from_girname(basename)

            if not isinstance(t, GObject):
                t.is_ptr = is_ptr
        else:
            t = self.__full_type_from_girname(name)
            t.is_ptr = cname and cname[-1] == '*'

        t = t.copy(userecord=not empty_maps_to_null
                   and cname and allow_access
                   and not cname.endswith("**"))
        t.add_with(pkg, specs=True)
        t.isArray = isArray
        return t


naming = AdaNaming()


def max_length(iter):
    """Return the length of the longuest element in iter"""
    longuest = 0
    for f in iter:
        longuest = max(longuest, len(f))
    return longuest


def fill_text(text, prefix, length, firstLineLength=0):
    """Split TEXT on several lines (with a given max length and a prefix).
    """

    line = ""
    result = []
    maxLen = firstLineLength or length - len(prefix)

    text=text.replace("\n\n", "\n<br>")

    for w in text.split():  # for each word (this loses whitespaces)
        if w.startswith("<br>"):
            result.append(line)
            maxLen = length - len(prefix)
            line = w[4:]

        elif len(line) + len(w) + 1 > maxLen:
            result.append(line)
            maxLen = length - len(prefix)
            line = w

        elif w:
            line += " " + w

    if line != "":
        result.append(line)
    return ("\n" + prefix).join(result)

def cleanup_doc(doc):
    """Replaces C features in the doc with appropriate Ada equivalents"""

    doc = doc.replace("%NULL", "null")
    doc = doc.replace("%TRUE", "True")
    doc = doc.replace("%FALSE", "False")

    doc = doc.replace("<para>", "")
    doc = doc.replace("</para>", "")
    doc = doc.replace("<note>", "\nNote: ")
    doc = doc.replace("</note>", "")

    subp = re.compile("([\S_]+)\(\)")
    doc = subp.sub(lambda x: naming.adamethod_name(x.group(1)), doc)

    types = re.compile("#([\w_]+)")
    doc = types.sub(lambda x: naming.type(x.group(1)).ada, doc)

    params = re.compile("@([\w_]+)")
    doc = params.sub(lambda x: x.group(1).title(), doc)

    return doc


def box(name, indent="   "):
    return indent + "-" * (len(name) + 6) + "\n" \
            + indent + "-- " + name + " --\n" \
            + indent + "-" * (len(name) + 6)


def indent_code(code, indent=3):
    """Return code properly indented and split on several lines.
       These are heuristics only, not perfect.
    """
    body = code.strip()
    if not body:
        return ""

    # Add newlines where needed, but preserve existing blank lines
    body = re.sub(";(?!\s*\n)", ";\n", body)
    body = re.sub("(?<!and )then(?!\s*\n)", "then\n", body)
    body = re.sub("(?<!or )else(?!\s*\n)", "else\n", body)
    body = re.sub("declare", "\ndeclare", body)
    body = re.sub("\n\s*\n+", "\n\n", body)

    parent_count = 0
    result = ""

    for l in body.splitlines():
        if l.find("--") != -1:
            eol_comment = l[l.find("--"):].strip()
            l = l[:l.find("--")]
        else:
            eol_comment = ""

        l = l.strip()

        if l.startswith("end") \
           or l.startswith("elsif")  \
           or l.startswith("else")  \
           or l.startswith("begin"):
            indent -= 3

        old_parent = parent_count
        parent_count = parent_count + l.count("(") - l.count(")")

        if not l:
            if eol_comment:
                result += " " * indent

        elif l[0] == '(':
            result += " " * (indent + 2)
            if parent_count > old_parent:
                indent += (parent_count - old_parent) * 3
        elif not old_parent:
            result += " " * indent
            if parent_count > old_parent:
                indent += (parent_count - old_parent) * 3
        else:
            if parent_count > old_parent:
                indent += (parent_count - old_parent) * 3
            result += " " * indent

        if old_parent > parent_count:
            indent -= (old_parent - parent_count) * 3

        result += l + eol_comment + "\n"

        if(l.endswith("then") and not l.endswith("and then")) \
           or l.endswith("loop") \
           or(l.endswith("else") and not l.endswith("or else"))\
           or l.endswith("begin") \
           or l.endswith("record") \
           or l.endswith("is") \
           or l.endswith("declare"):
            indent += 3

    return result


# The necessary setup to use a variable in a subprogram call. The returned
# values map to the following Ada code:
#   declare
#      $(tmpvars)    # A list of LocalVar
#   begin
#      $(precall)
#      Call ($(call), ...)
#      #(postcall)
#   end;
# and are used in case temporary variables are needed. If not, only 'call'
# will have a non-null value

VariableCall = namedtuple('VariableCall',
                          ['call', 'precall', 'postcall', 'tmpvars'])

class Local_Var(object):
    __slots__ = ["name", "type", "default", "aliased"]

    def __init__(self, name, type, default="", aliased=False):
        if isinstance(type, str):
            self.type = AdaType(type)
        else:
            self.type = type

        self.name = name
        self.default = default
        self.aliased = aliased

    def _type(self, lang):
        if isinstance(self.type, CType):
            if lang == "ada":
                return self.type.as_ada_param()
            elif lang == "c":
                return self.type.as_c_param()
        return self.type

    def spec(self, length=0, lang="ada"):
        """Format the declaration for the variable or parameter.
           'length' is the minimum length that the name should occupy (for
           proper alignment when there are several variables.
        """
        t = self._type(lang)
        aliased = ""
        if self.aliased:
            aliased = "aliased "

        if self.default:
            return "%-*s : %s%s := %s" % (
                length, self.name, aliased, t, self.default)
        else:
            return "%-*s : %s%s" % (length, self.name, aliased, t)

    def as_call(self, lang="ada", mode="in"):
        """Pass 'self' as a parameter to an Ada subprogram call, implemented
           in the given language.
           Returns an instance of VariableCall
        """
        wrapper = "%s"
        n = self.name
        if mode == "access_c":
            mode = "access"
            wrapper="%s'Access"

        if isinstance(self.type, CType):
            return self.type.as_call(n, lang=lang, mode=mode, wrapper=wrapper)
        else:
            return VariableCall(call=n, precall='', postcall='', tmpvars=[])


class Parameter(Local_Var):
    __slots__ = ["name", "type", "default", "aliased", "mode", "doc"]

    def __init__(self, name, type, default="", doc="", mode="in"):
        """A mode "access_c" indicates an "access" parameter for which
           calls will use a 'Access.
        """
        super(Parameter, self).__init__(name, type, default)
        self.mode = mode
        self.doc  = doc

    def _type(self, lang):
        t = super(Parameter, self)._type(lang)
        mode = self.mode
        if self.mode == "access_c":
            mode = "access"

        if mode != "in":
            return "%s %s" % (mode, t)
        return t

    def as_call(self, lang="ada"):
        return super(Parameter, self).as_call(lang, mode=self.mode)

    def direct_cmap(self):
        """Whether the parameter can be passed as is to C"""
        return self.type.direct_cmap()


max_profile_length = 79 - len(" is")


class Subprogram(object):
    """An Ada subprogram that we are generating"""

    def __init__(self, name, code="", plist=[], local_vars=[],
                 returns=None, doc=[], showdoc=True):
        """Create a new subprogram.
           'plist' is a list of Parameter.
           'local_vars' is a list of Local_Var.
           'doc' is a string or a list of paragraphs.
           'code' can be the empty string, in which case no body is output.
           The code will be automatically pretty-printed, and the appropriate
           pragma Unreferenced are also added automatically.
        """
        assert(returns is None or isinstance(returns, CType))
        self.name = naming.case(name)
        self.plist = plist
        self.returns = returns
        self.local = local_vars
        self.showdoc = showdoc
        self._import = None
        self._nested = []  # nested subprograms
        self._deprecated = (False, "") # True if deprecated
        self._manual_body = None  # Written by user explicitly

        self.lang = "ada"  # Language for the types of parameters

        if code and code[-1] != ";":
            self.code = code + ";"
        else:
            self.code = code

        if isinstance(doc, list):
            self.doc = doc
        else:
            self.doc = [doc]

    def import_c(self, cname):
        """Declares that 'self' is implemented as a pragma Import.
           This returns 'self' so that it can be chained:
              s = Subprogram(...).import_c('...')
        """
        self._import = 'pragma Import (C, %s, "%s");' % (self.name, cname)
        return self

    def set_param_lang(self, lang):
        """Set the language to use when printing the types of parameters.
           If "c", prints the C type corresponding to the "ada" types.
        """
        self.lang = lang

    def mark_deprecated(self, msg):
        """Mark the subprogram as deprecated"""

        self._deprecated = (True, msg)

    def add_nested(self, *args):
        """Add some nested subprograms"""
        for subp in args:
            self._nested.append(subp)
        return self

    def set_body(self, body):
        self._manual_body = body

    def _profile(self, indent="   ", lang="ada", maxlen=max_profile_length):
        """Compute the profile for the subprogram"""

        returns = self.returns and self.returns.as_return(lang)

        if returns:
            prefix = "function"

            if self.lang == "c":
                suffix = " return %s" % returns[1]
            else:
                suffix = " return %s" % returns[0]
        else:
            prefix = "procedure"
            suffix = ""

        if self.name:
            prefix = indent + prefix + " " + self.name
        else:
            prefix = "access %s" % prefix

        if self.plist:
            # First test: all parameters on same line
            plist = [p.spec(lang=lang) for p in self.plist]
            p = " (" + "; ".join(plist) + ")"

            # If too long, split on several lines
            if len(p) + len(prefix) + len(suffix) > maxlen:
                max = max_length([p.name for p in self.plist])
                plist = [p.spec(max, lang=lang) for p in self.plist]
                p = "\n   " + indent + "(" \
                    + (";\n    " + indent).join(plist) + ")"

        else:
            p = ""

        # Should the "return" go on a separate line ?
        if p and len(p.splitlines()[-1]) + len(suffix) > maxlen:
            return prefix + p + "\n   " + indent + suffix
        else:
            return prefix + p + suffix

    def spec(self, indent="   ", show_doc=True, maxlen=max_profile_length):
        """Return the spec of the subprogram"""

        if self.showdoc and show_doc:
            doc = [cleanup_doc(d) for d in self.doc]
            if self._deprecated[0]:
                doc += [cleanup_doc(self._deprecated[1])]
            doc += [cleanup_doc(p.doc) for p in self.plist]
        else:
            doc = []

        result = self._profile(indent, lang=self.lang, maxlen=maxlen) + ";"

        if self._import:
            result += "\n" + indent + self._import

        if self._deprecated[0]:
            result += "\n" + indent + "pragma Obsolescent (%s);" % self.name

        for d in doc:
            if d:
                if d.startswith("%PRE%"):
                    d = d[5:]
                    lines = ["\n" + indent + "-- " + l for l in d.split("\n")]
                    result += "".join(lines)
                else:
                    result += "\n" + indent + "-- "
                    result += fill_text(d, indent + "--  ", 79)

        return result

    def _find_unreferenced(self, local_vars="", indent="   "):
        """List the pragma Unreferenced statements that are needed for this
           subprogram.
        """
        unreferenced = []
        for p in self.plist:
            if not re.search(
               r'\b%s\b' % p.name, self.code + local_vars, re.IGNORECASE):
                unreferenced.append(p.name)

        if unreferenced:
            return indent + "   pragma Unreferenced (%s);\n" % (
                ", ".join(unreferenced))
        else:
            return ""

    def _format_local_vars(self, indent="   "):
        """The list of local variable declarations"""
        if self.local:
            max = max_length([p.name for p in self.local])
            result = [v.spec(max) for v in self.local]
            return indent + "   " + (";\n   " + indent).join(result) + ";\n"
        else:
            return ""

    def body(self, indent="   "):
        if self._manual_body:
            return self._manual_body

        if not self.code:
            return ""

        result = box(self.name) + "\n\n"
        profile = self._profile(lang=self.lang)
        result += profile

        if profile.find("\n") != -1:
            result += "\n   is\n"
        else:
            result += " is\n"

        local = self._format_local_vars(indent=indent)
        result += self._find_unreferenced(local_vars=local, indent=indent)

        for s in self._nested:
            result += s.spec(indent=indent + "   ") + "\n"
            result += s.body(indent=indent + "   ")

        result += local
        result += indent + "begin\n"
        result += indent_code(self.code, indent=6)
        return result + indent + "end %s;\n" % self.name

    def call(self, in_pkg="", extra_postcall=""):
        """A call to 'self'.
           The parameters that are passed to self are assumed to have the
           same name as in self's declaration. When 'self' is implemented
           as a pragma Import, proper conversions are done.
           'in_pkg' is used to fully qualify the name of the subprogram, to
           avoid ambiguities. This is optional. This can be either a string
           or an instance of Package.

           Returned value is a tuple:
               ("code", "variable_for_return", tmpvars=[])
           where "code" is the code to execute for the call, including
           creation of temporary variables, and "variable_for_return" is
           either None, or the code to get the result of the subprogram.
           So a call is:
               declare
                  tmp_vars;
               begin
                  code;
                  extra_postcall;
                  return variable_for_return;  --  Omitted for procedures
               end;
        """

        if self._import:
            lang = "c"
        else:
            lang = "ada"

        tmpvars  = []
        precall  = ""
        params   = []
        postcall = extra_postcall

        for arg in self.plist:
            c = arg.as_call(lang)   # An instance of VariableCall
            params.append(c.call)
            tmpvars.extend(c.tmpvars)
            precall += c.precall
            postcall += c.postcall

        if params:
            call = "%s (%s)" % (self.name, ", ".join(params))
        else:
            call = self.name

        if in_pkg:
            if isinstance(in_pkg, Package):
                call = "%s.%s" % (in_pkg.name, call)
            else:
                call = "%s.%s" % (in_pkg, call)

        returns = self.returns and self.returns.as_return(lang)
        if returns is not None:
            if lang == "c":
                tmpvars.extend(returns[3])
                if "%(tmp)s" in returns[2]:
                    # Result of Internal is used to create a temp. variable,
                    # which is then returned. This variable has the same type
                    # as the Ada type (not necessarily same as Internal)
                    tmpvars.append(Local_Var("Tmp_Return", returns[0]))
                    call = returns[2] % {"var":call, "tmp":"Tmp_Return"}
                    return ("%s%s;%s" % (precall, call, postcall),
                            "Tmp_Return",
                            tmpvars)

                elif postcall:
                    tmpvars.append(Local_Var("Tmp_Return", returns[1]))
                    call = "Tmp_Return := %s" % call
                    return ("%s%s;%s" % (precall, call, postcall),
                            returns[2] % {"var":"Tmp_Return"},
                            tmpvars)
                else:
                    # No need for a temporary variable
                    return (precall, returns[2] % {"var":call}, tmpvars)
            else:
                if postcall:
                    # We need to use a temporary variable, since there are
                    # cleanups to perform. This will not work if the function
                    # returns an unconstrained array though.
                    tmpvars.append(Local_Var("Tmp_Return", returns[0]))
                    call = "Tmp_Return := %s" % call
                    return ("%s%s;%s" % (precall, call, postcall),
                            "Tmp_Return",
                            tmpvars)
                else:
                    # No need for a temporary variable
                    return ("%s" % precall, call, tmpvars)
        else:
            # A procedure
            return ("%s%s;%s" % (precall, call, postcall), None, tmpvars)


class Section(object):
    """A group of types and subprograms in an Ada package.
       There is a single section with a given name in the package
    """

    group_getters_and_setters = True
    # If true, a getter will be displayed with its corresponding setter.
    # Only one doc will be displayed for the two, and no separation line
    # will be output.

    def __init__(self, name):
        self.name = name
        self.comment = ""
        self.subprograms = []  # All subprograms
        self.spec_code = ""  # hard-coded code
        self.body_code = ""  # hard-coded code

    def add_comment(self, comment, fill=True):
        """If 'fill' is true, the comment is automatically split on several
           lines if needed. Otherwise, the comment is assumed to be already
           formatted properly, minus the leading --
        """
        if comment == "":
            self.comment += "   --\n"
        elif fill:
            self.comment += "   -- " + fill_text(
                cleanup_doc (comment), "   --  ", 79) + "\n"
        else:
            self.comment += "\n".join(
                "   -- %s" % cleanup_doc(p)
                for p in comment.splitlines()) + "\n"

    def add(self, *args):
        """Add one or more objects to the section (subprogram, code,...)"""
        for a in args:
            if isinstance(a, Subprogram):
                self.subprograms.append(a)
            elif isinstance(a, str):
                self.spec_code += a + "\n"

    def add_code(self, code, specs=True):
        if specs:
            self.spec_code += code + "\n"
        else:
            self.body_code += code + "\n"

    def _group_subprograms(self):
        """Returns a list of list of subprograms. In each nested list, the
           subprograms are grouped and a single documentation is output for
           the whole group. At the same time, this preserves the order of
           groups, so they appear in the order in which the first subprogram
           in the group appeared.
        """

        if Section.group_getters_and_setters:
            result = []
            tmp = dict()  # group_name => [subprograms]

            gtk_new_index = 0;

            for s in self.subprograms:
                name = s.name.replace("Get_", "") \
                        .replace("Query_", "") \
                        .replace("Gtk_New", "") \
                        .replace("Initialize", "") \
                        .replace("Set_From_", "") \
                        .replace("Set_", "")
                if s.name == "Gtk_New":
                    # Always create a new group for Gtk_New, since they all
                    # have different parameters. But we still want to group
                    # Gtk_New and Initialize.
                    t = tmp["Gtk_New%d" % gtk_new_index] = [s]
                    result.append(t)
                elif s.name == "Initialize":
                    tmp["Gtk_New%d" % gtk_new_index].append(s)
                    gtk_new_index += 1
                elif name in tmp:
                    tmp[name].append(s)  # Also modified in result
                else:
                    tmp[name] = [s]
                    result.append(tmp[name])

            return result

        else:
            return [[s] for s in self.subprograms]

    def spec(self):
        """Return the spec of the section"""

        result = []

        if self.subprograms or self.spec_code:
            result.append("") # A separator with previous section

            if self.name:
                result.append(box(self.name))
            if self.comment:
                result.append(self.comment)
            else:
                result.append("")

            if self.spec_code:
                result.append(indent_code(self.spec_code))

            for group in self._group_subprograms():
                for s in group:
                    result.append(s.spec(show_doc=s == group[-1]))
                    if s == group[-1]:
                        result.append("")

        return "\n".join(result)

    def body(self):
        result = []

        if self.body_code:
            result.append(indent_code(self.body_code))

        self.subprograms.sort(lambda x, y: cmp(x.name, y.name))
        for s in self.subprograms:
            b = s.body()
            if b:
                result.append(b)

        result.append("")

        return "\n".join(result)


class Package(object):
    copyright_header = ""
    # Can be overridden by applications to change the copyright header

    def __init__(self, name, doc=[]):
        """'doc' is a list of strings, where each string is a paragraph"""
        self.name = name
        self.doc  = doc

        self.sections = []       # [Section]
        self.spec_withs = dict() #  "pkg" -> use:Boolean
        self.body_withs = dict() #  "pkg" -> use:Boolean
        self.private = []        # Private section

    def section(self, name):
        """Return an existing section (or create a new one) with the given
           name.
        """
        for s in self.sections:
            if s.name == name:
                return s

        s = Section(name)
        self.sections.append(s)
        return s

    def add_with(self, pkg, specs=True, do_use=True):
        """Add a with+use clause for pkg, where pkg can also be a list.
           Automatic casing is performed. If specs is True, the withs are
           added to the specs of the package, otherwise to the body
        """
        if pkg == "System":
            return   # Not needed, already done in gtk.ads

        if type(pkg) == str:
            pkg = [pkg]
        for p in pkg:
            if p.lower() == self.name.lower():
                continue   # No dependence on self
            if specs:
                self.spec_withs[p] = do_use or self.spec_withs.get(p, False)
                self.body_withs.pop(p, None) # Remove same with in body
            elif p not in self.spec_withs:
                self.body_withs[p] = do_use or self.body_withs.get(p, False)

    def add_private(self, code):
        self.private.append(code)

    def _output_withs(self, withs):
        if withs:
            result = []
            m = max_length(withs)
            for w in sorted(withs.keys()):
                if withs[w]:
                    result.append(
                        "with %-*s use %s;" % (m + 1, w + ";", w))
                else:
                    result.append("with %s;" % w)

            return "\n".join(result) + "\n\n"
        return ""

    def spec(self, out):
        """Returns the spec of the package, in the file `out`"""

        if Package.copyright_header:
            out.write(Package.copyright_header + "\n")

        if self.doc:
            for d in self.doc:
                if d.startswith("%PRE%"):
                    d = d[5:]
                    lines = ["\n-- " + l for l in d.split("\n")]
                    out.write("".join(lines))
                elif d:
                    out.write("-- " + fill_text(d, "--  ", 79))
                else:
                    out.write("--")
                out.write("\n")

        out.write('\npragma Warnings (Off, "*is already use-visible*");\n')

        out.write(self._output_withs(self.spec_withs))
        out.write("package %s is" % self.name)

        for s in self.sections:
            out.write(s.spec())

        if self.private:
            out.write("\nprivate\n")
            out.write("\n".join(self.private))

        out.write("\nend %s;" % self.name)

    def body(self, out):
        """Returns the body of the package"""

        body = ""
        for s in self.sections:
            b = s.body()
            if b:
                body += b

        if not body:
            return

        if Package.copyright_header:
            out.write(Package.copyright_header + "\n")

        out.write("pragma Style_Checks (Off);\n")
        out.write('pragma Warnings (Off, "*is already use-visible*");\n')
        out.write(self._output_withs(self.body_withs))
        out.write("package body %s is\n" % self.name)
        out.write(body)
        out.write("end %s;" % self.name)


naming.cname_to_adaname = {
    # Maps c methods to Ada subprograms.
    # All methods that are generated automatically will be added
    # as they are processed.
    "gtk_widget_get_direction":     "Gtk.Widget.Get_Direction",
    "gtk_widget_add_events":        "Gtk.Widget.Add_Event",
    "gtk_widget_set_size_request":  "Gtk.Widget.Set_Size_Request",
    "gtk_window_set_default_icon":  "Gtk.Window.Set_Default_Icon",
    "gtk_widget_set_has_window":    "Gtk.Widget.Set_Has_Window",
    "gtk_show_uri":                 "gtk_show_uri()",
    "gtk_widget_show":              "Gtk.Widget.Show",
    "gtk_icon_factory_add_default": "Gtk.Icon_Factory.Add_Default",
    "gtk_icon_factory_add":         "Gtk.Icon_Factory.Add",
    "gdk_pixbuf_new_from_data":     "Gdk.Pixbuf.Gdk_New_From_Data",
    "gdk_pixbuf_new_from_file":     "Gdk.Pixbuf.Gdk_New_From_File",
    "gdk_pixbuf_new_from_xpm_data": "Gdk.Pixbuf.Gdk_New_From_Xpm_Data",
    "gdk_pixbuf_animation_new_from_file":
                                    "Gdk.Pixbuf.Gdk_New_From_File",
    "gdk_pixbuf_new":               "Gdk.Pixbuf.Gdk_New",
    "gdk_pixbuf_new_subpixbuf":     "Gdk.Pixbuf.Gdk_New_Subpixbuf",
    "gtk_accel_map_add_entry":      "Gtk.Accel_Map.Add_Entry",
    "gtk_accel_map_change_entry":   "Gtk.Accel_Map.Change_Entry",

    # ??? Doesn't exist
    "gtk_activatable_get_action": "Gtk.Activatable.Get_Action",

    # Will be bound later
    "gtk_action_group_add_action_with_accel":
        "Gtk.Action_Group.Add_Action_With_Accel",
    "gtk_tool_item_set_expand": "Gtk.Tool_Item.Set_Expand",
    "gtk_builder_add_from_file": "Gtk.Builder.Add_From_File",
    "gtk_builder_add_from_string": "Gtk.Builder.Add_From_String",
}

naming.girname_to_ctype = {
    # Maps GIR's "name" to a "c:type". This isn't needed for the
    # classes themselves, since this is automatically read from the
    # GIR file.
    # Mostly used for properties. The values must correspond to
    # entries in self.type_exceptions.
    "GdkPixbuf.Pixbuf":    "GdkPixbuf",
    "Pango.EllipsizeMode": "PangoEllipsizeMode",
    "Pango.WrapMode":      "PangoWrapMode",
    "Pango.AttrList":      "PangoAttrList",
    "Gio.Icon":            "GIcon*",
    "IconSet":             "GtkIconSet*",
    "Gdk.Pixmap":          "GdkPixmap*",
    "Gdk.Image":           "GdkImage*",
    "GdkPixbuf.PixbufAnimation": "GdkPixbufAnimation*",
    "Gdk.Bitmap":          "GdkBitmap*",
    "GObject.Object":      "GObject*",
    "GObject.Closure":     "GClosure*",
    "Object":              "GtkObject",
}

naming.exceptions = {
    # Naming exceptions. In particular maps Ada keywords.
    "Entry": "GEntry",
    "Type":  "The_Type",
    "Range": "GRange",
    "Delay": "The_Delay",
    "Select": "Gtk_Select",
    "End":   "The_End",
    "Return": "Do_Return",
    "Function": "Func",
    "Digits": "Number_Of_Digits",
    "Reverse": "Gtk_Reverse",
}

naming.type_exceptions = {
    # Maps C types to type descriptions.
    # All standard widgets will be added automatically. Only special
    # namings are needed here
    "gboolean":          Enum("Boolean",
                              "Glib.Properties.Property_Boolean"),
    "gdouble":  Proxy("Gdouble", "Glib.Properties.Property_Double"),
    "gint":     Proxy("Gint",    "Glib.Properties.Property_Int"),
    "guint":    Proxy("Guint",   "Glib.Properties.Property_Uint"),
    "gfloat":   Proxy("Gfloat",  "Glib.Properties.Property_Float"),

    "PangoAttrList":     Proxy("Pango.Attributes.Pango_Attr_List", ""),
    "PangoEllipsizeMode":Enum("Pango.Layout.Pango_Ellipsize_Mode", ""),
    "PangoWrapMode":     Enum("Pango.Layout.Pango_Wrap_Mode", ""),
    "PangoLayout":       GObject("Pango.Layout.Pango_Layout"),

    "GdkEvent*":    Proxy("Gdk.Event.Gdk_Event", ""),

    "GObject*":     GObject("Glib.Object.GObject"),
    "GClosure*":    Proxy("System.Address", ""),
    "GValue":       Proxy("Glib.Values.GValue", ""),

    # Specific to this binding generator (referenced from binding.xml)
    "WidgetSList": List("Gtk.Widget.Widget_SList.GSList"),
    "WidgetList":  List("Gtk.Widget.Widget_List.GList"),
    "ObjectList":  List("Glib.Object.Object_Simple_List.GList"),
    "ObjectSList": List("Glib.Object.Object_List.GSList"),
    "StringList":  List("Gtk.Enums.String_List.Glist"),
    "MessagesList": List("Gtk.Status_Bar.Messages_List.GSlist"),

    "gpointer":       Proxy("System.Address", ""),
    "GDestroyNotify": Proxy("Glib.G_Destroy_Notify_Address"),
    "GIcon*":        Proxy("Glib.G_Icon.G_Icon"),

    "CellLayoutDataFunc": Proxy("Gtk.Cell_Layout.Cell_Data_Func", ""),

    "GtkPositionType":    Enum("Gtk.Enums.Gtk_Position_Type"),
    "GtkReliefStyle":     Enum("Gtk.Enums.Gtk_Relief_Style"),
    "GtkShadowType":      Enum("Gtk.Enums.Gtk_Shadow_Type"),
    "GtkArrowType":       Enum("Gtk.Enums.Gtk_Arrow_Type"),
    "GtkPackType":        Enum("Gtk.Enums.Gtk_Pack_Type"),
    "GtkJustification":   Enum("Gtk.Enums.Gtk_Justification"),
    "GtkScrollType":      Enum("Gtk.Enums.Gtk_Scroll_Type"),
    "GtkSelectionMode":   Enum("Gtk.Enums.Gtk_Selection_Mode"),
    "GtkSensitivityType": Enum("Gtk.Enums.Gtk_Sensitivity_Type"),
    "GtkUpdateType":      Enum("Gtk.Enums.Gtk_Update_Type"),
    "GtkButtonBoxStyle":  Enum("Gtk.Enums.Gtk_Button_Box_Style"),
    "GtkCurveType":       Enum("Gtk.Enums.Gtk_Curve_Type"),
    "GtkMetricType":      Enum("Gtk.Enums.Gtk_Metric_Type",
                               "Gtk.Enums.Property_Metric_Type"),
    "GtkAttachOptions":   Enum("Gtk.Enums.Gtk_Attach_Options"),
    "GtkOrientation":     Enum("Gtk.Enums.Gtk_Orientation"),

    "GtkCellEditable": Interface("Gtk.Cell_Editable.Gtk_Cell_Editable"),
    "GtkCellLayout":   Interface("Gtk.Cell_Layout.Gtk_Cell_Layout"),
    "GtkFileChooser":  Interface("Gtk.File_Chooser.Gtk_File_Chooser"),
    "GtkRecentChooser":
        Interface("Gtk.Recent_Chooser.Gtk_Recent_Chooser"),
    "GtkTreeSortable": Interface("Gtk.Tree_Sortable.Gtk_Tree_Sortable"),

    "GtkObject": GObject("Glib.Object.GObject"),

    "GtkAboutDialog":  GObject("Gtk.About_Dialog.Gtk_About_Dialog"),
    "GtkAccelGroup":   GObject("Gtk.Accel_Group.Gtk_Accel_Group"),
    "GtkAspectFrame":  GObject("Gtk.Aspect_Frame.Gtk_Aspect_Frame"),
    "GtkButtonBox":    GObject("Gtk.Button_Box.Gtk_Button_Box"),
    "GtkCellRenderer":
        GObject("Gtk.Cell_Renderer.Gtk_Cell_Renderer"),
    "GtkCheckButton":  GObject("Gtk.Check_Button.Gtk_Check_Button"),
    "GtkComboBox":     GObject("Gtk.Combo_Box.Gtk_Combo_Box"),
    "GtkDrawingArea":  GObject("Gtk.Drawing_Area.Gtk_Drawing_Area"),
    "GtkEntry":        GObject("Gtk.GEntry.Gtk_Entry"),
    "GtkEntryCompletion": GObject("Gtk.Entry_Completion.Gtk_Entry_Completion"),
    "GtkEventBox":     GObject("Gtk.Event_Box.Gtk_Event_Box"),
    "GtkFileFilter":   GObject("Gtk.File_Filter.Gtk_File_Filter"),
    "GtkHButtonBox":   GObject("Gtk.Hbutton_Box.Gtk_Hbutton_Box"),
    "GtkMenuItem":     GObject("Gtk.Menu_Item.Gtk_Menu_Item"),
    "GtkRadioAction":  GObject("Gtk.Radio_Action.Gtk_Radio_Action"),
    "GtkRadioButton":  GObject("Gtk.Radio_Button.Gtk_Radio_Button"),
    "GtkRange":        GObject("Gtk.GRange.Gtk_Range"),
    "GtkScaleButton":  GObject("Gtk.Scale_Button.Gtk_Scale_Button"),
    "GtkSizeGroup":    GObject("Gtk.Size_Group.Gtk_Size_Group"),
    "GtkSeparatorMenuItem":
        GObject("Gtk.Separator_Menu_Item.Gtk_Separator_Menu_Item"),
    "GtkSeparatorToolItem":
        GObject("Gtk.Separator_Tool_Item.Gtk_Separator_Tool_Item"),
    "GtkStatusbar":    GObject("Gtk.Status_Bar.Gtk_Status_Bar"),
    "GtkToggleAction": GObject("Gtk.Toggle_Action.Gtk_Toggle_Action"),
    "GtkToggleButton": GObject("Gtk.Toggle_Button.Gtk_Toggle_Button"),
    "GtkToolItem":     GObject("Gtk.Tool_Item.Gtk_Tool_Item"),
    "GtkTreeIter*":     Proxy("Gtk.Tree_Model.Gtk_Tree_Iter"),
    "GtkTreeModel":    GObject("Gtk.Tree_Model.Gtk_Tree_Model"),
    "GtkTreeViewRowSeparatorFunc":
        Proxy("Gtk.Tree_View.Gtk_Tree_View_Row_Separator_Func"),
    "GtkVButtonBox":   GObject("Gtk.Vbutton_Box.Gtk_Vbutton_Box"),
    "GtkVolumeButton": GObject("Gtk.Volume_Button.Gtk_Volume_Button"),

    "GtkBorder":          Proxy("Gtk.Style.Gtk_Border"),
    "GtkIconSet*":        Proxy("Gtk.Icon_Factory.Gtk_Icon_Set"),

    "GdkWindow":          Proxy("Gdk.Window.Gdk_Window"),
    "GdkPixmap*":         Proxy("Gdk.Pixmap.Gdk_Pixmap"),
    "GdkBitmap*":         Proxy("Gdk.Bitmap.Gdk_Bitmap"),
    "GdkImage*":          Proxy("Gdk.Image.Gdk_Image"),
    "GdkPixbuf":          GObject("Gdk.Pixbuf.Gdk_Pixbuf"),
    "GdkPixbufAnimation*": Proxy("Gdk.Pixbuf.Gdk_Pixbuf_Animation"),
    "GdkRectangle":       Proxy("Gdk.Rectangle.Gdk_Rectangle"),
    "GdkModifierType":    Proxy("Gdk.Types.Gdk_Modifier_Type"),
    "GdkKeyType":         Proxy("Gdk.Types.Gdk_Key_Type"),
}
