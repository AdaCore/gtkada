#!/usr/bin/env python

"""
Various formatting classes for Ada code
"""

import sys
import re
from collections import namedtuple, defaultdict


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

    for w in text.split():
        if len(line) + len(w) > maxLen:
            result.append(line)
            maxLen = length - len(prefix)
            line = w
        else:
            line += " " + w

    if line != "":
        result.append(line)
    return ("\n" + prefix).join(result)


def box(name, indent="   "):
    return indent + "-" * (len(name) + 6) + "\n" \
            + indent + "-- " + name + " --\n" \
            + indent + "-" * (len(name) + 6)


def space_out_camel_case(stringAsCamelCase):
    """Adds spaces to a camel case string.  Failure to space out string
       returns the original string.
         >>> space_out_camel_case('DMLSServicesOtherBSTextLLC')
       'DMLS Services Other BS Text LLC'
    """

    if stringAsCamelCase is None:
        return None

    pattern = re.compile('([A-Z][A-Z][a-z])|([a-z][A-Z])')
    return pattern.sub(
        lambda m: m.group()[:1] + "_" + m.group()[1:], stringAsCamelCase)


class AdaNaming(object):
    def __init__(self):
        self.cname_to_adaname = dict() # Maps c methods to Ada subprograms
        self.exceptions = {
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
        self.type_exceptions = { # tuple: (name, isenum)
            "PangoAttrList":      ("Pango.Attributes.Pango_Attr_List", False),
            "PangoEllipsizeMode": ("Pango.Layout.Pango_Ellipsize_Mode", True),
            "PangoWrapMode":      ("Pango.Layout.Pango_Wrap_Mode", True),
            "PangoLayout":        ("Pango.Layout.Pango_Layout", False),

            "GtkPositionType":    ("Gtk.Enums.Gtk_Position_Type", True),
            "GtkReliefStyle":     ("Gtk.Enums.Gtk_Relief_Style", True),
            "GtkShadowType":      ("Gtk.Enums.Gtk_Shadow_Type", True),
            "GtkArrowType":       ("Gtk.Enums.Gtk_Arrow_Type", True),
            "GtkPackType":        ("Gtk.Enums.Gtk_Pack_Type", True),
            "GtkJustification":   ("Gtk.Enums.Gtk_Justification", True),

            "GdkWindow":          ("Gdk.Window.Gdk_Window", False),
        }

    def map_cname(self, cname, adaname):
        """Register the mapping from c method's name to Ada subprogram"""
        self.cname_to_adaname[cname] = adaname

    def ada_name(self, cname):
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

    def case_type(self, name):
        """Return the default Ada name for a type"""

        name = name.replace("-", "_").title()

        if name.endswith("_"):
            name = name[:-1]

        return self.type_exceptions.get(
            name, self.exceptions.get(name, name))

    def full_type(self, cname):
        """Return the default full type name, including package"""
        name = self.type_exceptions.get(cname, None)
        if name is None:
            s = cname.split(".")
            if len(s) == 2:
                name = "%s.%s.%s_%s" % (
                    s[0], self.case(s[1]), s[0], self.case_type(s[1]))
            elif cname.startswith("Gtk"):
                name = "Gtk.%s.Gtk_%s" % (
                    self.case(cname[3:]), self.case(cname[3:]))
            else:
                name = cname
            return (name, False)  # Not an enumeration
        else:
            return name

naming = AdaNaming()


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
        l = l.strip()
        if l.startswith("end") \
           or l.startswith("elsif")  \
           or l.startswith("else")  \
           or l.startswith("begin"):
            indent -= 3

        old_parent = parent_count
        parent_count = parent_count + l.count("(") - l.count(")")

        if not l:
            pass
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

        result += l + "\n"

        if(l.endswith("then") and not l.endswith("and then")) \
           or l.endswith("loop") \
           or(l.endswith("else") and not l.endswith("or else"))\
           or l.endswith("begin") \
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
                  ...;
                  Tmp := ... (...);
                  ...;
                  return <converter % Tmp>
              end;
       Converter will contain a single %s which will be replaced by the
       name of the temporary variable that holds the result of the call
       to the function.
    """

    def __init__(self, name, cname, pkg, isArray=False):
        """A type a described in a .gir file
           'pkg' is an instance of Package, to which extra
           with clauses will be added if needed.
           'isArray' should be true for an array of the simple type 'name'.
        """

        def as_enumeration(name, cname):
            """Self is an enumeration, setup the conversion hooks"""
            if "." in name:
                pkg.add_with(name[0:name.rfind(".")])
            self.param = name
            self.cparam = cname
            self.convert = "%s'Pos (%%s)" % name
            self.postconvert = "%s'Val (%%s)" % name
            self.returns = (self.param, self.cparam, self.postconvert, [])

        def as_gobject(full, userecord=True):
            """Self is a descendant of GObject, setup the conversion hooks"""
            if "." in full:
                pkg.add_with(full[0:full.rfind(".")])
                full = full[full.rfind(".") + 1:]

            if userecord:
                self.param = "access %s_Record'Class" % full
            else:
                self.param = full

            self.cparam = "System.Address"
            self.convert = "Get_Object (%s)"
            self.is_ptr = False
            self.returns = (
                full, self.cparam,
                "%s (Get_User_Data (%%s, Stub))" % full,
                [Local_Var("Stub", AdaType("%s_Record" % full))])

        self.is_ptr = cname \
            and cname[-1] == '*' \
            and name != "utf8"   # not a "char*"

        self.param = None    # type as parameter
        self.returns = None  # type as return type
        self.cparam = None   # type for Ada subprograms binding to C
        self.convert = "%s"  # Convert from Ada parameter to C parameter
        self.postconvert = "%s" # Convert from C to Ada value
        self.cleanup = None  # If set, a tmp variable is created to hold the
                             # result of convert during the call, and is then
                             # free by calling this cleanup. Use "%s" as the
                             # name of the variable.
        self.isArray = isArray

        if self.is_ptr:
            basename = cname[0:-1]
            if basename[-1] == "*":
                basename = basename[0:-1]
        elif not cname:
            if name.startswith("Pango"):
                basename = name.replace(".", "") # for consistency: the GIR
                                                 # files are not...
            else:
                basename = "Gtk%s" % name   # Workaround GIR bug: they
                                            # use a "name" and not a C
                                            # type for <properties>
        else:
            basename = cname

        if name == "gboolean":
            as_enumeration("Boolean", "Gboolean")

        elif name == "utf8":
            self.param = "UTF8_String"
            self.cparam = "Interfaces.C.Strings.chars_ptr"
            self.convert = "New_String (%s)"
            self.cleanup = "Free (%s);"
            pkg.add_with("Interfaces.C.Strings", specs=False)
            self.returns = (
                "UTF8_String", "Interfaces.C.Strings.chars_ptr",
                "Value (%s)", [])

        elif name in ("gdouble", "gint", "guint", "gfloat"):
            self.param = name.title()

        elif name == "none":
            self.param = None

        else:
            full = naming.full_type(basename)
            #print "MANU name=", name, " cname=", cname, " base=", basename, " full=", full, " is_ptr", self.is_ptr

            if basename == "PangoLayout":
                as_gobject(full[0])

            elif full[1]:   # An enumeration ?
                as_enumeration(full[0], "Integer")

            elif self.is_ptr \
               and cname.startswith("Gtk"):
                 as_gobject(full[0], userecord=not cname.endswith("**"))

                 if cname.endswith("**"):
                     self.is_ptr = True # An out parameter

            else:
                full = full[0]
                if "." in full:
                    pkg.add_with(full[0:full.rfind(".")])
                self.param = full

        if self.cparam is None:
            self.cparam = self.param

        if self.returns is None:
            self.returns = (self.param, self.cparam, "%s", [])

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

        elif self.cleanup:
            tmp = "Tmp_%s" % name
            return VariableCall(
                call=wrapper % tmp,
                precall='%s := %s;' % (tmp, self.convert % name),
                postcall=self.cleanup % tmp,
                tmpvars=[Local_Var(name=tmp, type=self.cparam)])

        elif self.postconvert != "%s" and mode != "in":
            # An "out" parameter for an enumeration requires a temporary
            # variable: Internal(Enum'Pos (Param)) is invalid
            tmp = "Tmp_%s" % name
            return VariableCall(
                call=wrapper % tmp,
                precall="",
                postcall='%s := %s;' % (name, self.postconvert % tmp),
                tmpvars=[Local_Var(name=tmp, type=self.cparam)])

        else:
            return VariableCall(
                call=wrapper % (self.convert % name),
                precall='', postcall='', tmpvars=[])


class AdaType(CType):
    """Similar to a CType, but created directly from Ada types"""

    def __init__(self, adatype, ctype="", convert="%s"):
        """The 'adatype' type is represented as 'ctype' for subprograms
           that import C functions. The parameters of that type are converted
           from Ada to C by using 'convert'. 'convert' must use '%s' once
           to indicate where the name of the parameter should go
        """
        self.param   = adatype
        self.cparam  = ctype or adatype
        self.returns = (self.param, self.cparam, "%s", [])
        self.convert = convert
        self.postconvert = "%s"
        self.cleanup = None
        self.is_ptr  = adatype.startswith("access ")


class Local_Var(object):
    __slots__ = ["name", "type", "default", "aliased"]

    def __init__(self, name, type, default="", aliased=False):
        self.name = name

        if isinstance(type, str):
            self.type = AdaType(type)
        else:
            assert(isinstance(type, CType))
            self.type = type

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


class Subprogram(object):
    """An Ada subprogram that we are generating"""

    max_profile_length = 79 - len(" is")

    def __init__(self, name, code="", plist=[], local_vars=[],
                 returns=None, doc=[]):
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
        self._import = None
        self._nested = []  # nested subprograms
        self._deprecated = (False, "") # True if deprecated

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

    def mark_deprecated(self, msg):
        """Mark the subprogram as deprecated"""

        self._deprecated = (True, msg)

    def add_nested(self, *args):
        """Add some nested subprograms"""
        for subp in args:
            self._nested.append(subp)
        return self

    def _profile(self, indent="   ", lang="ada"):
        """Compute the profile for the subprogram"""

        returns = self.returns and self.returns.as_return(lang)

        if returns:
            prefix = indent + "function %s" % self.name

            if self._import:
                suffix = " return %s" % returns[1]
            else:
                suffix = " return %s" % returns[0]
        else:
            prefix = indent + "procedure %s" % self.name
            suffix = ""

        if self.plist:
            # First test: all parameters on same line
            plist = [p.spec(lang=lang) for p in self.plist]
            p = " (" + "; ".join(plist) + ")"

            # If too long, split on several lines
            if len(p) + len(prefix) + len(suffix) > \
               Subprogram.max_profile_length:

                max = max_length([p.name for p in self.plist])
                plist = [p.spec(max, lang=lang) for p in self.plist]
                p = "\n   " + indent + "(" \
                    + (";\n    " + indent).join(plist) + ")"

        else:
            p = ""

        return prefix + p + suffix

    def _cleanup_doc(self, doc):
        """Replaces C features in the doc with appropriate Ada equivalents"""

        doc = doc.replace("%NULL", "null")
        doc = doc.replace("%TRUE", "True")
        doc = doc.replace("%False", "True")

        subp = re.compile("([\S_]+)\(\)")
        doc = subp.sub(lambda x: naming.ada_name(x.group(1)), doc)

        types = re.compile("#([\w_]+)")
        doc = types.sub(lambda x: naming.full_type(x.group(1))[0], doc)

        return doc

    def spec(self, indent="   ", show_doc=True):
        """Return the spec of the subprogram"""

        if show_doc:
            doc = [self._cleanup_doc(d) for d in self.doc]
            if self._deprecated[0]:
                doc += [self._deprecated[1]]
            doc += [self._cleanup_doc(p.doc) for p in self.plist]
        else:
            doc = []

        if self._import:
            result = self._profile(indent, lang="c") + ";"
            result += "\n" + indent + self._import
        else:
            result = self._profile(indent, lang="ada") + ";"
            if self._deprecated[0]:
                result += "\n" + indent + "pragma Obsolescent;"

        for d in doc:
            if d:
                result += "\n" + indent + "-- " \
                    + fill_text(d, indent + "--  ", 79)

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
        if not self.code:
            return ""

        result = box(self.name) + "\n\n"

        profile = self._profile()
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
                if postcall:
                    tmpvars.append(Local_Var("Tmp_Return", returns[1]))
                    call = "Tmp_Return := %s" % call
                    return ("%s%s;%s" % (precall, call, postcall),
                            returns[2] % "Tmp_Return",
                            tmpvars)
                else:
                    # No need for a temporary variable
                    return (precall, returns[2] % call, tmpvars)
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
        self.code = ""  # hard-coded code

    def add_comment(self, comment):
        self.comment += "   -- " + fill_text(comment, "   --  ", 79) + "\n"

    def add(self, *args):
        """Add one or more objects to the section (subprogram, types,...)"""
        for a in args:
            if isinstance(a, Subprogram):
                self.subprograms.append(a)
            elif isinstance(a, str):
                self.code += a + "\n"

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

            for s in self.subprograms:
                name = s.name.replace("Get_", "") \
                        .replace("Query_", "") \
                        .replace("Gtk_New", "") \
                        .replace("Initialize", "") \
                        .replace("Set_", "")
                if name in tmp:
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

        if self.subprograms or self.code:
            result.append("") # A separator with previous section

            if self.name:
                result.append(box(self.name))
            if self.comment:
                result.append(self.comment)
            else:
                result.append("")

            if self.code:
                result.append(indent_code(self.code))

            for group in self._group_subprograms():
                for s in group:
                    result.append(s.spec(show_doc=s == group[-1]))
                    if s == group[-1]:
                        result.append("")

        return "\n".join(result)

    def body(self):
        result = []

        for s in self.subprograms:
            b = s.body()
            if b:
                result.append(b)

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
        if type(pkg) == str:
            pkg = [pkg]
        for p in pkg:
            if p.lower() == self.name.lower():
                continue   # No dependence on self
            t = p.title()
            if specs:
                self.spec_withs[t] = do_use or self.spec_withs.get(t, False)
            else:
                self.body_withs[t] = do_use or self.body_withs.get(t, False)

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
                    result.append(
                        "with %-*s;" % (m + 1, w + ";"))

            return "\n".join(result) + "\n\n"
        return ""

    def spec(self, out):
        """Returns the spec of the package, in the file `out`"""

        if Package.copyright_header:
            out.write(Package.copyright_header + "\n")

        if self.doc:
            for d in self.doc:
                out.write("-- " + fill_text(d, "--  ", 79))
                out.write("\n")
            out.write("\n")

        out.write("pragma Style_Checks (Off);\n")
        out.write('pragma Warnings (Off, "*is already use-visible*");\n')

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

        if Package.copyright_header:
            out.write(Package.copyright_header + "\n")

        out.write("pragma Style_Checks (Off);\n")
        out.write('pragma Warnings (Off, "*is already use-visible*");\n')

        out.write(self._output_withs(self.body_withs))
        out.write("package body %s is\n" % self.name)

        for s in self.sections:
            b = s.body()
            if b:
                out.write("\n")
                out.write(b)

        out.write("\nend %s;" % self.name)
