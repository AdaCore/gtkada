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


class AdaNaming(object):
    exceptions = {
        "Entry": "GEntry",
        "Type":  "Typ",
        "Range": "GRange",
        "Delay": "The_Delay",
        "Select": "Gtk_Select",
        "End":   "The_End",
        "Return": "Do_Return",
        "Function": "Func",
        "Digits": "Number_Of_Digits",
        "Reverse": "Gtk_Reverse",
    }

    @staticmethod
    def case(name):
        """Return the proper casing to use for 'name', taking keywords
           into account
        """
        name = name.replace("-", "_").title()

        if name.endswith("_"):
            name = name[:-1]

        return AdaNaming.exceptions.get(name, name)


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
    def __init__(self, name, cname, pkg, isArray=False):
        """A type a described in a .gir file
           'pkg' is an instance of Package, to which extra
           with clauses will be added if needed.
           'isArray' should be true for an array of the simple type 'name'.
        """
        self.is_ptr = cname \
            and cname[-1] == '*' \
            and name != "utf8"   # not a "char*"

        self.param = None    # type as parameter
        self.returns = None  # type as return type
        self.cparam = None   # type for Ada subprograms binding to C
        self.convert = "%s"  # Convert from Ada parameter to C parameter
        self.cleanup = None  # If set, a tmp variable is created to hold the
                             # result of convert during the call, and is then
                             # free by calling this cleanup. Use "%s" as the
                             # name of the variable.
        self.isArray = isArray

        if name == "gboolean":
            self.param = "Boolean"
        elif name == "utf8":
            self.param = "UTF8_String"
            self.cparam = "Interfaces.C.Strings.chars_ptr"
            self.convert = "New_String (%s)"
            self.cleanup = "Free (%s);"
            pkg.add_with("Interfaces.C.Strings", specs=False)
        elif name == "gfloat":
            self.param = "Float"
        elif name == "none":
            self.param = None
        else:
            # A type of the form "Gdk.Window" needs to be converted
            # ??? Should in fact look in the .gir file for the name
            s = name.split(".")
            if len(s) == 1:
                if self.is_ptr \
                   and cname.endswith("**") \
                   and cname.startswith("Gtk"):

                    pkg.add_with("Gtk.%s" % AdaNaming.case(cname[3:-2]))
                    self.param = "Gtk_%s" % cname[3:-2]
                    self.returns = "Gtk_%s" % cname[3:-2]
                    self.cparam = "System.Address"
                    self.convert = "Get_Object (%s)"  # ??? Incorrect
                    self.is_ptr = True  # Will be a "out" parameter

                elif self.is_ptr \
                   and cname.startswith("Gtk"):

                    pkg.add_with("Gtk.%s" % AdaNaming.case(cname[3:-1]))
                    self.param = "access Gtk_%s_Record'Class" % cname[3:-1]
                    self.returns = "Gtk_%s" % cname[3:-1]
                    self.cparam = "System.Address"
                    self.convert = "Get_Object (%s)"
                    self.is_ptr = False

                elif name == "PositionType":
                    pkg.add_with("Gtk.Enums")
                    self.param = "Gtk_Position_Type"
                    self.cparam = "Integer"
                    self.convert = "%s'Pos (%%s)" % self.param

                elif name == "ReliefStyle":
                    # ??? There is an <enumeration name="ReliefStyle"/>
                    pkg.add_with("Gtk.Enums")
                    self.param = "Gtk_Relief_Style"
                    self.cparam = "Integer"
                    self.convert = "%s'Pos (%%s)" % self.param

                else:
                    self.param = name
            else:
                pkg.add_with("%s.%s" % (s[0], AdaNaming.case(s[1])))
                self.param = "%s.%s.%s_%s" % (
                    s[0], AdaNaming.case(s[1]), s[0], s[1])

        if self.returns is None:
            self.returns = self.param

        if self.cparam is None:
            self.cparam = self.param

    def as_return(self):
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

    def as_call(self, name, lang="ada"):
        """'name' represents a parameter of type 'self'.
           Returns an instance of VariableCall.
        """
        if lang == "ada":
            return VariableCall(call=name, precall='', postcall='', tmpvars=[])
        elif self.cleanup:
            tmp = "Tmp_%s" % name
            return VariableCall(
                call=tmp,
                precall='%s := %s;' % (tmp, self.convert % name),
                postcall=self.cleanup % tmp,
                tmpvars=[Local_Var(name=tmp, type=self.cparam)])

        else:
            return VariableCall(
                call=self.convert % name, precall='', postcall='', tmpvars=[])


class AdaType(CType):
    """Similar to a CType, but created directly from Ada types"""

    def __init__(self, adatype, ctype, convert="%s"):
        """The 'adatype' type is represented as 'ctype' for subprograms
           that import C functions. The parameters of that type are converted
           from Ada to C by using 'convert'. 'convert' must use '%s' once
           to indicate where the name of the parameter should go
        """
        self.param   = adatype
        self.returns = adatype
        self.cparam  = ctype
        self.convert = convert
        self.cleanup = None
        self.is_ptr  = adatype.startswith("access ")


class Local_Var(object):
    __slots__ = ["name", "type", "default"]

    def __init__(self, name, type, default=""):
        assert(isinstance(type, str)
               or isinstance(type, CType))

        self.name = name
        self.type = type
        self.default = default

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
        if self.default:
            return "%-*s : %s := %s" % (length, self.name, t, self.default)
        else:
            return "%-*s : %s" % (length, self.name, t)

    def as_call(self, lang="ada"):
        """Pass 'self' as a parameter to an Ada subprogram call, implemented
           in the given language.
           Returns an instance of VariableCall
        """
        if isinstance(self.type, CType):
            return self.type.as_call(self.name, lang=lang)
        else:
            return VariableCall(
                call=self.name, precall='', postcall='', tmpvars=[])


class Parameter(Local_Var):
    __slots__ = ["name", "type", "default", "doc", "mode"]

    def __init__(self, name, type, default="", doc="", mode="in"):
        super(Parameter, self).__init__(name, type, default)
        self.mode = mode
        self.doc  = doc

    def _type(self, lang):
        t = super(Parameter, self)._type(lang)
        if self.mode != "in":
            return "%s %s" % (self.mode, t)
        return t


class Subprogram(object):
    """An Ada subprogram that we are generating"""

    max_profile_length = 79 - len(" is")

    def __init__(self, name, code="", plist=[], local_vars=[],
                 returns=None, doc=""):
        """Create a new subprogram.
           'plist' is a list of Parameter
           'local_vars' is a list of Local_Var
           'code' can be the empty string, in which case no body is output.
           The code will be automatically pretty-printed, and the appropriate
           pragma Unreferenced are also added automatically.
        """
        assert(returns is None or isinstance(returns, str))
        self.name = AdaNaming.case(name)
        self.plist = plist
        self.returns = returns
        self.local = local_vars
        self._import = None
        self._nested = []  # nested subprograms

        if code and code[-1] != ";":
            self.code = code + ";"
        else:
            self.code = code

        self.doc = doc

    def import_c(self, cname):
        """Declares that 'self' is implemented as a pragma Import.
           This returns 'self' so that it can be chained:
              s = Subprogram(...).import_c('...')
        """
        self._import = 'pragma Import (C, %s, "%s");' % (self.name, cname)
        return self

    def add_nested(self, *args):
        """Add some nested subprograms"""
        for subp in args:
            self._nested.append(subp)
        return self

    def _profile(self, indent="   ", lang="ada"):
        """Compute the profile for the subprogram"""

        if self.returns:
            prefix = indent + "function %s" % self.name
            suffix = " return %s" % self.returns
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

    def spec(self, indent="   "):
        """Return the spec of the subprogram"""

        doc = [self.doc] + [p.doc for p in self.plist]

        if self._import:
            result = self._profile(indent, lang="c") + ";"
            result += "\n" + indent + self._import
        else:
            result = self._profile(indent, lang="ada") + ";"

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

    def call(self, in_pkg="", add_return=True):
        """A call to 'self'.
           The parameters that are passed to self are assumed to have the
           same name as in self's declaration. When 'self' is implemented
           as a pragma Import, proper conversions are done.
           'in_pkg' is used to fully qualify the name of the subprogram, to
           avoid ambiguities. This is optional. This can be either a string
           or an instance of Package.
           'add_returns' is whether we should prefix the call with "return"
           when appropriate.
           Returns value is an instance of VariableCall.

        """

        if self._import:
            lang = "c"
        else:
            lang = "ada"

        tmpvars  = []
        precall  = ""
        params   = []
        postcall = ""

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

        if add_return:
            if self.returns is not None:
                call = "return %s;" % call
            else:
                call = "%s;" % call

        return VariableCall(
            call=call,
            precall=precall,
            postcall=postcall,
            tmpvars=tmpvars)


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
                name = s.name.replace("Get_", "").replace("Set_", "")
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
                if s != group[-1]:
                    s.doc = None
                result.append(s.spec())
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

        out.write(self._output_withs(self.spec_withs))
        out.write("package %s is" % self.name)

        for s in self.sections:
            out.write("\n")
            out.write(s.spec())

        if self.private:
            out.write("\nprivate\n")
            out.write("\n".join(self.private))

        out.write("\nend %s;" % self.name)

    def body(self, out):
        """Returns the body of the package"""

        if Package.copyright_header:
            out.write(Package.copyright_header + "\n")

        out.write(self._output_withs(self.body_withs))
        out.write("package body %s is\n" % self.name)

        for s in self.sections:
            b = s.body()
            if b:
                out.write("\n")
                out.write(b)

        out.write("\nend %s;" % self.name)
