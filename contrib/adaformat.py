#!/usr/bin/env python

"""
Various formatting classes for Ada code
"""

import sys
import re


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


class CType(object):
    def __init__(self, name, cname):
        """A type a described in a .gir file"""
        self.name = name
        self.cname = cname
        self.is_ptr = cname \
            and cname[-1] == '*' \
            and self.name != "utf8"   # not a "char*"

        self.param = None    # type as parameter
        self.pkg = None      # Needed package
        self.returns = None  # type as return type

        if name == "gboolean":
            self.param = "Boolean"
        elif name == "utf8":
            self.param = "UTF8_String"
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
                   and cname.startswith("Gtk"):

                    self.pkg = "Gtk.%s" % cname[3:-1]
                    self.param = "access Gtk_%s_Record'Class" % cname[3:-1]
                    self.returns = "Gtk_%s" % cname[3:-1]
                    self.is_ptr = False

                elif self.name == "PositionType":
                    self.pkg = "Gtk.Enums"
                    self.param = "Gtk_Position_Type"

                elif self.name == "ReliefStyle":
                    # ??? There is an <enumeration name="ReliefStyle"/>
                    self.pkg = "Gtk.Enums"
                    self.param = "Gtk_Relief_Style"

                else:
                    self.param = self.name
            else:
                self.pkg = "%s.%s" % (s[0], s[1])
                self.param = "%s.%s.%s_%s" % (s[0], s[1], s[0], s[1])

        if self.returns is None:
            self.returns = self.param


    def as_return(self, pkg):
        if self.pkg:
            pkg.add_with(self.pkg)
        return self.returns

    def as_param(self, pkg):
        """Converts self to a description for an Ada parameter to a
           subprogram. 'pkg' is an instance of Package, to which extra
           with clauses will be added if needed.
        """
        if self.pkg:
            pkg.add_with(self.pkg)
        return self.param


class Local_Var(object):
    __slots__ = ["name", "type", "default"]

    def __init__(self, name, type, default=""):
        assert(isinstance(type, str))

        self.name = name
        self.type = type
        self.default = default

    def spec(self, length=0):
        """Format the declaration for the variable or parameter.
           'length' is the minimum length that the name should occupy (for
           proper alignment when there are several variables.
        """
        if self.default:
            return "%-*s : %s := %s" % (
                length, self.name, self.type, self.default)
        else:
            return "%-*s : %s" % (length, self.name, self.type)


class Parameter(Local_Var):
    __slots__ = ["name", "type", "default", "doc"]

    def __init__(self, name, type, default="", doc="", mode="in"):
        if mode != "in":
            type = "%s %s" % (mode, type)
        super(Parameter, self).__init__(name, type, default)
        self.doc  = doc


class Subprogram(object):
    """An Ada subprogram that we are generating"""

    max_profile_length = 79 - len(" is")

    def __init__(self, name, code="", plist=[], local=[],
                 returns=None, doc=""):
        """Create a new subprogram.
           'plist' is a list of Parameter
           'local' is a list of Local_Var
           'code' can be the empty string, in which case no body is output.
           The code will be automatically pretty-printed, and the appropriate
           pragma Unreferenced are also added automatically.
        """
        assert(returns is None or isinstance(returns, str))
        self.name = name
        self.plist = plist
        self.returns = returns
        self.local = local
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

    def _profile(self, indent="   "):
        """Compute the profile for the subprogram"""

        if self.returns:
            prefix = indent + "function %s" % self.name
            suffix = " return %s" % self.returns
        else:
            prefix = indent + "procedure %s" % self.name
            suffix = ""

        if self.plist:
            # First test: all parameters on same line
            plist = [p.spec() for p in self.plist]
            p = " (" + "; ".join(plist) + ")"

            # If too long, split on several lines
            if len(p) + len(prefix) + len(suffix) > \
               Subprogram.max_profile_length:

                max = max_length([p.name for p in self.plist])
                plist = [p.spec(max) for p in self.plist]
                p = "\n   " + indent + "(" \
                    + (";\n    " + indent).join(plist) + ")"

        else:
            p = ""

        return prefix + p + suffix

    def spec(self, indent="   "):
        """Return the spec of the subprogram"""

        result = self._profile(indent) + ";"

        if self._import:
            result += "\n" + indent + self._import

        doc = [self.doc] + [p.doc for p in self.plist]

        for d in doc:
            if d:
                result += "\n" + indent + "-- " \
                    + fill_text(d, indent + "--  ", 79)

        return result + "\n"

    def _indent(self, code, indent=3):
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
        result += local

        for s in self._nested:
            result += s.spec(indent=indent + "   ")
            result += s.body(indent=indent + "   ")

        result += indent + "begin\n"
        result += self._indent(self.code, indent=6)
        return result + indent + "end %s;\n" % self.name


class Section(object):
    """A group of types and subprograms in an Ada package.
       There is a single section with a given name in the package
    """

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
            result.append(self.code)

        for s in self.subprograms:
            result.append(s.spec())

        return "\n".join(result)

    def body(self):
        result = []

        for s in self.subprograms:
            b = s.body()
            if b:
                result.append(b)

        return "\n".join(result)


class Package(object):
    def __init__(self, name):
        self.name = name

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
            return "\n".join(result)
        return ""

    def spec(self, out):
        """Returns the spec of the package, in the file `out`"""

        out.write(self._output_withs(self.spec_withs))
        out.write("\n\n")
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

        out.write(self._output_withs(self.body_withs))
        out.write("\n\n")
        out.write("package body %s is" % self.name)

        for s in self.sections:
            b = s.body()
            if b:
                out.write("\n")
                out.write(b)

        out.write("\nend %s;" % self.name)
