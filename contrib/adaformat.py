#!/usr/bin/env python

"""
Various formatting classes for Ada code
"""

import sys
from collections import namedtuple


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

class Parameter(object):
    __slots__ = ["name", "type", "doc", "mode"]

    def __init__(self, name, type, doc="", mode="in"):
        assert(isinstance(type, str))

        self.name = name
        self.type = type
        self.doc  = doc
        self.mode = mode

    def spec(self, length=0):
        if self.mode == "in":
            m = ""
        else:
            m = self.mode + " "

        return "%-*s : %s%s" % (length, self.name, m, self.type)


class Subprogram(object):
    """An Ada subprogram that we are generating"""

    def __init__(self, name, plist=[], returns=None, doc=""):
        assert(returns is None or isinstance(returns, str))
        self.name = name
        self.plist = plist # List of parameters Parameter
        self.returns = returns
        self.doc = doc

    def spec(self):
        """Return the spec of the subprogram"""

        if self.returns:
            prefix = "   function %s" % self.name
            suffix = " return %s" % self.returns
        else:
            prefix = "   procedure %s" % self.name
            suffix = ""

        doc = [self.doc] + [p.doc for p in self.plist]

        if self.plist:
            # First test: all parameters on same line
            plist = [p.spec() for p in self.plist]
            p = " (" + "; ".join(plist) + ")"

            # If too long, split on several lines
            if len(p) + len(prefix) + len(suffix)> 78:
                max = max_length([p.name for p in self.plist])
                plist = [p.spec(max) for p in self.plist]
                p = "\n      (" + ";\n       ".join(plist) + ")"

        else:
            p = ""

        prefix += p
        prefix += suffix
        prefix += ";"

        for d in doc:
            if d:
                prefix += "\n   -- " + fill_text(d, "   --  ", 79)

        return prefix + "\n"


class Type(object):
    def __init__(self, decl):
        self.decl = decl

    def spec(self):
        return self.decl


class Section(object):
    """A group of types and subprograms in an Ada package.
       There is a single section with a given name in the package
    """

    def __init__(self, name):
        self.name = name
        self.comment = ""
        self.subprograms = []  # All subprograms
        self.types = [] # All types
        self.code = ""  # hard-coded code

    def add_comment(self, comment):
        self.comment += "   -- " + fill_text(comment, "   --  ", 79) + "\n"

    def add(self, *args):
        """Add one or more objects to the section (subprogram, types,...)"""
        for a in args:
            if isinstance(a, Subprogram):
                self.subprograms.append(a)
            elif isinstance(a, Type):
                self.types.append(a)
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

        if self.types:
            for t in self.types:
                result.append(t.spec())
            result.append("")

        for s in self.subprograms:
            result.append(s.spec())

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

    def spec(self):
        """Return the spec of the package"""

        result = []
        out = sys.stdout

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
