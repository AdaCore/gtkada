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

uri = "http://www.gtk.org/introspection/core/1.0"
glib_uri = "http://www.gtk.org/introspection/glib/1.0"
namespace = QName(uri, "namespace").text
ntype = QName(uri, "type").text
ndoc = QName(uri, "doc").text
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

def fill_text(text, prefix, length, firstLineLength=0):
    """Split TEXT on several lines (with a given max length and a prefix)"""

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


def to_ada_type(ctype):
    if ctype == "gboolean":
        return "Boolean"
    elif ctype == "utf8":
        return "UTF8_String"
    elif ctype == "gfloat":
        return "Float"
    else:
        return ctype


def to_ada_name(cname):
    c = cname.replace("-", "_")
    return c.title()


def max_length(iter):
    """Return the length of the longuest element in iter"""
    longuest = 0
    for f in iter:
        longuest = max(longuest, len(f))
    return longuest


class Subprogram(object):
    """An Ada subprogram that we are generating"""

    def __init__(self, name, plist=[], returns=None, doc=""):
        self.name = name
        self.plist = plist # List of parameters  (name, type)
        self.returns = returns
        self.doc = doc

    def spec(self):
        """Return the spec of the subprogram"""

        if self.returns:
            ret = self.returns
            if ret == "none":
                ret = None
            else:
                ret = to_ada_type(ret)
        else:
            ret = None

        if ret:
            prefix = "   function %s" % self.name
        else:
            prefix = "   procedure %s" % self.name

        if self.plist:
            max = max_length([p[0] for p in self.plist])
            plist = ["%-*s : %s" % (max, p[0], p[1]) for p in self.plist]
            p = " (" + "; ".join(plist) + ")"
            if len(p) + len(prefix) > 78:
                p = "\n      (" + ";\n       ".join(plist) + ")"

        else:
            p = ""

        prefix += p

        if ret:
            prefix += " return %s" % ret

        prefix += ";"

        if self.doc:
            prefix += "\n   -- " \
                + fill_text(self.doc, "   --  ", 79)

        return prefix + "\n"


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
        params = c.find(QName(uri, "parameters").text)
        if not params:
            return []
        nparam = QName(uri, "parameter").text
        result = []

        for p in params.findall(nparam):
            type = p.find(ntype)
            result.append(
                (to_ada_name(p.get("name")), to_ada_type(type.get("name"))))

        return result

    def _format_subprogram(
        self, node, name, plist=[], returns=None, doc=""):
        """print a subprogram declaration"""

        sub = Subprogram(name=name,
                         plist=plist + self._parameters(node),
                         returns=returns and returns.get("name"),
                         doc=doc)
        print sub.spec()

    def _constructors(self):
        n = QName(uri, "constructor").text
        for c in self.node.findall(n):
            name = c.get("name").title()
            self._format_subprogram(
                c, "Gtk_%s" % name,
                plist=[("Self", "out %(ns)s_%(name)s" % self._subst)],
                doc=c.findtext(ndoc, ""))
            self._format_subprogram(
                c, ("Initialize_%s" % name).replace("_New", ""),
                [("Self",
                  "access %(ns)s_%(name)s_Record'Class" % self._subst)])

    def _methods(self):
        n = self.node.findall(QName(uri, "method").text)
        for c in n:
            self._format_subprogram(
              c, c.get("name").title(),
              plist=[("Self", "access %(ns)s_%(name)s_Record" % self._subst)],
              returns=c.find(nreturn).find(ntype),
              doc=c.findtext(ndoc, ""))

    def _header(self):
        print """
with %(ns)s.%(parent)s;

package %(ns)s.%(name)s is

   type %(ns)s_%(name)s_Record
      is new %(parent)s.%(ns)s_%(parent)s_Record with private;
   type %(ns)s_%(name)s is access all %(ns)s_%(name)s_Record'Class;
""" % self._subst

    def _properties(self):
        n = QName(uri, "property")

        props = list(self.node.findall(n.text))
        if props:
            adaprops = []

            for p in props:
                flags = []
                if p.get("readable", "1") != "0":
                    flags.append("read")
                if p.get("writable", "1") != "0":
                    flags.append("write")
                flags = "-".join(flags)

                adaprops.append({
                    "cname": p.get("name"),
                    "name": to_ada_name(p.get("name")) + "_Property",
                    "flags": flags,
                    "doc": p.findtext(ndoc, ""),
                    "type":  to_ada_type(p.find(ntype).get("name"))})

            adaprops.sort(lambda x,y: x["name"] <> y["name"])

            print """   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  <properties>"""

            for p in adaprops:
                print """   --
   --  Name:  %(name)s
   --  Type:  %(type)s
   --  Flags: %(flags)s""" % p

                if p["doc"]:
                    print "   --  Descr:%s" % fill_text(
                        p["doc"], "   --      ", 79, 65)

            print "   --  </properties>\n"

            for p in adaprops:
                d = '   %s : constant Glib.Properties.Property_%s' % (
                    p["name"], p["type"])
                print d + ";"
                self._private += d + ":=\n" \
                    + '      Glib.Properties.Build ("%s");\n' % p["cname"]
            print ""

    def _signals(self):
        n = QName(glib_uri, "signal").text

        signals = list(self.node.findall(n))
        if signals:
            adasignals = []
            for s in signals:
                sub = Subprogram(
                    name="Handler",
                    plist=[
                      ("Self",
                       "access %(ns)s_%(name)s_Record'Class" % self._subst)],
                    returns=s.find(nreturn).find(ntype).get("name"))
                adasignals.append({
                    "name": s.get("name"),
                    "profile": fill_text(sub.spec(), "   --      ", 79, 69),
                    "doc": s.findtext(ndoc, "")})

            adasignals.sort(lambda x,y: x["name"] <> y["name"])

            print """   -------------
   -- Signals --
   -------------
   --  <signals>
   --  The following new signals are defined for this widget:"""

            for s in adasignals:
                print """   --
   --  "%(name)s"
   --    %(profile)s""" % s
                if s["doc"]:
                    print "   --    %s""" % fill_text(
                        s["doc"], "   --     ", 79, 69)

            print "   --  </signals>\n"

            for s in adasignals:
                print '   Signal_%s : constant Glib.Signal_Name := "%s";' % (
                    to_ada_name(s["name"]), s["name"])

    def _print_private(self):
        print "\nprivate"
        print self._private

    def _footer(self):
        print """end %(ns)s.%(name)s;""" % self._subst

    def generate(self):
        self._header()
        self._constructors()
        self._methods()
        self._properties()
        self._signals()
        self._print_private()
        self._footer()

p = GIR("/usr/share/gir-1.0/Gtk-2.0.gir")
cl = p.getClass("Button")
cl.generate()
