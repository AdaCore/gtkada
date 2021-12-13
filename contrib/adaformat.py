#!/usr/bin/env python

"""
Various formatting classes for Ada code
"""

import re
import copy
from collections import namedtuple

# A lot of subprograms below take a "lang" parameter, which indicates how
# values should be converted:
#
#     LANG should be one of:
#        "ada":  the value of the parameter is read from an Ada
#           value (as subprogram whose code is Ada) and passed to a
#           similar subprogram. No conversion needed.
#        "ada->c": all Ada values will be converted to their C equivalent,
#           since the target subprogram's code is written in C.
#        "c->ada": case of C callbacks: the value is passed from C to
#           an Ada subprogram in the user application.
#
#     As an example, here is what the types for a list of strings
#     would look like in all languages:
#
#           "ada"    -> String_List
#           "c->ada" -> chars_ptr_array_access (no bounds given by C)
#           "ada->c" -> chars_ptr_array (bounds will be ignored anyway,
#                         and it is simpler to pass this type).


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
        self.set_ada_name(ada)
        self.property = property
        self.default_record_field_value = None

        self.is_ptr = False

        self.cleanup = None
        # If set, a tmp variable is created to hold the result of convert
        # during the call, and is then free by calling this cleanup. Use
        # "%s" as the name of the variable.

        self.isArray = False

        # In some cases, Ada provides a special value for a parameter that
        # indicates that NULL should be passed to C. Such a test is only done
        # when allow_none is True. val_or_null is then a function in charge
        # of converting the value to a System.Address unless it is equal to
        # a specific null value.

        self.allow_none = False
        self.val_or_null = None

        # If True, the value returned from C must be freed by the caller

        self.transfer_ownership = False

    def set_ada_name(self, ada):
        self.ada = ada       # Fully qualified Ada type
        self.param = ada     # type as parameter
        self.cparam = ada    # type for Ada subprograms binding to C

    def convert_from_c(self):
        """How to convert the value returned from C to Ada.
           This function returns a tuple:
              [0] = name of the Ada type
              [1] = name of the C type
              [2] = Conversion from C type to Ada type. The value is read
                    from "%(var)s". It can also use "%(tmp)s" if a temporary
                    variable is needed.
              [3] = List of needed temporary variables (except for the one
                    corresponding to "%(tmp)s".
              [4] = Name of the C type, for "out" parameters. Often same as [1]
              [5] = Convert from [4] to the Ada type. Often same as [2]
        """
        return (self.param, self.cparam, "%(var)s", [], self.cparam, "%(var)s")

    def convert_from_c_add_with(self, pkg, specs=False):
        """Add the "with" statements needed to do the conversion stated
           in convert_from_c().
        """
        pass

    def convert_to_c(self, pkg=None):
        """How to convert from Ada parameter to C parameter. If it uses %(tmp)s,
           we assume the converter sets the value of the temporary variable
           itself.
           It can also use %(var)s which will be substituted by the name of the
           parameter.
           Otherwise, it is used as " Tmp := <convert>".
           It might be necessary to also override add_with() to add the
           necessary with statements.
        """

        if self.allow_none and self.val_or_null:
            self.cparam = "System.Address"
            return "%s (%%(var)s'Address)" % self.val_or_null
        else:
            return "%(var)s"

    def direct_c_map(self):
        """Whether the parameter can be passed as is to C"""
        return self.convert_to_c(pkg=None) == "%(var)s"

    def as_property(self):
        """The type to use for the property"""
        return self.property

    def as_return(self, pkg=None):
        """See CType documentation for a description of the returned tuple"""

        returns = self.convert_from_c()

        if returns and pkg:
            # Avoid full dotted notation when inside the package itself
            return (returns[0].replace("%s." % pkg.name, ""),
                    returns[1].replace("%s." % pkg.name, ""),
                    returns[2],
                    returns[3],
                    returns[4],
                    returns[5])
        else:
            return returns

    def record_field_type(self, pkg=None):
        """The type to use when self is used in a record.
           [pkg] should be the current package, to avoid fully qualified name
           that reference that package.
        """
        return self.as_c_param(pkg=pkg)

    def as_ada_param(self, pkg):
        """Converts self to a description for an Ada parameter to a
           subprogram.
           `pkg` is the package in which we insert the name. It is used to
           avoid qualified name when in the same package
        """
        return self.param.replace("%s." % pkg.name, "")

    def as_c_param(self, pkg=None):
        """Returns the C type (as a parameter to a subprogram that imports
           a C function)
        """
        if pkg:
            return self.cparam.replace("%s." % pkg.name, "")
        else:
            return self.cparam

    def as_call(
            self, name, pkg, wrapper="%s", lang="ada", mode="in", value=None,
            is_temporary_variable=True):
        """'name' represents a parameter of type 'self'.
           'pkg' is the Package instance in which the call occurs.
           'wrapper' is used in the call itself, and %s is replaced by the
              name of the variable (or the temporary variable).
           'mode' is the mode for the Ada subprogram, and is automatically
              converted when generating a subprogram as a direct C import.
           :param is_temporary_variable: should be true if the corresponding
              variable is a variable local to the subprogram, as opposed to a
              parameter. In this case, we can sometimes avoid creating a second
              temporary variable, thus increasing efficiency.
           Returns an instance of VariableCall.
           See comments at the beginning of this package for valid LANG values
        """
        assert lang in ("ada", "c->ada", "ada->c")
        assert mode in ("in", "out", "access", "not null access",
                        "in out"), "Incorrect mode: %s" % mode

        if lang == "ada":
            return VariableCall(
                call=wrapper % name, precall='', postcall='', tmpvars=[])

        elif lang == "ada->c":
            returns = self.convert_from_c()
            ret = returns and returns[2]

            additional_tmp_vars = [] if not returns else returns[3]

            # An "out" parameter for an enumeration requires a temporary
            # variable: Internal(Enum'Pos(Param)) is invalid
            # Unless we are already using a temporary variable.

            if (ret and
               ret != "%(var)s" and
               mode != "in" and
               not is_temporary_variable):

                tmp = "Tmp_%s" % name

                if mode in ("out", "access"):
                    tmpvars = [Local_Var(
                        name=tmp, type=returns[4], aliased=True)]
                else:
                    tmpvars = [
                        Local_Var(name=tmp, type=returns[4], aliased=True,
                                  default=self.convert_to_c(pkg=pkg) % {
                            "var": name})]

                if "%(tmp)s" in ret:
                    tmp2 = "Tmp2_%s" % name
                    tmpvars += [Local_Var(name=tmp2, type=returns[4])]
                    postcall = "%s; %s := %s;" % (
                        returns[5] % {"var": tmp, "tmp": tmp2},
                        name,
                        tmp2)
                else:
                    postcall = "%s := %s;" % (
                        name,
                        returns[5] % {"var": tmp})

                call = VariableCall(
                    call=wrapper % tmp,
                    precall="",
                    postcall=postcall,
                    tmpvars=tmpvars + additional_tmp_vars)

            elif "%(tmp)" in self.convert_to_c(pkg=pkg):
                # The conversion sets the temporary variable itself
                tmp = "Tmp_%s" % name
                call = VariableCall(
                    call=wrapper % tmp,
                    precall=self.convert_to_c(pkg=pkg) % {
                        "var": name, "tmp": tmp},
                    postcall=self.cleanup % tmp,
                    tmpvars=[Local_Var(name=tmp, type=self.cparam)] + [])

            elif self.cleanup:
                tmp = "Tmp_%s" % name
                conv = self.convert_to_c(pkg=pkg) % {"var": name}

                # Initialize the temporary variable with a default value, in
                # case it is an unconstrained type (a chars_ptr_array for
                # instance)
                call = VariableCall(
                    call=wrapper % tmp,
                    precall='',
                    postcall=self.cleanup % tmp,
                    tmpvars=[Local_Var(
                        name=tmp, type=AdaType(self.cparam), default=conv)])

            else:
                conv = self.convert_to_c(pkg=pkg) % {"var": name}
                call = VariableCall(
                    call=wrapper % conv, precall='', postcall="", tmpvars=[])

            return call

        elif lang == "c->ada":
            ret = self.convert_from_c()
            self.convert_from_c_add_with(pkg=pkg)

            # Do we need a temporary variable ?
            # An "out" parameter for an enumeration requires a temporary
            # variable: Internal(Enum'Pos (Param)) is invalid

            ret_convert = ret and ret[2]

            if ret_convert and ret_convert != "%(var)s" and mode != "in":
                tmp = "Tmp_%s" % name
                tmpvars = [Local_Var(name=tmp, type=self.ada)] + ret[3]

                if "%(tmp)s" in ret_convert:
                    tmp2 = "Tmp2_%s" % name
                    tmpvars += [Local_Var(name=tmp2, type=self.cparam)]
                    postcall = "%s; %s := %s;" % (
                        ret_convert % {"var": tmp, "tmp": tmp2},
                        name,
                        tmp2)
                else:
                    postcall = "%s := %s;" % (
                        name,
                        self.convert_to_c(pkg=pkg) % {"var": tmp})

                return VariableCall(
                    call=wrapper % tmp,
                    precall="",
                    postcall=postcall,
                    tmpvars=tmpvars)

            else:
                return VariableCall(
                    call=wrapper % (ret[2] % {"var": name}),
                    precall='', postcall='', tmpvars=ret[3])

    def add_with(self, pkg=None, specs=False):
        """Add required withs for this type"""
        if pkg:
            pkg.add_with(package_name(self.ada))

        if pkg and self.allow_none and self.val_or_null:
            base = self.val_or_null
            pkg.add_with(package_name(base), specs=specs)

    def copy(self):
        """Return a copy of self, possibly modifying some properties."""
        return copy.deepcopy(self)


class Enum(CType):

    def __init__(self, ada, property=None):
        base = ada[ada.rfind(".") + 1:] or ada
        if property is None:
            CType.__init__(self, ada, "Gtk.Enums.Property_%s" % base)
        else:
            CType.__init__(self, ada, property)

        if self.ada.lower() == "boolean":
            self.cparam = "Glib.Gboolean"
        else:
            # Do not convert enumerations to integers. We want to pass the
            # associated literal in case the enumeration in C does not start
            # at 0, or as holes in the series.
            self.cparam = self.ada

    def convert_from_c(self):
        if self.ada.lower() == "boolean":
            if self.ada == "Boolean":
                # Do not use Boolean'Val for more flexibility, in case C
                # returns another value than 0 and 1 (as is the case for
                # gtk_status_icon_position_menu on OSX for instance).
                conv = "%(var)s /= 0"
            else:
                conv = "%s'Val (%%(var)s)" % self.ada
            return (self.param, self.cparam, conv, [],

                    # for out parameters
                    self.cparam, conv)
        else:
            return super(Enum, self).convert_from_c()

    def convert_to_c(self, pkg=None):
        if self.ada.lower() == "boolean":
            return "%s'Pos (%%(var)s)" % self.ada
        else:
            return super(Enum, self).convert_to_c(pkg=pkg)

    def record_field_type(self, pkg=None):
        if pkg:
            return self.ada.replace("%s." % pkg.name, "")
        else:
            return self.ada

    @staticmethod
    def register_ada_decl(pkg, ctype, ada=None):
        """Register an enumeration type.
        :param pkg: is the name of the current package in which the enumeration
        will be defined.
        """

        # Compute the Ada name automatically if needed.
        if not ada:
            ada = naming.type(name="", cname=ctype).ada

        full_name = "%s.%s" % (pkg, ada)
        t = Enum(full_name, "%s.Property_%s" % (pkg, ada))
        naming.add_type_exception(cname=ctype, type=t)

        # Add the special cases for properties that GIR file use
        t = ctype.replace("Pango", "Pango.").replace("Gdk", "Gdk.")
        naming.girname_to_ctype[t] = ctype


class GObject(CType):

    def __init__(self, ada, userecord=True, allow_none=False, classwide=False):
        CType.__init__(self, ada, "Glib.Properties.Property_Object")
        self.cparam = "System.Address"
        self.is_ptr = False
        self.classwide = classwide  # Parameter should include "'Class"
        self.userecord = userecord  # Parameter should be "access .._Record"
        self.allow_none = allow_none

    def convert_from_c(self):
        stub = "Stub_%s" % (base_name(self.ada), )

        if self.ada == "Glib.Object.GObject":
            conv = "Get_User_Data (%%(var)s, %s)" % stub
        else:
            conv = "%s (Get_User_Data (%%(var)s, %s))" % (self.ada, stub)

        return (self.param,
                self.cparam,
                conv,
                [Local_Var(
                    stub, AdaType("%s_Record" % self.ada, in_spec=False))],

                # for out parameters
                self.cparam, conv)

    def convert_to_c(self, pkg=None):
        if self.allow_none:
            return "Get_Object_Or_Null (GObject (%(var)s))"
        else:
            return "Get_Object (%(var)s)"

    def as_ada_param(self, pkg):
        if self.userecord:
            prefix = "" if self.allow_none else "not null "

            if self.classwide:
                self.param = "%saccess %s_Record'Class" % (prefix, self.ada)
            else:
                self.param = "%saccess %s_Record" % (prefix, self.ada)

        return super(GObject, self).as_ada_param(pkg)

    def copy(self):
        result = CType.copy(self)
        return result


class Tagged(GObject):

    """Tagged types that map C objects, but do not derive from GObject"""

    def convert_from_c(self):
        return (self.param, self.cparam, "From_Object (%(var)s)", [],

                # for out parameters
                self.cparam, "From_Object (%(var)s)")

    def convert_to_c(self, pkg=None):
        return "Get_Object (%(var)s)"

    def as_ada_param(self, pkg):
        # Make sure to bind as a CType here, not as a GOBject
        return CType.as_ada_param(self, pkg)


class UTF8(CType):

    def __init__(self):
        CType.__init__(self, "UTF8_String", "Glib.Properties.Property_String")
        self.cparam = "Gtkada.Types.Chars_Ptr"
        self.cleanup = "Free (%s);"

    def convert_from_c(self):
        conv = "Gtkada.Bindings.Value_Allowing_Null (%(var)s)"

        if self.transfer_ownership:
            conv = "Gtkada.Bindings.Value_And_Free (%(var)s)"

        return (self.param, self.cparam, conv, [],

                # for out parameters
                self.cparam, conv)

    def convert_from_c_add_with(self, pkg, specs=False):
        if pkg:
            pkg.add_with("Gtkada.Bindings", specs=specs)
            pkg.add_with("Gtkada.Types", specs=specs)

    def convert_to_c(self, pkg=None):
        if self.allow_none:
            return 'if %(var)s = "" then %(tmp)s :=' \
                + ' Gtkada.Types.Null_Ptr; else'\
                + ' %(tmp)s := New_String (%(var)s); end if;'
        else:
            return "New_String (%(var)s)"

    def add_with(self, pkg, specs=False):
        super(UTF8, self).add_with(pkg)
        if pkg:
            pkg.add_with("Gtkada.Types", specs=specs,
                         might_be_unused=True)


class SignalName(UTF8):
    """
    A special kind of utf8 used for signal names
    """
    def __init__(self):
        super(SignalName, self).__init__()
        self.set_ada_name("Glib.Signal_Name")
        self.cparam = "Gtkada.Types.Chars_Ptr"

    def convert_to_c(self, pkg=None):
        return "New_String (String (%(var)s))"


class UTF8_List(CType):

    def __init__(self):
        CType.__init__(self, "GNAT.Strings.String_List", "")
        self.cparam = "Gtkada.Types.chars_ptr_array"
        self.cleanup = "Gtkada.Types.Free (%s);"

    def convert_from_c(self):
        # Use a temporary variable to store the result of To_String_List,
        # because in some cases the result will need to be freed. For instance,
        # when a callback from C receives a list of strings as
        # chars_ptr_array_access, we create a temporary String_List to call the
        # Ada callback, and then need to free the temporary String_List.

        conv = "To_String_List (%(var)s.all)"

        if self.transfer_ownership:
            conv = "To_String_List_And_Free (%(var)s)"

        return (self.param, "chars_ptr_array_access", conv, [],

                # for out parameters
                "chars_ptr_array_access", conv)

    def record_field_type(self, pkg=None):
        return "Gtkada.Types.char_array_access"

    def convert_to_c(self, pkg=None):
        return "From_String_List (%(var)s)"

    def add_with(self, pkg=None, specs=False):
        super(UTF8_List, self).add_with(pkg=pkg)
        if pkg:
            pkg.add_with("GNAT.Strings", specs=True)
            pkg.add_with("Gtkada.Bindings", specs=specs, might_be_unused=True)


class Record(CType):

    def __init__(self, ada, property=None, val_or_null=None):
        """
           :param val_or_null: if specified, and the null constant is passed
             as a parameter, then System.Null_Address is passed to C. If
             unspecified, any value passed by the user is given as is to C.
        """

        if property is None:
            CType.__init__(self, ada, "Glib.Properties.Property_Boxed")
        else:
            CType.__init__(self, ada, property)

        self.val_or_null = val_or_null

        # Do not change self.cparam: when passing a read-only parameter to
        # C, GNAT will automatically pass the address of the record
        self.cparam = ada

    def convert_from_c(self):
        conv = "%(var)s.all"  # convert C -> Ada,
        if self.transfer_ownership:
            conv = "From_Object_Free (%(var)s)"

        return (self.ada,
                "access %s" % self.ada,
                conv,
                [],

                # for out parameters
                self.ada, "%(var)s")

    def convert_to_c(self, pkg=None):
        if self.allow_none and self.val_or_null:
            self.cparam = "System.Address"
            return "%s (%%(var)s'Address)" % self.val_or_null
        else:
            return "%(var)s"

    @staticmethod
    def register_ada_record(pkg, ctype, ada=None):
        """Register a <record> type.
        [pkg] is the name of the current package in which the enumeration
        will be defined.
        """

        adaname = base_name(ada or naming.type(name="", cname=ctype).ada)
        full_name = "%s.%s" % (pkg, adaname)
        t = Record(full_name)
        naming.add_type_exception(cname="%s*" % ctype, type=t)
        naming.add_type_exception(cname=ctype, type=t)


class Proxy(CType):

    def __init__(self, ada, property=None, val_or_null=None,
                 from_gvalue=None, default_record_field=None):
        """:param val_or_null: is used when GIR indicates the parameter has
           allow-none=1, and is used to test whether we should pass NULL
           to C or a pointer to the Ada data.

           :param from_gvalue: is the function used to retrieve this type from
           a GValue, in particular when processing callbacks. The default is to
           retrieve a C_Proxy and Cast as appropriate.

           :param str default_record_field: the default value to set in record
              type declarations for fields of that type. No default value set
              if this is None.
        """

        if property is None:
            CType.__init__(self, ada, "Glib.Properties.Property_Boxed")
        else:
            CType.__init__(self, ada, property)

        self.val_or_null = val_or_null
        self.default_record_field_value = default_record_field

    def record_field_type(self, pkg=None):
        if self.isArray and self.array_fixed_size is not None:
            return "%s (1 .. %s)" % (
                self.as_c_param(pkg=pkg),
                self.array_fixed_size)
        elif self.is_ptr or self.isArray:
            return "access %s" % self.as_c_param(pkg=pkg)
        else:
            return self.as_c_param(pkg=pkg)


class Callback(CType):

    def __init__(self, ada):
        CType.__init__(self, ada, "")
        self.cparam = "System.Address"

    def __repr__(self):
        return "<Callback %s>" % self.ada

    def convert_from_c(self):
        # Never return such a callback to Ada (because in fact we are pointing
        # to a function in one of the bodies of GtkAda, not the actual user
        # callback.
        return None

    def convert_to_c(self, pkg=None):
        return "%(var)s'Address"


class Interface(CType):

    def __init__(self, ada):
        CType.__init__(self, ada, "Glib.Properties.Property_Interface")
        self.cparam = ada
        self.is_ptr = False


class List(CType):

    def __init__(self, ada):
        CType.__init__(self, ada, "Glib.Properties.Property_Object")
        self.__adapkg = ada[:ada.rfind(".")]
        self.cparam = "System.Address"
        self.is_ptr = False

    def convert_from_c(self):
        conv = "%s.Set_Object (%%(tmp)s, %%(var)s)" % self.__adapkg
        return (   # Use %(tmp)s so forces the use of temporary var.
            self.param, self.cparam, conv, [],

            # for out parameters
            self.cparam, conv)

    @staticmethod
    def register_ada_list(pkg, ada, ctype, single=False):
        """Register a list of GObject instantiated in Ada"""
        if single:
            gtype = "GSlist"
            name = "SList"
        else:
            gtype = "Glist"
            name = "List"

        listCname = "%s%s" % (ctype, name)  # Default list name
        ada = ada or naming.type(cname=listCname).ada

        t = List("%s.%s.%s" % (pkg, ada, gtype))
        naming.add_type_exception(listCname, t)

    def convert_to_c(self, pkg=None):
        return "%s.Get_Object (%%(var)s)" % self.__adapkg

    def add_with(self, pkg=None, specs=False):
        # A list comes from an instantiation (pkg.instance.glist), so we need
        # to skip backward two "."
        if pkg:
            p = self.ada.rfind(".")
            if p != -1:
                p = self.ada[:p].rfind(".")
                if p != -1:
                    pkg.add_with(self.ada[:p], specs=True)


class AdaType(CType):

    def __init__(self, adatype, pkg=None, in_spec=True, ctype="",
                 convert="%(var)s"):
        """The 'adatype' type is represented as 'ctype' for subprograms
           that import C functions. The parameters of that type are converted
           from Ada to C by using 'convert'. 'convert' must use '%s' once
           to indicate where the name of the parameter should go
        """
        CType.__init__(self, adatype, "")
        self.param = adatype
        self.cparam = ctype or adatype
        self.__convert = convert
        self.cleanup = None
        self.is_ptr = adatype.startswith("access ")

        # ??? Why do we need to call this explicitly ?
        if pkg:
            self.add_with(pkg)

    def convert_to_c(self, pkg=None):
        return self.__convert


class AdaTypeArray(CType):

    """An array of scalar types"""

    def __init__(self, adatype):
        CType.__init__(self, adatype, "")
        self.param = "%s_Array" % naming.case(adatype)
        self.cparam = "System.Address"
        self.isArray = True

    def convert_to_c(self, pkg=None):
        return "%(var)s (%(var)s'First)'Address"

    def convert_from_c(self):
        # ??? This implementation is specialized for the Gtk.Clipboard pkg,
        # which is the only place where we use it
        c = ("Atom_Arrays.To_Array " +
             "(Atom_Arrays.Convert (%(var)s), Integer (N_Atoms))")

        return (self.param,       # name of Ada type
                self.cparam,      # name of C type
                c,                # convert from C to Ada
                [                 # list of temporary variables needed
                ],
                self.cparam,      # name of C type for out parameters
                c)                # convert from previous line to Ada type

    def record_field_type(self, pkg=None):
        if self.isArray and self.array_fixed_size is not None:
            return "%s (1 .. %s)" % (
                self.as_ada_param(pkg=pkg),
                self.array_fixed_size)
        else:
            return self.as_c_param(pkg=pkg)


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

        if not girname:
            return ""
        elif girname.startswith("Gdk") or girname.startswith("Gtk"):
            default = girname
        else:
            default = "Gtk%s" % girname

        return self.girname_to_ctype.get(girname, default)

    def adamethod_name(self, cname, warning_if_not_found=True):
        """Return the ada name corresponding to the C method's name"""
        try:
            return self.cname_to_adaname[cname]
        except KeyError:
            if warning_if_not_found and cname.lower().startswith("gtk_"):
                print("Name quoted in doc has no Ada binding: %s" % cname)
            self.cname_to_adaname[cname] = cname  # Display warning once only
            return cname

    def case(self, name, protect=True):
        """Return the proper casing to use for 'name', taking keywords
           into account. This is for packages.
        """
        name = self.__camel_case_to_ada(name.replace("-", "_")).title()
        if name.endswith("_"):
            name = name[:-1]

        if protect:
            return self.protect_keywords(name)
        else:
            return name

    def protect_keywords(self, name):
        return ".".join(self.exceptions.get(n, n) for n in name.split("."))

    def __camel_case_to_ada(self, name):
        """Converts a name with CamelCase to Camel_Case"""

        if not name:
            return name

        result = name[0]
        prev = result

        for r in name[1:]:
            if prev != "_" \
                    and prev != "." \
                    and not prev.isupper() \
                    and r.isupper():
                result += "_%s" % r
            else:
                result += r

            prev = r

        return result

    def __full_type_from_girname(self, girname):
        """Return the type description from a GIR name"""
        return self.type_exceptions.get(
            girname,  # First try GIR name as is in the table (gint, ...)
            self.type_exceptions.get(
                self.ctype_from_girname(girname),  # Else the C type

                # Else return the GIR name itself
                Proxy(self.__camel_case_to_ada(girname))))

    def type(self, name="", cname=None, pkg=None, isArray=False,
             allow_access=True, allow_none=False, userecord=True,
             useclass=True,
             array_fixed_size=None,
             transfer_ownership=False):
        """Build an instance of CType for the corresponding cname.
           A type a described in a .gir file

           :param pkg: an instance of Package, to which extra
              with clauses will be added if needed.
           :param allow_none: if True, then an empty string maps to a
              NULL pointer in C, rather than an empty C string. For a GObject,
              the parameter is passed as "access" rather than
              "not null access".
           :param use_record: is only used for GObject types.
           :param isArray: should be true for an array of the simple type
              'name'.
           :param allow_access: should be True if the parameter can be
              represented as 'access Type', rather than an explicit type, in
              the case of GObject descendants.
           :param array_fixed_size: if specified, this is the size of the
              array. The binding is different in this case, since we can't use
              a fat pointer.

        """

        if cname is None:
            cname = self.girname_to_ctype.get(name, None)

        if (cname == "gchar**" or
           name == "array_of_utf8" or
           name == "array_of_filename"):
            t = UTF8_List()
        elif (cname in ("gint**", "int**") or
              name in ("array_of_gint", "array_of_guint", "array_of_gint8",
                       "array_of_guint8", "array_of_guint16")):
            t = AdaTypeArray("gint")
            isArray = True
        elif name in ("array_of_Gdk.Atom", ):
            t = AdaTypeArray("Gdk_Atom")
            isArray = True
        elif name in ("array_of_gdouble", ):
            t = AdaTypeArray("gdouble")
            isArray = True
        elif name in ("array_of_gchar", ):
            t = AdaTypeArray("gchar")
            isArray = True
        elif cname == "void":
            return None
        elif name == "utf8" or cname == "gchar*" or cname == "char*":
            t = UTF8()
        elif name == "SignalName":
            t = SignalName()
        elif cname:
            # Check whether the C type, including trailing "*", maps
            # directly to an Ada type.
            # t = self.type_exceptions.get(
            #    cname,
            #    Proxy(self.__camel_case_to_ada(cname)))
            t = self.__full_type_from_girname(cname)
            is_ptr = False

            if t.ada[-1] == "*":
                # No, try without the trailing "*"
                t = self.__full_type_from_girname(cname[0:-1])

                if t.ada[-1] != "*":
                    is_ptr = True    # Yes, so we had a pointer
                else:
                    basename = cname[0:-1]  # Remove all "*"
                    if basename[-1] == "*":
                        basename = basename[0:-1]
                    t = self.__full_type_from_girname(basename)

            if not isinstance(t, GObject) \
               and not isinstance(t, Interface):
                t.is_ptr = is_ptr
        else:
            t = self.__full_type_from_girname(name)
            t.is_ptr = cname and cname[-1] == '*'

        t = t.copy()
        t.isArray = isArray
        t.array_fixed_size = array_fixed_size
        t.classwide = useclass
        t.allow_none = allow_none
        t.userecord = userecord
        t.transfer_ownership = transfer_ownership

        # Needs to be called last, since the output might depend on all the
        # attributes set above

        t.add_with(pkg)

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

    text = text.replace("\n\n", "\n<br>")

    # Do we have a list item ? If yes, preserve the indentation

    if text.lstrip().startswith("* "):
        line += text[:text.find("*")]

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

    def replace_type(x):
        t = naming.type(x.group(1))
        t.userecord = False
        return t.ada

    # get_package might have been called before we had the XML node
    # from the Gir file, and therefore no doc for the package. We can
    # now override it, unless it came from binding.xml

    subp = re.compile(r"([\S_]+)\(\)")
    doc = subp.sub(lambda x: naming.adamethod_name(x.group(1)), doc)

    types = re.compile(r"#([\w_]+)")
    doc = types.sub(replace_type, doc)

    params = re.compile(r"@([\w_]+)")
    doc = params.sub(lambda x: x.group(1).title(), doc)

    enums = re.compile(r"%([A-Z][\w_]+)")
    doc = enums.sub(lambda x: naming.adamethod_name(x.group(1)), doc)

    doc = doc.replace("<emphasis>", "*") \
        .replace("</emphasis>", "*") \
        .replace("<literal>", "'") \
        .replace("</literal>", "'") \
        .replace("<firstterm>", "'") \
        .replace("</firstterm>", "'") \
        .replace("<![CDATA[", "") \
        .replace("]]>", "") \
        .replace("&nbsp;", " ") \
        .replace("<parameter>", "'") \
        .replace("</parameter>", "'") \
        .replace("<filename>", "'") \
        .replace("</filename>", "'") \
        .replace("<footnote>", "[") \
        .replace("</footnote>", "]") \
        .replace("<keycap>", "'") \
        .replace("</keycap>", "'") \
        .replace("<keycombo>", "[") \
        .replace("</keycombo>", "]") \
        .replace("<entry>", "\n\n") \
        .replace("</entry>", "") \
        .replace("<row>", "") \
        .replace("</row>", "") \
        .replace("<tbody>", "") \
        .replace("</tbody>", "") \
        .replace("</tgroup>", "") \
        .replace("<informaltable>", "") \
        .replace("</informaltable>", "") \
        .replace("<note>", "\nNote: ") \
        .replace("</note>", "")

    doc = re.sub("<tgroup[^>]*>", "", doc)
    doc = re.sub("<term><parameter>(.*?)</parameter>&nbsp;:</term>",
                 r"\1:", doc)

    # Lists

    doc = re.sub("<listitem>(\n?<simpara>|\n?<para>)?", "\n\n   * ", doc)

    doc = doc.replace("</para></listitem>", "") \
        .replace("</listitem>", "") \
        .replace("<simpara>", "") \
        .replace("</simpara>", "") \
        .replace("<para>", "\n\n") \
        .replace("</para>", "")

    # Definition of terms (variablelists)

    doc = doc.replace("<variablelist>", "") \
        .replace("</variablelist>", "") \
        .replace("<varlistentry>", "") \
        .replace("</varlistentry>", "") \
        .replace("<term>", "'") \
        .replace("</term>", "'")

    doc = re.sub(r"<variablelist[^>]*>", "", doc)
    doc = re.sub(r"<title>(.*?)</title>", r"\n\n== \1 ==\n\n", doc)
    doc = re.sub(r"<refsect\d[^>]*>", "", doc)
    doc = re.sub(r"</refsect\d>", "", doc)

    doc = doc.replace("<example>", "") \
             .replace("</example>", "") \
             .replace("<informalexample>", "") \
             .replace("</informalexample>", "") \
             .replace("<itemizedlist>", "").replace("</itemizedlist>", "") \
             .replace("<orderedlist>", "").replace("</orderedlist>", "") \
             .replace("&percnt;", "%") \
             .replace("&lt;", "<").replace("&gt;", ">") \
             .replace("&ast;", "*") \
             .replace("<programlisting>", "\n\n__PRE__<programlisting>")

    doc = re.sub("<programlisting>(.*?)</programlisting>",
                 lambda m: re.sub(
                     "\n\n+", "\n",
                     indent_code(m.group(1), addnewlines=False)),
                 doc,
                 flags=re.DOTALL or re.MULTILINE)

    doc = re.sub("\n\n\n+", "\n\n", doc)

    return doc


def format_doc(doc, indent, separate_paragraphs=True, fill=True):
    """Transform the doc from a list of strings to a single string"""

    result = ""
    prev = ""

    # Make sure the doc is a list of paragraphs

    if not isinstance(doc, list):
        doc = [doc]

    # Cleanup the XML tags in each paragraph. This could result in
    # new paragraphs being created

    cleaned = []

    for d in doc:
        d = cleanup_doc(d)
        if fill:
            cleaned.extend(d.split("\n\n"))
        else:
            cleaned.append(d)

    prefix = "\n" + indent + "--"

    for d in cleaned:

        # Separate paragraphs with an empty line, unless it is a markup
        # or we are at the end
        if separate_paragraphs:
            if prev != "" and not prev.lstrip().startswith("<"):
                result += prefix

        if d:
            if d.lstrip().startswith("__PRE__"):
                d = d.lstrip()[7:]
                result += "".join(prefix + " " + p for p in d.splitlines())
            elif fill:
                result += prefix + " "
                result += fill_text(d, indent + "--  ", 79)
            else:
                result += "".join(prefix + " " + p for p in d.splitlines())

            prev = d

    if result and separate_paragraphs and result[0] == "\n":
        result = result[1:]

    return result


def box(name, indent="   "):
    return indent + "-" * (len(name) + 6) + "\n" \
        + indent + "-- " + name + " --\n" \
        + indent + "-" * (len(name) + 6)


def indent_code(code, indent=3, addnewlines=True):
    """Return code properly indented and split on several lines.
       These are heuristics only, not perfect.
    """
    body = code.strip()
    if not body:
        return ""

    if addnewlines:
        # Add newlines where needed, but preserve existing blank lines
        body = re.sub(r";(?!\s*\n)", ";\n", body)
        body = re.sub(r"(?<!and )then(?!\s*\n)", "then\n", body)
        body = re.sub(r"(?<!or )else(?!\s*\n)", "else\n", body)
        body = re.sub(r"declare", "\ndeclare", body)
        body = re.sub(r"\bdo\b", "do\n", body)
        body = re.sub(r"\n\s*\n+", "\n\n", body)

    parent_count = 0
    result = ""

    for line in body.splitlines():
        if line.find("--") != -1:
            eol_comment = line[line.find("--"):].strip()
            line = line[:line.find("--")]
        else:
            eol_comment = ""

        line = line.strip()

        if line.startswith("end") \
           or line.startswith("elsif")  \
           or line.startswith("else")  \
           or line.startswith("begin") \
           or line.startswith("}"):   # for C
            indent -= 3

        old_parent = parent_count
        parent_count = parent_count + line.count("(") - line.count(")")

        if not line:
            if eol_comment:
                result += " " * indent

        elif line[0] == '(':
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

        result += line + eol_comment + "\n"

        if (line.endswith("then") and not line.endswith("and then")) \
           or line.endswith("loop") \
           or (line.endswith("else") and not line.endswith("or else"))\
           or line.endswith("begin") \
           or line.endswith("{") \
           or line.endswith("record") \
           or line.endswith("is") \
           or line.endswith("do") \
           or line.endswith("declare"):
            indent += 3

        # Case of generic instantiation:
        #     package A is
        #         new B ();
        if line.startswith("new"):
            indent -= 3

    return result


# The necessary setup to use a variable in a subprogram call. The returned
# values map to the following Ada code:
#   declare
# $(tmpvars)    # A list of LocalVar
#   begin
#      $(precall)
#      Call ($(call), ...)
# (postcall)
#   end;
# and are used in case temporary variables are needed. If not, only 'call'
# will have a non-null value

VariableCall = namedtuple('VariableCall',
                          ['call', 'precall', 'postcall', 'tmpvars'])


class Local_Var(object):
    __slots__ = ["name", "type", "default", "aliased", "constant"]

    def __init__(self, name, type, default="", aliased=False, constant=False):
        self.set_type(type)
        self.name = name
        self.default = default
        self.aliased = aliased
        self.constant = constant

    def __repr__(self):
        return "<Local_Var name=%s type=%s>" % (self.name, self.type)

    def set_type(self, type):
        if isinstance(type, str):
            self.type = AdaType(type)
        else:
            self.type = type

    def _type(self, lang, pkg):
        """`pkg` is the package in which we insert the variable, and is used
           to add necessary with statements, if any.
        """
        if lang == "ada":
            return self.type.as_ada_param(pkg)
        elif lang == "ada->c":
            return self.type.as_c_param(pkg)
        elif lang == "c->ada":
            return self.type.convert_from_c()[1]

    def spec(self, pkg, length=0, lang="ada", show_default=True):
        """Format the declaration for the variable or parameter.
           'length' is the minimum length that the name should occupy (for
           proper alignment when there are several variables.
        """
        t = self._type(lang=lang, pkg=pkg)
        aliased = ""
        if self.aliased:
            aliased = "aliased "

        if self.default and show_default:
            return "%-*s : %s%s%s := %s" % (
                length, self.name,
                "constant " if self.constant else "",
                aliased, t, self.default)
        else:
            return "%-*s : %s%s" % (length, self.name, aliased, t)

    def as_call(self, pkg, lang="ada", value=None):
        """Pass self (or the value) as a parameter to an Ada subprogram call,
           implemented in the given language. See comments at the beginning
           of this package for valid values of LANG.
           'pkg' is the instance of Package in which the call occurs.
           :return: an instance of VariableCall
        """
        n = value or self.name
        if isinstance(self.type, CType):
            return self.type.as_call(
                name=n, pkg=pkg, lang=lang, wrapper="%s",
                is_temporary_variable=True)
        else:
            return VariableCall(call=n, precall='', postcall='', tmpvars=[])


class Parameter(Local_Var):
    __slots__ = ["name", "type", "default", "aliased", "mode", "doc",
                 "ada_binding", "for_function", "is_temporary_variable",
                 "c_mode", "ownership", "is_caller_allocates"]

    def __init__(self, name, type, default="", doc="", mode="in",
                 for_function=False, ada_binding=True,
                 is_temporary_variable=False, c_mode="in", ownership="none",
                 is_caller_allocates=False):
        """
           'mode' is the mode for the Ada subprogram, and is automatically
              converted when generating a subprogram as a direct C import.

           :param for_function: whether the parameter belongs to a procedure
              or a function.

           :param ada_binding: if False, the parameter will not be displayed
              in the profile of Ada subprograms (although, of course, it will
              be passed to the C subprograms)

           :param is_temporary_variable: True if this parameter represents a
              local variable. In this case, we can sometimes avoid creating
              other such variables, a minor optimization.

           :param 'c_mode' is the original mode of parameter in C

           :param 'ownership' means that caller should free memory if 'full'

           :param is_caller_allocates: True if memory for the parameter should
              be allocated by method caller
        """
        assert mode in ("in", "out", "in out", "not null access",
                        "access"), "Incorrect mode: %s" % mode

        super(Parameter, self).__init__(name, type, default=default)
        self.mode = mode
        self.doc = doc
        self.ada_binding = ada_binding
        self.for_function = for_function
        self.is_temporary_variable = is_temporary_variable
        self.c_mode = c_mode
        self.ownership = ownership
        self.is_caller_allocates = is_caller_allocates

    def _type(self, lang, pkg):
        mode = self.mode

        if lang == "ada->c" and mode != "in" and self.for_function:
            mode = "access"

        if mode in ("in out", "out") and hasattr(self.type, "userecord"):
            userec = self.type.userecord
            self.type.userecord = False
            t = super(Parameter, self)._type(lang=lang, pkg=pkg)
            self.type.userecord = userec
        else:
            t = super(Parameter, self)._type(lang=lang, pkg=pkg)

        if mode != "in":
            return "%s %s" % (mode, t)
        return t

    def as_call(self, pkg, lang="ada", value=None):
        """'pkg' is the package instance in which the call occurs."""

        assert lang in ("ada", "c->ada", "ada->c")

        if not self.ada_binding:
            if self.default is not None:
                return VariableCall(call=self.default,
                                    precall='', postcall='', tmpvars=[])
            else:
                return VariableCall(call="Parameter not bound in Ada",
                                    precall='', postcall='', tmpvars=[])
        else:
            wrapper = "%s"
            n = value or self.name

            # We know that for a C function, an "in out" parameter is
            # implemented as an "access parameter", so we'll need to take a
            # 'Access. In the call to as_call() below, though, we still pass
            # the original mode ("in out") not "access", so that as_call()
            # knows whether we need to make a copy of the parameter or not.

            if lang == "ada->c" and self.mode != "in" and self.for_function:
                wrapper = "%s'Access"

            if isinstance(self.type, CType):
                return self.type.as_call(
                    name=n, pkg=pkg, lang=lang, mode=self.mode,
                    wrapper=wrapper,
                    is_temporary_variable=self.is_temporary_variable)
            else:
                return VariableCall(
                    call=n, precall='', postcall='', tmpvars=[])

    def direct_c_map(self):
        """Whether the parameter can be passed as is to C"""
        return self.type.direct_c_map()

    def value(self):
        if not self.ada_binding:
            if self.default is not None:
                return self.default
            else:
                return "Parameter not bound in Ada"
        else:
            return self.ada_binding


def base_name(qname):
    """Return the basename for a fully qualified name:
        Pkg.Name  => Name
    """
    if "." in qname:
        return qname[qname.rfind(".") + 1:]
    else:
        return qname


def package_name(qname):
    """Return the package part of a fully qualified name:
       Pkg.Child.Name  => Pkg.Child
       Name      => ""
    """
    if "." in qname:
        index = qname.rfind(".")
        if qname[index - 1] != '.':   # ignore ".." in ranges
            return qname[:index]
    return ""


max_profile_length = 79 - len(" is")


class Subprogram(object):

    """An Ada subprogram that we are generating"""

    def __init__(self, name, code="", plist=[], local_vars=[],
                 returns=None, doc=[], showdoc=True, convention=None,
                 lang="ada", allow_none=True):
        """Create a new subprogram.
           'plist' is a list of Parameter.
           'local_vars' is a list of Local_Var.
           'doc' is a string or a list of paragraphs.
           'code' can be the empty string, in which case no body is output.
           'lang' is the language for the types of parameters (see comment at
               the top of this file).
           'allow_none': when this is an anonymous subprogram (and therefore
               used for a callback), this indicates whether the callback can be
               null or not).
           The code will be automatically pretty-printed, and the appropriate
           pragma Unreferenced are also added automatically.
        """
        assert(returns is None or isinstance(returns, CType))
        assert(lang in ("ada", "c->ada", "ada->c"))

        self.name = name
        self.plist = plist
        self.returns = returns
        self.local = local_vars
        self.showdoc = showdoc
        self.convention = convention   # "lang"
        self.allow_none = allow_none
        self._import = None
        self._nested = []  # nested subprograms
        self._deprecated = (False, "")  # True if deprecated
        self._manual_body = None  # Written by user explicitly

        self.lang = lang  # Language for the types of parameters

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

    def set_body(self, body):
        """Overrides the body of the subprogram (after "is")"""
        self._manual_body = body

    def profile(self, pkg, indent="   ", maxlen=max_profile_length,
                as_type=False, specs=False):
        """Compute the profile for the subprogram"""

        returns = self.returns and self.returns.as_return(pkg=pkg)

        if returns:
            prefix = "function"
            if self.lang == "ada->c":
                suffix = " return %s" % returns[1]
            elif self.lang == "c->ada":
                suffix = " return %s" % returns[1]
            else:
                suffix = " return %s" % returns[0]
        else:
            prefix = "procedure"
            suffix = ""

        if (not as_type) and self.name:
            prefix = indent + prefix + " " + base_name(self.name)
        elif self.allow_none:
            prefix = "access %s" % prefix
        else:
            prefix = "not null access %s" % prefix

        if self.plist:
            # First test: all parameters on same line
            plist = [p.spec(pkg=pkg, lang=self.lang) for p in self.plist]
            p = " (" + "; ".join(plist) + ")"

            # If too long, split on several lines
            if len(p) + len(prefix) + len(suffix) > maxlen:
                max = max_length([p_iter.name for p_iter in self.plist])
                plist = [p_iter.spec(pkg=pkg, length=max, lang=self.lang,
                                     show_default=self.lang == "ada")
                         for p_iter in self.plist]
                p = "\n   " + indent + "(" \
                    + (";\n    " + indent).join(plist) + ")"

        else:
            p = ""

        # Should the "return" go on a separate line ?
        if p and suffix and len(p.splitlines()[-1]) + len(suffix) > maxlen:
            return prefix + p + "\n   " + indent + suffix
        else:
            return prefix + p + suffix

    def add_withs_for_subprogram(self, pkg, in_specs):
        """
        Add required withs to the package (either in specs or body)
        """
        if self.returns:
            r = self.returns.as_return(pkg=pkg)
            if self.lang == "ada->c":
                self.returns.add_with(pkg=pkg, specs=in_specs)
            elif self.lang == "c->ada":
                self.returns.add_with(pkg=pkg, specs=in_specs)
            else:
                pkg.add_with(package_name(r[0]), specs=in_specs)

        if self.plist:
            for p in self.plist:
                p.type.add_with(pkg=pkg, specs=in_specs)

    def formatted_doc(self, indent="   "):
        if self.showdoc:
            doc = [d for d in self.doc]
            if self._deprecated[0]:
                doc += [self._deprecated[1]]
            doc += [p.doc for p in self.plist]
        else:
            doc = []

        return format_doc(doc, indent=indent, separate_paragraphs=False)

    def spec(self, pkg, indent="   ", show_doc=True,
             maxlen=max_profile_length, as_type=False):
        """Return the spec of the subprogram"""

        result = self.profile(
            pkg=pkg, indent=indent, maxlen=maxlen, as_type=as_type) + ";"

        if self._import:
            result += "\n" + indent + self._import

        if self._deprecated[0]:
            result += "\n" + indent + "pragma Obsolescent (%s);" % self.name

        if self.convention:
            result += "\n" + indent \
                + "pragma Convention (%s, %s);" % (self.convention, self.name)

        if show_doc:
            doc = self.formatted_doc(indent=indent)
        else:
            doc = ""

        return result + doc

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

    def _format_local_vars(self, pkg, indent="   "):
        """The list of local variable declarations"""
        if self.local:
            max = max_length([p.name for p in self.local])
            result = []
            seen = set()

            for v in self.local:
                if v.name not in seen:
                    seen.add(v.name)
                    result.append(v.spec(pkg=pkg, length=max))

            return indent + "   " + (";\n   " + indent).join(result) + ";\n"
        else:
            return ""

    def body(self, pkg, indent="   "):
        if not self.code and not self._manual_body:
            return ""

        result = box(base_name(self.name), indent=indent) + "\n\n"
        profile = self.profile(pkg=pkg, indent=indent)
        result += profile

        if profile.find("\n") != -1:
            result += "\n" + indent + "is\n"
        else:
            result += " is\n"

        local = self._format_local_vars(pkg=pkg, indent=indent)
        auto = self._find_unreferenced(local_vars=local, indent=indent)

        for s in self._nested:
            auto += s.spec(pkg=pkg, indent=indent + "   ") + "\n"
            auto += s.body(pkg=pkg, indent=indent + "   ")

        auto += local
        auto += indent + "begin\n"
        auto += indent_code(self.code, indent=len(indent) + 3)

        if self._manual_body:
            result += indent + self._manual_body % {"auto": auto}
        else:
            result += auto

        return result + indent + "end %s;\n" % base_name(self.name)

    def call(self, in_pkg=None, extra_postcall="", values=dict(), lang=None):
        """A call to 'self'.
           The parameters that are passed to self are assumed to have the
           same name as in self's declaration. When 'self' is implemented
           as a pragma Import, proper conversions are done.
           'in_pkg' is used to fully qualify the name of the subprogram, to
           avoid ambiguities. This is optional. This is an instance of Package.

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

           See comments at the beginning of this package for valid LANG values.
        """

        assert(in_pkg is not None)
        assert(isinstance(in_pkg, Package))

        if lang:
            pass
        elif self._import:
            lang = "ada->c"
        else:
            lang = "ada"

        assert(lang in ("ada", "c->ada", "ada->c"))

        tmpvars = []
        precall = ""
        params = []
        postcall = extra_postcall

        for arg in self.plist:
            c = arg.as_call(
                pkg=in_pkg,
                lang=lang,   # An instance of VariableCall
                value=values.get(arg.name.lower(), None))
            params.append(c.call)
            tmpvars.extend(c.tmpvars)
            precall += c.precall
            postcall = c.postcall + postcall

        if params:
            call = "%s (%s)" % (self.name, ", ".join(params))
        else:
            call = self.name

        returns = self.returns and self.returns.as_return(pkg=in_pkg)
        if returns is not None:
            if lang == "ada->c":
                self.returns.convert_from_c_add_with(pkg=in_pkg)

                tmpvars.extend(returns[3])
                if "%(tmp)s" in returns[2]:
                    # Result of Internal is used to create a temp. variable,
                    # which is then returned. This variable has the same type
                    # as the Ada type (not necessarily same as Internal)
                    call = returns[2] % {"var": call, "tmp": "Tmp_Return"}

                    tmpvars.append(Local_Var("Tmp_Return", returns[0]))
                    result = ("%s%s;%s" % (precall, call, postcall),
                              "Tmp_Return",
                              tmpvars)

                elif postcall:
                    tmpvars.append(Local_Var("Tmp_Return", returns[1]))
                    call = "Tmp_Return := %s" % call
                    result = ("%s%s;%s" % (precall, call, postcall),
                              returns[2] % {"var": "Tmp_Return"},
                              tmpvars)

                else:
                    # No need for a temporary variable
                    result = (precall, returns[2] % {"var": call}, tmpvars)

            else:  # "ada" or "c->ada"
                if postcall:
                    # We need to use a temporary variable, since there are
                    # cleanups to perform. This will not work if the function
                    # returns an unconstrained array though.
                    tmpvars.append(Local_Var("Tmp_Return", returns[0]))
                    call = "Tmp_Return := %s" % call
                    result = ("%s%s;%s" % (precall, call, postcall),
                              "Tmp_Return",
                              tmpvars)
                else:
                    # No need for a temporary variable
                    result = (precall, call, tmpvars)

        else:
            # A procedure
            result = ("%s%s;%s" % (precall, call, postcall), None, tmpvars)

        return result

    def call_to_string(self, call, pkg=None, lang="ada"):
        """CALL is the result of call() above.
           This function returns a string that contains the code for the
           subprogram.
        """
        result = call[0]
        if call[1]:
            if lang == "c->ada":
                # The return value (Ada) needs to be converted back to C (this
                # is the returned value from a callback, for instance)
                result += "return %s" % (
                    self.returns.convert_to_c(pkg=pkg) % {"var": call[1]}, )
            else:
                result += "return %s" % call[1]
        return result


class Code(object):
    """
    Some text to insert in a package.  This can be either some code, or the
    comments for the code. In the latter case, the comment will be
    automatically formatted (and C names substituted as appropriate).
    """

    def __init__(self, content, iscomment=False, fill=True):
        """:param:`fill` whether to reflow the text if this is a comment.
           :param:`add_newline` whether the block should have a leading newline
        """
        self.content = content
        self.iscomment = iscomment
        self.fill = fill
        self.add_newline = True

    def format(self, indent=""):
        """Return the code that should be written into a package"""
        if self.iscomment:
            return format_doc(self.content, indent=indent, fill=self.fill)
        else:
            return indent_code(
                self.content, indent=len(indent), addnewlines=False)


class Section(object):
    """A group of types and subprograms in an Ada package.
       There is a single section with a given name in the package
    """

    group_getters_and_setters = False
    # If true, a getter will be displayed with its corresponding setter.
    # Only one doc will be displayed for the two, and no separation line
    # will be output.

    sort_alphabetically = False
    # If true, subprograms are all sorted alphabetically, otherwise the
    # order is alphabetical for getters, but setters appear just after the
    # getter.

    def __init__(self, pkg, name):
        self.pkg = pkg  # The instance of Package in which the section is
        self.name = name
        self.__comment = ""
        self.__objects = []  # List of objects. These are tuples:
        #    (Code or Subprogram or Package instance,
        #     in_spec)

        # Whether we should sort the objects. If yes, code always comes before
        # subprograms. Otherwise, they are output in the order they were added
        self.sort_objects = (
            not Section.sort_alphabetically or
            Section.group_getters_and_setters)

    def add_comment(self, comment, fill=True):
        """If 'fill' is true, the comment is automatically split on several
           lines if needed. Otherwise, the comment is assumed to be already
           formatted properly, minus the leading --
        """
        if comment == "":
            self.__comment += "   --\n"
        else:
            self.__comment += ("".join(
                format_doc(comment, indent="   ", fill=fill)) +
                "\n")

    def add(self, obj, in_spec=True, add_newline=True):
        """Add one or more objects to the section (subprogram, code,...).
           :param:`add_newline` indicates whether the object should be
           followed by a blank line. There is never a blank line between some
           code and the following comment.
        """

        iscode = False

        if isinstance(obj, bytes) or isinstance(obj, str):
            obj = Code(obj)
            iscode = True
        elif isinstance(obj, Package):
            obj.isnested = True

        obj.add_newline = add_newline

        if iscode:
            #  Take care of duplicated entries. May happen in case of
            #  subpackages in GIR files
            for o, s in self.__objects:
                if s == in_spec and isinstance(o, Code):
                    if o.content == obj.content:
                        return False
        self.__objects.append((obj, in_spec))

        return True

    def _group_objects(self):
        """Returns a list of subprograms for the specs. In each nested list,
           the subprograms are grouped and a single documentation is output for
           the whole group. At the same time, this preserves the order of
           groups, so they appear in the order in which the first subprogram
           in the group appeared.
        """

        if self.sort_objects:
            code = []
            subprograms = []
            tmp = dict()  # group_name => [subprograms]

            gtk_new_index = 0

            for obj, in_spec in self.__objects:
                if not in_spec:
                    continue

                if isinstance(obj, Code) or isinstance(obj, str):
                    code.append([obj])

                elif isinstance(obj, Subprogram) or isinstance(obj, Package):
                    b = base_name(obj.name)
                    name = b.replace("Get_", "") \
                        .replace("Query_", "") \
                        .replace("Gtk_New", "") \
                        .replace("Gdk_New", "") \
                        .replace("Initialize", "") \
                        .replace("Set_From_", "") \
                        .replace("Set_", "")

                    if b in ("Gtk_New", "Gdk_New", "G_New"):
                        # Always create a new group for Gtk_New, since they all
                        # have different parameters. But we still want to group
                        # Gtk_New and Initialize.
                        t = tmp["Gtk_New%d" % gtk_new_index] = [obj]
                        subprograms.append(t)
                    elif b == "Initialize":
                        tmp["Gtk_New%d" % gtk_new_index].append(obj)
                        gtk_new_index += 1
                    elif name in tmp:
                        tmp[name].append(obj)  # Also modified in result
                    else:
                        tmp[name] = [obj]
                        subprograms.append(tmp[name])

                else:
                    print("Unexpected contents in package %s\n" %
                          (type(obj), ))

            return code + subprograms

        else:
            return [[obj]
                    for obj, in_spec in self.__objects
                    if in_spec]

    def spec(self, pkg, indent):
        """Return the spec of the section"""

        result = ""
        add_newline = False

        for group in self._group_objects():
            for obj in group:
                # If the previous object requested a trailing newline, and the
                # current object is not a comment, then add the newline now.
                if (add_newline and
                   (not isinstance(obj, Code) or not obj.iscomment)):

                    result += "\n"

                if isinstance(obj, Code):
                    result += obj.format(indent=indent).strip("\n") + "\n"
                    add_newline = obj.add_newline

                elif isinstance(obj, Subprogram):
                    show_doc = ((not Section.group_getters_and_setters and
                                 not obj.name.startswith("Gtk_New")) or
                                obj == group[-1])
                    result += obj.spec(pkg=pkg,
                                       show_doc=show_doc,
                                       indent=indent).strip("\n") + "\n"
                    add_newline = (hasattr(obj, "add_newline") and
                                   obj.add_newline and
                                   show_doc)

                elif isinstance(obj, Package):
                    result += obj.spec().strip("\n") + "\n"
                    add_newline = (hasattr(obj, "add_newline") and
                                   obj.add_newline)

                elif isinstance(obj, str):
                    print("Not adding unicode to package: %s\n" % (
                        obj.encode('UTF-8'), ))

        if add_newline:
            result += "\n"

        if result:
            if self.__comment:
                result = self.__comment + "\n" + result
            elif self.name:
                result = "\n" + result
            if self.name:
                result = box(self.name) + "\n" + result

        return result

    def body(self, pkg, indent):
        result = []

        for obj, in_spec in self.__objects:
            if in_spec or not isinstance(obj, Code):
                continue
            result.append(obj.format(indent=indent))  # ignores obj.add_newline

        body_subprograms = [(obj, in_spec) for obj, in_spec in self.__objects
                            if not isinstance(obj, Code)]
        body_subprograms.sort(key=lambda x: base_name(x[0].name))

        # First output for the subprograms only defined in the body

        for obj, in_spec in body_subprograms:
            if not in_spec:
                if isinstance(obj, Subprogram):
                    result.append(obj.spec(pkg=pkg, indent=indent))
                else:
                    result.append(obj.spec())
                result.append("")

        # Then output all the bodiesx

        for obj, in_spec in body_subprograms:
            if isinstance(obj, Subprogram):
                b = obj.body(pkg=pkg, indent=indent)
            else:
                b = obj.body() + "\n"
            if b:
                result.append(b)

        return "\n".join(result)


class Package(object):
    copyright_header = ""
    # Can be overridden by applications to change the copyright header

    def __init__(self, name, doc=[], isnested=False):
        """'doc' is a list of strings, where each string is a paragraph"""
        self.name = name
        self.doc = doc

        self.sections = []       # [Section]
        self.spec_withs = dict()  # "pkg" -> use:Boolean
        self.body_withs = dict()  # "pkg" -> use:Boolean
        self.private = []        # Private section
        self.language_version = ""  # a pragma to be put just after the headers
        self.formal_params = ""  # generic formal parameters
        self.isnested = isnested

    def __repr__(self):
        return "<Package %s>" % self.name

    def section(self, name):
        """Return an existing section (or create a new one) with the given
           name.
        """
        for s in self.sections:
            if s.name == name:
                return s

        s = Section(self, name)
        self.sections.append(s)
        return s

    def add_with(self, pkg, specs=True, do_use=True, might_be_unused=False):
        """Add a with+use clause for pkg, where pkg can also be a list.
           Automatic casing is performed. If specs is True, the withs are
           added to the specs of the package, otherwise to the body.

           :param:`might_be_unused` True if the package might not be used and
              requires a pragma Warnings Off.
        """
        if pkg in ("", "System"):
            return

        if type(pkg) == str:
            pkg = [pkg]
        for p in pkg:
            def sublist(sub, parent):
                """return a non-empty list if sub is not a sublist of parent"""
                return [elem for elem in sub if elem not in parent]
            l_name = self.name.lower().split(".")
            l_p_name = p.lower().split(".")
            if not sublist(l_p_name, l_name):
                continue   # Already imported by construction

            p_info = (
                do_use or self.spec_withs.get(p, False),   # do_use
                might_be_unused)

            if specs:
                self.spec_withs[p] = p_info
                self.body_withs.pop(p, None)  # Remove same with in body
            elif p not in self.spec_withs:
                self.body_withs[p] = p_info

    def add_private(self, code, at_end=False):
        if at_end:
            self.private.append(code)
        else:
            self.private.insert(0, code)

    def _output_withs(self, withs):
        if withs:
            result = []
            m = max_length(withs)
            had_warnings_off = False

            # sort so that all packages for which 'might_be_unused' is True
            # are last in the list

            for w in sorted(list(withs.keys()),
                            key=lambda w: 'zz%s' % w if withs[w][1] else w):
                do_use, might_be_unused = withs[w]

                if might_be_unused and not had_warnings_off:
                    result.append("pragma Warnings(Off);  --  might be unused")
                    had_warnings_off = True

                if do_use:
                    result.append(
                        "with %-*s use %s;" % (m + 1, w + ";", w))
                else:
                    result.append("with %s;" % w)

            if had_warnings_off:
                result.append("pragma Warnings(On);")

            return "\n".join(result) + "\n"
        return ""

    def section_order(self, name):
        """Return a numerical order for sections"""
        order = {"": 0,
                 "Callbacks": 1,
                 "Enumeration Properties": 2,

                 # Primitive operations first
                 "Constructors": 3,
                 "Methods": 4,
                 "GtkAda additions": 5,
                 "Inherited subprograms (from interfaces)": 6,

                 # Then non-primitive (so that we can freeze the type, for
                 # instance by instantiating lists)
                 "Functions": 8,
                 "Lists": 9,

                 # General data independent of the type
                 "Properties": 10,
                 "Signals": 11,

                 # Instantiating new generic packages freezes the types, so
                 # should be last
                 "Interfaces": 12,
                 }
        return order.get(name, 1000)

    def spec(self):
        """Returns the spec of the package, in the file `out`"""

        result = []

        if not self.isnested:
            indent = ""
            if Package.copyright_header:
                result.append(Package.copyright_header)

            if self.language_version:
                result.append(self.language_version)

            if self.doc:
                result.append(format_doc(self.doc, indent=""))

            result.append('')
            result.append('pragma Warnings (Off, "*is already use-visible*");')
            result.append(self._output_withs(self.spec_withs))

        else:
            indent = "   "

        if self.formal_params:
            result.append(indent + "generic")
            result.append(indent + "   %s" % self.formal_params)
        result.append(indent + "package %s is\n" % self.name)

        self.sections.sort(key=lambda x: (self.section_order(x.name)))

        for s in self.sections:
            sec = s.spec(pkg=self, indent=indent + "   ")
            if sec:
                result.append(sec.strip("\n") + "\n")

        if self.private:
            result.append(indent + "private")
            result.extend(self.private)

        result.append(indent + "end %s;" % self.name)
        return "\n".join(result)

    def body(self):
        """Returns the body of the package"""

        result = []
        body = ""

        if self.isnested:
            indent = "   "
        else:
            indent = ""

        for s in self.sections:
            b = s.body(pkg=self, indent=indent + "   ")
            if b:
                body += "\n" + b

        if not body:
            return ""

        if not self.isnested:
            if Package.copyright_header:
                result.append(Package.copyright_header)

            if self.language_version:
                result.append(self.language_version)

            result.append("pragma Style_Checks (Off);")
            result.append('pragma Warnings (Off, "*is already use-visible*");')
            result.append(self._output_withs(self.body_withs))

        result.append(indent + "package body %s is" % self.name)
        result.append(body)
        result.append(indent + "end %s;" % self.name)
        return "\n".join(result)
