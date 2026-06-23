------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  Provides language binding support to `GtkBuilder`.
--
--  The goal of `GtkBuilderScope` is to look up programming-language-specific
--  values for strings that are given in a `GtkBuilder` UI file.
--
--  The primary intended audience is bindings that want to provide deeper
--  integration of `GtkBuilder` into the language.
--
--  A `GtkBuilderScope` instance may be used with multiple `GtkBuilder`
--  objects, even at once.
--
--  By default, GTK will use its own implementation of `GtkBuilderScope` for
--  the C language which can be created via [ctorGtk.BuilderCScope.new].
--
--  If you implement `GtkBuilderScope` for a language binding, you may want to
--  (partially) derive from or fall back to a [classGtk.BuilderCScope], as that
--  class implements support for automatic lookups from C symbols.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Types;      use Glib.Types;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Bindings; use Gtkada.Bindings;
with Gtkada.Types;    use Gtkada.Types;

package Gtk.Builder_Scope is

   type Gtk_Builder_Scope is new Glib.Types.GType_Interface;
   Null_Gtk_Builder_Scope : constant Gtk_Builder_Scope;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_builder_scope_get_type");

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Builder_Scope"

   function "+" (W : Gtk_Builder_Scope) return Gtk_Builder_Scope;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Create_Closure is access function
     (Self          : Gtk_Builder_Scope;
      Builder       : System.Address;
      Function_Name : Gtkada.Types.Chars_Ptr;
      Flags         : Gtk.Enums.Gtk_Builder_Closure_Flags;
      Object        : System.Address) return System.Address;
   pragma Convention (C, Virtual_Create_Closure);
   --  Create a closure with the given arguments. See
   --  gtk_builder_create_closure for more details on those. The C
   --  implementation will try to use dlsym to locate the function name and
   --  then g_cclosure_new to create a closure for the symbol. The default
   --  implementation just fails and returns null.

   type Virtual_Get_Type_From_Function is access function
     (Self          : Gtk_Builder_Scope;
      Builder       : System.Address;
      Function_Name : Gtkada.Types.Chars_Ptr) return GType;
   pragma Convention (C, Virtual_Get_Type_From_Function);
   --  Try to lookup a `GType` via the given function name, specified
   --  explicitly in a GtkBuilder file, like via the "type-func" attribute in
   --  the `<object>` tag. This function is very rarely used. The C
   --  implementation will use dlsym and call the resulting function as a
   --  `GTypeFunc`. The default implementation will fail and just return
   --  G_TYPE_INVALID.

   type Virtual_Get_Type_From_Name is access function
     (Self      : Gtk_Builder_Scope;
      Builder   : System.Address;
      Type_Name : Gtkada.Types.Chars_Ptr) return GType;
   pragma Convention (C, Virtual_Get_Type_From_Name);
   --  Try to lookup a `GType` via the its name. See
   --  gtk_builder_get_type_from_name for more details. The C implementation
   --  will use g_type_from_name and if that fails try to guess the correct
   --  function name for registering the type and then use dlsym to load it.
   --  The default implementation just tries g_type_from_name and otherwise
   --  fails.

   subtype Builder_Scope_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Create_Closure
     (Self    : Builder_Scope_Interface_Descr;
      Handler : Virtual_Create_Closure);
   pragma Import (C, Set_Create_Closure, "gtkada_Builder_Scope_set_create_closure");

   procedure Set_Get_Type_From_Function
     (Self    : Builder_Scope_Interface_Descr;
      Handler : Virtual_Get_Type_From_Function);
   pragma Import (C, Set_Get_Type_From_Function, "gtkada_Builder_Scope_set_get_type_from_function");

   procedure Set_Get_Type_From_Name
     (Self    : Builder_Scope_Interface_Descr;
      Handler : Virtual_Get_Type_From_Name);
   pragma Import (C, Set_Get_Type_From_Name, "gtkada_Builder_Scope_set_get_type_from_name");
   --  See Glib.Object.Add_Interface

private

   Null_Gtk_Builder_Scope : constant Gtk_Builder_Scope :=
      Gtk_Builder_Scope (Glib.Types.Null_Interface);
end Gtk.Builder_Scope;
