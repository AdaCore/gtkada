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

--  A `GtkBuilderScope` implementation for the C language.
--
--  `GtkBuilderCScope` instances use symbols explicitly added to Builder with
--  prior calls to [methodGtk.BuilderCScope.add_callback_symbol]. If developers
--  want to do that, they are encouraged to create their own scopes for that
--  purpose.
--
--  In the case that symbols are not explicitly added; GTK will uses
--  `GModule`'s introspective features (by opening the module null) to look at
--  the application's symbol table. From here it tries to match the signal
--  function names given in the interface description with symbols in the
--  application.
--
--  Note that unless [methodGtk.BuilderCScope.add_callback_symbol] is called
--  for all signal callbacks which are referenced by the loaded XML, this
--  functionality will require that `GModule` be supported on the platform.

pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.Object;       use Glib.Object;
with Glib.Types;        use Glib.Types;
with Gtk.Builder_Scope; use Gtk.Builder_Scope;

package Gtk.Builder_CScope is

   type Gtk_Builder_C_Scope_Record is new GObject_Record with null record;
   type Gtk_Builder_C_Scope is access all Gtk_Builder_C_Scope_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gcallback is access procedure;
   --  The type used for callback functions in structure definitions and
   --  function signatures. This doesn't mean that all callback functions must
   --  take no parameters and return void. The required signature of a callback
   --  function is determined by the context in which is used (e.g. the signal
   --  to which it is connected). Use G_CALLBACK to cast the callback function
   --  to a Gcallback.

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Builder_C_Scope);
   procedure Initialize
      (Self : not null access Gtk_Builder_C_Scope_Record'Class);
   --  Creates a new `GtkBuilderCScope` object to use with future `GtkBuilder`
   --  instances.
   --  Calling this function is only necessary if you want to add custom
   --  callbacks via [methodGtk.BuilderCScope.add_callback_symbol].
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Builder_C_Scope_New return Gtk_Builder_C_Scope;
   --  Creates a new `GtkBuilderCScope` object to use with future `GtkBuilder`
   --  instances.
   --  Calling this function is only necessary if you want to add custom
   --  callbacks via [methodGtk.BuilderCScope.add_callback_symbol].

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_builder_cscope_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Callback_Symbol
      (Self            : not null access Gtk_Builder_C_Scope_Record;
       Callback_Name   : UTF8_String;
       Callback_Symbol : Gcallback);
   --  Adds the Callback_Symbol to the scope of Builder under the given
   --  Callback_Name.
   --  Using this function overrides the behavior of
   --  [methodGtk.Builder.create_closure] for any callback symbols that are
   --  added. Using this method allows for better encapsulation as it does not
   --  require that callback symbols be declared in the global namespace.
   --  @param Callback_Name The name of the callback, as expected in the XML
   --  @param Callback_Symbol The callback pointer

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.BuilderScope"

   package Implements_Gtk_Builder_Scope is new Glib.Types.Implements
     (Gtk.Builder_Scope.Gtk_Builder_Scope, Gtk_Builder_C_Scope_Record, Gtk_Builder_C_Scope);
   function "+"
     (Widget : access Gtk_Builder_C_Scope_Record'Class)
   return Gtk.Builder_Scope.Gtk_Builder_Scope
   renames Implements_Gtk_Builder_Scope.To_Interface;
   function "-"
     (Interf : Gtk.Builder_Scope.Gtk_Builder_Scope)
   return Gtk_Builder_C_Scope
   renames Implements_Gtk_Builder_Scope.To_Object;

end Gtk.Builder_CScope;
