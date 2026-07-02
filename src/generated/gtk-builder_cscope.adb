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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Ada.Unchecked_Conversion;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Builder_CScope is

   procedure C_Gtk_Builder_Cscope_Add_Callback_Symbol
      (Self            : System.Address;
       Callback_Name   : Gtkada.Types.Chars_Ptr;
       Callback_Symbol : System.Address);
   pragma Import (C, C_Gtk_Builder_Cscope_Add_Callback_Symbol, "gtk_builder_cscope_add_callback_symbol");
   --  Adds the Callback_Symbol to the scope of Builder under the given
   --  Callback_Name.
   --  Using this function overrides the behavior of
   --  [methodGtk.Builder.create_closure] for any callback symbols that are
   --  added. Using this method allows for better encapsulation as it does not
   --  require that callback symbols be declared in the global namespace.
   --  @param Callback_Name The name of the callback, as expected in the XML
   --  @param Callback_Symbol The callback pointer

   function To_Gcallback is new Ada.Unchecked_Conversion
     (System.Address, Gcallback);

   function To_Address is new Ada.Unchecked_Conversion
     (Gcallback, System.Address);

   procedure Internal_Gcallback (Data : System.Address);
   pragma Convention (C, Internal_Gcallback);

   ------------------------
   -- Internal_Gcallback --
   ------------------------

   procedure Internal_Gcallback (Data : System.Address) is
      Func : constant Gcallback := To_Gcallback (Data);
   begin
      Func.all;
   end Internal_Gcallback;

   package Type_Conversion_Gtk_Builder_C_Scope is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Builder_C_Scope_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Builder_C_Scope);

   -----------------------------
   -- Gtk_Builder_C_Scope_New --
   -----------------------------

   function Gtk_Builder_C_Scope_New return Gtk_Builder_C_Scope is
      Self : constant Gtk_Builder_C_Scope := new Gtk_Builder_C_Scope_Record;
   begin
      Gtk.Builder_CScope.Initialize (Self);
      return Self;
   end Gtk_Builder_C_Scope_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Builder_C_Scope) is
   begin
      Self := new Gtk_Builder_C_Scope_Record;
      Gtk.Builder_CScope.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Builder_C_Scope_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_builder_cscope_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -------------------------
   -- Add_Callback_Symbol --
   -------------------------

   procedure Add_Callback_Symbol
      (Self            : not null access Gtk_Builder_C_Scope_Record;
       Callback_Name   : UTF8_String;
       Callback_Symbol : Gcallback)
   is
      Tmp_Callback_Name : Gtkada.Types.Chars_Ptr := New_String (Callback_Name);
   begin
      if Callback_Symbol = null then
         C_Gtk_Builder_Cscope_Add_Callback_Symbol (Get_Object (Self), Tmp_Callback_Name, System.Null_Address);
         Free (Tmp_Callback_Name);
      else
         C_Gtk_Builder_Cscope_Add_Callback_Symbol (Get_Object (Self), Tmp_Callback_Name, Internal_Gcallback'Address);
         Free (Tmp_Callback_Name);
      end if;
   end Add_Callback_Symbol;

end Gtk.Builder_CScope;
