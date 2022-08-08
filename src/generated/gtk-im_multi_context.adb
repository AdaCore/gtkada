------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.IM_Multi_Context is

   package Type_Conversion_Gtk_IM_Multi_Context is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_IM_Multi_Context_Record);
   pragma Unreferenced (Type_Conversion_Gtk_IM_Multi_Context);

   ------------------------------
   -- Gtk_IM_Multi_Context_New --
   ------------------------------

   function Gtk_IM_Multi_Context_New return Gtk_IM_Multi_Context is
      Self : constant Gtk_IM_Multi_Context := new Gtk_IM_Multi_Context_Record;
   begin
      Gtk.IM_Multi_Context.Initialize (Self);
      return Self;
   end Gtk_IM_Multi_Context_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_IM_Multi_Context) is
   begin
      Self := new Gtk_IM_Multi_Context_Record;
      Gtk.IM_Multi_Context.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_IM_Multi_Context_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_im_multicontext_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ----------------------
   -- Append_Menuitems --
   ----------------------

   procedure Append_Menuitems
      (Self      : not null access Gtk_IM_Multi_Context_Record;
       Menushell : not null access Gtk.Menu_Shell.Gtk_Menu_Shell_Record'Class)
   is
      procedure Internal (Self : System.Address; Menushell : System.Address);
      pragma Import (C, Internal, "gtk_im_multicontext_append_menuitems");
   begin
      Internal (Get_Object (Self), Get_Object (Menushell));
   end Append_Menuitems;

   --------------------
   -- Get_Context_Id --
   --------------------

   function Get_Context_Id
      (Self : not null access Gtk_IM_Multi_Context_Record)
       return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_im_multicontext_get_context_id");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Context_Id;

   --------------------
   -- Set_Context_Id --
   --------------------

   procedure Set_Context_Id
      (Self       : not null access Gtk_IM_Multi_Context_Record;
       Context_Id : UTF8_String)
   is
      procedure Internal
         (Self       : System.Address;
          Context_Id : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_im_multicontext_set_context_id");
      Tmp_Context_Id : Gtkada.Types.Chars_Ptr := New_String (Context_Id);
   begin
      Internal (Get_Object (Self), Tmp_Context_Id);
      Free (Tmp_Context_Id);
   end Set_Context_Id;

end Gtk.IM_Multi_Context;
