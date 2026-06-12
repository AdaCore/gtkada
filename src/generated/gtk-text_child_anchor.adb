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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Text_Child_Anchor is

   -----------------
   -- Get_Widgets --
   -----------------

   function Get_Widgets
     (Anchor : not null access Gtk_Text_Child_Anchor_Record)
   return Gtk_Widget_Array
   is
      use type Guint;

      type Address_Array is array (Natural) of System.Address;
      pragma Convention (C, Address_Array);
      type Address_Array_Access is access all Address_Array;

      function Internal
        (Anchor  : System.Address;
         Out_Len : access Guint) return Address_Array_Access;
      pragma Import (C, Internal, "gtk_text_child_anchor_get_widgets");

      procedure G_Free (Mem : Address_Array_Access);
      pragma Import (C, G_Free, "g_free");

      Len : aliased Guint;
      Arr : constant Address_Array_Access :=
      Internal (Get_Object (Anchor), Len'Access);
   begin
      declare
         Result : Gtk_Widget_Array (0 .. Natural (Len) - 1);
         Stub   : Gtk.Widget.Gtk_Widget_Record;
      begin
         for J in Result'Range loop
            Result (J) :=
            Gtk.Widget.Gtk_Widget (Get_User_Data (Arr (J), Stub));
         end loop;

         G_Free (Arr);
         return Result;
      end;
   end Get_Widgets;

   ----------------------
   -- Get_Child_Anchor --
   ----------------------

   function Get_Child_Anchor
     (Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Gtk_Text_Child_Anchor
   is
      function Internal
        (Iter : Gtk.Text_Iter.Gtk_Text_Iter) return System.Address;
      pragma Import (C, Internal, "gtk_text_iter_get_child_anchor");
      Stub : Gtk_Text_Child_Anchor_Record;
   begin
      return Gtk_Text_Child_Anchor
        (Get_User_Data (Internal (Iter), Stub));
   end Get_Child_Anchor;

   package Type_Conversion_Gtk_Text_Child_Anchor is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Text_Child_Anchor_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Text_Child_Anchor);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Anchor : out Gtk_Text_Child_Anchor) is
   begin
      Anchor := new Gtk_Text_Child_Anchor_Record;
      Gtk.Text_Child_Anchor.Initialize (Anchor);
   end Gtk_New;

   ------------------------------
   -- Gtk_New_With_Replacement --
   ------------------------------

   procedure Gtk_New_With_Replacement
      (Anchor    : out Gtk_Text_Child_Anchor;
       Character : UTF8_String)
   is
   begin
      Anchor := new Gtk_Text_Child_Anchor_Record;
      Gtk.Text_Child_Anchor.Initialize_With_Replacement (Anchor, Character);
   end Gtk_New_With_Replacement;

   -------------------------------
   -- Gtk_Text_Child_Anchor_New --
   -------------------------------

   function Gtk_Text_Child_Anchor_New return Gtk_Text_Child_Anchor is
      Anchor : constant Gtk_Text_Child_Anchor := new Gtk_Text_Child_Anchor_Record;
   begin
      Gtk.Text_Child_Anchor.Initialize (Anchor);
      return Anchor;
   end Gtk_Text_Child_Anchor_New;

   ------------------------------------------------
   -- Gtk_Text_Child_Anchor_New_With_Replacement --
   ------------------------------------------------

   function Gtk_Text_Child_Anchor_New_With_Replacement
      (Character : UTF8_String) return Gtk_Text_Child_Anchor
   is
      Anchor : constant Gtk_Text_Child_Anchor := new Gtk_Text_Child_Anchor_Record;
   begin
      Gtk.Text_Child_Anchor.Initialize_With_Replacement (Anchor, Character);
      return Anchor;
   end Gtk_Text_Child_Anchor_New_With_Replacement;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Anchor : not null access Gtk_Text_Child_Anchor_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_text_child_anchor_new");
   begin
      if not Anchor.Is_Created then
         Set_Object (Anchor, Internal);
      end if;
   end Initialize;

   ---------------------------------
   -- Initialize_With_Replacement --
   ---------------------------------

   procedure Initialize_With_Replacement
      (Anchor    : not null access Gtk_Text_Child_Anchor_Record'Class;
       Character : UTF8_String)
   is
      function Internal
         (Character : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_text_child_anchor_new_with_replacement");
      Tmp_Character : Gtkada.Types.Chars_Ptr := New_String (Character);
      Tmp_Return    : System.Address;
   begin
      if not Anchor.Is_Created then
         Tmp_Return := Internal (Tmp_Character);
         Free (Tmp_Character);
         Set_Object (Anchor, Tmp_Return);
      end if;
   end Initialize_With_Replacement;

   -----------------
   -- Get_Deleted --
   -----------------

   function Get_Deleted
      (Anchor : not null access Gtk_Text_Child_Anchor_Record) return Boolean
   is
      function Internal (Anchor : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_child_anchor_get_deleted");
   begin
      return Internal (Get_Object (Anchor)) /= 0;
   end Get_Deleted;

end Gtk.Text_Child_Anchor;
