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
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Text_Tag is

   function Convert (R : Gtk.Text_Tag.Gtk_Text_Tag) return System.Address is
   begin
      return Get_Object (R);
   end Convert;

   function Convert (R : System.Address) return Gtk.Text_Tag.Gtk_Text_Tag is
      Stub : Gtk.Text_Tag.Gtk_Text_Tag_Record;begin
         return Gtk.Text_Tag.Gtk_Text_Tag (Glib.Object.Get_User_Data (R, Stub));
      end Convert;

   package Type_Conversion_Gtk_Text_Tag is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Text_Tag_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Text_Tag);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Tag : out Gtk_Text_Tag; Name : UTF8_String := "") is
   begin
      Tag := new Gtk_Text_Tag_Record;
      Gtk.Text_Tag.Initialize (Tag, Name);
   end Gtk_New;

   ----------------------
   -- Gtk_Text_Tag_New --
   ----------------------

   function Gtk_Text_Tag_New (Name : UTF8_String := "") return Gtk_Text_Tag is
      Tag : constant Gtk_Text_Tag := new Gtk_Text_Tag_Record;
   begin
      Gtk.Text_Tag.Initialize (Tag, Name);
      return Tag;
   end Gtk_Text_Tag_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Tag  : not null access Gtk_Text_Tag_Record'Class;
       Name : UTF8_String := "")
   is
      function Internal
         (Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_text_tag_new");
      Tmp_Name   : Gtkada.Types.Chars_Ptr;
      Tmp_Return : System.Address;
   begin
      if not Tag.Is_Created then
         if Name = "" then
            Tmp_Name := Gtkada.Types.Null_Ptr;
         else
            Tmp_Name := New_String (Name);
         end if;
         Tmp_Return := Internal (Tmp_Name);
         Free (Tmp_Name);
         Set_Object (Tag, Tmp_Return);
      end if;
   end Initialize;

   -------------
   -- Changed --
   -------------

   procedure Changed
      (Tag          : not null access Gtk_Text_Tag_Record;
       Size_Changed : Boolean)
   is
      procedure Internal
         (Tag          : System.Address;
          Size_Changed : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_text_tag_changed");
   begin
      Internal (Get_Object (Tag), Boolean'Pos (Size_Changed));
   end Changed;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority
      (Tag : not null access Gtk_Text_Tag_Record) return Glib.Gint
   is
      function Internal (Tag : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_text_tag_get_priority");
   begin
      return Internal (Get_Object (Tag));
   end Get_Priority;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
      (Tag      : not null access Gtk_Text_Tag_Record;
       Priority : Glib.Gint)
   is
      procedure Internal (Tag : System.Address; Priority : Glib.Gint);
      pragma Import (C, Internal, "gtk_text_tag_set_priority");
   begin
      Internal (Get_Object (Tag), Priority);
   end Set_Priority;

end Gtk.Text_Tag;
