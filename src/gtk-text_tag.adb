------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Gtk; use Gtk;

with Glib.Type_Conversion_Hooks;

package body Gtk.Text_Tag is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Text_Tag_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget : out Gtk_Text_Tag;
      Name   : String := "") is
   begin
      Widget := new Gtk_Text_Tag_Record;
      Initialize (Widget, Name);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Gtk_Text_Tag_Record'Class;
      Name   : String := "")
   is
      function Internal (Name : String) return System.Address;
      pragma Import (C, Internal, "gtk_text_tag_new");

      function Internal_No_Name (Dummy : System.Address) return System.Address;
      pragma Import (C, Internal_No_Name, "gtk_text_tag_new");
      --  Same as Internal except that we need to pass a null address for
      --  the name.

   begin
      if Name = "" then
         Set_Object (Widget, Internal_No_Name (System.Null_Address));
      else
         Set_Object (Widget, Internal (Name & ASCII.NUL));
      end if;
   end Initialize;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority (Tag : access Gtk_Text_Tag_Record) return Gint is
      function Internal (Tag : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_text_tag_get_priority");

   begin
      return Internal (Get_Object (Tag));
   end Get_Priority;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (Tag      : access Gtk_Text_Tag_Record;
      Priority : Gint)
   is
      procedure Internal (Tag : System.Address; Priority : Gint);
      pragma Import (C, Internal, "gtk_text_tag_set_priority");

   begin
      Internal (Get_Object (Tag), Priority);
   end Set_Priority;

   -------------
   -- Convert --
   -------------

   function Convert (W : Gtk_Text_Tag) return System.Address is
   begin
      if W = null then
         return System.Null_Address;
      else
         return Get_Object (W);
      end if;
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (W : System.Address) return Gtk_Text_Tag is
      Stub : Gtk_Text_Tag_Record;
   begin
      return Gtk_Text_Tag (Get_User_Data (W, Stub));
   end Convert;

end Gtk.Text_Tag;
