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

package body Gtk.Text_Child_Anchor is

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

   -------------------------------
   -- Gtk_Text_Child_Anchor_New --
   -------------------------------

   function Gtk_Text_Child_Anchor_New return Gtk_Text_Child_Anchor is
      Anchor : constant Gtk_Text_Child_Anchor := new Gtk_Text_Child_Anchor_Record;
   begin
      Gtk.Text_Child_Anchor.Initialize (Anchor);
      return Anchor;
   end Gtk_Text_Child_Anchor_New;

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

   -----------------
   -- Get_Widgets --
   -----------------

   function Get_Widgets
      (Anchor : not null access Gtk_Text_Child_Anchor_Record)
       return Gtk.Widget.Widget_SList.GSlist
   is
      function Internal (Anchor : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_text_child_anchor_get_widgets");
      Tmp_Return : Gtk.Widget.Widget_SList.GSlist;
   begin
      Gtk.Widget.Widget_SList.Set_Object (Tmp_Return, Internal (Get_Object (Anchor)));
      return Tmp_Return;
   end Get_Widgets;

end Gtk.Text_Child_Anchor;
