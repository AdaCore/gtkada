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

package body Gtk.Text_Attributes is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Text_Attr : out Gtk_Text_Attributes)
   is
      function Internal return Gtk_Text_Attributes;
      pragma Import (C, Internal, "gtk_text_attributes_new");
   begin
      Text_Attr := Internal;
   end Gtk_New;

   -----------------------
   -- Set_Strikethrough --
   -----------------------

   procedure Set_Strikethrough
     (Appearance : Gtk_Text_Appearance;
      Strikethrough : Boolean)
   is
      procedure Internal
        (Appearance    : Gtk_Text_Appearance;
         Strikethrough : Gboolean);

      pragma Import
        (C, Internal, "ada_text_appearance_set_strikethrough");
   begin
      Internal (Appearance, Boolean'Pos (Strikethrough));
   end Set_Strikethrough;

   -----------------------
   -- Get_Strikethrough --
   -----------------------

   function Get_Strikethrough
     (Appearance : Gtk_Text_Appearance) return Boolean

   is
      function Internal (Appearance : Gtk_Text_Appearance) return Gboolean;
      pragma Import
        (C, Internal, "ada_text_appearance_get_strikethrough");
   begin
      return Boolean'Val (Internal (Appearance));
   end Get_Strikethrough;

   -------------------
   -- Set_Invisible --
   -------------------

   procedure Set_Invisible
     (Text_Attr : Gtk_Text_Attributes;
      Invisible : Boolean)
   is
      procedure Internal
        (Text_Attr : Gtk_Text_Attributes;
         Invisible : Gboolean);

      pragma Import
        (C, Internal, "ada_text_attribute_set_invisible");
   begin
      Internal (Text_Attr, Boolean'Pos (Invisible));
   end Set_Invisible;

   -------------------
   -- Get_Invisible --
   -------------------

   function Get_Invisible
     (Text_Attr : Gtk_Text_Attributes) return Boolean
   is
      function Internal (Text_Attr : Gtk_Text_Attributes) return Gboolean;
      pragma Import
        (C, Internal, "ada_text_attribute_get_invisible");
   begin
      return Boolean'Val (Internal (Text_Attr));
   end Get_Invisible;

   -----------------------
   -- Set_BG_Full_Height --
   -----------------------

   procedure Set_Bg_Full_Height
     (Text_Attr   : Gtk_Text_Attributes;
      Full_Height : Boolean)
   is
      procedure Internal
        (Text_Attr   : Gtk_Text_Attributes;
         Full_Height : Gboolean);

      pragma Import
        (C, Internal, "ada_text_attribute_set_bg_full_height");
   begin
      Internal (Text_Attr, Boolean'Pos (Full_Height));
   end Set_Bg_Full_Height;

   -----------------------
   -- Get_BG_Full_Height --
   -----------------------

   function Get_Bg_Full_Height
     (Text_Attr : Gtk_Text_Attributes) return Boolean

   is
      function Internal (Text_Attr : Gtk_Text_Attributes) return Gboolean;
      pragma Import
        (C, Internal, "ada_text_attribute_get_bg_full_height");
   begin
      return Boolean'Val (Internal (Text_Attr));
   end Get_Bg_Full_Height;

   ------------------
   -- Set_Editable --
   ------------------

   procedure Set_Editable
     (Text_Attr : Gtk_Text_Attributes;
      Editable  : Boolean)
   is
      procedure Internal
        (Text_Attr    : Gtk_Text_Attributes;
         Editable     : Gboolean);

      pragma Import (C, Internal, "ada_text_attribute_set_editable");
   begin
      Internal (Text_Attr, Boolean'Pos (Editable));
   end Set_Editable;

   ------------------
   -- Get_Editable --
   ------------------

   function Get_Editable
     (Text_Attr : Gtk_Text_Attributes) return Boolean

   is
      function Internal (Text_Attr : Gtk_Text_Attributes) return Gboolean;
      pragma Import
        (C, Internal, "ada_text_attribute_get_editable");
   begin
      return Boolean'Val (Internal (Text_Attr));
   end Get_Editable;

   --------------
   -- Set_Tabs --
   --------------

   procedure Set_Tabs
      (Text_Attr : Gtk_Text_Attributes;
       Tabs      : Pango.Tabs.Pango_Tab_Array)
   is
      procedure Internal (A : Gtk_Text_Attributes; T : System.Address);
      pragma Import (C, Internal, "ada_text_attribute_set_tabs");
   begin
      Internal (Text_Attr, Tabs.Get_Object);
   end Set_Tabs;

   function Get_Tabs
     (Text_Attr : Gtk_Text_Attributes) return Pango.Tabs.Pango_Tab_Array
   is
      function Internal (A : Gtk_Text_Attributes) return System.Address;
      pragma Import (C, Internal, "ada_text_attribute_get_tabs");
   begin
      return Pango.Tabs.From_Object (Internal (Text_Attr));
   end Get_Tabs;

end Gtk.Text_Attributes;
