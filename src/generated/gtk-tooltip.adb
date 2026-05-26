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

package body Gtk.Tooltip is

   package Type_Conversion_Gtk_Tooltip is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Tooltip_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Tooltip);

   ----------------
   -- Set_Custom --
   ----------------

   procedure Set_Custom
      (Self          : not null access Gtk_Tooltip_Record;
       Custom_Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Self          : System.Address;
          Custom_Widget : System.Address);
      pragma Import (C, Internal, "gtk_tooltip_set_custom");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Custom_Widget)));
   end Set_Custom;

   --------------
   -- Set_Icon --
   --------------

   procedure Set_Icon
      (Self      : not null access Gtk_Tooltip_Record;
       Paintable : Gdk.Paintable.Gdk_Paintable)
   is
      procedure Internal
         (Self      : System.Address;
          Paintable : Gdk.Paintable.Gdk_Paintable);
      pragma Import (C, Internal, "gtk_tooltip_set_icon");
   begin
      Internal (Get_Object (Self), Paintable);
   end Set_Icon;

   -------------------------
   -- Set_Icon_From_Gicon --
   -------------------------

   procedure Set_Icon_From_Gicon
      (Self   : not null access Gtk_Tooltip_Record;
       G_Icon : Glib.G_Icon.G_Icon)
   is
      procedure Internal
         (Self   : System.Address;
          G_Icon : Glib.G_Icon.G_Icon);
      pragma Import (C, Internal, "gtk_tooltip_set_icon_from_gicon");
   begin
      Internal (Get_Object (Self), G_Icon);
   end Set_Icon_From_Gicon;

   -----------------------------
   -- Set_Icon_From_Icon_Name --
   -----------------------------

   procedure Set_Icon_From_Icon_Name
      (Self      : not null access Gtk_Tooltip_Record;
       Icon_Name : UTF8_String := "")
   is
      procedure Internal
         (Self      : System.Address;
          Icon_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_tooltip_set_icon_from_icon_name");
      Tmp_Icon_Name : Gtkada.Types.Chars_Ptr;
   begin
      if Icon_Name = "" then
         Tmp_Icon_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Icon_Name := New_String (Icon_Name);
      end if;
      Internal (Get_Object (Self), Tmp_Icon_Name);
      Free (Tmp_Icon_Name);
   end Set_Icon_From_Icon_Name;

   ----------------
   -- Set_Markup --
   ----------------

   procedure Set_Markup
      (Self   : not null access Gtk_Tooltip_Record;
       Markup : UTF8_String := "")
   is
      procedure Internal
         (Self   : System.Address;
          Markup : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_tooltip_set_markup");
      Tmp_Markup : Gtkada.Types.Chars_Ptr;
   begin
      if Markup = "" then
         Tmp_Markup := Gtkada.Types.Null_Ptr;
      else
         Tmp_Markup := New_String (Markup);
      end if;
      Internal (Get_Object (Self), Tmp_Markup);
      Free (Tmp_Markup);
   end Set_Markup;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
      (Self : not null access Gtk_Tooltip_Record;
       Text : UTF8_String := "")
   is
      procedure Internal
         (Self : System.Address;
          Text : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_tooltip_set_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr;
   begin
      if Text = "" then
         Tmp_Text := Gtkada.Types.Null_Ptr;
      else
         Tmp_Text := New_String (Text);
      end if;
      Internal (Get_Object (Self), Tmp_Text);
      Free (Tmp_Text);
   end Set_Text;

   ------------------
   -- Set_Tip_Area --
   ------------------

   procedure Set_Tip_Area
      (Self : not null access Gtk_Tooltip_Record;
       Rect : Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
         (Self : System.Address;
          Rect : Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_tooltip_set_tip_area");
   begin
      Internal (Get_Object (Self), Rect);
   end Set_Tip_Area;

end Gtk.Tooltip;
