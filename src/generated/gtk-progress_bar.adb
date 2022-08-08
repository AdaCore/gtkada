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

package body Gtk.Progress_Bar is

   package Type_Conversion_Gtk_Progress_Bar is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Progress_Bar_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Progress_Bar);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Progress_Bar : out Gtk_Progress_Bar) is
   begin
      Progress_Bar := new Gtk_Progress_Bar_Record;
      Gtk.Progress_Bar.Initialize (Progress_Bar);
   end Gtk_New;

   --------------------------
   -- Gtk_Progress_Bar_New --
   --------------------------

   function Gtk_Progress_Bar_New return Gtk_Progress_Bar is
      Progress_Bar : constant Gtk_Progress_Bar := new Gtk_Progress_Bar_Record;
   begin
      Gtk.Progress_Bar.Initialize (Progress_Bar);
      return Progress_Bar;
   end Gtk_Progress_Bar_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Progress_Bar : not null access Gtk_Progress_Bar_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_progress_bar_new");
   begin
      if not Progress_Bar.Is_Created then
         Set_Object (Progress_Bar, Internal);
      end if;
   end Initialize;

   -------------------
   -- Get_Ellipsize --
   -------------------

   function Get_Ellipsize
      (Progress_Bar : not null access Gtk_Progress_Bar_Record)
       return Pango.Layout.Pango_Ellipsize_Mode
   is
      function Internal
         (Progress_Bar : System.Address)
          return Pango.Layout.Pango_Ellipsize_Mode;
      pragma Import (C, Internal, "gtk_progress_bar_get_ellipsize");
   begin
      return Internal (Get_Object (Progress_Bar));
   end Get_Ellipsize;

   ------------------
   -- Get_Fraction --
   ------------------

   function Get_Fraction
      (Progress_Bar : not null access Gtk_Progress_Bar_Record)
       return Gdouble
   is
      function Internal (Progress_Bar : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_progress_bar_get_fraction");
   begin
      return Internal (Get_Object (Progress_Bar));
   end Get_Fraction;

   ------------------
   -- Get_Inverted --
   ------------------

   function Get_Inverted
      (Progress_Bar : not null access Gtk_Progress_Bar_Record)
       return Boolean
   is
      function Internal (Progress_Bar : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_progress_bar_get_inverted");
   begin
      return Internal (Get_Object (Progress_Bar)) /= 0;
   end Get_Inverted;

   --------------------
   -- Get_Pulse_Step --
   --------------------

   function Get_Pulse_Step
      (Progress_Bar : not null access Gtk_Progress_Bar_Record)
       return Gdouble
   is
      function Internal (Progress_Bar : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_progress_bar_get_pulse_step");
   begin
      return Internal (Get_Object (Progress_Bar));
   end Get_Pulse_Step;

   -------------------
   -- Get_Show_Text --
   -------------------

   function Get_Show_Text
      (Progress_Bar : not null access Gtk_Progress_Bar_Record)
       return Boolean
   is
      function Internal (Progress_Bar : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_progress_bar_get_show_text");
   begin
      return Internal (Get_Object (Progress_Bar)) /= 0;
   end Get_Show_Text;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
      (Progress_Bar : not null access Gtk_Progress_Bar_Record)
       return UTF8_String
   is
      function Internal
         (Progress_Bar : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_progress_bar_get_text");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Progress_Bar)));
   end Get_Text;

   -----------
   -- Pulse --
   -----------

   procedure Pulse (Progress_Bar : not null access Gtk_Progress_Bar_Record) is
      procedure Internal (Progress_Bar : System.Address);
      pragma Import (C, Internal, "gtk_progress_bar_pulse");
   begin
      Internal (Get_Object (Progress_Bar));
   end Pulse;

   -------------------
   -- Set_Ellipsize --
   -------------------

   procedure Set_Ellipsize
      (Progress_Bar : not null access Gtk_Progress_Bar_Record;
       Mode         : Pango.Layout.Pango_Ellipsize_Mode)
   is
      procedure Internal
         (Progress_Bar : System.Address;
          Mode         : Pango.Layout.Pango_Ellipsize_Mode);
      pragma Import (C, Internal, "gtk_progress_bar_set_ellipsize");
   begin
      Internal (Get_Object (Progress_Bar), Mode);
   end Set_Ellipsize;

   ------------------
   -- Set_Fraction --
   ------------------

   procedure Set_Fraction
      (Progress_Bar : not null access Gtk_Progress_Bar_Record;
       Fraction     : Gdouble)
   is
      procedure Internal (Progress_Bar : System.Address; Fraction : Gdouble);
      pragma Import (C, Internal, "gtk_progress_bar_set_fraction");
   begin
      Internal (Get_Object (Progress_Bar), Fraction);
   end Set_Fraction;

   ------------------
   -- Set_Inverted --
   ------------------

   procedure Set_Inverted
      (Progress_Bar : not null access Gtk_Progress_Bar_Record;
       Inverted     : Boolean)
   is
      procedure Internal
         (Progress_Bar : System.Address;
          Inverted     : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_progress_bar_set_inverted");
   begin
      Internal (Get_Object (Progress_Bar), Boolean'Pos (Inverted));
   end Set_Inverted;

   --------------------
   -- Set_Pulse_Step --
   --------------------

   procedure Set_Pulse_Step
      (Progress_Bar : not null access Gtk_Progress_Bar_Record;
       Fraction     : Gdouble)
   is
      procedure Internal (Progress_Bar : System.Address; Fraction : Gdouble);
      pragma Import (C, Internal, "gtk_progress_bar_set_pulse_step");
   begin
      Internal (Get_Object (Progress_Bar), Fraction);
   end Set_Pulse_Step;

   -------------------
   -- Set_Show_Text --
   -------------------

   procedure Set_Show_Text
      (Progress_Bar : not null access Gtk_Progress_Bar_Record;
       Show_Text    : Boolean)
   is
      procedure Internal
         (Progress_Bar : System.Address;
          Show_Text    : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_progress_bar_set_show_text");
   begin
      Internal (Get_Object (Progress_Bar), Boolean'Pos (Show_Text));
   end Set_Show_Text;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
      (Progress_Bar : not null access Gtk_Progress_Bar_Record;
       Text         : UTF8_String := "")
   is
      procedure Internal
         (Progress_Bar : System.Address;
          Text         : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_progress_bar_set_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr;
   begin
      if Text = "" then
         Tmp_Text := Gtkada.Types.Null_Ptr;
      else
         Tmp_Text := New_String (Text);
      end if;
      Internal (Get_Object (Progress_Bar), Tmp_Text);
      Free (Tmp_Text);
   end Set_Text;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Progress_Bar_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Internal (Get_Object (Self));
   end Get_Orientation;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : not null access Gtk_Progress_Bar_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Self        : System.Address;
          Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Orientation);
   end Set_Orientation;

end Gtk.Progress_Bar;
