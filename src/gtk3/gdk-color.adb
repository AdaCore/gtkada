------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2018, AdaCore                     --
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

with Glib.Generic_Properties; use Glib.Generic_Properties;
pragma Elaborate_All (Glib.Generic_Properties);
with System;
with Gtkada.Bindings;

package body Gdk.Color is

   function To_Address
     (C : Gdk_Color; Add : System.Address) return System.Address;
   package Color_Properties is new Generic_Internal_Boxed_Property
     (Gdk_Color, Gdk_Color_Type, To_Address);

   procedure Set_Value (Value : in out Glib.Values.GValue; Val : Gdk_Color)
                        renames Color_Properties.Set_Value;
   function  Get_Value (Value : Glib.Values.GValue) return Gdk_Color
                        renames Color_Properties.Get_Value;

   ----------------
   -- To_Address --
   ----------------

   function To_Address
     (C : Gdk_Color; Add : System.Address) return System.Address is
   begin
      if C = Null_Color then
         return System.Null_Address;
      else
         return Add;
      end if;
   end To_Address;

   -----------
   -- Equal --
   -----------

   function Equal (Colora, Colorb : Gdk_Color) return Boolean is
   begin
      return Colora.Red = Colorb.Red
        and then Colora.Blue = Colorb.Blue
        and then Colora.Green = Colorb.Green;
   end Equal;

   ----------
   -- Blue --
   ----------

   function Blue (Color : Gdk_Color) return Guint16 is
   begin
      return Color.Blue;
   end Blue;

   ----------
   -- Copy --
   ----------

   procedure Copy (Source : Gdk_Color; Destination : out Gdk_Color) is
      type Gdk_Color_Access is access Gdk_Color;
      pragma Convention (C, Gdk_Color_Access);

      function Internal (Source : System.Address) return Gdk_Color_Access;
      pragma Import (C, Internal, "gdk_color_copy");

   begin
      Destination := Internal (Source'Address).all;
   end Copy;

   -----------
   -- Green --
   -----------

   function Green (Color : Gdk_Color) return Guint16 is
   begin
      return Color.Green;
   end Green;

   -----------
   -- Parse --
   -----------

   function Parse (Spec : String) return Gdk_Color is
      function Internal (Spec : String; Color : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_color_parse");

      Color : aliased Gdk_Color;

   begin
      if Internal (Spec & ASCII.NUL, Color'Address) = 0 then
         raise Wrong_Color with "Invalid color: '" & Spec & "'";
      end if;

      return Color;
   end Parse;

   ---------------
   -- To_String --
   ---------------

   function To_String (Color : Gdk_Color) return String is
      Result  : aliased String (1 .. 8);
      Len     : Gint;

      function sprintf
        (S : System.Address; Format : String;
         Arg1 : Gint; Arg2 : Gint; Arg3 : Gint) return Gint;
      pragma Import (C, sprintf, "c_sprintf");

   begin
      Len := sprintf
        (Result'Address, "#%02X%02X%02X" & ASCII.NUL,
         Gint (Color.Red / 256),
         Gint (Color.Green / 256),
         Gint (Color.Blue / 256));
      return Result (1 .. Integer (Len));
   end To_String;

   -----------
   -- Pixel --
   -----------

   function Pixel (Color : Gdk_Color) return Guint32 is
   begin
      return Color.Pixel;
   end Pixel;

   ---------
   -- Red --
   ---------

   function Red (Color : Gdk_Color) return Guint16 is
   begin
      return Color.Red;
   end Red;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel (Color : in out Gdk_Color; Pixel : Guint32) is
   begin
      Color.Pixel := Pixel;
   end Set_Pixel;

   -------------
   -- Set_Rgb --
   -------------

   procedure Set_Rgb (Color : out Gdk_Color; Red, Green, Blue : Guint16) is
   begin
      Color.Red := Red;
      Color.Green := Green;
      Color.Blue := Blue;
   end Set_Rgb;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_Gdk_Color;
      Value  : Gdk_Color) is
   begin
      Color_Properties.Set_Property
        (Object, Color_Properties.Property (Name), Value);
   end Set_Property;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_Gdk_Color) return Gdk_Color is
   begin
      return Color_Properties.Get_Property
        (Object, Color_Properties.Property (Name));
   end Get_Property;

   --------------
   -- To_Array --
   --------------

   function To_Array
      (Colors   : Gdk_Color_Unconstrained_Array;
       N_Colors : Gint) return Gdk_Color_Array
   is
      Result : Gdk_Color_Array (1 .. Integer (N_Colors));
   begin
      for R in Result'Range loop
         Result (R) := Colors (Colors'First + R - Result'First);
      end loop;
      return Result;
   end To_Array;

   -----------------------
   -- Gdk_Color_Or_Null --
   -----------------------

   function Gdk_Color_Or_Null (Val : System.Address) return System.Address is
      function Internal is new Gtkada.Bindings.Generic_To_Address_Or_Null
        (Gdk_Color, Null_Color, Equal);
   begin
      return Internal (Val);
   end Gdk_Color_Or_Null;

end Gdk.Color;
