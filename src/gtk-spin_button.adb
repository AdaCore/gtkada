-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with System;
with Gdk; use Gdk;
with Gtk.Util; use Gtk.Util;

package body Gtk.Spin_Button is

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Spin_Button : access Gtk_Spin_Button_Record;
      Adjustment  : Gtk.Adjustment.Gtk_Adjustment;
      Climb_Rate  : in Gfloat;
      The_Digits  : in Gint)
   is
      procedure Internal
        (Spin_Button : in System.Address;
         Adjustment  : in System.Address;
         Climb_Rate  : in Gfloat;
         The_Digits  : in Gint);
      pragma Import (C, Internal, "gtk_spin_button_configure");
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      if Adjustment = null then
         Internal (Get_Object (Spin_Button), System.Null_Address,
                   Climb_Rate, The_Digits);
      else
         Internal
           (Get_Object (Spin_Button), Get_Object (Adjustment), Climb_Rate,
            The_Digits);
      end if;
   end Configure;

   --------------------
   -- Get_Adjustment --
   --------------------

   function Get_Adjustment (Spin_Button : access Gtk_Spin_Button_Record)
     return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Spin_Button : in System.Address)
                         return           System.Address;
      pragma Import (C, Internal, "gtk_spin_button_get_adjustment");
      Stub : Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Spin_Button)), Stub));
   end Get_Adjustment;

   ------------------------
   -- Get_Value_As_Float --
   ------------------------

   function Get_Value_As_Float (Spin_Button : access Gtk_Spin_Button_Record)
     return Gfloat
   is
      function Internal (Spin_Button : in System.Address)
                         return           Gfloat;
      pragma Import (C, Internal, "gtk_spin_button_get_value_as_float");
   begin
      return Internal (Get_Object (Spin_Button));
   end Get_Value_As_Float;

   ----------------------
   -- Get_Value_As_Int --
   ----------------------

   function Get_Value_As_Int (Spin_Button : access Gtk_Spin_Button_Record)
     return Gint
   is
      function Internal (Spin_Button : in System.Address)
                         return           Gint;
      pragma Import (C, Internal, "gtk_spin_button_get_value_as_int");
   begin
      return Internal (Get_Object (Spin_Button));
   end Get_Value_As_Int;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Spin_Button : out Gtk_Spin_Button;
      Adjustment  : Gtk.Adjustment.Gtk_Adjustment;
      Climb_Rate  : in Gfloat;
      The_Digits  : in Gint)
   is
   begin
      Spin_Button := new Gtk_Spin_Button_Record;
      Initialize (Spin_Button, Adjustment, Climb_Rate, The_Digits);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Spin_Button : access Gtk_Spin_Button_Record'Class;
      Adjustment  : Gtk.Adjustment.Gtk_Adjustment;
      Climb_Rate  : in Gfloat;
      The_Digits  : in Gint)
   is
      function Internal
        (Adjustment : in System.Address;
         Climb_Rate : in Gfloat;
         The_Digits : in Gint)
         return          System.Address;
      pragma Import (C, Internal, "gtk_spin_button_new");
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      if Adjustment = null then
         Set_Object (Spin_Button,
                     Internal (System.Null_Address, Climb_Rate, The_Digits));
      else
         Set_Object
           (Spin_Button,
            Internal (Get_Object (Adjustment), Climb_Rate, The_Digits));
      end if;
      Initialize_User_Data (Spin_Button);
   end Initialize;

   --------------------
   -- Set_Adjustment --
   --------------------

   procedure Set_Adjustment
     (Spin_Button : access Gtk_Spin_Button_Record;
      Adjustment  : Gtk.Adjustment.Gtk_Adjustment)
   is
      procedure Internal
        (Spin_Button : in System.Address;
         Adjustment  : in System.Address);
      pragma Import (C, Internal, "gtk_spin_button_set_adjustment");
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      if Adjustment = null then
         Internal (Get_Object (Spin_Button), System.Null_Address);
      else
         Internal (Get_Object (Spin_Button), Get_Object (Adjustment));
      end if;
   end Set_Adjustment;

   ----------------
   -- Set_Digits --
   ----------------

   procedure Set_Digits
     (Spin_Button : access Gtk_Spin_Button_Record;
      The_Digits  : in Gint)
   is
      procedure Internal
        (Spin_Button : in System.Address;
         The_Digits  : in Gint);
      pragma Import (C, Internal, "gtk_spin_button_set_digits");
   begin
      Internal (Get_Object (Spin_Button), The_Digits);
   end Set_Digits;

   -----------------
   -- Set_Numeric --
   -----------------

   procedure Set_Numeric
     (Spin_Button : access Gtk_Spin_Button_Record;
      Numeric     : in Boolean)
   is
      procedure Internal
        (Spin_Button : in System.Address;
         Numeric     : in Gint);
      pragma Import (C, Internal, "gtk_spin_button_set_numeric");
   begin
      Internal (Get_Object (Spin_Button), Boolean'Pos (Numeric));
   end Set_Numeric;

   -----------------------
   -- Set_Snap_To_Ticks --
   -----------------------

   procedure Set_Snap_To_Ticks
    (Spin_Button   : access Gtk_Spin_Button_Record;
     Snap_To_Ticks : in Boolean)
   is
      procedure Internal
        (Spin_Button   : in System.Address;
         Snap_To_Ticks : in Gint);
      pragma Import (C, Internal, "gtk_spin_button_set_snap_to_ticks");
   begin
      Internal (Get_Object (Spin_Button), Boolean'Pos (Snap_To_Ticks));
   end Set_Snap_To_Ticks;

   -----------------------
   -- Set_Update_Policy --
   -----------------------

   procedure Set_Update_Policy
     (Spin_Button : access Gtk_Spin_Button_Record;
      Policy      : in Gtk_Spin_Button_Update_Policy)
   is
      procedure Internal
        (Spin_Button : in System.Address;
         Policy      : in Gint);
      pragma Import (C, Internal, "gtk_spin_button_set_update_policy");
   begin
      Internal
        (Get_Object (Spin_Button), Gtk_Spin_Button_Update_Policy'Pos (Policy));
   end Set_Update_Policy;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Spin_Button : access Gtk_Spin_Button_Record;
      Value       : in Gfloat)
   is
      procedure Internal
        (Spin_Button : in System.Address;
         Value       : in Gfloat);
      pragma Import (C, Internal, "gtk_spin_button_set_value");
   begin
      Internal (Get_Object (Spin_Button), Value);
   end Set_Value;

   --------------
   -- Set_Wrap --
   --------------

   procedure Set_Wrap
     (Spin_Button : access Gtk_Spin_Button_Record;
      Wrap        : in Boolean)
   is
      procedure Internal
        (Spin_Button : in System.Address;
         Wrap        : in Gint);
      pragma Import (C, Internal, "gtk_spin_button_set_wrap");
   begin
      Internal (Get_Object (Spin_Button), Boolean'Pos (Wrap));
   end Set_Wrap;

   ----------
   -- Spin --
   ----------

   procedure Spin
     (Spin_Button : access Gtk_Spin_Button_Record;
      Direction   : in Gtk.Enums.Gtk_Arrow_Type;
      Step        : in Gfloat)
   is
      procedure Internal
        (Spin_Button : in System.Address;
         Direction   : in Guint;
         Step        : in Gfloat);
      pragma Import (C, Internal, "gtk_spin_button_spin");
   begin
      Internal (Get_Object (Spin_Button),
                Gtk.Enums.Gtk_Arrow_Type'Pos (Direction),
                Step);
   end Spin;

   --------------
   -- Generate --
   --------------

   procedure Generate (N : in Node_Ptr; File : in File_Type) is
      S   : String_Ptr;
      Top : constant String_Ptr := Get_Field (Find_Top_Widget (N), "name");

   begin
      if not N.Specific_Data.Created then
         S := Get_Field (N, "name");
         Add_Package ("Adjustment");
         Put_Line
           (File, "   Gtk_New (" & To_Ada (S.all) & "_Adj, " &
            To_Float (Get_Field (N, "value").all) & ", " &
            To_Float (Get_Field (N, "lower").all) & ", " &
            To_Float (Get_Field (N, "upper").all) & ", " &
            To_Float (Get_Field (N, "step").all)  & ", " &
            To_Float (Get_Field (N, "page").all)  & ", " &
            To_Float (Get_Field (N, "page_size").all) & ");");
         Add_Package ("Spin_Button");
         Put_Line (File, "   Gtk_New (" & To_Ada (Top.all) & "." &
           To_Ada (S.all) & ", " & To_Ada (S.all) & "_Adj, " &
           To_Float (Get_Field (N, "climb_rate").all) & ", " &
           Get_Field (N, "digits").all & ");");
         N.Specific_Data.Created := True;
      end if;

      GEntry.Generate (N, File);

      Gen_Set (N, "Spin_Button", "numeric", File);
      Gen_Set (N, "Spin_Button", "Snap_To_Ticks", "snap", "", "", "", File);
      Gen_Set (N, "Spin_Button", "update_policy", File);
      Gen_Set (N, "Spin_Button", "value", File, Is_Float => True);
      Gen_Set (N, "Spin_Button", "wrap", File);
   end Generate;

   procedure Generate
     (Spin_Button : in out Object.Gtk_Object; N : in Node_Ptr)
   is
      use Gtk.Adjustment;

      S   : String_Ptr;
      Adj : Gtk_Adjustment;

   begin
      if not N.Specific_Data.Created then
         Gtk_New
           (Adj,
            Gfloat'Value (Get_Field (N, "value").all),
            Gfloat'Value (Get_Field (N, "lower").all),
            Gfloat'Value (Get_Field (N, "upper").all),
            Gfloat'Value (Get_Field (N, "step").all),
            Gfloat'Value (Get_Field (N, "page").all),
            Gfloat'Value (Get_Field (N, "page_size").all));
         Gtk_New (Gtk_Spin_Button (Spin_Button), Adj,
           Gfloat'Value (Get_Field (N, "climb_rate").all),
           Gint'Value (Get_Field (N, "digits").all));
         Set_Object (Get_Field (N, "name"), Spin_Button);
         N.Specific_Data.Created := True;
      end if;

      GEntry.Generate (Spin_Button, N);

      S := Get_Field (N, "numeric");

      if S /= null then
         Set_Numeric (Gtk_Spin_Button (Spin_Button), Boolean'Value (S.all));
      end if;

      S := Get_Field (N, "snap");

      if S /= null then
         Set_Snap_To_Ticks
           (Gtk_Spin_Button (Spin_Button), Boolean'Value (S.all));
      end if;

      S := Get_Field (N, "update_policy");

      if S /= null then
         Set_Update_Policy
           (Gtk_Spin_Button (Spin_Button),
            Gtk_Spin_Button_Update_Policy'Value (S (S'First + 4 .. S'Last)));
      end if;

      S := Get_Field (N, "value");

      if S /= null then
         Set_Value (Gtk_Spin_Button (Spin_Button), Gfloat'Value (S.all));
      end if;

      S := Get_Field (N, "wrap");

      if S /= null then
         Set_Wrap (Gtk_Spin_Button (Spin_Button), Boolean'Value (S.all));
      end if;
   end Generate;

end Gtk.Spin_Button;
