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

package body Gtk.Scale is

   ----------------
   -- Draw_Value --
   ----------------

   procedure Draw_Value (Scale : access Gtk_Scale_Record) is
      procedure Internal (Scale : in System.Address);
      pragma Import (C, Internal, "gtk_scale_draw_value");

   begin
      Internal (Get_Object (Scale));
   end Draw_Value;

   ---------------------
   -- Get_Value_Width --
   ---------------------

   function Get_Value_Width (Scale  : access Gtk_Scale_Record) return Gint is
      function Internal (Scale  : in System.Address) return Gint;
      pragma Import (C, Internal, "gtk_scale_get_value_width");

   begin
      return Internal (Get_Object (Scale));
   end Get_Value_Width;

   --------------------
   -- Gtk_New_Hscale --
   --------------------

   procedure Gtk_New_Hscale
     (Scale      : out Gtk_Scale;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment) is
   begin
      Scale := new Gtk_Scale_Record;
      Initialize_Hscale (Scale, Adjustment);
   end Gtk_New_Hscale;

   --------------------
   -- Gtk_New_Hscale --
   --------------------

   procedure Gtk_New_Vscale
     (Scale      : out Gtk_Scale;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment) is
   begin
      Scale := new Gtk_Scale_Record;
      Initialize_Vscale (Scale, Adjustment);
   end Gtk_New_Vscale;

   -----------------------
   -- Initialize_Hscale --
   -----------------------

   procedure Initialize_Hscale
     (Scale      : access Gtk_Scale_Record;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment)
   is
      function Internal (Adjustment : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_hscale_new");

   begin
      Set_Object (Scale, Internal (Get_Object (Adjustment)));
      Initialize_User_Data (Scale);
   end Initialize_Hscale;

   -----------------------
   -- Initialize_Vscale --
   -----------------------

   procedure Initialize_Vscale
     (Scale      : access Gtk_Scale_Record;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment)
   is
      function Internal (Adjustment : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_vscale_new");

   begin
      Set_Object (Scale, Internal (Get_Object (Adjustment)));
      Initialize_User_Data (Scale);
   end Initialize_Vscale;

   ----------------
   -- Set_Digits --
   ----------------

   procedure Set_Digits
     (Scale      : access Gtk_Scale_Record;
      The_Digits : in Gint)
   is
      procedure Internal
        (Scale      : in System.Address;
         The_Digits : in Gint);
      pragma Import (C, Internal, "gtk_scale_set_digits");

   begin
      Internal (Get_Object (Scale), The_Digits);
   end Set_Digits;

   --------------------
   -- Set_Draw_Value --
   --------------------

   procedure Set_Draw_Value
     (Scale      : access Gtk_Scale_Record;
      Draw_Value : in Boolean)
   is
      procedure Internal
        (Scale      : in System.Address;
         Draw_Value : in Gint);
      pragma Import (C, Internal, "gtk_scale_set_draw_value");

   begin
      Internal (Get_Object (Scale), Boolean'Pos (Draw_Value));
   end Set_Draw_Value;

   -------------------
   -- Set_Value_Pos --
   -------------------

   procedure Set_Value_Pos
     (Scale : access Gtk_Scale_Record;
      Pos   : in Gtk_Position_Type)
   is
      procedure Internal
        (Scale : in System.Address;
         Pos   : in Gint);
      pragma Import (C, Internal, "gtk_scale_set_value_pos");

   begin
      Internal (Get_Object (Scale), Gtk_Position_Type'Pos (Pos));
   end Set_Value_Pos;

   --------------
   -- Generate --
   --------------

   procedure Generate (N : in Node_Ptr; File : in File_Type) is
      S     : String_Ptr;
      Class : String_Ptr := Get_Field (N, "class");

   begin
      if not N.Specific_Data.Created then
         S := Get_Field (N, "name");
         Add_Package ("Adjustment");
         Put_Line
           (File, "   Adjustment.Gtk_New (" & To_Ada (S.all) & "_Adj, " &
            To_Float (Get_Field (N, "hvalue").all) & ", " &
            To_Float (Get_Field (N, "hlower").all) & ", " &
            To_Float (Get_Field (N, "hupper").all) & ", " &
            To_Float (Get_Field (N, "hstep").all)  & ", " &
            To_Float (Get_Field (N, "hpage").all)  & ", " &
            To_Float (Get_Field (N, "hpage_size").all) & ");");

         Gen_New (N, "Scale", S.all & "Adj", "",
           Class (Class'First + 3) & "scale", File => File);
      end if;

      GRange.Generate (N, File);
      Gen_Set (N, "Scale", "digits", File => File);
      Gen_Set (N, "Scale", "draw_value", File => File);
      Gen_Set (N, "Scale", "value_pos", File => File);
   end Generate;

   procedure Generate (Scale : in out Object.Gtk_Object; N : in Node_Ptr) is
      S   : String_Ptr;
      Adj : Adjustment.Gtk_Adjustment;
      Class : String_Ptr := Get_Field (N, "class");

   begin
      if not N.Specific_Data.Created then
         Adjustment.Gtk_New
           (Adj,
            Gfloat'Value (Get_Field (N, "hvalue").all),
            Gfloat'Value (Get_Field (N, "hlower").all),
            Gfloat'Value (Get_Field (N, "hupper").all),
            Gfloat'Value (Get_Field (N, "hstep").all),
            Gfloat'Value (Get_Field (N, "hpage").all),
            Gfloat'Value (Get_Field (N, "hpage_size").all));

         if Class (Class'First + 3) = 'H' then
            Gtk_New_Hscale (Gtk_Scale (Scale), Adj);
         else
            Gtk_New_Vscale (Gtk_Scale (Scale), Adj);
         end if;

         Set_Object (Get_Field (N, "name"), Scale);
         N.Specific_Data.Created := True;
      end if;

      GRange.Generate (Scale, N);

      S := Get_Field (N, "digits");

      if S /= null then
         Set_Digits
           (Gtk_Scale (Scale), Gint'Value (S.all));
      end if;

      S := Get_Field (N, "draw_value");

      if S /= null then
         Set_Draw_Value
           (Gtk_Scale (Scale), Boolean'Value (S.all));
      end if;

      S := Get_Field (N, "value_pos");

      if S /= null then
         Set_Value_Pos
           (Gtk_Scale (Scale),
            Gtk_Position_Type'Value (S (S'First + 4 .. S'Last)));
      end if;
   end Generate;

end Gtk.Scale;
