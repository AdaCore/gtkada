-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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

with Gtk.Adjustment;
with Gtk.Util; use Gtk.Util;
with System;

package body Gtk.Progress_Bar is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Progress_Bar : out Gtk_Progress_Bar;
      Adjustment   : in Gtk.Adjustment.Gtk_Adjustment :=
        Gtk.Adjustment.Null_Adjustment)
   is
   begin
      Progress_Bar := new Gtk_Progress_Bar_Record;
      Initialize (Progress_Bar, Adjustment);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Progress_Bar : access Gtk_Progress_Bar_Record'Class;
                         Adjustment   : in Gtk.Adjustment.Gtk_Adjustment)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_progress_bar_new");

      function Internal2 (Adjustment : in System.Address)
        return System.Address;
      pragma Import (C, Internal2, "gtk_progress_bar_new_with_adjustment");

      use type Gtk.Adjustment.Gtk_Adjustment;

   begin
      if Adjustment = null then
         Set_Object (Progress_Bar, Internal);
      else
         Set_Object (Progress_Bar, Internal2 (Get_Object (Adjustment)));
      end if;

      Initialize_User_Data (Progress_Bar);
   end Initialize;

   -------------------------
   -- Set_Activity_Blocks --
   -------------------------

   procedure Set_Activity_Blocks
     (Progress_Bar : access Gtk_Progress_Bar_Record;
      Blocks       : in Guint)
   is
      procedure Internal
        (Progress_Bar : in System.Address;
         Blocks       : in Guint);
      pragma Import (C, Internal, "gtk_progress_bar_set_activity_blocks");

   begin
      Internal (Get_Object (Progress_Bar), Blocks);
   end Set_Activity_Blocks;

   -----------------------
   -- Set_Activity_Step --
   -----------------------

   procedure Set_Activity_Step
     (Progress_Bar : access Gtk_Progress_Bar_Record;
      Step         : in Guint)
   is
      procedure Internal
        (Progress_Bar : in System.Address;
         Step         : in Guint);
      pragma Import (C, Internal, "gtk_progress_bar_set_activity_step");

   begin
      Internal (Get_Object (Progress_Bar), Step);
   end Set_Activity_Step;

   -------------------
   -- Set_Bar_Style --
   -------------------

   procedure Set_Bar_Style
     (Progress_Bar  : access Gtk_Progress_Bar_Record;
      Style         : in Gtk_Progress_Bar_Style)
   is
      procedure Internal
        (Progress_Bar : in System.Address;
         Style        : in Gint);
      pragma Import (C, Internal, "gtk_progress_bar_set_bar_style");

   begin
      Internal (Get_Object (Progress_Bar), Gtk_Progress_Bar_Style'Pos (Style));
   end Set_Bar_Style;

   -------------------------
   -- Set_Discrete_Blocks --
   -------------------------

   procedure Set_Discrete_Blocks
     (Progress_Bar : access Gtk_Progress_Bar_Record;
      Blocks       : in Guint)
   is
      procedure Internal
        (Progress_Bar : in System.Address;
         Blocks       : in Guint);
      pragma Import (C, Internal, "gtk_progress_bar_set_discrete_blocks");

   begin
      Internal (Get_Object (Progress_Bar), Blocks);
   end Set_Discrete_Blocks;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
     (Progress_Bar : access Gtk_Progress_Bar_Record;
      Orientation  : in Gtk_Progress_Bar_Orientation)
   is
      procedure Internal
        (Progress_Bar : in System.Address;
         Orientation  : in Gint);
      pragma Import (C, Internal, "gtk_progress_bar_set_orientation");

   begin
      Internal (Get_Object (Progress_Bar),
                Gtk_Progress_Bar_Orientation'Pos (Orientation));
   end Set_Orientation;

   ------------
   -- Update --
   ------------

   procedure Update
     (Progress_Bar : access Gtk_Progress_Bar_Record;
      Percentage   : in Gfloat)
   is
      procedure Internal
        (Progress_Bar : in System.Address;
         Percentage   : in Gfloat);
      pragma Import (C, Internal, "gtk_progress_bar_update");

   begin
      Internal (Get_Object (Progress_Bar), Percentage);
   end Update;

   --------------
   -- Generate --
   --------------

   procedure Generate (N : in Node_Ptr; File : in File_Type) is
   begin
      Gen_New (N, "Progress_Bar", File => File);
      Progress.Generate (N, File);
      Gen_Set (N, "Progress_Bar", "bar_style", File => File);
      Gen_Set (N, "Progress_Bar", "orientation", File => File);
   end Generate;

   procedure Generate
     (Progress_Bar : in out Object.Gtk_Object; N : in Node_Ptr)
   is
      S : String_Ptr;
   begin
      if not N.Specific_Data.Created then
         Gtk_New (Gtk_Progress_Bar (Progress_Bar));
         Set_Object (Get_Field (N, "name"), Progress_Bar);
         N.Specific_Data.Created := True;
      end if;

      Progress.Generate (Progress_Bar, N);

      S := Get_Field (N, "bar_style");

      if S /= null then
         Set_Bar_Style
           (Gtk_Progress_Bar (Progress_Bar),
            Gtk_Progress_Bar_Style'Value (S (S'First + 4 .. S'Last)));
      end if;

      S := Get_Field (N, "orientation");

      if S /= null then
         Set_Orientation
           (Gtk_Progress_Bar (Progress_Bar),
            Gtk_Progress_Bar_Orientation'Value (S (S'First + 4 .. S'Last)));
      end if;
   end Generate;

end Gtk.Progress_Bar;
