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

package body Gtk.Window is

   ----------------------
   -- Activate_Default --
   ----------------------

   function Activate_Default (Window : access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : in System.Address) return Gint;
      pragma Import (C, Internal, "gtk_window_activate_default");
   begin
      return To_Boolean (Internal (Get_Object (Window)));
   end Activate_Default;

   --------------------
   -- Activate_Focus --
   --------------------

   function Activate_Focus (Window : access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : in System.Address) return Gint;
      pragma Import (C, Internal, "gtk_window_activate_focus");
   begin
      return To_Boolean (Internal (Get_Object (Window)));
   end Activate_Focus;

   ---------------------
   -- Add_Accel_Group --
   ---------------------

   procedure Add_Accel_Group (Window      : access Gtk_Window_Record;
                              Accel_Group : in Gtk_Accel_Group'Class) is
      procedure Internal (Window : System.Address;
                          Accel_Group : System.Address);
      pragma Import (C, Internal, "gtk_window_add_accel_group");
   begin
      Internal (Get_Object (Window), Get_Object (Accel_Group));
   end Add_Accel_Group;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Window   : in out Gtk_Window;
                      The_Type : in  Gtk_Window_Type) is
   begin
      Window := new Gtk_Window_Record;
      Initialize (Window, The_Type);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Window : access Gtk_Window_Record'Class;
                         The_Type : in Gtk_Window_Type) is
      function Internal (T : in Integer) return System.Address;
      pragma Import (C, Internal, "gtk_window_new");
   begin
      Set_Object (Window, Internal (Gtk_Window_Type'Pos (The_Type)));
      Initialize_User_Data (Window);
   end Initialize;

   ------------------------
   -- Remove_Accel_Group --
   ------------------------

   procedure Remove_Accel_Group (Window : access Gtk_Window_Record;
                                 Accel_Group : in Gtk_Accel_Group'Class) is
      procedure Internal (Window : System.Address;
                          Accel_Group : System.Address);
      pragma Import (C, Internal, "gtk_window_remove_accel_group");
   begin
      Internal (Get_Object (Window), Get_Object (Accel_Group));
   end Remove_Accel_Group;

   -----------------
   -- Set_Default --
   -----------------

   procedure Set_Default (Window   : access Gtk_Window_Record;
                          Defaultw : in     Widget.Gtk_Widget) is
      procedure Internal (Window, Defaultw : in System.Address);
      pragma Import (C, Internal, "gtk_window_set_default");
   begin
      Internal (Get_Object (Window), Get_Object (Defaultw));
   end Set_Default;

   ----------------------
   -- Set_Default_Size --
   ----------------------

   procedure Set_Default_Size (Window : access Gtk_Window_Record;
                               Width  : in Gint;
                               Height : in Gint) is
      procedure Internal (Window : System.Address;
                          Width  : Gint;
                          Height : Gint);
      pragma Import (C, Internal, "gtk_window_set_default_size");
   begin
      Internal (Get_Object (Window), Width, Height);
   end Set_Default_Size;

   ---------------
   -- Set_Focus --
   ---------------

   procedure Set_Focus (Window : access Gtk_Window_Record;
                        Focus  : in Gtk_Widget) is
      procedure Internal (Window : in System.Address;
                          Focus  : in System.Address);
      pragma Import (C, Internal, "gtk_window_set_focus");
   begin
      Internal (Get_Object (Window), Get_Object (Focus));
   end Set_Focus;

   ---------------
   -- Set_Modal --
   ---------------

   procedure Set_Modal (Window : access Gtk_Window_Record;
                        Modal  : in Boolean) is
      procedure Internal (Window : System.Address; Modal : Integer);
      pragma Import (C, Internal, "gtk_window_set_modal");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Modal));
   end Set_Modal;

   ----------------
   -- Set_Policy --
   ----------------

   procedure Set_Policy (Window       : access Gtk_Window_Record;
                         Allow_Shrink : in     Boolean;
                         Allow_Grow   : in     Boolean;
                         Auto_Shrink  : in     Boolean) is
      procedure Internal (Window       : in System.Address;
                          Allow_Shrink : in Gint;
                          Allow_Grow   : in Gint;
                          Auto_Shrink  : in Gint);
      pragma Import (C, Internal, "gtk_window_set_policy");
   begin
      Internal (Get_Object (Window), To_Gint (Allow_Shrink),
                To_Gint (Allow_Grow), To_Gint (Auto_Shrink));
   end Set_Policy;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position (Window   : access Gtk_Window_Record;
                           Position : in     Gtk_Window_Position) is
      procedure Internal (Window : in System.Address;
                          Position : in Gtk_Window_Position);
      pragma Import (C, Internal, "gtk_window_set_position");
   begin
      Internal (Get_Object (Window), Position);
   end Set_Position;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title (Window : access Gtk_Window_Record;
                        Title  : in     String) is
      procedure Internal (W : in System.Address;
                          T : in String);
      pragma Import (C, Internal, "gtk_window_set_title");
   begin
      Internal (Get_Object (Window), Title & Ascii.NUL);
   end Set_Title;

   -----------------
   -- Set_Wmclass --
   -----------------

   procedure Set_Wmclass (Window        : access Gtk_Window_Record;
                          Wmclass_Name  : in String;
                          Wmclass_Class : in String) is
      procedure Internal (W : System.Address;
                          N : String;
                          C : String);
      pragma Import (C, Internal, "gtk_window_set_wmclass");
   begin
      Internal (Get_Object (Window),
                Wmclass_Name & ASCII.NUL,
                Wmclass_Class & ASCII.NUL);
   end Set_Wmclass;

   --------------
   -- Generate --
   --------------

   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type) is
   begin
      Gen_New (N, "Window", Get_Field (N, "type").all, File => File);
      Bin.Generate (N, File);
      Gen_Set (N, "Window", "title", File, '"');
      Gen_Set (N, "Window", "Policy", "allow_shrink", "allow_grow",
        "auto_shrink", "", File);
      Gen_Set (N, "Window", "position", File);
      Gen_Set (N, "Window", "modal", File);
   end Generate;

   procedure Generate (Window : in out Gtk_Object;
                       N      : in Node_Ptr) is
      S, S2, S3 : String_Ptr;
   begin
      if not N.Specific_Data.Created then
         S := Get_Field (N, "type");
         Gtk_New (Gtk_Window (Window),
                  Gtk_Window_Type'Value (S (S'First + 4 .. S'Last)));
         Set_Object (Get_Field (N, "name"), Window);
         N.Specific_Data.Created := True;
      end if;

      Bin.Generate (Window, N);

      S := Get_Field (N, "title");

      if S /= null then
         Set_Title (Gtk_Window (Window), S.all);
      end if;

      S := Get_Field (N, "allow_shrink");
      S2 := Get_Field (N, "allow_grow");
      S3 := Get_Field (N, "auto_shrink");

      if S /= null and then S2 /= null and then S3 /= null then
         Set_Policy
           (Gtk_Window (Window), Boolean'Value (S.all), Boolean'Value (S2.all),
            Boolean'Value (S3.all));
      end if;

      S := Get_Field (N, "position");

      if S /= null then
         Set_Position (Gtk_Window (Window),
           Enums.Gtk_Window_Position'Value (S (S'First + 4 .. S'Last)));
      end if;

      S := Get_Field (N, "modal");

      if S /= null then
         Set_Modal (Gtk_Window (Window), Boolean'Value (S.all));
      end if;
   end Generate;

end Gtk.Window;
