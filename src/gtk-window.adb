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

with System;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group;  use Gtk.Accel_Group;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Widget;       use Gtk.Widget;
with Interfaces.C.Strings; use Interfaces.C.Strings;

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
                              Accel_Group : in Gtk_Accel_Group) is
      procedure Internal (Window : System.Address;
                          Accel_Group : Gtk_Accel_Group);
      pragma Import (C, Internal, "gtk_window_add_accel_group");
   begin
      Internal (Get_Object (Window), Accel_Group);
   end Add_Accel_Group;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Window   : out Gtk_Window;
                      The_Type : in  Gtk_Window_Type := Window_Toplevel) is
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
                                 Accel_Group : in Gtk_Accel_Group) is
      procedure Internal (Window : System.Address;
                          Accel_Group : Gtk_Accel_Group);
      pragma Import (C, Internal, "gtk_window_remove_accel_group");
   begin
      Internal (Get_Object (Window), Accel_Group);
   end Remove_Accel_Group;

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

   ------------------------
   -- Set_Geometry_Hints --
   ------------------------

   procedure Set_Geometry_Hints
     (Window          : access Gtk_Window_Record;
      Geometry_Widget : Gtk_Widget;
      Geometry        : Gdk_Geometry;
      Geom_Mask       : Gdk_Window_Hints)
   is
      procedure Internal (Window   : System.Address;
                          Wid      : System.Address;
                          Geometry : in out Gdk_Geometry;
                          Mask     : Gdk_Window_Hints);
      pragma Import (C, Internal, "gtk_window_set_geometry_hints");

      Geom : Gdk_Geometry := Geometry;
      Wid  : System.Address := System.Null_Address;

   begin
      if Geometry_Widget /= null then
         Wid := Get_Object (Geometry_Widget);
      end if;

      Internal (Get_Object (Window), Wid, Geom, Geom_Mask);
   end Set_Geometry_Hints;

   ---------------
   -- Set_Modal --
   ---------------

   procedure Set_Modal (Window : access Gtk_Window_Record;
                        Modal  : in Boolean := True) is
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

   -----------------------
   -- Set_Transient_For --
   -----------------------

   procedure Set_Transient_For (Window : access Gtk_Window_Record;
                                Parent : access Gtk_Window_Record'Class)
   is
      procedure Internal (Window : System.Address;
                          Parent : System.Address);
      pragma Import (C, Internal, "gtk_window_set_transient_for");
   begin
      Internal (Get_Object (Window), Get_Object (Parent));
   end Set_Transient_For;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title (Window : access Gtk_Window_Record;
                        Title  : in     String) is
      procedure Internal (W : in System.Address;
                          T : in String);
      pragma Import (C, Internal, "gtk_window_set_title");
   begin
      Internal (Get_Object (Window), Title & ASCII.NUL);
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

   --------------------------
   -- Get_Transient_Parent --
   --------------------------

   function Get_Transient_Parent (Window : access Gtk_Window_Record)
      return Gtk_Window
   is
      function Internal (Window : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gtk_window_get_transient_parent");
      Stub : Gtk_Window_Record;
   begin
      return Gtk_Window (Get_User_Data (Internal (Get_Object (Window)), Stub));
   end Get_Transient_Parent;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title (Window : access Gtk_Window_Record) return String is
      function Internal (Window : System.Address)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "ada_gtk_window_get_title");
      S : constant chars_ptr := Internal (Get_Object (Window));

   begin
      if S /= Null_Ptr then
         return Value (S);
      else
         return "";
      end if;
   end Get_Title;

end Gtk.Window;
