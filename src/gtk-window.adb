-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
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

package body Gtk.Window is

   ------------------------
   --  Activate_Default  --
   ------------------------

   function Activate_Default (Window : in Gtk_Window'Class) return Boolean is
      function Internal (Window : in System.Address) return Gint;
      pragma Import (C, Internal, "gtk_window_activate_default");
   begin
      return To_Boolean (Internal (Get_Object (Window)));
   end Activate_Default;


   ----------------------
   --  Activate_Focus  --
   ----------------------

   function Activate_Focus (Window : in Gtk_Window'Class) return Boolean is
      function Internal (Window : in System.Address) return Gint;
      pragma Import (C, Internal, "gtk_window_activate_focus");
   begin
      return To_Boolean (Internal (Get_Object (Window)));
   end Activate_Focus;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Window   : out Gtk_Window;
                      The_Type : in  Gtk_Window_Type) is
      function Internal (T : in Integer) return System.Address;
      pragma Import (C, Internal, "gtk_window_new");
   begin
      Set_Object (Window, Internal (Gtk_Window_Type'Pos (The_Type)));
   end Gtk_New;

   ----------------
   --  Position  --
   ----------------

   procedure Position (Window   : in out Gtk_Window'Class;
                       Position : in     Gtk_Window_Position) is
      procedure Internal (Window : in System.Address;
                          Position : in Gtk_Window_Position);
      pragma Import (C, Internal, "gtk_window_position");
   begin
      Internal (Get_Object (Window), Position);
   end Position;


   -------------------
   --  Set_Default  --
   -------------------

   procedure Set_Default (Window   : in out Gtk_Window'Class;
                          Defaultw : in     Widget.Gtk_Widget'Class) is
      procedure Internal (Window, Defaultw : in System.Address);
      pragma Import (C, Internal, "gtk_window_set_default");
   begin
      Internal (Get_Object (Window), Get_Object (Defaultw));
   end Set_Default;


   -----------------
   --  Set_Focus  --
   -----------------

   procedure Set_Focus (Window : in out Gtk_Window'Class) is
      procedure Internal (Window : in System.Address);
      pragma Import (C, Internal, "gtk_window_set_focus");
   begin
      Internal (Get_Object (Window));
   end Set_Focus;


   ------------------
   --  Set_Policy  --
   ------------------

   procedure Set_Policy (Window       : in out Gtk_Window'Class;
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


   -------------------
   -- Set_Title --
   -------------------

   procedure Set_Title (Window : in out Gtk_Window'Class;
                        Title  : in     String) is
      procedure Internal (W : in System.Address;
                          T : in String);
      pragma Import (C, Internal, "gtk_window_set_title");
   begin
      Internal (Get_Object (Window), Title & Ascii.NUL);
   end Set_Title;


end Gtk.Window;
