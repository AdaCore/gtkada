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

with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Bin;    use Gtk.Bin;
with Gtk.Enums;  use Gtk.Enums;
with Gtk.Widget; use Gtk.Widget;

package Gtk.Window is

   type Gtk_Window_Record is new Bin.Gtk_Bin_Record with private;
   type Gtk_Window is access all Gtk_Window_Record'Class;

   procedure Gtk_New (Window   : in out Gtk_Window;
                      The_Type : in  Gtk_Window_Type);
   procedure Initialize (Window : access Gtk_Window_Record'Class;
                         The_Type : in Gtk_Window_Type);

   procedure Set_Title (Window : access Gtk_Window_Record;
                        Title  : in String);

   procedure Set_Focus (Window : access Gtk_Window_Record;
                        Focus   : in Gtk_Widget);

   procedure Set_Default (Window   : access Gtk_Window_Record;
                          Defaultw : in     Gtk_Widget);

   procedure Set_Modal (Window : access Gtk_Window_Record;
                        Modal  : in Boolean);

   procedure Set_Policy (Window       : access Gtk_Window_Record;
                         Allow_Shrink : in     Boolean;
                         Allow_Grow   : in     Boolean;
                         Auto_Shrink  : in     Boolean);

   procedure Set_Position (Window   : access Gtk_Window_Record;
                           Position : in     Enums.Gtk_Window_Position);

   function Activate_Focus (Window : access Gtk_Window_Record) return Boolean;

   function Activate_Default (Window : access Gtk_Window_Record)
                              return Boolean;

   procedure Set_Default_Size (Window : access Gtk_Window_Record;
                               Width  : in Gint;
                               Height : in Gint);

   procedure Set_Wmclass (Window        : access Gtk_Window_Record;
                          Wmclass_Name  : in String;
                          Wmclass_Class : in String);

   procedure Add_Accel_Group (Window      : access Gtk_Window_Record;
                              Accel_Group : in Gtk_Accel_Group'Class);

   procedure Remove_Accel_Group (Window : access Gtk_Window_Record;
                                 Accel_Group : in Gtk_Accel_Group'Class);

   --  The two following procedures are used to generate and create widgets
   --  from a Node.

   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type);

   procedure Generate (Window : in out Gtk_Object; N : in Node_Ptr);

private
   type Gtk_Window_Record is new Bin.Gtk_Bin_Record with null record;

end Gtk.Window;
