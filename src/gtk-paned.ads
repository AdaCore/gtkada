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

with Gtk.Container;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Object; use Gtk.Object;

package Gtk.Paned is

   type Gtk_Paned_Record is new Gtk.Container.Gtk_Container_Record
     with private;
   subtype Gtk_Hpaned_Record is Gtk_Paned_Record;
   subtype Gtk_Vpaned_Record is Gtk_Paned_Record;

   type Gtk_Paned is access all Gtk_Paned_Record'Class;
   subtype Gtk_Hpaned is Gtk_Paned;
   subtype Gtk_Vpaned is Gtk_Paned;

   procedure Add1 (Paned : access Gtk_Paned_Record;
                   Child : access Gtk_Widget_Record'Class);
   procedure Add2 (Paned : access Gtk_Paned_Record;
                   Child : access Gtk_Widget_Record'Class);
   function Get_Child1 (Paned : access Gtk_Paned_Record)
                        return Gtk.Widget.Gtk_Widget;

   --  Get the fields from the C structures
   function Get_Child1_Resize (Paned : access Gtk_Paned_Record) return Boolean;
   function Get_Child2_Resize (Paned : access Gtk_Paned_Record) return Boolean;
   function Get_Child1_Shrink (Paned : access Gtk_Paned_Record) return Boolean;
   function Get_Child2_Shrink (Paned : access Gtk_Paned_Record) return Boolean;


   function Get_Child2 (Paned : access Gtk_Paned_Record)
                        return Gtk.Widget.Gtk_Widget;
   procedure Gtk_New_Vpaned (Widget : out Gtk_Paned);
   procedure Gtk_New_Hpaned (Widget : out Gtk_Paned);
   procedure Initialize_Vpaned (Widget : access Gtk_Paned_Record'Class);
   procedure Initialize_Hpaned (Widget : access Gtk_Paned_Record'Class);
   procedure Pack1 (Paned : access Gtk_Paned_Record;
                    Child : access Gtk_Widget_Record'Class;
                    Resize : in Boolean;
                    Shrink : in Boolean);
   procedure Pack2 (Paned : access Gtk_Paned_Record;
                    Child : access Gtk_Widget_Record'Class;
                    Resize : in Boolean;
                    Shrink : in Boolean);

   procedure Set_Gutter_Size (Paned : access Gtk_Paned_Record;
                              Size  : in Guint16);
   procedure Set_Handle_Size (Paned : access Gtk_Paned_Record;
                              Size  : in Guint16);

   --  The two following procedures are used to generate and create widgets
   --  from a Node.

   procedure Generate (N    : in Node_Ptr;
                       File : in File_Type);

   procedure Generate (Paned : in out Gtk_Object; N : in Node_Ptr);

private
   type Gtk_Paned_Record is new Gtk.Container.Gtk_Container_Record
     with null record;

end Gtk.Paned;
