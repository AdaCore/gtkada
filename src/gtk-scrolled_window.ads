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

with Gtk.Adjustment;
with Gtk.Container;
with Gtk.Enums;

package Gtk.Scrolled_Window is

   type Gtk_Scrolled_Window is new Container.Gtk_Container with private;

   procedure Gtk_New
     (Scrolled_Window :    out Gtk_Scrolled_Window'Class;
      Hadjustment     : in     Adjustment.Gtk_Adjustment'Class :=
        Adjustment.Null_Adjustment;
      Vadjustment     : in     Adjustment.Gtk_Adjustment'Class :=
        Adjustment.Null_Adjustment);
   --  mapping: Gtk_New gtkscrolledwindow.h gtk_scrolled_window_new

   procedure Construct
     (Scrolled_Window : in out Gtk_Scrolled_Window'Class;
      Hadjustment     : in     Adjustment.Gtk_Adjustment'Class :=
        Adjustment.Null_Adjustment;
      Vadjustment     : in     Adjustment.Gtk_Adjustment'Class :=
        Adjustment.Null_Adjustment);
   --  mapping: Construct gtkscrolledwindow.h gtk_scrolled_window_construct

   function Get_Hadjustment
     (Scrolled_Window : in Gtk_Scrolled_Window'Class)
      return               Adjustment.Gtk_Adjustment;
   --  mapping: Get_Hadjustement gtkscrolledwindow.h \
   --  mapping:                  gtk_scrolled_window_get_hadjustment

   function Get_Vadjustment
     (Scrolled_Window : in Gtk_Scrolled_Window'Class)
      return               Adjustment.Gtk_Adjustment;
   --  mapping: Get_Vadjustement gtkscrolledwindow.h \
   --  mapping:                  gtk_scrolled_window_get_vadjustment

   procedure Set_Policy (Scrolled_Window    : in out Gtk_Scrolled_Window'Class;
                         H_Scrollbar_Policy : in     Enums.Gtk_Policy_Type;
                         V_Scrollbar_Policy : in     Enums.Gtk_Policy_Type);
   --  mapping: Set_Policy gtkscrolledwindow.h gtk_scrolled_window_set_policy

private

   type Gtk_Scrolled_Window is new Container.Gtk_Container with null record;

   --  mapping: USE_OBJECT_ORIENTED gtkscrolledwindow.h \
   --  mapping:                     gtk_scrolled_window_get_type

end Gtk.Scrolled_Window;
