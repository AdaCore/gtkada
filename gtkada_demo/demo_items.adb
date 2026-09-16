------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                    Copyright (C) 1998-2026, AdaCore                      --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Glib.Object; use Glib.Object;

package body Demo_Items is

   Demo_Item_Class : aliased Glib.Object.Ada_GObject_Class :=
     Glib.Object.Uninitialized_Class;

   --------------
   -- Get_Type --
   --------------

   function Get_Type return Glib.GType is
   begin
      --  No Class_Init: there are no virtual methods to override and no
      --  GObject properties to declare, since the fields above are only
      --  ever read from Ada.
      Glib.Object.Initialize_Class_Record
        (Ancestor     => Glib.GType_Object,
         Class_Record => Demo_Item_Class,
         Type_Name    => "GtkAdaDemoItem");
      return Demo_Item_Class.The_Type;
   end Get_Type;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self  : out Demo_Item;
      Title : String;
      Run   : Demo_Function := null;
      Help  : Help_Function := null) is
   begin
      Self := new Demo_Item_Record;
      Self.Title := To_Unbounded_String (Title);
      Self.Run := Run;
      Self.Help := Help;
      G_New (Self, Get_Type);
   end Gtk_New;

   ------------------
   -- To_Demo_Item --
   ------------------

   function To_Demo_Item (Object : Glib.Object.GObject) return Demo_Item is
   begin
      if Object /= null and then Object.all in Demo_Item_Record'Class then
         return Demo_Item (Object);
      else
         return null;
      end if;
   end To_Demo_Item;

   --------------
   -- Title_Of --
   --------------

   function Title_Of (Object : Glib.Object.GObject) return String is
      Item : constant Demo_Item := To_Demo_Item (Object);
   begin
      if Item = null then
         return "";
      else
         return To_String (Item.Title);
      end if;
   end Title_Of;

end Demo_Items;
