-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                        Copyright (C) 2000                         --
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

with Gdk.Types;
with Gtk.Accel_Group;
with Gtk.Widget;
with Gtk.Object;
with Gtkada.Types;

package Gtk.Item_Factory is

   type Gtk_Item_Factory_Record is new Object.Gtk_Object_Record with private;
   type Gtk_Item_Factory is access all Gtk_Item_Factory_Record'Class;

   type Item_Type_Enum is
     (Title,
      --  Create a title item

      Item,
      --  Create a simple item

      Check_Item,
      --  Create a check item

      Toggle_Item,
      --  Create a toggle item

      Radio_Item,
      --  Create a radio item

      Tearoff,
      --  Create a tearoff item

      Separator,
      --  Create a separator

      Branch,
      --  Create an item to hold sub items

      Last_Branch
      --  Create a right justified item to hold sub items
     );
   --  Identify the predefined item types used to create a
   --  Gtk_Item_Factory_Entry.

   type Gtk_Item_Factory_Entry is private;
   type Gtk_Item_Factory_Entry_Array is array (Gint range <>) of
     Gtk_Item_Factory_Entry;

   procedure Gtk_New
     (Ifactory       : out Gtk_Item_Factory;
      Container_Type : in Gtk_Type;
      Path           : in String;
      Accel_Group    : in Gtk.Accel_Group.Gtk_Accel_Group);
   --  Possible values of Container_Type are:
   --    - Gtk.Option_Menu.Get_Type
   --    - Gtk.Menu_Bar.Get_Type
   --    - Gtk.Menu.Get_Type

   procedure Initialize
     (Ifactory       : access Gtk_Item_Factory_Record'Class;
      Container_Type : in Gtk_Type;
      Path           : in String := "";
      Accel_Group    : in Gtk.Accel_Group.Gtk_Accel_Group);

   procedure Add_Foreign
     (Accel_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Full_Path    : in String;
      Accel_Group  : in Gtk.Accel_Group.Gtk_Accel_Group;
      Keyval       : in Guint;
      Modifiers    : in Gdk.Types.Gdk_Modifier_Type);

   procedure Delete_Entries
     (Ifactory  : access Gtk_Item_Factory_Record;
      Entries   : in Gtk_Item_Factory_Entry_Array);

   procedure Delete_Entry
     (Ifactory : access Gtk_Item_Factory_Record;
      Ientry   : in Gtk_Item_Factory_Entry);

   procedure Delete_Item
     (Ifactory : access Gtk_Item_Factory_Record;
      Path     : in String);

   function From_Widget
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gtk_Item_Factory;

   function Get_Item
     (Ifactory : access Gtk_Item_Factory_Record;
      Path     : in String) return Gtk.Widget.Gtk_Widget;

   function Get_Item_By_Action
     (Ifactory : access Gtk_Item_Factory_Record;
      Action   : in Guint) return Gtk.Widget.Gtk_Widget;

   function Get_Type return Gtk_Type;
   --  Return the internal value associated with a Gtk_Item_Factory

   function Get_Widget
     (Ifactory : access Gtk_Item_Factory_Record;
      Path     : in String) return Gtk.Widget.Gtk_Widget;

   function Get_Widget_By_Action
     (Ifactory : access Gtk_Item_Factory_Record;
      Action   : in Guint) return Gtk.Widget.Gtk_Widget;

   procedure Parse_Rc (File_Name : in String);

   procedure Parse_Rc_String (Rc_String : in String);

   function Path_From_Widget
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return String;

   procedure Popup
     (Ifactory     : access Gtk_Item_Factory_Record;
      X            : in Guint;
      Y            : in Guint;
      Mouse_Button : in Guint;
      Time         : in Guint32);

   generic
      type Data_Type (<>) is limited private;
   package Data_Item is

      type Data_Type_Access is access all Data_Type;

      type Gtk_Print_Func is access procedure
        (Func_Data : Data_Type_Access;
         Str       : String);  --  gchar* ???

      type Gtk_Translate_Func is access function
        (Path        : String;  --  const gchar* ???
         Func_Data   : Data_Type_Access) return Gtkada.Types.Chars_Ptr;

      type Limited_Widget is limited private;

      function To_Widget
        (Widget : in Limited_Widget) return Gtk.Widget.Gtk_Widget;

      type Gtk_Item_Factory_Callback is access procedure
        (Callback_Data   : in Data_Type_Access;
         Callback_Action : in Guint;
         Widget          : in Limited_Widget);

      function Gtk_New
        (Path            : in String;
         Accelerator     : in String := "";
         Callback        : in Gtk_Item_Factory_Callback := null;
         Item_Type       : in Item_Type_Enum;
         Callback_Action : in Guint := 0) return Gtk_Item_Factory_Entry;
      --  Create a Gtk_Item_Factory_Entry.
      --  It is up to you to call Free at an appropriate point to avoid memory
      --  leaks.

      function Gtk_New
        (Path            : in String;
         Accelerator     : in String := "";
         Callback        : in Gtk_Item_Factory_Callback := null;
         Item_Type       : in String := "";
         Callback_Action : in Guint := 0) return Gtk_Item_Factory_Entry;
      --  Create a Gtk_Item_Factory_Entry.
      --  It is up to you to call Free at an appropriate point to avoid memory
      --  leaks.

      procedure Free (Ientry : in out Gtk_Item_Factory_Entry);
      --  Free all the dynamic data associated with an item factory entry.

      procedure Free (Ientries : in out Gtk_Item_Factory_Entry_Array);
      --  Free all the dynamic data associated with each item factory entry.

      procedure Create_Item
        (Ifactory      : access Gtk_Item_Factory_Record'Class;
         Ientry        : in Gtk_Item_Factory_Entry;
         Callback_Data : in Data_Type_Access;
         Callback_Type : in Guint);
      --  Callback_Type = 0 -> Gtk_Item_Factory_Callback
      --  Callback_Type = 1 -> Gtk_Item_Factory_Callback1

      procedure Create_Items
        (Ifactory      : access Gtk_Item_Factory_Record'Class;
         Entries       : in Gtk_Item_Factory_Entry_Array;
         Callback_Data : in Data_Type_Access);

      function Popup_Data
        (Ifactory : access Gtk_Item_Factory_Record'Class)
         return Data_Type_Access;

      function Popup_Data_From_Widget
        (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
         return Data_Type_Access;

      procedure Popup_With_Data
        (Ifactory     : access Gtk_Item_Factory_Record'Class;
         Popup_Data   : in Data_Type_Access;
         Destroy      : in System.Address; --  Gtk_Destroy_Notify ???
         X            : in Guint;
         Y            : in Guint;
         Mouse_Button : in Guint;
         Time         : in Guint32);

      procedure Print_Func
        (File_Pointer : in Data_Type_Access;
         Str          : in String);

      procedure Set_Translate_Func
        (Ifactory : access Gtk_Item_Factory_Record'Class;
         Func     : in Gtk_Translate_Func;
         Data     : in Data_Type_Access;
         Notify   : in System.Address);  --  Gtk_Destroy_Notify ???

   private
      type Limited_Widget is new System.Address;
   end Data_Item;

private
   type Gtk_Item_Factory_Record is new Gtk.Object.Gtk_Object_Record
     with null record;

   type Gtk_Item_Factory_Entry is record
      Path            : Gtkada.Types.Chars_Ptr;
      Accelerator     : Gtkada.Types.Chars_Ptr;
      Callback        : System.Address;
      Callback_Action : Guint;
      Item_Type       : Gtkada.Types.Chars_Ptr;
   end record;
   pragma Convention (C, Gtk_Item_Factory_Entry);

   pragma Import (C, Get_Type, "gtk_item_factory_get_type");

end Gtk.Item_Factory;

--  Functions not bound:
--  gtk_item_factory_dump_items
--  gtk_item_factory_dump_rc
