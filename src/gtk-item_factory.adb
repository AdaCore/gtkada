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

with Gdk; use Gdk;
with Gtk.Accel_Group;
with Gtk.Widget;
with Interfaces.C.Strings;
with System;
with Unchecked_Conversion;

package body Gtk.Item_Factory is

   package ICS renames Interfaces.C.Strings;

   -----------------
   -- Add_Foreign --
   -----------------

   procedure Add_Foreign
     (Accel_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Full_Path    : in String;
      Accel_Group  : in Gtk.Accel_Group.Gtk_Accel_Group;
      Keyval       : in Guint;
      Modifiers    : in Gdk.Types.Gdk_Modifier_Type)
   is
      procedure Internal
        (Accel_Widget : in System.Address;
         Full_Path    : in String;
         Accel_Group  : in Gtk.Accel_Group.Gtk_Accel_Group;
         Keyval       : in Guint;
         Modifiers    : in Gint);
      pragma Import (C, Internal, "gtk_item_factory_add_foreign");
   begin
      Internal (Get_Object (Accel_Widget),
                Full_Path & ASCII.Nul,
                Accel_Group,
                Keyval,
                Gdk.Types.Gdk_Modifier_Type'Pos (Modifiers));
   end Add_Foreign;

   --------------------
   -- Delete_Entries --
   --------------------

   procedure Delete_Entries
     (Ifactory  : access Gtk_Item_Factory_Record;
      Entries   : in Gtk_Item_Factory_Entry_Array)
   is
      procedure Internal
        (Ifactory  : in System.Address;
         N_Entries : in Guint;
         Entries   : in System.Address);
      pragma Import (C, Internal, "gtk_item_factory_delete_entries");

   begin
      Internal (Get_Object (Ifactory), Entries'Length, Entries'Address);
   end Delete_Entries;

   ------------------
   -- Delete_Entry --
   ------------------

   procedure Delete_Entry
     (Ifactory : access Gtk_Item_Factory_Record;
      Ientry   : in Gtk_Item_Factory_Entry)
   is
      procedure Internal
        (Ifactory : in System.Address;
         Ientry   : in Gtk_Item_Factory_Entry);
      pragma Import (C, Internal, "gtk_item_factory_delete_entry");
   begin
      Internal (Get_Object (Ifactory), Ientry);
   end Delete_Entry;

   -----------------
   -- Delete_Item --
   -----------------

   procedure Delete_Item
     (Ifactory : access Gtk_Item_Factory_Record;
      Path     : in String)
   is
      procedure Internal
        (Ifactory : in System.Address;
         Path     : in String);
      pragma Import (C, Internal, "gtk_item_factory_delete_item");
   begin
      Internal (Get_Object (Ifactory),
                Path & ASCII.Nul);
   end Delete_Item;

   -----------------
   -- From_Widget --
   -----------------

   function From_Widget
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gtk_Item_Factory
   is
      function Internal (Widget : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_item_factory_from_widget");
      Stub : Gtk_Item_Factory_Record;
   begin
      return Gtk_Item_Factory
        (Get_User_Data (Internal (Get_Object (Widget)), Stub));
   end From_Widget;

   --------------
   -- Get_Item --
   --------------

   function Get_Item
     (Ifactory : access Gtk_Item_Factory_Record;
      Path     : in String) return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Ifactory : in System.Address;
         Path     : in String) return System.Address;
      pragma Import (C, Internal, "gtk_item_factory_get_item");

      Stub : Gtk.Widget.Gtk_Widget_Record;

   begin
      return Gtk.Widget.Gtk_Widget
        (Get_User_Data (Internal (Get_Object (Ifactory), Path & ASCII.Nul),
                        Stub));
   end Get_Item;

   ------------------------
   -- Get_Item_By_Action --
   ------------------------

   function Get_Item_By_Action
     (Ifactory : access Gtk_Item_Factory_Record;
      Action   : in Guint) return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Ifactory : in System.Address;
         Action   : in Guint) return System.Address;
      pragma Import (C, Internal, "gtk_item_factory_get_item_by_action");

      Stub : Gtk.Widget.Gtk_Widget_Record;

   begin
      return Gtk.Widget.Gtk_Widget
        (Get_User_Data (Internal (Get_Object (Ifactory), Action), Stub));
   end Get_Item_By_Action;

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget
     (Ifactory : access Gtk_Item_Factory_Record;
      Path     : in String) return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Ifactory : in System.Address;
         Path     : in String) return System.Address;
      pragma Import (C, Internal, "gtk_item_factory_get_widget");

      Stub : Gtk.Widget.Gtk_Widget_Record;

   begin
      return Gtk.Widget.Gtk_Widget
        (Get_User_Data (Internal (Get_Object (Ifactory), Path & ASCII.Nul),
                        Stub));
   end Get_Widget;

   --------------------------
   -- Get_Widget_By_Action --
   --------------------------

   function Get_Widget_By_Action
     (Ifactory : access Gtk_Item_Factory_Record;
      Action   : in Guint) return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Ifactory : in System.Address;
         Action   : in Guint) return System.Address;
      pragma Import (C, Internal, "gtk_item_factory_get_widget_by_action");

      Stub : Gtk.Widget.Gtk_Widget_Record;

   begin
      return Gtk.Widget.Gtk_Widget
        (Get_User_Data (Internal (Get_Object (Ifactory), Action), Stub));
   end Get_Widget_By_Action;

   --------------
   -- Parse_Rc --
   --------------

   procedure Parse_Rc (File_Name : in String) is
      procedure Internal (File_Name : in String);
      pragma Import (C, Internal, "gtk_item_factory_parse_rc");
   begin
      Internal (File_Name & ASCII.Nul);
   end Parse_Rc;

   ---------------------
   -- Parse_Rc_String --
   ---------------------

   procedure Parse_Rc_String (Rc_String : in String) is
      procedure Internal (Rc_String : in String);
      pragma Import (C, Internal, "gtk_item_factory_parse_rc_string");
   begin
      Internal (Rc_String & ASCII.Nul);
   end Parse_Rc_String;

   ----------------------
   -- Path_From_Widget --
   ----------------------

   function Path_From_Widget
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return String
   is
      function Internal
        (Widget : in System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_item_factory_path_from_widget");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Widget)));
   end Path_From_Widget;

   -----------
   -- Popup --
   -----------

   procedure Popup
     (Ifactory     : access Gtk_Item_Factory_Record;
      X            : in Guint;
      Y            : in Guint;
      Mouse_Button : in Guint;
      Time         : in Guint32)
   is
      procedure Internal
        (Ifactory     : in System.Address;
         X            : in Guint;
         Y            : in Guint;
         Mouse_Button : in Guint;
         Time         : in Guint32);
      pragma Import (C, Internal, "gtk_item_factory_popup");
   begin
      Internal (Get_Object (Ifactory), X, Y, Mouse_Button, Time);
   end Popup;

   ---------------
   -- Data_Item --
   ---------------

   package body Data_Item is

      -----------------
      -- Create_Item --
      -----------------

      procedure Create_Item
        (Ifactory      : access Gtk_Item_Factory_Record'Class;
         Ientry        : in Gtk_Item_Factory_Entry;
         Callback_Data : in Data_Type;
         Callback_Type : in Guint)
      is
         procedure Internal
           (Ifactory      : in System.Address;
            Ientry        : in Gtk_Item_Factory_Entry;
            Callback_Data : in Data_Type;
            Callback_Type : in Guint);
         pragma Import (C, Internal, "gtk_item_factory_create_item");
      begin
         Internal (Get_Object (Ifactory),
                   Ientry,
                   Callback_Data,
                   Callback_Type);
      end Create_Item;

      ------------------
      -- Create_Items --
      ------------------

      procedure Create_Items
        (Ifactory      : access Gtk_Item_Factory_Record'Class;
         Entries       : in Gtk_Item_Factory_Entry_Array;
         Callback_Data : in Data_Type)
      is
         procedure Internal
           (Ifactory      : in System.Address;
            N_Entries     : in Guint;
            Entries       : in System.Address;
            Callback_Data : in System.Address);
         pragma Import (C, Internal, "gtk_item_factory_create_items");

      begin
         Internal (Get_Object (Ifactory),
                   Entries'Length,
                   Entries'Address,
                   Callback_Data'Address);
      end Create_Items;

      ----------
      -- Free --
      ----------

      procedure Free (Ientry : in out Gtk_Item_Factory_Entry) is
      begin
         ICS.Free (Ientry.Path);
         ICS.Free (Ientry.Accelerator);
         ICS.Free (Ientry.Item_Type);
      end Free;
 
      procedure Free (Ientries : in out Gtk_Item_Factory_Entry_Array) is
      begin
         for J in Ientries'Range loop
            Free (Ientries (J));
         end loop;
      end Free;

      -------------
      -- Gtk_New --
      -------------

      function Gtk_New
        (Path            : in  String;
         Accelerator     : in  String := "";
         Callback        : in  Gtk_Item_Factory_Callback := null;
         Item_Type       : in  Item_Type_Enum;
         Callback_Action : in  Guint := 0) return Gtk_Item_Factory_Entry
      is
         function Item_Type_String (Item_Type : Item_Type_Enum) return String;

         function Item_Type_String (Item_Type : Item_Type_Enum) return String is
         begin
            case Item_Type is
               when Title       => return "<Title>";
               when Item        => return "<Item>";
               when Check_Item  => return "<CheckItem>";
               when Toggle_Item => return "<ToggleItem>";
               when Radio_Item  => return "<RadioItem>";
               when Tearoff     => return "<Tearoff>";
               when Separator   => return "<Separator>";
               when Branch      => return "<Branch>";
               when Last_Branch => return "<LastBranch>";
            end case;
         end Item_Type_String;

      begin
         return Gtk_New (Path, Accelerator, Callback,
           Item_Type_String (Item_Type), Callback_Action);
      end Gtk_New;

      function Gtk_New
        (Path            : in String;
         Accelerator     : in String := "";
         Callback        : in Gtk_Item_Factory_Callback := null;
         Item_Type       : in String := "";
         Callback_Action : in Guint := 0) return Gtk_Item_Factory_Entry
      is
         function To_Address is new
           Unchecked_Conversion (Gtk_Item_Factory_Callback, System.Address);

         Ientry : Gtk_Item_Factory_Entry;
      begin
         Ientry.Path := ICS.New_String (Path);

         if Accelerator = "" then
            Ientry.Accelerator := ICS.Null_Ptr;
         else
            Ientry.Accelerator := ICS.New_String (Accelerator);
         end if;

         Ientry.Callback        := To_Address (Callback);
         Ientry.Callback_Action := Callback_Action;

         if Item_Type = "" then
            Ientry.Item_Type := ICS.Null_Ptr;
         else
            Ientry.Item_Type := ICS.New_String (Item_Type);
         end if;

         return Ientry;
      end Gtk_New;

      ----------------
      -- Popup_Data --
      ----------------

      function Popup_Data
        (Ifactory : access Gtk_Item_Factory_Record'Class)
         return Data_Type_Access
      is
         function Internal
           (Ifactory : in System.Address) return Data_Type_Access;
         pragma Import (C, Internal, "gtk_item_factory_popup_data");
      begin
         return Internal (Get_Object (Ifactory));
      end Popup_Data;

      ----------------------------
      -- Popup_Data_From_Widget --
      ----------------------------

      function Popup_Data_From_Widget
        (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
         return Data_Type_Access
      is
         function Internal
           (Widget : in System.Address) return Data_Type_Access;
         pragma Import (C, Internal,
           "gtk_item_factory_popup_data_from_widget");
      begin
         return Internal (Get_Object (Widget));
      end Popup_Data_From_Widget;

      ---------------------
      -- Popup_With_Data --
      ---------------------

      procedure Popup_With_Data
        (Ifactory     : access Gtk_Item_Factory_Record'Class;
         Popup_Data   : in Data_Type;
         Destroy      : in System.Address;  --  Gtk_Destroy_Notify ???
         X            : in Guint;
         Y            : in Guint;
         Mouse_Button : in Guint;
         Time         : in Guint32)
      is
         procedure Internal
           (Ifactory     : in System.Address;
            Popup_Data   : in Data_Type;
            Destroy      : in System.Address;
            X            : in Guint;
            Y            : in Guint;
            Mouse_Button : in Guint;
            Time         : in Guint32);
         pragma Import (C, Internal, "gtk_item_factory_popup_with_data");

      begin
         Internal (Get_Object (Ifactory),
                   Popup_Data,
                   Destroy,
                   X,
                   Y,
                   Mouse_Button,
                   Time);
      end Popup_With_Data;

      ----------------
      -- Print_Func --
      ----------------

      procedure Print_Func (File_Pointer : in Data_Type; Str : in String) is
         procedure Internal
           (FILE_Pointer : in System.Address;
            Str          : in String);
         pragma Import (C, Internal, "gtk_item_factory_print_func");
      begin
         Internal (File_Pointer'Address, Str & ASCII.Nul);
      end Print_Func;

      ------------------------
      -- Set_Translate_Func --
      ------------------------

      procedure Set_Translate_Func
        (Ifactory : access Gtk_Item_Factory_Record'Class;
         Func     : in Gtk_Translate_Func;
         Data     : in Data_Type;
         Notify   : in System.Address)  --  Gtk_Destroy_Notify ???
      is
         procedure Internal
           (Ifactory : in System.Address;
            Func     : in Gtk_Translate_Func;  --  ???
            Data     : in Data_Type;
            Notify   : in System.Address);
         pragma Import (C, Internal, "gtk_item_factory_set_translate_func");

      begin
         Internal (Get_Object (Ifactory),
                   Func,
                   Data,
                   Notify);
      end Set_Translate_Func;

      ---------------
      -- To_Widget --
      ---------------

      function To_Widget
        (Widget : in Limited_Widget) return Gtk.Widget.Gtk_Widget
      is
         Stub : Gtk.Widget.Gtk_Widget_Record;
      begin
         return Gtk.Widget.Gtk_Widget (Get_User_Data
           (System.Address (Widget), Stub));
      end To_Widget;

   end Data_Item;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Ifactory       : in out Gtk_Item_Factory;
      Container_Type : in     Gtk_Type;
      Path           : in     String;
      Accel_Group    : in     Gtk.Accel_Group.Gtk_Accel_Group) is
   begin
      Ifactory := new Gtk_Item_Factory_Record;
      Initialize (Ifactory, Container_Type, Path, Accel_Group);
   end Gtk_New;
   
   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Ifactory       : access Gtk_Item_Factory_Record'Class;
      Container_Type : in Gtk_Type;
      Path           : in String := "";
      Accel_Group    : in Gtk.Accel_Group.Gtk_Accel_Group)
   is
      function Internal
        (Container_Type : in Gtk_Type;
         Path           : in String;
         Accel_Group    : in Gtk.Accel_Group.Gtk_Accel_Group)
         return System.Address;
      pragma Import (C, Internal, "gtk_item_factory_new");
   begin
      Set_Object (Ifactory, Internal (Container_Type,
                                      Path & ASCII.Nul,
                                      Accel_Group));
   end Initialize;

end Gtk.Item_Factory;
