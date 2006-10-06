-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2006, AdaCore                   --
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

with Gdk.Color;                use Gdk.Color;
with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Glib.Types;               use Glib.Types;
with Glib.Properties;          use Glib.Properties;
with Glib.Properties.Creation; use Glib.Properties.Creation;
with Glib.Values;              use Glib.Values;
with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Combo_Box;            use Gtk.Combo_Box;
with Gtk.Container;            use Gtk.Container;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Event_Box;            use Gtk.Event_Box;
with Gtk.Label;                use Gtk.Label;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Notebook;             use Gtk.Notebook;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Spin_Button;          use Gtk.Spin_Button;
with Gtk.Table;                use Gtk.Table;
with Gtk.Toggle_Button;        use Gtk.Toggle_Button;
with Gtk.Tooltips;             use Gtk.Tooltips;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;
with Gtkada.Handlers;          use Gtkada.Handlers;
with Gtkada.Types;             use Gtkada.Types;
with Pango.Font;               use Pango.Font;
with System;                   use System;
with System.Address_Image;

package body Gtkada.Properties is

   type Properties_Editor_Record is new Gtk_Window_Record with record
      Object      : GObject;
      Tips        : Gtk_Tooltips;
      Table       : Gtk_Table; --  Properties;
      Child_Table : Gtk_Table; --  Child properties;
      Type_Color : Gdk_Color := Null_Color;
   end record;
   type Properties_Editor is access all Properties_Editor_Record'Class;

   Global_Editor : Properties_Editor;
   --  The unique properties editor for this application.

   procedure Editor_Delete (Editor : access GObject_Record'Class);
   --  Called when the properties editor Editor is destroyed

   procedure Gtk_New (Editor : out Properties_Editor);
   --  Create a new empty properties editor

   procedure Object_Destroyed (Editor, Object : System.Address);
   pragma Convention (C, Object_Destroyed);
   --  Called when the object that the editor is showing has been destroyed.

   procedure Work_On
     (Editor : access Properties_Editor_Record'Class;
      Object : GObject := null);
   --  Show the properties of Object in the editor

   procedure Show_Properties_From_Type
     (Editor : access Properties_Editor_Record'Class;
      T      : GType);
   --  Show the properties associated with Editor.Object, for its specific
   --  ancestor type T.

   procedure Show_Properties
     (Editor     : access Properties_Editor_Record'Class;
      Table      : Gtk_Table;
      Is_Child_Prop : Boolean;
      T          : GType;
      Properties : Param_Spec_Array);
   --  Add the description for all properties in Properties.

   function Property_Widget
     (Object        : access GObject_Record'Class;
      Property      : Param_Spec;
      Is_Child_Prop : Boolean) return Gtk_Widget;
   --  Return the widget to use to edit that property

   type Pspec_Callback is access procedure
     (Editor        : access Gtk_Widget_Record'Class;
      Value         : GValue);
   --  Set the value of Editor based on the contents of Value

   type Property_Modified_Data is record
      Pspec         : Param_Spec;
      Editor        : Gtk_Widget;
      Callback      : Pspec_Callback;
      Is_Child_Prop : Boolean;
   end record;
   package Prop_Callback is new Gtk.Handlers.User_Callback
     (GObject_Record, Property_Modified_Data);

   type Controller_Callback is access procedure
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class);
   --  Set the contents of Value (already initialized) to the value found in
   --  editor. This is specialized for each type of property

   type Controller_Data is record
      Object        : GObject;
      Pspec         : Param_Spec;
      Is_Child_Prop : Boolean;
      Callback      : Controller_Callback;
   end record;
   package Controller_Cb is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Controller_Data);

   package Handler_Id_User_Data is new Glib.Object.User_Data (Handler_Id);

   procedure Property_Modified
     (Object : access GObject_Record'Class;
      Data   : Property_Modified_Data);
   --  Called when a property has been modified, and dispatch to the callback
   --  in Data.

   procedure Controller_Called
     (Editor : access Gtk_Widget_Record'Class;
      Data   : Controller_Data);
   --  An editor has been modified by the user, call the appropriate controller

   procedure Block_Controller
     (Editor : access Gtk_Widget_Record'Class; Block  : Boolean);
   --  Temporarily disable the controller callback on Editor, so that when the
   --  property is changed, and then the editor is changed, this doesn't
   --  return in another call to the property modified callback to prevent
   --  infinite loops.

   procedure Bool_Changed
     (Editor        : access Gtk_Widget_Record'Class;
      Value         : GValue);
   procedure String_Changed
     (Editor        : access Gtk_Widget_Record'Class;
      Value         : GValue);
   procedure Int_Changed
     (Editor        : access Gtk_Widget_Record'Class;
      Value         : GValue);
   procedure Uint_Changed
     (Editor        : access Gtk_Widget_Record'Class;
      Value         : GValue);
   procedure Float_Changed
     (Editor        : access Gtk_Widget_Record'Class;
      Value         : GValue);
   procedure Double_Changed
     (Editor        : access Gtk_Widget_Record'Class;
      Value         : GValue);
   procedure Enum_Changed
     (Editor        : access Gtk_Widget_Record'Class;
      Value         : GValue);
   procedure Pointer_Changed
     (Editor        : access Gtk_Widget_Record'Class;
      Value         : GValue);
   procedure Object_Changed
     (Editor        : access Gtk_Widget_Record'Class;
      Value         : GValue);
   --  A property has been modified

   procedure Bool_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class);
   procedure String_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class);
   procedure Int_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class);
   procedure Uint_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class);
   procedure Float_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class);
   procedure Double_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class);
   procedure Enum_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class);
   --  The user modified the property

   ------------------
   -- Bool_Changed --
   ------------------

   procedure Bool_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Value         : GValue) is
   begin
      Set_Active (Gtk_Toggle_Button (Editor), Get_Boolean (Value));
   end Bool_Changed;

   --------------------
   -- String_Changed --
   --------------------

   procedure String_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Value         : GValue) is
   begin
      Set_Text (Gtk_Entry (Editor), Get_String (Value));
   end String_Changed;

   -----------------
   -- Int_Changed --
   -----------------

   procedure Int_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Value         : GValue) is
   begin
      Set_Value (Gtk_Spin_Button (Editor), Gdouble (Get_Int (Value)));
   end Int_Changed;

   -------------------
   -- Float_Changed --
   -------------------

   procedure Float_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Value         : GValue) is
   begin
      Set_Value (Gtk_Spin_Button (Editor), Gdouble (Get_Float (Value)));
   end Float_Changed;

   --------------------
   -- Double_Changed --
   --------------------

   procedure Double_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Value         : GValue) is
   begin
      Set_Value (Gtk_Spin_Button (Editor), Get_Double (Value));
   end Double_Changed;

   ---------------------
   -- Pointer_Changed --
   ---------------------

   procedure Pointer_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Value         : GValue)
   is
      Val   : constant System.Address := Get_Address (Value);
   begin
      if Val = System.Null_Address then
         Set_Text (Gtk_Label (Editor), "Pointer: (null)");
      else
         Set_Text
           (Gtk_Label (Editor), "Pointer: 0x" & System.Address_Image (Val));
      end if;
   end Pointer_Changed;

   --------------------
   -- Object_Changed --
   --------------------

   procedure Object_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Value         : GValue)
   is
      Val   : constant System.Address := Get_Address (Value);
      Stub  : GObject_Record;
      Obj   : constant GObject := Get_User_Data (Val, Stub);
   begin
      if Obj = null then
         Set_Text (Gtk_Label (Editor), "Object: (null)");
      else
         Set_Text
           (Gtk_Label (Editor),
            "Object: 0x" & System.Address_Image (Obj.all'Address));
      end if;
   end Object_Changed;

   ------------------
   -- Enum_Changed --
   ------------------

   procedure Enum_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Value  : GValue) is
   begin
      Set_Active (Gtk_Combo_Box (Editor), Get_Enum (Value));
   end Enum_Changed;

   ------------------
   -- Uint_Changed --
   ------------------

   procedure Uint_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Value  : GValue) is
   begin
      Set_Value (Gtk_Spin_Button (Editor), Gdouble (Get_Uint (Value)));
   end Uint_Changed;

   -------------------
   -- Bool_Modified --
   -------------------

   procedure Bool_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class) is
   begin
      Set_Boolean (Value, Get_Active (Gtk_Toggle_Button (Editor)));
   end Bool_Modified;

   ------------------
   -- Int_Modified --
   ------------------

   procedure Int_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class) is
   begin
      Set_Int (Value, Gint (Get_Value (Gtk_Spin_Button (Editor))));
   end Int_Modified;

   --------------------
   -- Float_Modified --
   --------------------

   procedure Float_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class) is
   begin
      Set_Float (Value, Gfloat (Get_Value (Gtk_Spin_Button (Editor))));
   end Float_Modified;

   ---------------------
   -- Double_Modified --
   ---------------------

   procedure Double_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class) is
   begin
      Set_Double (Value, Get_Value (Gtk_Spin_Button (Editor)));
   end Double_Modified;

   -------------------
   -- Enum_Modified --
   -------------------

   procedure Enum_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class) is
   begin
      Set_Enum (Value, Get_Active (Gtk_Combo_Box (Editor)));
   end Enum_Modified;

   -------------------
   -- Uint_Modified --
   -------------------

   procedure Uint_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class) is
   begin
      Set_Uint (Value, Guint (Get_Value (Gtk_Spin_Button (Editor))));
   end Uint_Modified;

   ---------------------
   -- String_Modified --
   ---------------------

   procedure String_Modified
     (Value  : in out GValue;
      Editor : access Gtk_Widget_Record'Class) is
   begin
      Set_String (Value, Get_Text (Gtk_Entry (Editor)));
   end String_Modified;

   -----------------------
   -- Property_Modified --
   -----------------------

   procedure Property_Modified
     (Object : access GObject_Record'Class;
      Data   : Property_Modified_Data)
   is
      Value : GValue;
   begin
      Init (Value, Value_Type (Data.Pspec));
      if Data.Is_Child_Prop then
         Child_Get_Property
           (Gtk_Container (Get_Parent (Gtk_Widget (Object))),
            Gtk_Widget (Object), Pspec_Name (Data.Pspec), Value);
      else
         Get_Property (Object, Pspec_Name (Data.Pspec), Value);
      end if;

      Block_Controller (Data.Editor, True);
      Data.Callback (Data.Editor, Value);
      Unset (Value);
      Block_Controller (Data.Editor, False);
   end Property_Modified;

   ----------------------
   -- Block_Controller --
   ----------------------

   procedure Block_Controller
     (Editor : access Gtk_Widget_Record'Class;
      Block  : Boolean)
   is
      Id : Handler_Id;
   begin
      Id := Handler_Id_User_Data.Get (Editor, "gtkada-properties-controller");
      if Block then
         Handler_Block (Editor, Id);
      else
         Handler_Unblock (Editor, Id);
      end if;

   exception
      when Gtkada.Types.Data_Error =>
         --  No such user data
         null;
   end Block_Controller;

   -----------------------
   -- Controller_Called --
   -----------------------

   procedure Controller_Called
     (Editor : access Gtk_Widget_Record'Class;
      Data   : Controller_Data)
   is
      Value : GValue;
   begin
      Init (Value, Value_Type (Data.Pspec));
      Data.Callback (Value, Editor);

      if Data.Is_Child_Prop then
         Child_Set_Property
           (Gtk_Container (Get_Parent (Gtk_Widget (Data.Object))),
            Gtk_Widget (Data.Object), Pspec_Name (Data.Pspec), Value);
      else
         Set_Property (Data.Object, Pspec_Name (Data.Pspec), Value);
      end if;
      Unset (Value);
   end Controller_Called;

   ---------------------
   -- Property_Widget --
   ---------------------

   function Property_Widget
     (Object        : access GObject_Record'Class;
      Property      : Param_Spec;
      Is_Child_Prop : Boolean) return Gtk_Widget
   is
      procedure Connect_Controller
        (Editor        : access Gtk_Widget_Record'Class;
         Signal        : String;
         Callback      : Controller_Callback);
      --  Connect a controller to Editor, ie propagate changes done to
      --  controller onto Property.

      procedure Connect_Property
        (Editor   : access Gtk_Widget_Record'Class;
         Callback : Pspec_Callback);
      --  Setup Callback so that it is called when the property is modified

      ----------------------
      -- Connect_Property --
      ----------------------

      procedure Connect_Property
        (Editor   : access Gtk_Widget_Record'Class;
         Callback : Pspec_Callback)
      is
         Id : Handler_Id;
      begin
         if Is_Child_Prop then
            Id := Prop_Callback.Connect
              (Object, Signal_Child_Notify & "::" & Pspec_Name (Property),
               Prop_Callback.To_Marshaller (Property_Modified'Access),
               User_Data   => (Pspec         => Property,
                               Is_Child_Prop => Is_Child_Prop,
                               Editor        => Gtk_Widget (Editor),
                               Callback      => Callback));
         else
            Id := Prop_Callback.Connect
              (Object, "notify::" & Pspec_Name (Property),
               Prop_Callback.To_Marshaller (Property_Modified'Access),
               User_Data   => (Pspec         => Property,
                               Is_Child_Prop => Is_Child_Prop,
                               Editor        => Gtk_Widget (Editor),
                               Callback      => Callback));
         end if;
         Add_Watch (Id, Editor);
      end Connect_Property;

      ------------------------
      -- Connect_Controller --
      ------------------------

      procedure Connect_Controller
        (Editor        : access Gtk_Widget_Record'Class;
         Signal        : String;
         Callback      : Controller_Callback)
      is
         Id : Handler_Id;
      begin
         Id := Controller_Cb.Connect
           (Editor, Signal,
            Controller_Cb.To_Marshaller (Controller_Called'Access),
            Controller_Data'
              (Pspec         => Property,
               Callback      => Callback,
               Is_Child_Prop => Is_Child_Prop,
               Object        => GObject (Object)));
         Add_Watch (Id, Object);
         Handler_Id_User_Data.Set (Editor, Id, "gtkada-properties-controller");
      end Connect_Controller;

      Toggle  : Gtk_Check_Button;
      Ent     : Gtk_Entry;
      Spin    : Gtk_Spin_Button;
      E_Klass : Enum_Class;
      Val     : Enum_Value;
      K       : Guint;
      Combo   : Gtk_Combo_Box;
      Label   : Gtk_Label;

   begin
      if Value_Type (Property) = GType_Boolean then
         Gtk_New (Toggle);
         Connect_Property   (Toggle, Bool_Changed'Access);
         Connect_Controller (Toggle, "toggled", Bool_Modified'Access);
         return Gtk_Widget (Toggle);

      elsif Value_Type (Property) = GType_String then
         Gtk_New (Ent);
         Connect_Property   (Ent, String_Changed'Access);
         Connect_Controller (Ent, "changed", String_Modified'Access);
         return Gtk_Widget (Ent);

      elsif Value_Type (Property) = GType_Int then
         Gtk_New (Spin,
                  Min => Gdouble (Minimum (Param_Spec_Int (Property))),
                  Max => Gdouble (Maximum (Param_Spec_Int (Property))),
                  Step => 1.0);
         Connect_Property   (Spin, Int_Changed'Access);
         Connect_Controller (Spin, "value_changed", Int_Modified'Access);
         return Gtk_Widget (Spin);

      elsif Value_Type (Property) = GType_Uint then
         Gtk_New (Spin,
                  Min => Gdouble (Minimum (Param_Spec_Uint (Property))),
                  Max => Gdouble (Maximum (Param_Spec_Uint (Property))),
                  Step => 1.0);
         Connect_Property   (Spin, Uint_Changed'Access);
         Connect_Controller (Spin, "value_changed", Uint_Modified'Access);
         return Gtk_Widget (Spin);

      elsif Value_Type (Property) = GType_Float then
         Gtk_New (Spin,
                  Min => Gdouble (Minimum (Param_Spec_Float (Property))),
                  Max => Gdouble (Maximum (Param_Spec_Float (Property))),
                  Step => 1.0);
         Connect_Property   (Spin, Float_Changed'Access);
         Connect_Controller (Spin, "value_changed", Float_Modified'Access);
         return Gtk_Widget (Spin);

      elsif Value_Type (Property) = GType_Double then
         Gtk_New (Spin,
                  Min => Minimum (Param_Spec_Double (Property)),
                  Max => Maximum (Param_Spec_Double (Property)),
                  Step => 1.0);
         Connect_Property   (Spin, Double_Changed'Access);
         Connect_Controller (Spin, "value_changed", Double_Modified'Access);
         return Gtk_Widget (Spin);

      elsif Fundamental (Value_Type (Property)) = GType_Enum then
         Gtk_New_Text (Combo);
         E_Klass := Enumeration (Param_Spec_Enum (Property));
         K := 0;
         loop
            Val := Nth_Value (E_Klass, K);
            exit when Val = null;
            Append_Text (Combo, Nick (Val));
            K := K + 1;
         end loop;
         Connect_Property   (Combo, Enum_Changed'Access);
         Connect_Controller (Combo, "changed", Enum_Modified'Access);
         return Gtk_Widget (Combo);

      elsif Fundamental (Value_Type (Property)) = GType_Pointer then
         Gtk_New (Label);
         Set_Alignment (Label, 0.0, 0.5);
         Connect_Property (Label, Pointer_Changed'Access);
         return Gtk_Widget (Label);

      elsif Fundamental (Value_Type (Property)) = GType_Object then
         Gtk_New (Label);
         Set_Alignment (Label, 0.0, 0.5);
         Connect_Property (Label, Object_Changed'Access);
         return Gtk_Widget (Label);
      end if;

      return null;
   end Property_Widget;

   ----------------------
   -- Object_Destroyed --
   ----------------------

   procedure Object_Destroyed (Editor, Object : System.Address) is
      Stub : Properties_Editor_Record;
      Ed   : constant Properties_Editor :=
        Properties_Editor (Get_User_Data (Editor, Stub));
      pragma Unreferenced (Object, Stub);
   begin
      Work_On (Ed, null);
      Hide (Ed);
   end Object_Destroyed;

   -------------------
   -- Editor_Delete --
   -------------------

   procedure Editor_Delete (Editor : access GObject_Record'Class) is
      Ed : constant Properties_Editor := Properties_Editor (Editor);
   begin
      if Global_Editor = Ed then
         Global_Editor := null;
      end if;

      Unref (Ed.Tips);
   end Editor_Delete;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Editor : out Properties_Editor) is
      Scrolled : Gtk_Scrolled_Window;
      Note     : Gtk_Notebook;
      Label    : Gtk_Label;
   begin
      Editor := new Properties_Editor_Record;
      Gtk.Window.Initialize (Editor, Window_TopleveL);
      Set_Title (Editor, "Properties editor");
      Set_Default_Size (Editor, 400, 400);

      Object_Callback.Connect (Editor, "destroy", Editor_Delete'Access);

      Gtk_New (Editor.Tips);
      Ref (Editor.Tips);
      Sink (Editor.Tips);

      Gtk_New (Note);
      Add (Editor, Note);

      --  Page 1: Properties
      Gtk_New (Label, "Properties");

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Append_Page (Note, Scrolled, Label);

      Gtk_New (Editor.Table, Rows => 0, Columns => 2, Homogeneous => False);
      Add_With_Viewport (Scrolled, Editor.Table);
      Set_Col_Spacing  (Editor.Table, 0, 5);
      Set_Row_Spacings (Editor.Table, 0);
      Modify_Font (Editor.Table, From_String ("Sans 8"));

      --  Page 2: Child properties

      Gtk_New (Label, "Child Properties");
      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Append_Page (Note, Scrolled, Label);

      Gtk_New
        (Editor.Child_Table, Rows => 0, Columns => 2, Homogeneous => False);
      Add_With_Viewport (Scrolled, Editor.Child_Table);
      Set_Col_Spacing  (Editor.Child_Table, 0, 5);
      Set_Row_Spacings (Editor.Child_Table, 0);
      Modify_Font (Editor.Child_Table, From_String ("Sans 8"));
   end Gtk_New;

   ---------------------
   -- Show_Properties --
   ---------------------

   procedure Show_Properties
     (Editor        : access Properties_Editor_Record'Class;
      Table         : Gtk_Table;
      Is_Child_Prop : Boolean;
      T             : GType;
      Properties    : Param_Spec_Array)
   is
      Parent_Inserted : Boolean := False;
      Row : Guint;
      Label : Gtk_Label;
      Event : Gtk_Event_Box;
      Success : Boolean;
      W       : Gtk_Widget;
      Can_Modify : Boolean;
   begin
      for P in Properties'Range loop
         if Owner_Type (Properties (P)) = T then
            Row := Get_Property (Table, N_Rows_Property);

            if not Parent_Inserted then
               if Editor.Type_Color = Null_Color then
                  Editor.Type_Color := Parse ("red");
                  Alloc_Color (Get_Default_Colormap, Editor.Type_Color,
                               Success => Success);
               end if;

               Resize (Table, Row + 2, 2);
               Gtk_New (Label, Type_Name (T));
               Set_Alignment (Label, 0.0, 0.5);
               Modify_Fg (Label, State_Normal, Editor.Type_Color);
               Attach (Table, Label, 0, 1, Row, Row + 1,
                       Yoptions => 0);
               Row := Row + 1;
               Parent_Inserted := True;
            else
               Resize (Table, Row + 1, 2);
            end if;

            Can_Modify := (Flags (Properties (P)) and Param_Writable) /= 0
              and then (Flags (Properties (P)) and Param_Construct_Only) = 0;

            Gtk_New (Event);
            Attach (Table, Event, 0, 1, Row, Row + 1,
                    Xpadding => 10, Yoptions => 0);
            Set_Tip (Editor.Tips, Event, Description (Properties (P)));

            Gtk_New (Label, Nick_Name (Properties (P)));
            Set_Alignment (Label, 0.0, 0.5);
            Set_Sensitive (Label, Can_Modify);
            Add (Event, Label);

            if (Flags (Properties (P)) and Param_Readable) = 0 then
               Gtk_New (Label, "<unreadable>");
               Set_Alignment (Label, 0.0, 0.5);
               W := Gtk_Widget (Label);
               Can_Modify := False;
            else
               W := Property_Widget
                 (Editor.Object, Properties (P), Is_Child_Prop);
               if W = null then
                  Gtk_New (Label, "uneditable type: "
                           & Type_Name (Value_Type (Properties (P))));
                  Set_Alignment (Label, 0.0, 0.5);
                  W := Gtk_Widget (Label);
                  Can_Modify := False;
               end if;
            end if;

            if W /= null then
               Attach (Table, W, 1, 2, Row, Row + 1, Yoptions => 0);
               Set_Sensitive (W, Can_Modify);

               if Is_Child_Prop then
                  Child_Notify
                    (Gtk_Widget (Editor.Object), Pspec_Name (Properties (P)));
               else
                  Notify (Editor.Object, Pspec_Name (Properties (P)));
               end if;
            end if;
         end if;
      end loop;
   end Show_Properties;

   -------------------------------
   -- Show_Properties_From_Type --
   -------------------------------

   procedure Show_Properties_From_Type
     (Editor : access Properties_Editor_Record'Class;
      T      : GType) is
   begin
      if Is_Interface (T) then
         Show_Properties
           (Editor        => Editor,
            Table         => Editor.Table,
            Is_Child_Prop => False,
            T             => T,
            Properties    => Interface_List_Properties
              (Default_Interface_Peek (T)));
      else
         Show_Properties
           (Editor        => Editor,
            Table         => Editor.Table,
            Is_Child_Prop => False,
            T             => T,
            Properties    => Class_List_Properties
              (GObject_Class (Class_Peek (T))));
      end if;
   end Show_Properties_From_Type;

   -------------
   -- Work_On --
   -------------

   procedure Work_On
     (Editor : access Properties_Editor_Record'Class;
      Object : GObject := null)
   is
      T : GType;
   begin
      if Editor.Object /= null then
         Weak_Unref
           (Editor.Object, Object_Destroyed'Access, Get_Object (Editor));
         Resize (Editor.Table, 0, 2);
         Resize (Editor.Child_Table, 0, 2);
      end if;

      Editor.Object := Object;

      if Editor.Object /= null then
         Weak_Ref
           (Editor.Object, Object_Destroyed'Access, Get_Object (Editor));

         T := Get_Type (Object);
         Set_Title (Editor, "Properties of " & Type_Name (T));

         while T /= GType_Invalid loop
            Show_Properties_From_Type (Editor, T);
            T := Parent (T);
         end loop;

         declare
            Ifaces : constant GType_Array := Interfaces (Get_Type (Object));
         begin
            for F in Ifaces'Range loop
               Show_Properties_From_Type (Editor, Ifaces (F));
            end loop;
         end;

         --  Show the child properties owned by the parent and that apply to
         --  the current object.

         if Object.all in Gtk_Widget_Record'Class
           and then Get_Parent (Gtk_Widget (Object)) /= null
         then
            T := Get_Type (Get_Parent (Gtk_Widget (Object)));
            while T /= GType_Invalid loop
               if Is_A (T, Gtk.Container.Get_Type) then
                  Show_Properties
                    (Editor        => Editor,
                     Table         => Editor.Child_Table,
                     Is_Child_Prop => True,
                     T             => T,
                     Properties    => Class_List_Child_Properties
                       (GObject_Class (Class_Peek (T))));
               end if;
               T := Parent (T);
            end loop;
         end if;
      end if;

      Show_All (Editor);
   end Work_On;

   -----------------------------
   -- Popup_Properties_Editor --
   -----------------------------

   procedure Popup_Properties_Editor
     (Object : access Glib.Object.GObject_Record'Class)
   is
   begin
      if Global_Editor = null then
         Gtk_New (Global_Editor);
      end if;

      Work_On (Global_Editor, GObject (Object));
      Present (Global_Editor);
   end Popup_Properties_Editor;

end Gtkada.Properties;
