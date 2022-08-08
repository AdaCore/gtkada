------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Ada.Unchecked_Conversion;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Color_Button is

   package Type_Conversion_Gtk_Color_Button is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Color_Button_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Color_Button);

   --------------------------
   -- Gtk_Color_Button_New --
   --------------------------

   function Gtk_Color_Button_New return Gtk_Color_Button is
      Button : constant Gtk_Color_Button := new Gtk_Color_Button_Record;
   begin
      Gtk.Color_Button.Initialize (Button);
      return Button;
   end Gtk_Color_Button_New;

   -------------------------------------
   -- Gtk_Color_Button_New_With_Color --
   -------------------------------------

   function Gtk_Color_Button_New_With_Color
      (Color : Gdk.Color.Gdk_Color) return Gtk_Color_Button
   is
      Button : constant Gtk_Color_Button := new Gtk_Color_Button_Record;
   begin
      Gtk.Color_Button.Initialize_With_Color (Button, Color);
      return Button;
   end Gtk_Color_Button_New_With_Color;

   ------------------------------------
   -- Gtk_Color_Button_New_With_Rgba --
   ------------------------------------

   function Gtk_Color_Button_New_With_Rgba
      (Rgba : Gdk.RGBA.Gdk_RGBA) return Gtk_Color_Button
   is
      Button : constant Gtk_Color_Button := new Gtk_Color_Button_Record;
   begin
      Gtk.Color_Button.Initialize_With_Rgba (Button, Rgba);
      return Button;
   end Gtk_Color_Button_New_With_Rgba;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Button : out Gtk_Color_Button) is
   begin
      Button := new Gtk_Color_Button_Record;
      Gtk.Color_Button.Initialize (Button);
   end Gtk_New;

   ------------------------
   -- Gtk_New_With_Color --
   ------------------------

   procedure Gtk_New_With_Color
      (Button : out Gtk_Color_Button;
       Color  : Gdk.Color.Gdk_Color)
   is
   begin
      Button := new Gtk_Color_Button_Record;
      Gtk.Color_Button.Initialize_With_Color (Button, Color);
   end Gtk_New_With_Color;

   -----------------------
   -- Gtk_New_With_Rgba --
   -----------------------

   procedure Gtk_New_With_Rgba
      (Button : out Gtk_Color_Button;
       Rgba   : Gdk.RGBA.Gdk_RGBA)
   is
   begin
      Button := new Gtk_Color_Button_Record;
      Gtk.Color_Button.Initialize_With_Rgba (Button, Rgba);
   end Gtk_New_With_Rgba;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Button : not null access Gtk_Color_Button_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_color_button_new");
   begin
      if not Button.Is_Created then
         Set_Object (Button, Internal);
      end if;
   end Initialize;

   ---------------------------
   -- Initialize_With_Color --
   ---------------------------

   procedure Initialize_With_Color
      (Button : not null access Gtk_Color_Button_Record'Class;
       Color  : Gdk.Color.Gdk_Color)
   is
      function Internal (Color : Gdk.Color.Gdk_Color) return System.Address;
      pragma Import (C, Internal, "gtk_color_button_new_with_color");
   begin
      if not Button.Is_Created then
         Set_Object (Button, Internal (Color));
      end if;
   end Initialize_With_Color;

   --------------------------
   -- Initialize_With_Rgba --
   --------------------------

   procedure Initialize_With_Rgba
      (Button : not null access Gtk_Color_Button_Record'Class;
       Rgba   : Gdk.RGBA.Gdk_RGBA)
   is
      function Internal (Rgba : Gdk.RGBA.Gdk_RGBA) return System.Address;
      pragma Import (C, Internal, "gtk_color_button_new_with_rgba");
   begin
      if not Button.Is_Created then
         Set_Object (Button, Internal (Rgba));
      end if;
   end Initialize_With_Rgba;

   ---------------
   -- Get_Alpha --
   ---------------

   function Get_Alpha
      (Button : not null access Gtk_Color_Button_Record) return Guint16
   is
      function Internal (Button : System.Address) return Guint16;
      pragma Import (C, Internal, "gtk_color_button_get_alpha");
   begin
      return Internal (Get_Object (Button));
   end Get_Alpha;

   ---------------
   -- Get_Color --
   ---------------

   procedure Get_Color
      (Button : not null access Gtk_Color_Button_Record;
       Color  : out Gdk.Color.Gdk_Color)
   is
      procedure Internal
         (Button : System.Address;
          Color  : out Gdk.Color.Gdk_Color);
      pragma Import (C, Internal, "gtk_color_button_get_color");
   begin
      Internal (Get_Object (Button), Color);
   end Get_Color;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title
      (Button : not null access Gtk_Color_Button_Record) return UTF8_String
   is
      function Internal
         (Button : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_color_button_get_title");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Button)));
   end Get_Title;

   ---------------
   -- Set_Alpha --
   ---------------

   procedure Set_Alpha
      (Button : not null access Gtk_Color_Button_Record;
       Alpha  : Guint16)
   is
      procedure Internal (Button : System.Address; Alpha : Guint16);
      pragma Import (C, Internal, "gtk_color_button_set_alpha");
   begin
      Internal (Get_Object (Button), Alpha);
   end Set_Alpha;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
      (Button : not null access Gtk_Color_Button_Record;
       Color  : Gdk.Color.Gdk_Color)
   is
      procedure Internal
         (Button : System.Address;
          Color  : Gdk.Color.Gdk_Color);
      pragma Import (C, Internal, "gtk_color_button_set_color");
   begin
      Internal (Get_Object (Button), Color);
   end Set_Color;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
      (Button : not null access Gtk_Color_Button_Record;
       Title  : UTF8_String)
   is
      procedure Internal
         (Button : System.Address;
          Title  : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_color_button_set_title");
      Tmp_Title : Gtkada.Types.Chars_Ptr := New_String (Title);
   begin
      Internal (Get_Object (Button), Tmp_Title);
      Free (Tmp_Title);
   end Set_Title;

   -----------------
   -- Add_Palette --
   -----------------

   procedure Add_Palette
      (Self            : not null access Gtk_Color_Button_Record;
       Orientation     : Gtk.Enums.Gtk_Orientation;
       Colors_Per_Line : Glib.Gint;
       N_Colors        : Glib.Gint;
       Colors          : array_of_Gdk_RGBA)
   is
      procedure Internal
         (Self            : System.Address;
          Orientation     : Gtk.Enums.Gtk_Orientation;
          Colors_Per_Line : Glib.Gint;
          N_Colors        : Glib.Gint;
          Colors          : array_of_Gdk_RGBA);
      pragma Import (C, Internal, "gtk_color_chooser_add_palette");
   begin
      Internal (Get_Object (Self), Orientation, Colors_Per_Line, N_Colors, Colors);
   end Add_Palette;

   ---------------------------
   -- Do_Set_Related_Action --
   ---------------------------

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Color_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_do_set_related_action");
   begin
      Internal (Get_Object (Self), Get_Object (Action));
   end Do_Set_Related_Action;

   ---------------------
   -- Get_Action_Name --
   ---------------------

   function Get_Action_Name
      (Self : not null access Gtk_Color_Button_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_actionable_get_action_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Action_Name;

   -----------------------------
   -- Get_Action_Target_Value --
   -----------------------------

   function Get_Action_Target_Value
      (Self : not null access Gtk_Color_Button_Record)
       return Glib.Variant.Gvariant
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_actionable_get_action_target_value");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Action_Target_Value;

   ------------------------
   -- Get_Related_Action --
   ------------------------

   function Get_Related_Action
      (Self : not null access Gtk_Color_Button_Record)
       return Gtk.Action.Gtk_Action
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_activatable_get_related_action");
      Stub_Gtk_Action : Gtk.Action.Gtk_Action_Record;
   begin
      return Gtk.Action.Gtk_Action (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Action));
   end Get_Related_Action;

   --------------
   -- Get_Rgba --
   --------------

   procedure Get_Rgba
      (Self  : not null access Gtk_Color_Button_Record;
       Color : out Gdk.RGBA.Gdk_RGBA)
   is
      procedure Internal
         (Self  : System.Address;
          Color : out Gdk.RGBA.Gdk_RGBA);
      pragma Import (C, Internal, "gtk_color_chooser_get_rgba");
   begin
      Internal (Get_Object (Self), Color);
   end Get_Rgba;

   -------------------------------
   -- Get_Use_Action_Appearance --
   -------------------------------

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Color_Button_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_activatable_get_use_action_appearance");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Use_Action_Appearance;

   -------------------
   -- Get_Use_Alpha --
   -------------------

   function Get_Use_Alpha
      (Self : not null access Gtk_Color_Button_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_color_chooser_get_use_alpha");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Use_Alpha;

   ---------------------
   -- Set_Action_Name --
   ---------------------

   procedure Set_Action_Name
      (Self        : not null access Gtk_Color_Button_Record;
       Action_Name : UTF8_String := "")
   is
      procedure Internal
         (Self        : System.Address;
          Action_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_actionable_set_action_name");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr;
   begin
      if Action_Name = "" then
         Tmp_Action_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Action_Name := New_String (Action_Name);
      end if;
      Internal (Get_Object (Self), Tmp_Action_Name);
      Free (Tmp_Action_Name);
   end Set_Action_Name;

   -----------------------------
   -- Set_Action_Target_Value --
   -----------------------------

   procedure Set_Action_Target_Value
      (Self         : not null access Gtk_Color_Button_Record;
       Target_Value : Glib.Variant.Gvariant)
   is
      procedure Internal
         (Self         : System.Address;
          Target_Value : System.Address);
      pragma Import (C, Internal, "gtk_actionable_set_action_target_value");
   begin
      Internal (Get_Object (Self), Get_Object (Target_Value));
   end Set_Action_Target_Value;

   ------------------------------
   -- Set_Detailed_Action_Name --
   ------------------------------

   procedure Set_Detailed_Action_Name
      (Self                 : not null access Gtk_Color_Button_Record;
       Detailed_Action_Name : UTF8_String)
   is
      procedure Internal
         (Self                 : System.Address;
          Detailed_Action_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_actionable_set_detailed_action_name");
      Tmp_Detailed_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Detailed_Action_Name);
   begin
      Internal (Get_Object (Self), Tmp_Detailed_Action_Name);
      Free (Tmp_Detailed_Action_Name);
   end Set_Detailed_Action_Name;

   ------------------------
   -- Set_Related_Action --
   ------------------------

   procedure Set_Related_Action
      (Self   : not null access Gtk_Color_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_set_related_action");
   begin
      Internal (Get_Object (Self), Get_Object (Action));
   end Set_Related_Action;

   --------------
   -- Set_Rgba --
   --------------

   procedure Set_Rgba
      (Self  : not null access Gtk_Color_Button_Record;
       Color : Gdk.RGBA.Gdk_RGBA)
   is
      procedure Internal (Self : System.Address; Color : Gdk.RGBA.Gdk_RGBA);
      pragma Import (C, Internal, "gtk_color_chooser_set_rgba");
   begin
      Internal (Get_Object (Self), Color);
   end Set_Rgba;

   -------------------------------
   -- Set_Use_Action_Appearance --
   -------------------------------

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Color_Button_Record;
       Use_Appearance : Boolean)
   is
      procedure Internal
         (Self           : System.Address;
          Use_Appearance : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_activatable_set_use_action_appearance");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Use_Appearance));
   end Set_Use_Action_Appearance;

   -------------------
   -- Set_Use_Alpha --
   -------------------

   procedure Set_Use_Alpha
      (Self      : not null access Gtk_Color_Button_Record;
       Use_Alpha : Boolean)
   is
      procedure Internal (Self : System.Address; Use_Alpha : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_color_chooser_set_use_alpha");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Use_Alpha));
   end Set_Use_Alpha;

   ----------------------------
   -- Sync_Action_Properties --
   ----------------------------

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Color_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_sync_action_properties");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Action)));
   end Sync_Action_Properties;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Color_Button_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Color_Button_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_Color_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Color_Button_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Color_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Color_Button_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Color_Button_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Color_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Color_Button_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Color_Button_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Color_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------------
   -- Marsh_GObject_Void --
   ------------------------

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   ---------------------------------
   -- Marsh_Gtk_Color_Button_Void --
   ---------------------------------

   procedure Marsh_Gtk_Color_Button_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Color_Button_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Color_Button := Gtk_Color_Button (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Color_Button_Void;

   ------------------
   -- On_Color_Set --
   ------------------

   procedure On_Color_Set
      (Self  : not null access Gtk_Color_Button_Record;
       Call  : Cb_Gtk_Color_Button_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "color-set" & ASCII.NUL, Call, After);
   end On_Color_Set;

   ------------------
   -- On_Color_Set --
   ------------------

   procedure On_Color_Set
      (Self  : not null access Gtk_Color_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "color-set" & ASCII.NUL, Call, After, Slot);
   end On_Color_Set;

end Gtk.Color_Button;
