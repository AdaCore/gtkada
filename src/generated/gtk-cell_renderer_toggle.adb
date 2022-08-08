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

package body Gtk.Cell_Renderer_Toggle is

   package Type_Conversion_Gtk_Cell_Renderer_Toggle is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Cell_Renderer_Toggle_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Cell_Renderer_Toggle);

   ----------------------------------
   -- Gtk_Cell_Renderer_Toggle_New --
   ----------------------------------

   function Gtk_Cell_Renderer_Toggle_New return Gtk_Cell_Renderer_Toggle is
      Self : constant Gtk_Cell_Renderer_Toggle := new Gtk_Cell_Renderer_Toggle_Record;
   begin
      Gtk.Cell_Renderer_Toggle.Initialize (Self);
      return Self;
   end Gtk_Cell_Renderer_Toggle_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Cell_Renderer_Toggle) is
   begin
      Self := new Gtk_Cell_Renderer_Toggle_Record;
      Gtk.Cell_Renderer_Toggle.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Cell_Renderer_Toggle_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_cell_renderer_toggle_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ---------------------
   -- Get_Activatable --
   ---------------------

   function Get_Activatable
      (Self : not null access Gtk_Cell_Renderer_Toggle_Record)
       return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_cell_renderer_toggle_get_activatable");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Activatable;

   ----------------
   -- Get_Active --
   ----------------

   function Get_Active
      (Self : not null access Gtk_Cell_Renderer_Toggle_Record)
       return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_cell_renderer_toggle_get_active");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Active;

   ---------------
   -- Get_Radio --
   ---------------

   function Get_Radio
      (Self : not null access Gtk_Cell_Renderer_Toggle_Record)
       return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_cell_renderer_toggle_get_radio");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Radio;

   ---------------------
   -- Set_Activatable --
   ---------------------

   procedure Set_Activatable
      (Self    : not null access Gtk_Cell_Renderer_Toggle_Record;
       Setting : Boolean)
   is
      procedure Internal (Self : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_cell_renderer_toggle_set_activatable");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Setting));
   end Set_Activatable;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active
      (Self    : not null access Gtk_Cell_Renderer_Toggle_Record;
       Setting : Boolean)
   is
      procedure Internal (Self : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_cell_renderer_toggle_set_active");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Setting));
   end Set_Active;

   ---------------
   -- Set_Radio --
   ---------------

   procedure Set_Radio
      (Self  : not null access Gtk_Cell_Renderer_Toggle_Record;
       Radio : Boolean)
   is
      procedure Internal (Self : System.Address; Radio : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_cell_renderer_toggle_set_radio");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Radio));
   end Set_Radio;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Cell_Renderer_Toggle_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Cell_Renderer_Toggle_UTF8_String_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_UTF8_String_Void);

   procedure Connect
      (Object  : access Gtk_Cell_Renderer_Toggle_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Cell_Renderer_Toggle_UTF8_String_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Cell_Renderer_Toggle_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_UTF8_String_Void);

   procedure Marsh_Gtk_Cell_Renderer_Toggle_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Cell_Renderer_Toggle_UTF8_String_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Cell_Renderer_Toggle_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Cell_Renderer_Toggle_UTF8_String_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Cell_Renderer_Toggle_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Cell_Renderer_Toggle_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------------------------
   -- Marsh_GObject_UTF8_String_Void --
   ------------------------------------

   procedure Marsh_GObject_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_UTF8_String_Void;

   -----------------------------------------------------
   -- Marsh_Gtk_Cell_Renderer_Toggle_UTF8_String_Void --
   -----------------------------------------------------

   procedure Marsh_Gtk_Cell_Renderer_Toggle_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Cell_Renderer_Toggle_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Cell_Renderer_Toggle := Gtk_Cell_Renderer_Toggle (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Cell_Renderer_Toggle_UTF8_String_Void;

   ----------------
   -- On_Toggled --
   ----------------

   procedure On_Toggled
      (Self  : not null access Gtk_Cell_Renderer_Toggle_Record;
       Call  : Cb_Gtk_Cell_Renderer_Toggle_UTF8_String_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "toggled" & ASCII.NUL, Call, After);
   end On_Toggled;

   ----------------
   -- On_Toggled --
   ----------------

   procedure On_Toggled
      (Self  : not null access Gtk_Cell_Renderer_Toggle_Record;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "toggled" & ASCII.NUL, Call, After, Slot);
   end On_Toggled;

end Gtk.Cell_Renderer_Toggle;
