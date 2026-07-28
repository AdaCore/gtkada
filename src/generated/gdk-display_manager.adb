------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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
with System;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gdk.Display_Manager is

   package Type_Conversion_Gdk_Display_Manager is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Display_Manager_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Display_Manager);

   -------------------------
   -- Get_Default_Display --
   -------------------------

   function Get_Default_Display
      (Self : not null access Gdk_Display_Manager_Record)
       return Gdk.Gdk_Display
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_display_manager_get_default_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Gdk_Display (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Display));
   end Get_Default_Display;

   -------------------
   -- List_Displays --
   -------------------

   function List_Displays
      (Self : not null access Gdk_Display_Manager_Record)
       return Gdk.Display.Display_List.Glist
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_display_manager_list_displays");
      Tmp_Return : Gdk.Display.Display_List.Glist;
   begin
      Gdk.Display.Display_List.Set_Object (Tmp_Return, Internal (Get_Object (Self)));
      return Tmp_Return;
   end List_Displays;

   ------------------
   -- Open_Display --
   ------------------

   function Open_Display
      (Self : not null access Gdk_Display_Manager_Record;
       Name : UTF8_String := "") return Gdk.Gdk_Display
   is
      function Internal
         (Self : System.Address;
          Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gdk_display_manager_open_display");
      Tmp_Name         : Gtkada.Types.Chars_Ptr;
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
      Tmp_Return       : System.Address;
   begin
      if Name = "" then
         Tmp_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Name := New_String (Name);
      end if;
      Tmp_Return := Internal (Get_Object (Self), Tmp_Name);
      Free (Tmp_Name);
      return Gdk.Gdk_Display (Get_User_Data (Tmp_Return, Stub_Gdk_Display));
   end Open_Display;

   -------------------------
   -- Set_Default_Display --
   -------------------------

   procedure Set_Default_Display
      (Self    : not null access Gdk_Display_Manager_Record;
       Display : not null access Gdk.Display.Gdk_Display_Record'Class)
   is
      procedure Internal (Self : System.Address; Display : System.Address);
      pragma Import (C, Internal, "gdk_display_manager_set_default_display");
   begin
      Internal (Get_Object (Self), Get_Object (Display));
   end Set_Default_Display;

   ---------
   -- Get --
   ---------

   function Get return Gdk_Display_Manager is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_display_manager_get");
      Stub_Gdk_Display_Manager : Gdk_Display_Manager_Record;
   begin
      return Gdk.Display_Manager.Gdk_Display_Manager (Get_User_Data (Internal, Stub_Gdk_Display_Manager));
   end Get;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gdk_Display_Manager_Gdk_Display_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gdk_Display_Manager_Gdk_Display_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Display_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Display_Void);

   procedure Connect
      (Object  : access Gdk_Display_Manager_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Display_Manager_Gdk_Display_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gdk_Display_Manager_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Display_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gdk_Display_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Display_Void);

   procedure Marsh_Gdk_Display_Manager_Gdk_Display_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gdk_Display_Manager_Gdk_Display_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gdk_Display_Manager_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Display_Manager_Gdk_Display_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gdk_Display_Manager_Gdk_Display_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gdk_Display_Manager_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Display_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Display_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------------------------
   -- Marsh_GObject_Gdk_Display_Void --
   ------------------------------------

   procedure Marsh_GObject_Gdk_Display_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Display_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gdk.Gdk_Display (Unchecked_To_Object (Params, 1)));
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Display_Void;

   ------------------------------------------------
   -- Marsh_Gdk_Display_Manager_Gdk_Display_Void --
   ------------------------------------------------

   procedure Marsh_Gdk_Display_Manager_Gdk_Display_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gdk_Display_Manager_Gdk_Display_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gdk_Display_Manager := Gdk_Display_Manager (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gdk.Gdk_Display (Unchecked_To_Object (Params, 1)));
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gdk_Display_Manager_Gdk_Display_Void;

   -----------------------
   -- On_Display_Opened --
   -----------------------

   procedure On_Display_Opened
      (Self  : not null access Gdk_Display_Manager_Record;
       Call  : Cb_Gdk_Display_Manager_Gdk_Display_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "display-opened" & ASCII.NUL, Call, After);
   end On_Display_Opened;

   -----------------------
   -- On_Display_Opened --
   -----------------------

   procedure On_Display_Opened
      (Self  : not null access Gdk_Display_Manager_Record;
       Call  : Cb_GObject_Gdk_Display_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "display-opened" & ASCII.NUL, Call, After, Slot);
   end On_Display_Opened;

end Gdk.Display_Manager;
