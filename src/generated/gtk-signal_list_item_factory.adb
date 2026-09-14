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

package body Gtk.Signal_List_Item_Factory is

   package Type_Conversion_Gtk_Signal_List_Item_Factory is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Signal_List_Item_Factory_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Signal_List_Item_Factory);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Signal_List_Item_Factory) is
   begin
      Self := new Gtk_Signal_List_Item_Factory_Record;
      Gtk.Signal_List_Item_Factory.Initialize (Self);
   end Gtk_New;

   --------------------------------------
   -- Gtk_Signal_List_Item_Factory_New --
   --------------------------------------

   function Gtk_Signal_List_Item_Factory_New return Gtk_Signal_List_Item_Factory is
      Self : constant Gtk_Signal_List_Item_Factory := new Gtk_Signal_List_Item_Factory_Record;
   begin
      Gtk.Signal_List_Item_Factory.Initialize (Self);
      return Self;
   end Gtk_Signal_List_Item_Factory_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Signal_List_Item_Factory_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_signal_list_item_factory_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Signal_List_Item_Factory_Gtk_List_Item_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Signal_List_Item_Factory_Gtk_List_Item_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_List_Item_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_List_Item_Void);

   procedure Connect
      (Object  : access Gtk_Signal_List_Item_Factory_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Signal_List_Item_Factory_Gtk_List_Item_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Signal_List_Item_Factory_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_List_Item_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gtk_List_Item_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_List_Item_Void);

   procedure Marsh_Gtk_Signal_List_Item_Factory_Gtk_List_Item_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Signal_List_Item_Factory_Gtk_List_Item_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Signal_List_Item_Factory_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Signal_List_Item_Factory_Gtk_List_Item_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Signal_List_Item_Factory_Gtk_List_Item_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Signal_List_Item_Factory_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_List_Item_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_List_Item_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   --------------------------------------
   -- Marsh_GObject_Gtk_List_Item_Void --
   --------------------------------------

   procedure Marsh_GObject_Gtk_List_Item_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_List_Item_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.List_Item.Gtk_List_Item (Unchecked_To_Object (Params, 1)));
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_List_Item_Void;

   -----------------------------------------------------------
   -- Marsh_Gtk_Signal_List_Item_Factory_Gtk_List_Item_Void --
   -----------------------------------------------------------

   procedure Marsh_Gtk_Signal_List_Item_Factory_Gtk_List_Item_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Signal_List_Item_Factory_Gtk_List_Item_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Signal_List_Item_Factory := Gtk_Signal_List_Item_Factory (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.List_Item.Gtk_List_Item (Unchecked_To_Object (Params, 1)));
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Signal_List_Item_Factory_Gtk_List_Item_Void;

   -------------
   -- On_Bind --
   -------------

   procedure On_Bind
      (Self  : not null access Gtk_Signal_List_Item_Factory_Record;
       Call  : Cb_Gtk_Signal_List_Item_Factory_Gtk_List_Item_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "bind" & ASCII.NUL, Call, After);
   end On_Bind;

   -------------
   -- On_Bind --
   -------------

   procedure On_Bind
      (Self  : not null access Gtk_Signal_List_Item_Factory_Record;
       Call  : Cb_GObject_Gtk_List_Item_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "bind" & ASCII.NUL, Call, After, Slot);
   end On_Bind;

   --------------
   -- On_Setup --
   --------------

   procedure On_Setup
      (Self  : not null access Gtk_Signal_List_Item_Factory_Record;
       Call  : Cb_Gtk_Signal_List_Item_Factory_Gtk_List_Item_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "setup" & ASCII.NUL, Call, After);
   end On_Setup;

   --------------
   -- On_Setup --
   --------------

   procedure On_Setup
      (Self  : not null access Gtk_Signal_List_Item_Factory_Record;
       Call  : Cb_GObject_Gtk_List_Item_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "setup" & ASCII.NUL, Call, After, Slot);
   end On_Setup;

   -----------------
   -- On_Teardown --
   -----------------

   procedure On_Teardown
      (Self  : not null access Gtk_Signal_List_Item_Factory_Record;
       Call  : Cb_Gtk_Signal_List_Item_Factory_Gtk_List_Item_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "teardown" & ASCII.NUL, Call, After);
   end On_Teardown;

   -----------------
   -- On_Teardown --
   -----------------

   procedure On_Teardown
      (Self  : not null access Gtk_Signal_List_Item_Factory_Record;
       Call  : Cb_GObject_Gtk_List_Item_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "teardown" & ASCII.NUL, Call, After, Slot);
   end On_Teardown;

   ---------------
   -- On_Unbind --
   ---------------

   procedure On_Unbind
      (Self  : not null access Gtk_Signal_List_Item_Factory_Record;
       Call  : Cb_Gtk_Signal_List_Item_Factory_Gtk_List_Item_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "unbind" & ASCII.NUL, Call, After);
   end On_Unbind;

   ---------------
   -- On_Unbind --
   ---------------

   procedure On_Unbind
      (Self  : not null access Gtk_Signal_List_Item_Factory_Record;
       Call  : Cb_GObject_Gtk_List_Item_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "unbind" & ASCII.NUL, Call, After, Slot);
   end On_Unbind;

end Gtk.Signal_List_Item_Factory;
