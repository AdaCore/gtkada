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
with Glib.Values;              use Glib.Values;
with Gtk.Arguments;            use Gtk.Arguments;
with Gtkada.Bindings;          use Gtkada.Bindings;

package body Gtk.Print_Operation_Preview is

   -----------------
   -- Is_Selected --
   -----------------

   function Is_Selected
      (Preview : Gtk_Print_Operation_Preview;
       Page_Nr : Glib.Gint) return Boolean
   is
      function Internal
         (Preview : Gtk_Print_Operation_Preview;
          Page_Nr : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_print_operation_preview_is_selected");
   begin
      return Internal (Preview, Page_Nr) /= 0;
   end Is_Selected;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Page_Setup_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Page_Setup_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Print_Context_Gtk_Page_Setup_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Print_Context_Gtk_Page_Setup_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Print_Operation_Preview_Gtk_Print_Context_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Print_Operation_Preview_Gtk_Print_Context_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Print_Context_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Print_Context_Void);

   procedure Connect
      (Object  : Gtk_Print_Operation_Preview;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Page_Setup_Void;
       After   : Boolean);

   procedure Connect
      (Object  : Gtk_Print_Operation_Preview;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Print_Operation_Preview_Gtk_Print_Context_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : Gtk_Print_Operation_Preview;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Print_Context_Gtk_Page_Setup_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : Gtk_Print_Operation_Preview;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Print_Context_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gtk_Print_Context_Gtk_Page_Setup_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Print_Context_Gtk_Page_Setup_Void);

   procedure Marsh_GObject_Gtk_Print_Context_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Print_Context_Void);

   procedure Marsh_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Page_Setup_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Page_Setup_Void);

   procedure Marsh_Gtk_Print_Operation_Preview_Gtk_Print_Context_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Print_Operation_Preview_Gtk_Print_Context_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : Gtk_Print_Operation_Preview;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Page_Setup_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Page_Setup_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : Gtk_Print_Operation_Preview;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Print_Operation_Preview_Gtk_Print_Context_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Print_Operation_Preview_Gtk_Print_Context_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : Gtk_Print_Operation_Preview;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Print_Context_Gtk_Page_Setup_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Print_Context_Gtk_Page_Setup_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : Gtk_Print_Operation_Preview;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Print_Context_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Print_Context_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ---------------------------------------------------------
   -- Marsh_GObject_Gtk_Print_Context_Gtk_Page_Setup_Void --
   ---------------------------------------------------------

   procedure Marsh_GObject_Gtk_Print_Context_Gtk_Page_Setup_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Print_Context_Gtk_Page_Setup_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Print_Context.Gtk_Print_Context (Unchecked_To_Object (Params, 1)), Gtk.Page_Setup.Gtk_Page_Setup (Unchecked_To_Object (Params, 2)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Print_Context_Gtk_Page_Setup_Void;

   ------------------------------------------
   -- Marsh_GObject_Gtk_Print_Context_Void --
   ------------------------------------------

   procedure Marsh_GObject_Gtk_Print_Context_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Print_Context_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Print_Context.Gtk_Print_Context (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Print_Context_Void;

   -----------------------------------------------------------------------------
   -- Marsh_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Page_Setup_Void --
   -----------------------------------------------------------------------------

   procedure Marsh_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Page_Setup_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Page_Setup_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Print_Operation_Preview := Gtk_Print_Operation_Preview (Unchecked_To_Interface (Params, 0));
   begin
      H (Obj, Gtk.Print_Context.Gtk_Print_Context (Unchecked_To_Object (Params, 1)), Gtk.Page_Setup.Gtk_Page_Setup (Unchecked_To_Object (Params, 2)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Page_Setup_Void;

   --------------------------------------------------------------
   -- Marsh_Gtk_Print_Operation_Preview_Gtk_Print_Context_Void --
   --------------------------------------------------------------

   procedure Marsh_Gtk_Print_Operation_Preview_Gtk_Print_Context_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Print_Operation_Preview_Gtk_Print_Context_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Print_Operation_Preview := Gtk_Print_Operation_Preview (Unchecked_To_Interface (Params, 0));
   begin
      H (Obj, Gtk.Print_Context.Gtk_Print_Context (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Print_Operation_Preview_Gtk_Print_Context_Void;

   ----------------------
   -- On_Got_Page_Size --
   ----------------------

   procedure On_Got_Page_Size
      (Self  : Gtk_Print_Operation_Preview;
       Call  : Cb_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Page_Setup_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "got-page-size" & ASCII.NUL, Call, After);
   end On_Got_Page_Size;

   ----------------------
   -- On_Got_Page_Size --
   ----------------------

   procedure On_Got_Page_Size
      (Self  : Gtk_Print_Operation_Preview;
       Call  : Cb_GObject_Gtk_Print_Context_Gtk_Page_Setup_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "got-page-size" & ASCII.NUL, Call, After, Slot);
   end On_Got_Page_Size;

   --------------
   -- On_Ready --
   --------------

   procedure On_Ready
      (Self  : Gtk_Print_Operation_Preview;
       Call  : Cb_Gtk_Print_Operation_Preview_Gtk_Print_Context_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "ready" & ASCII.NUL, Call, After);
   end On_Ready;

   --------------
   -- On_Ready --
   --------------

   procedure On_Ready
      (Self  : Gtk_Print_Operation_Preview;
       Call  : Cb_GObject_Gtk_Print_Context_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "ready" & ASCII.NUL, Call, After, Slot);
   end On_Ready;

   function "+" (W : Gtk_Print_Operation_Preview) return Gtk_Print_Operation_Preview is
   begin
      return W;
   end "+";

end Gtk.Print_Operation_Preview;
