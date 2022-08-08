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

package body Gtk.Overlay is

   package Type_Conversion_Gtk_Overlay is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Overlay_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Overlay);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Overlay) is
   begin
      Self := new Gtk_Overlay_Record;
      Gtk.Overlay.Initialize (Self);
   end Gtk_New;

   ---------------------
   -- Gtk_Overlay_New --
   ---------------------

   function Gtk_Overlay_New return Gtk_Overlay is
      Self : constant Gtk_Overlay := new Gtk_Overlay_Record;
   begin
      Gtk.Overlay.Initialize (Self);
      return Self;
   end Gtk_Overlay_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Overlay_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_overlay_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -----------------
   -- Add_Overlay --
   -----------------

   procedure Add_Overlay
      (Self   : not null access Gtk_Overlay_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Widget : System.Address);
      pragma Import (C, Internal, "gtk_overlay_add_overlay");
   begin
      Internal (Get_Object (Self), Get_Object (Widget));
   end Add_Overlay;

   ------------------------------
   -- Get_Overlay_Pass_Through --
   ------------------------------

   function Get_Overlay_Pass_Through
      (Self   : not null access Gtk_Overlay_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean
   is
      function Internal
         (Self   : System.Address;
          Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_overlay_get_overlay_pass_through");
   begin
      return Internal (Get_Object (Self), Get_Object (Widget)) /= 0;
   end Get_Overlay_Pass_Through;

   ---------------------
   -- Reorder_Overlay --
   ---------------------

   procedure Reorder_Overlay
      (Self  : not null access Gtk_Overlay_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Index : Glib.Gint)
   is
      procedure Internal
         (Self  : System.Address;
          Child : System.Address;
          Index : Glib.Gint);
      pragma Import (C, Internal, "gtk_overlay_reorder_overlay");
   begin
      Internal (Get_Object (Self), Get_Object (Child), Index);
   end Reorder_Overlay;

   ------------------------------
   -- Set_Overlay_Pass_Through --
   ------------------------------

   procedure Set_Overlay_Pass_Through
      (Self         : not null access Gtk_Overlay_Record;
       Widget       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Pass_Through : Boolean)
   is
      procedure Internal
         (Self         : System.Address;
          Widget       : System.Address;
          Pass_Through : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_overlay_set_overlay_pass_through");
   begin
      Internal (Get_Object (Self), Get_Object (Widget), Boolean'Pos (Pass_Through));
   end Set_Overlay_Pass_Through;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Widget_Gdk_Rectangle_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Widget_Gdk_Rectangle_Boolean);

   procedure Connect
      (Object  : access Gtk_Overlay_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Overlay_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Gdk_Rectangle_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gtk_Widget_Gdk_Rectangle_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Widget_Gdk_Rectangle_Boolean);

   procedure Marsh_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Overlay_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Overlay_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Gdk_Rectangle_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Widget_Gdk_Rectangle_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ----------------------------------------------------
   -- Marsh_GObject_Gtk_Widget_Gdk_Rectangle_Boolean --
   ----------------------------------------------------

   procedure Marsh_GObject_Gtk_Widget_Gdk_Rectangle_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Widget_Gdk_Rectangle_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)), Unchecked_To_Gdk_Rectangle_Access (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Widget_Gdk_Rectangle_Boolean;

   --------------------------------------------------------
   -- Marsh_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean --
   --------------------------------------------------------

   procedure Marsh_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Overlay := Gtk_Overlay (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)), Unchecked_To_Gdk_Rectangle_Access (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean;

   ---------------------------
   -- On_Get_Child_Position --
   ---------------------------

   procedure On_Get_Child_Position
      (Self  : not null access Gtk_Overlay_Record;
       Call  : Cb_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "get-child-position" & ASCII.NUL, Call, After);
   end On_Get_Child_Position;

   ---------------------------
   -- On_Get_Child_Position --
   ---------------------------

   procedure On_Get_Child_Position
      (Self  : not null access Gtk_Overlay_Record;
       Call  : Cb_GObject_Gtk_Widget_Gdk_Rectangle_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "get-child-position" & ASCII.NUL, Call, After, Slot);
   end On_Get_Child_Position;

end Gtk.Overlay;
