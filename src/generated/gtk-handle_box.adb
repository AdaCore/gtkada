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

package body Gtk.Handle_Box is

   package Type_Conversion_Gtk_Handle_Box is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Handle_Box_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Handle_Box);

   ------------------------
   -- Gtk_Handle_Box_New --
   ------------------------

   function Gtk_Handle_Box_New return Gtk_Handle_Box is
      Handle_Box : constant Gtk_Handle_Box := new Gtk_Handle_Box_Record;
   begin
      Gtk.Handle_Box.Initialize (Handle_Box);
      return Handle_Box;
   end Gtk_Handle_Box_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Handle_Box : out Gtk_Handle_Box) is
   begin
      Handle_Box := new Gtk_Handle_Box_Record;
      Gtk.Handle_Box.Initialize (Handle_Box);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Handle_Box : not null access Gtk_Handle_Box_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_handle_box_new");
   begin
      if not Handle_Box.Is_Created then
         Set_Object (Handle_Box, Internal);
      end if;
   end Initialize;

   ------------------------
   -- Get_Child_Detached --
   ------------------------

   function Get_Child_Detached
      (Handle_Box : not null access Gtk_Handle_Box_Record) return Boolean
   is
      function Internal (Handle_Box : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_handle_box_get_child_detached");
   begin
      return Internal (Get_Object (Handle_Box)) /= 0;
   end Get_Child_Detached;

   -------------------------
   -- Get_Handle_Position --
   -------------------------

   function Get_Handle_Position
      (Handle_Box : not null access Gtk_Handle_Box_Record)
       return Gtk.Enums.Gtk_Position_Type
   is
      function Internal
         (Handle_Box : System.Address) return Gtk.Enums.Gtk_Position_Type;
      pragma Import (C, Internal, "gtk_handle_box_get_handle_position");
   begin
      return Internal (Get_Object (Handle_Box));
   end Get_Handle_Position;

   ---------------------
   -- Get_Shadow_Type --
   ---------------------

   function Get_Shadow_Type
      (Handle_Box : not null access Gtk_Handle_Box_Record)
       return Gtk.Enums.Gtk_Shadow_Type
   is
      function Internal
         (Handle_Box : System.Address) return Gtk.Enums.Gtk_Shadow_Type;
      pragma Import (C, Internal, "gtk_handle_box_get_shadow_type");
   begin
      return Internal (Get_Object (Handle_Box));
   end Get_Shadow_Type;

   -------------------
   -- Get_Snap_Edge --
   -------------------

   function Get_Snap_Edge
      (Handle_Box : not null access Gtk_Handle_Box_Record)
       return Gtk.Enums.Gtk_Position_Type
   is
      function Internal
         (Handle_Box : System.Address) return Gtk.Enums.Gtk_Position_Type;
      pragma Import (C, Internal, "gtk_handle_box_get_snap_edge");
   begin
      return Internal (Get_Object (Handle_Box));
   end Get_Snap_Edge;

   -------------------------
   -- Set_Handle_Position --
   -------------------------

   procedure Set_Handle_Position
      (Handle_Box : not null access Gtk_Handle_Box_Record;
       Position   : Gtk.Enums.Gtk_Position_Type)
   is
      procedure Internal
         (Handle_Box : System.Address;
          Position   : Gtk.Enums.Gtk_Position_Type);
      pragma Import (C, Internal, "gtk_handle_box_set_handle_position");
   begin
      Internal (Get_Object (Handle_Box), Position);
   end Set_Handle_Position;

   ---------------------
   -- Set_Shadow_Type --
   ---------------------

   procedure Set_Shadow_Type
      (Handle_Box : not null access Gtk_Handle_Box_Record;
       The_Type   : Gtk.Enums.Gtk_Shadow_Type)
   is
      procedure Internal
         (Handle_Box : System.Address;
          The_Type   : Gtk.Enums.Gtk_Shadow_Type);
      pragma Import (C, Internal, "gtk_handle_box_set_shadow_type");
   begin
      Internal (Get_Object (Handle_Box), The_Type);
   end Set_Shadow_Type;

   -------------------
   -- Set_Snap_Edge --
   -------------------

   procedure Set_Snap_Edge
      (Handle_Box : not null access Gtk_Handle_Box_Record;
       Edge       : Gtk.Enums.Gtk_Position_Type)
   is
      procedure Internal
         (Handle_Box : System.Address;
          Edge       : Gtk.Enums.Gtk_Position_Type);
      pragma Import (C, Internal, "gtk_handle_box_set_snap_edge");
   begin
      Internal (Get_Object (Handle_Box), Edge);
   end Set_Snap_Edge;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Handle_Box_Gtk_Widget_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Handle_Box_Gtk_Widget_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Widget_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Widget_Void);

   procedure Connect
      (Object  : access Gtk_Handle_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Handle_Box_Gtk_Widget_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Handle_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Widget_Void);

   procedure Marsh_Gtk_Handle_Box_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Handle_Box_Gtk_Widget_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Handle_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Handle_Box_Gtk_Widget_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Handle_Box_Gtk_Widget_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Handle_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Widget_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   -----------------------------------
   -- Marsh_GObject_Gtk_Widget_Void --
   -----------------------------------

   procedure Marsh_GObject_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Widget_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Widget_Void;

   ------------------------------------------
   -- Marsh_Gtk_Handle_Box_Gtk_Widget_Void --
   ------------------------------------------

   procedure Marsh_Gtk_Handle_Box_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Handle_Box_Gtk_Widget_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Handle_Box := Gtk_Handle_Box (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Handle_Box_Gtk_Widget_Void;

   -----------------------
   -- On_Child_Attached --
   -----------------------

   procedure On_Child_Attached
      (Self  : not null access Gtk_Handle_Box_Record;
       Call  : Cb_Gtk_Handle_Box_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "child-attached" & ASCII.NUL, Call, After);
   end On_Child_Attached;

   -----------------------
   -- On_Child_Attached --
   -----------------------

   procedure On_Child_Attached
      (Self  : not null access Gtk_Handle_Box_Record;
       Call  : Cb_GObject_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "child-attached" & ASCII.NUL, Call, After, Slot);
   end On_Child_Attached;

   -----------------------
   -- On_Child_Detached --
   -----------------------

   procedure On_Child_Detached
      (Self  : not null access Gtk_Handle_Box_Record;
       Call  : Cb_Gtk_Handle_Box_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "child-detached" & ASCII.NUL, Call, After);
   end On_Child_Detached;

   -----------------------
   -- On_Child_Detached --
   -----------------------

   procedure On_Child_Detached
      (Self  : not null access Gtk_Handle_Box_Record;
       Call  : Cb_GObject_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "child-detached" & ASCII.NUL, Call, After, Slot);
   end On_Child_Detached;

end Gtk.Handle_Box;
