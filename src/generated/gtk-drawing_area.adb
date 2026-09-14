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
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Drawing_Area is

   procedure C_Gtk_Drawing_Area_Set_Draw_Func
      (Self      : System.Address;
       Draw_Func : System.Address;
       User_Data : System.Address;
       Destroy   : System.Address);
   pragma Import (C, C_Gtk_Drawing_Area_Set_Draw_Func, "gtk_drawing_area_set_draw_func");
   --  Setting a draw function is the main thing you want to do when using a
   --  drawing area.
   --  The draw function is called whenever GTK needs to draw the contents of
   --  the drawing area to the screen.
   --  The draw function will be called during the drawing stage of GTK. In
   --  the drawing stage it is not allowed to change properties of any GTK
   --  widgets or call any functions that would cause any properties to be
   --  changed. You should restrict yourself exclusively to drawing your
   --  contents in the draw function.
   --  If what you are drawing does change, call [methodGtk.Widget.queue_draw]
   --  on the drawing area. This will cause a redraw and will call Draw_Func
   --  again.
   --  @param Draw_Func callback that lets you draw the drawing area's
   --  contents
   --  @param User_Data user data passed to Draw_Func
   --  @param Destroy destroy notifier for User_Data

   function To_Gtk_Drawing_Area_Draw_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Drawing_Area_Draw_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Drawing_Area_Draw_Func, System.Address);

   procedure Internal_Gtk_Drawing_Area_Draw_Func
      (Drawing_Area : System.Address;
       Cr           : Cairo.Cairo_Context;
       Width        : Glib.Gint;
       Height       : Glib.Gint;
       User_Data    : System.Address);
   pragma Convention (C, Internal_Gtk_Drawing_Area_Draw_Func);
   --  @param Drawing_Area the `GtkDrawingArea` to redraw
   --  @param Cr the context to draw to
   --  @param Width the actual width of the contents. This value will be at
   --  least as wide as GtkDrawingArea:width.
   --  @param Height the actual height of the contents. This value will be at
   --  least as wide as GtkDrawingArea:height.
   --  @param User_Data user data

   -----------------------------------------
   -- Internal_Gtk_Drawing_Area_Draw_Func --
   -----------------------------------------

   procedure Internal_Gtk_Drawing_Area_Draw_Func
      (Drawing_Area : System.Address;
       Cr           : Cairo.Cairo_Context;
       Width        : Glib.Gint;
       Height       : Glib.Gint;
       User_Data    : System.Address)
   is
      Func                  : constant Gtk_Drawing_Area_Draw_Func := To_Gtk_Drawing_Area_Draw_Func (User_Data);
      Stub_Gtk_Drawing_Area : Gtk_Drawing_Area_Record;
   begin
      Func (Gtk.Drawing_Area.Gtk_Drawing_Area (Get_User_Data (Drawing_Area, Stub_Gtk_Drawing_Area)), Cr, Width, Height);
   end Internal_Gtk_Drawing_Area_Draw_Func;

   package Type_Conversion_Gtk_Drawing_Area is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Drawing_Area_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Drawing_Area);

   --------------------------
   -- Gtk_Drawing_Area_New --
   --------------------------

   function Gtk_Drawing_Area_New return Gtk_Drawing_Area is
      Self : constant Gtk_Drawing_Area := new Gtk_Drawing_Area_Record;
   begin
      Gtk.Drawing_Area.Initialize (Self);
      return Self;
   end Gtk_Drawing_Area_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Drawing_Area) is
   begin
      Self := new Gtk_Drawing_Area_Record;
      Gtk.Drawing_Area.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Drawing_Area_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_drawing_area_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ------------------------
   -- Get_Content_Height --
   ------------------------

   function Get_Content_Height
      (Self : not null access Gtk_Drawing_Area_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_drawing_area_get_content_height");
   begin
      return Internal (Get_Object (Self));
   end Get_Content_Height;

   -----------------------
   -- Get_Content_Width --
   -----------------------

   function Get_Content_Width
      (Self : not null access Gtk_Drawing_Area_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_drawing_area_get_content_width");
   begin
      return Internal (Get_Object (Self));
   end Get_Content_Width;

   ------------------------
   -- Set_Content_Height --
   ------------------------

   procedure Set_Content_Height
      (Self   : not null access Gtk_Drawing_Area_Record;
       Height : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Height : Glib.Gint);
      pragma Import (C, Internal, "gtk_drawing_area_set_content_height");
   begin
      Internal (Get_Object (Self), Height);
   end Set_Content_Height;

   -----------------------
   -- Set_Content_Width --
   -----------------------

   procedure Set_Content_Width
      (Self  : not null access Gtk_Drawing_Area_Record;
       Width : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Width : Glib.Gint);
      pragma Import (C, Internal, "gtk_drawing_area_set_content_width");
   begin
      Internal (Get_Object (Self), Width);
   end Set_Content_Width;

   -------------------
   -- Set_Draw_Func --
   -------------------

   procedure Set_Draw_Func
      (Self      : not null access Gtk_Drawing_Area_Record;
       Draw_Func : Gtk_Drawing_Area_Draw_Func)
   is
   begin
      if Draw_Func = null then
         C_Gtk_Drawing_Area_Set_Draw_Func (Get_Object (Self), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Drawing_Area_Set_Draw_Func (Get_Object (Self), Internal_Gtk_Drawing_Area_Draw_Func'Address, To_Address (Draw_Func), System.Null_Address);
      end if;
   end Set_Draw_Func;

   package body Set_Draw_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Drawing_Area_Draw_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Drawing_Area_Draw_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Drawing_Area_Draw_Func, System.Address);

      procedure Internal_Cb
         (Drawing_Area : System.Address;
          Cr           : Cairo.Cairo_Context;
          Width        : Glib.Gint;
          Height       : Glib.Gint;
          User_Data    : System.Address);
      pragma Convention (C, Internal_Cb);
      --  Whenever Drawing_Area needs to redraw, this function will be called.
      --  This function should exclusively redraw the contents of the drawing
      --  area and must not call any widget functions that cause changes.
      --  @param Drawing_Area the `GtkDrawingArea` to redraw
      --  @param Cr the context to draw to
      --  @param Width the actual width of the contents. This value will be at
      --  least as wide as GtkDrawingArea:width.
      --  @param Height the actual height of the contents. This value will be
      --  at least as wide as GtkDrawingArea:height.
      --  @param User_Data user data

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Drawing_Area : System.Address;
          Cr           : Cairo.Cairo_Context;
          Width        : Glib.Gint;
          Height       : Glib.Gint;
          User_Data    : System.Address)
      is
         D                     : constant Users.Internal_Data_Access := Users.Convert (User_Data);
         Stub_Gtk_Drawing_Area : Gtk.Drawing_Area.Gtk_Drawing_Area_Record;
      begin
         To_Gtk_Drawing_Area_Draw_Func (D.Func) (Gtk.Drawing_Area.Gtk_Drawing_Area (Get_User_Data (Drawing_Area, Stub_Gtk_Drawing_Area)), Cr, Width, Height, D.Data.all);
      end Internal_Cb;

      -------------------
      -- Set_Draw_Func --
      -------------------

      procedure Set_Draw_Func
         (Self      : not null access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
          Draw_Func : Gtk_Drawing_Area_Draw_Func;
          User_Data : User_Data_Type)
      is
         D : System.Address;
      begin
         if Draw_Func = null then
            C_Gtk_Drawing_Area_Set_Draw_Func (Get_Object (Self), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Draw_Func), User_Data);
            C_Gtk_Drawing_Area_Set_Draw_Func (Get_Object (Self), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Draw_Func;

   end Set_Draw_Func_User_Data;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Drawing_Area_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority)
   is
      procedure Internal
         (Self     : System.Address;
          Message  : Gtkada.Types.Chars_Ptr;
          Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);
      pragma Import (C, Internal, "gtk_accessible_announce");
      Tmp_Message : Gtkada.Types.Chars_Ptr := New_String (Message);
   begin
      Internal (Get_Object (Self), Tmp_Message, Priority);
      Free (Tmp_Message);
   end Announce;

   -----------------------
   -- Get_Accessible_Id --
   -----------------------

   function Get_Accessible_Id
      (Self : not null access Gtk_Drawing_Area_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_id");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end Get_Accessible_Id;

   ---------------------------
   -- Get_Accessible_Parent --
   ---------------------------

   function Get_Accessible_Parent
      (Self : not null access Gtk_Drawing_Area_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_parent");
   begin
      return Internal (Get_Object (Self));
   end Get_Accessible_Parent;

   -------------------------
   -- Get_Accessible_Role --
   -------------------------

   function Get_Accessible_Role
      (Self : not null access Gtk_Drawing_Area_Record)
       return Gtk.Accessible.Gtk_Accessible_Role
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible_Role;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_role");
   begin
      return Internal (Get_Object (Self));
   end Get_Accessible_Role;

   --------------------
   -- Get_At_Context --
   --------------------

   function Get_At_Context
      (Self : not null access Gtk_Drawing_Area_Record)
       return Gtk.Atcontext.Gtk_Atcontext
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_accessible_get_at_context");
      Stub_Gtk_Atcontext : Gtk.Atcontext.Gtk_Atcontext_Record;
   begin
      return Gtk.Atcontext.Gtk_Atcontext (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Atcontext));
   end Get_At_Context;

   ----------------
   -- Get_Bounds --
   ----------------

   function Get_Bounds
      (Self   : not null access Gtk_Drawing_Area_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean
   is
      function Internal
         (Self       : System.Address;
          Acc_X      : access Glib.Gint;
          Acc_Y      : access Glib.Gint;
          Acc_Width  : access Glib.Gint;
          Acc_Height : access Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accessible_get_bounds");
      Acc_X      : aliased Glib.Gint;
      Acc_Y      : aliased Glib.Gint;
      Acc_Width  : aliased Glib.Gint;
      Acc_Height : aliased Glib.Gint;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_X'Access, Acc_Y'Access, Acc_Width'Access, Acc_Height'Access);
      X := Acc_X;
      Y := Acc_Y;
      Width := Acc_Width;
      Height := Acc_Height;
      return Tmp_Return /= 0;
   end Get_Bounds;

   --------------------------------
   -- Get_First_Accessible_Child --
   --------------------------------

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Drawing_Area_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_first_accessible_child");
   begin
      return Internal (Get_Object (Self));
   end Get_First_Accessible_Child;

   ---------------------------------
   -- Get_Next_Accessible_Sibling --
   ---------------------------------

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Drawing_Area_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_next_accessible_sibling");
   begin
      return Internal (Get_Object (Self));
   end Get_Next_Accessible_Sibling;

   ------------------------
   -- Get_Platform_State --
   ------------------------

   function Get_Platform_State
      (Self  : not null access Gtk_Drawing_Area_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean
   is
      function Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accessible_get_platform_state");
   begin
      return Internal (Get_Object (Self), State) /= 0;
   end Get_Platform_State;

   --------------------
   -- Reset_Property --
   --------------------

   procedure Reset_Property
      (Self     : not null access Gtk_Drawing_Area_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property)
   is
      procedure Internal
         (Self     : System.Address;
          Property : Gtk.Accessible.Gtk_Accessible_Property);
      pragma Import (C, Internal, "gtk_accessible_reset_property");
   begin
      Internal (Get_Object (Self), Property);
   end Reset_Property;

   --------------------
   -- Reset_Relation --
   --------------------

   procedure Reset_Relation
      (Self     : not null access Gtk_Drawing_Area_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation)
   is
      procedure Internal
         (Self     : System.Address;
          Relation : Gtk.Accessible.Gtk_Accessible_Relation);
      pragma Import (C, Internal, "gtk_accessible_reset_relation");
   begin
      Internal (Get_Object (Self), Relation);
   end Reset_Relation;

   -----------------
   -- Reset_State --
   -----------------

   procedure Reset_State
      (Self  : not null access Gtk_Drawing_Area_Record;
       State : Gtk.Accessible.Gtk_Accessible_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_State);
      pragma Import (C, Internal, "gtk_accessible_reset_state");
   begin
      Internal (Get_Object (Self), State);
   end Reset_State;

   ---------------------------
   -- Set_Accessible_Parent --
   ---------------------------

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Drawing_Area_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible)
   is
      procedure Internal
         (Self         : System.Address;
          Parent       : Gtk.Accessible.Gtk_Accessible;
          Next_Sibling : Gtk.Accessible.Gtk_Accessible);
      pragma Import (C, Internal, "gtk_accessible_set_accessible_parent");
   begin
      Internal (Get_Object (Self), Parent, Next_Sibling);
   end Set_Accessible_Parent;

   ------------------------------------
   -- Update_Next_Accessible_Sibling --
   ------------------------------------

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Drawing_Area_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible)
   is
      procedure Internal
         (Self        : System.Address;
          New_Sibling : Gtk.Accessible.Gtk_Accessible);
      pragma Import (C, Internal, "gtk_accessible_update_next_accessible_sibling");
   begin
      Internal (Get_Object (Self), New_Sibling);
   end Update_Next_Accessible_Sibling;

   ---------------------------
   -- Update_Platform_State --
   ---------------------------

   procedure Update_Platform_State
      (Self  : not null access Gtk_Drawing_Area_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State);
      pragma Import (C, Internal, "gtk_accessible_update_platform_state");
   begin
      Internal (Get_Object (Self), State);
   end Update_Platform_State;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Drawing_Area_Gint_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Drawing_Area_Gint_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gint_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gint_Gint_Void);

   procedure Connect
      (Object  : access Gtk_Drawing_Area_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Drawing_Area_Gint_Gint_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Drawing_Area_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gint_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gint_Gint_Void);

   procedure Marsh_Gtk_Drawing_Area_Gint_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Drawing_Area_Gint_Gint_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Drawing_Area_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Drawing_Area_Gint_Gint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Drawing_Area_Gint_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Drawing_Area_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gint_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ----------------------------------
   -- Marsh_GObject_Gint_Gint_Void --
   ----------------------------------

   procedure Marsh_GObject_Gint_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gint_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gint (Params, 1), Unchecked_To_Gint (Params, 2));
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gint_Gint_Void;

   -------------------------------------------
   -- Marsh_Gtk_Drawing_Area_Gint_Gint_Void --
   -------------------------------------------

   procedure Marsh_Gtk_Drawing_Area_Gint_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Drawing_Area_Gint_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Drawing_Area := Gtk_Drawing_Area (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gint (Params, 1), Unchecked_To_Gint (Params, 2));
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Drawing_Area_Gint_Gint_Void;

   ---------------
   -- On_Resize --
   ---------------

   procedure On_Resize
      (Self  : not null access Gtk_Drawing_Area_Record;
       Call  : Cb_Gtk_Drawing_Area_Gint_Gint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "resize" & ASCII.NUL, Call, After);
   end On_Resize;

   ---------------
   -- On_Resize --
   ---------------

   procedure On_Resize
      (Self  : not null access Gtk_Drawing_Area_Record;
       Call  : Cb_GObject_Gint_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "resize" & ASCII.NUL, Call, After, Slot);
   end On_Resize;

end Gtk.Drawing_Area;
