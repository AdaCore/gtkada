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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Snapshot is

   package Type_Conversion_Gtk_Snapshot is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Snapshot_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Snapshot);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Snapshot) is
   begin
      Self := new Gtk_Snapshot_Record;
      Gtk.Snapshot.Initialize (Self);
   end Gtk_New;

   ----------------------
   -- Gtk_Snapshot_New --
   ----------------------

   function Gtk_Snapshot_New return Gtk_Snapshot is
      Self : constant Gtk_Snapshot := new Gtk_Snapshot_Record;
   begin
      Gtk.Snapshot.Initialize (Self);
      return Self;
   end Gtk_Snapshot_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Snapshot_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_snapshot_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ------------------
   -- Append_Cairo --
   ------------------

   function Append_Cairo
      (Self   : not null access Gtk_Snapshot_Record;
       Bounds : in out graphene_rect_t) return Cairo.Cairo_Context
   is
      function Internal
         (Self       : System.Address;
          Acc_Bounds : access graphene_rect_t) return Cairo.Cairo_Context;
      pragma Import (C, Internal, "gtk_snapshot_append_cairo");
      Acc_Bounds : aliased graphene_rect_t := Bounds;
      Tmp_Return : Cairo.Cairo_Context;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Bounds'Access);
      Bounds := Acc_Bounds;
      return Tmp_Return;
   end Append_Cairo;

   ------------------
   -- Append_Color --
   ------------------

   procedure Append_Color
      (Self   : not null access Gtk_Snapshot_Record;
       Color  : Gdk.RGBA.Gdk_RGBA;
       Bounds : in out graphene_rect_t)
   is
      procedure Internal
         (Self   : System.Address;
          Color  : Gdk.RGBA.Gdk_RGBA;
          Bounds : in out graphene_rect_t);
      pragma Import (C, Internal, "gtk_snapshot_append_color");
   begin
      Internal (Get_Object (Self), Color, Bounds);
   end Append_Color;

   -------------------
   -- Append_Layout --
   -------------------

   procedure Append_Layout
      (Self   : not null access Gtk_Snapshot_Record;
       Layout : not null access Pango.Layout.Pango_Layout_Record'Class;
       Color  : Gdk.RGBA.Gdk_RGBA)
   is
      procedure Internal
         (Self   : System.Address;
          Layout : System.Address;
          Color  : Gdk.RGBA.Gdk_RGBA);
      pragma Import (C, Internal, "gtk_snapshot_append_layout");
   begin
      Internal (Get_Object (Self), Get_Object (Layout), Color);
   end Append_Layout;

   ------------------
   -- Append_Paste --
   ------------------

   procedure Append_Paste
      (Self   : not null access Gtk_Snapshot_Record;
       Bounds : in out graphene_rect_t;
       Nth    : Gsize)
   is
      procedure Internal
         (Self   : System.Address;
          Bounds : in out graphene_rect_t;
          Nth    : Gsize);
      pragma Import (C, Internal, "gtk_snapshot_append_paste");
   begin
      Internal (Get_Object (Self), Bounds, Nth);
   end Append_Paste;

   --------------------
   -- Append_Texture --
   --------------------

   procedure Append_Texture
      (Self    : not null access Gtk_Snapshot_Record;
       Texture : not null access Gdk.Texture.Gdk_Texture_Record'Class;
       Bounds  : in out graphene_rect_t)
   is
      procedure Internal
         (Self    : System.Address;
          Texture : System.Address;
          Bounds  : in out graphene_rect_t);
      pragma Import (C, Internal, "gtk_snapshot_append_texture");
   begin
      Internal (Get_Object (Self), Get_Object (Texture), Bounds);
   end Append_Texture;

   ---------------------------
   -- Gl_Shader_Pop_Texture --
   ---------------------------

   procedure Gl_Shader_Pop_Texture
      (Self : not null access Gtk_Snapshot_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_snapshot_gl_shader_pop_texture");
   begin
      Internal (Get_Object (Self));
   end Gl_Shader_Pop_Texture;

   -----------------
   -- Perspective --
   -----------------

   procedure Perspective
      (Self  : not null access Gtk_Snapshot_Record;
       Depth : Interfaces.C.C_float)
   is
      procedure Internal
         (Self  : System.Address;
          Depth : Interfaces.C.C_float);
      pragma Import (C, Internal, "gtk_snapshot_perspective");
   begin
      Internal (Get_Object (Self), Depth);
   end Perspective;

   ---------
   -- Pop --
   ---------

   procedure Pop (Self : not null access Gtk_Snapshot_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_snapshot_pop");
   begin
      Internal (Get_Object (Self));
   end Pop;

   ---------------
   -- Push_Blur --
   ---------------

   procedure Push_Blur
      (Self   : not null access Gtk_Snapshot_Record;
       Radius : Gdouble)
   is
      procedure Internal (Self : System.Address; Radius : Gdouble);
      pragma Import (C, Internal, "gtk_snapshot_push_blur");
   begin
      Internal (Get_Object (Self), Radius);
   end Push_Blur;

   ---------------
   -- Push_Clip --
   ---------------

   procedure Push_Clip
      (Self   : not null access Gtk_Snapshot_Record;
       Bounds : in out graphene_rect_t)
   is
      procedure Internal
         (Self   : System.Address;
          Bounds : in out graphene_rect_t);
      pragma Import (C, Internal, "gtk_snapshot_push_clip");
   begin
      Internal (Get_Object (Self), Bounds);
   end Push_Clip;

   ---------------
   -- Push_Copy --
   ---------------

   procedure Push_Copy (Self : not null access Gtk_Snapshot_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_snapshot_push_copy");
   begin
      Internal (Get_Object (Self));
   end Push_Copy;

   ---------------------
   -- Push_Cross_Fade --
   ---------------------

   procedure Push_Cross_Fade
      (Self     : not null access Gtk_Snapshot_Record;
       Progress : Gdouble)
   is
      procedure Internal (Self : System.Address; Progress : Gdouble);
      pragma Import (C, Internal, "gtk_snapshot_push_cross_fade");
   begin
      Internal (Get_Object (Self), Progress);
   end Push_Cross_Fade;

   ------------------
   -- Push_Opacity --
   ------------------

   procedure Push_Opacity
      (Self    : not null access Gtk_Snapshot_Record;
       Opacity : Gdouble)
   is
      procedure Internal (Self : System.Address; Opacity : Gdouble);
      pragma Import (C, Internal, "gtk_snapshot_push_opacity");
   begin
      Internal (Get_Object (Self), Opacity);
   end Push_Opacity;

   -----------------
   -- Push_Repeat --
   -----------------

   procedure Push_Repeat
      (Self         : not null access Gtk_Snapshot_Record;
       Bounds       : in out graphene_rect_t;
       Child_Bounds : in out graphene_rect_t)
   is
      procedure Internal
         (Self         : System.Address;
          Bounds       : in out graphene_rect_t;
          Child_Bounds : in out graphene_rect_t);
      pragma Import (C, Internal, "gtk_snapshot_push_repeat");
   begin
      Internal (Get_Object (Self), Bounds, Child_Bounds);
   end Push_Repeat;

   -------------
   -- Restore --
   -------------

   procedure Restore (Self : not null access Gtk_Snapshot_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_snapshot_restore");
   begin
      Internal (Get_Object (Self));
   end Restore;

   ------------
   -- Rotate --
   ------------

   procedure Rotate
      (Self  : not null access Gtk_Snapshot_Record;
       Angle : Interfaces.C.C_float)
   is
      procedure Internal
         (Self  : System.Address;
          Angle : Interfaces.C.C_float);
      pragma Import (C, Internal, "gtk_snapshot_rotate");
   begin
      Internal (Get_Object (Self), Angle);
   end Rotate;

   ----------
   -- Save --
   ----------

   procedure Save (Self : not null access Gtk_Snapshot_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_snapshot_save");
   begin
      Internal (Get_Object (Self));
   end Save;

   -----------
   -- Scale --
   -----------

   procedure Scale
      (Self     : not null access Gtk_Snapshot_Record;
       Factor_X : Interfaces.C.C_float;
       Factor_Y : Interfaces.C.C_float)
   is
      procedure Internal
         (Self     : System.Address;
          Factor_X : Interfaces.C.C_float;
          Factor_Y : Interfaces.C.C_float);
      pragma Import (C, Internal, "gtk_snapshot_scale");
   begin
      Internal (Get_Object (Self), Factor_X, Factor_Y);
   end Scale;

   --------------
   -- Scale_3D --
   --------------

   procedure Scale_3D
      (Self     : not null access Gtk_Snapshot_Record;
       Factor_X : Interfaces.C.C_float;
       Factor_Y : Interfaces.C.C_float;
       Factor_Z : Interfaces.C.C_float)
   is
      procedure Internal
         (Self     : System.Address;
          Factor_X : Interfaces.C.C_float;
          Factor_Y : Interfaces.C.C_float;
          Factor_Z : Interfaces.C.C_float);
      pragma Import (C, Internal, "gtk_snapshot_scale_3d");
   begin
      Internal (Get_Object (Self), Factor_X, Factor_Y, Factor_Z);
   end Scale_3D;

   ------------------
   -- To_Paintable --
   ------------------

   function To_Paintable
      (Self : not null access Gtk_Snapshot_Record;
       Size : in out graphene_size_t) return Gdk.Paintable.Gdk_Paintable
   is
      function Internal
         (Self     : System.Address;
          Acc_Size : access graphene_size_t)
          return Gdk.Paintable.Gdk_Paintable;
      pragma Import (C, Internal, "gtk_snapshot_to_paintable");
      Acc_Size   : aliased graphene_size_t := Size;
      Tmp_Return : Gdk.Paintable.Gdk_Paintable;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Size'Access);
      Size := Acc_Size;
      return Tmp_Return;
   end To_Paintable;

   ---------------
   -- Translate --
   ---------------

   procedure Translate
      (Self  : not null access Gtk_Snapshot_Record;
       Point : in out graphene_point_t)
   is
      procedure Internal
         (Self  : System.Address;
          Point : in out graphene_point_t);
      pragma Import (C, Internal, "gtk_snapshot_translate");
   begin
      Internal (Get_Object (Self), Point);
   end Translate;

end Gtk.Snapshot;
