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
with Glib.Error;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

use type Glib.Error.GError;

package body Gsk.Renderer is

   package Type_Conversion_Gsk_Renderer is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gsk_Renderer_Record);
   pragma Unreferenced (Type_Conversion_Gsk_Renderer);

   ----------------------------------
   -- Gsk_Renderer_New_For_Surface --
   ----------------------------------

   function Gsk_Renderer_New_For_Surface
      (Surface : not null access Gdk.Surface.Gdk_Surface_Record'Class)
       return Gsk_Renderer
   is
      Self : constant Gsk_Renderer := new Gsk_Renderer_Record;
   begin
      Gsk.Renderer.Initialize_For_Surface (Self, Surface);
      return Self;
   end Gsk_Renderer_New_For_Surface;

   -------------------------
   -- Gtk_New_For_Surface --
   -------------------------

   procedure Gtk_New_For_Surface
      (Self    : out Gsk_Renderer;
       Surface : not null access Gdk.Surface.Gdk_Surface_Record'Class)
   is
   begin
      Self := new Gsk_Renderer_Record;
      Gsk.Renderer.Initialize_For_Surface (Self, Surface);
   end Gtk_New_For_Surface;

   ----------------------------
   -- Initialize_For_Surface --
   ----------------------------

   procedure Initialize_For_Surface
      (Self    : not null access Gsk_Renderer_Record'Class;
       Surface : not null access Gdk.Surface.Gdk_Surface_Record'Class)
   is
      function Internal (Surface : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_renderer_new_for_surface");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Surface)));
      end if;
   end Initialize_For_Surface;

   -----------------
   -- Get_Surface --
   -----------------

   function Get_Surface
      (Self : not null access Gsk_Renderer_Record) return Gdk.Gdk_Surface
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_renderer_get_surface");
      Stub_Gdk_Surface : Gdk.Surface.Gdk_Surface_Record;
   begin
      return Gdk.Gdk_Surface (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Surface));
   end Get_Surface;

   -----------------
   -- Is_Realized --
   -----------------

   function Is_Realized
      (Self : not null access Gsk_Renderer_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_renderer_is_realized");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Realized;

   -------------
   -- Realize --
   -------------

   function Realize
      (Self    : not null access Gsk_Renderer_Record;
       Surface : access Gdk.Surface.Gdk_Surface_Record'Class;
       Error   : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Surface   : System.Address;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_renderer_realize");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Surface)), Acc_Error'Access);
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end Realize;

   -------------------------
   -- Realize_For_Display --
   -------------------------

   function Realize_For_Display
      (Self    : not null access Gsk_Renderer_Record;
       Display : not null access Gdk.Display.Gdk_Display_Record'Class;
       Error   : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Display   : System.Address;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_renderer_realize_for_display");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Get_Object (Display), Acc_Error'Access);
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end Realize_For_Display;

   ------------
   -- Render --
   ------------

   procedure Render
      (Self   : not null access Gsk_Renderer_Record;
       Root   : Gsk.Render_Node.Gsk_Render_Node;
       Region : Cairo.Region.Cairo_Region)
   is
      procedure Internal
         (Self   : System.Address;
          Root   : System.Address;
          Region : Cairo.Region.Cairo_Region);
      pragma Import (C, Internal, "gsk_renderer_render");
   begin
      Internal (Get_Object (Self), Get_Object (Root), Region);
   end Render;

   --------------------
   -- Render_Texture --
   --------------------

   function Render_Texture
      (Self     : not null access Gsk_Renderer_Record;
       Root     : Gsk.Render_Node.Gsk_Render_Node;
       Viewport : access Graphene.Rect.Graphene_Rect_T)
       return Gdk.Texture.Gdk_Texture
   is
      function Internal
         (Self     : System.Address;
          Root     : System.Address;
          Viewport : access Graphene.Rect.Graphene_Rect_T)
          return System.Address;
      pragma Import (C, Internal, "gsk_renderer_render_texture");
      Stub_Gdk_Texture : Gdk.Texture.Gdk_Texture_Record;
   begin
      return Gdk.Texture.Gdk_Texture (Get_User_Data (Internal (Get_Object (Self), Get_Object (Root), Viewport), Stub_Gdk_Texture));
   end Render_Texture;

   ---------------
   -- Unrealize --
   ---------------

   procedure Unrealize (Self : not null access Gsk_Renderer_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gsk_renderer_unrealize");
   begin
      Internal (Get_Object (Self));
   end Unrealize;

end Gsk.Renderer;
