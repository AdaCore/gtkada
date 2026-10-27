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
with Glib.Object; use Glib.Object;

package body Gsk.Texture_Scale_Node is

   --------------------------------
   -- Gsk_Texture_Scale_Node_New --
   --------------------------------

   function Gsk_Texture_Scale_Node_New
      (Texture : not null access Gdk.Texture.Gdk_Texture_Record'Class;
       Bounds  : not null access Graphene.Rect.Graphene_Rect_T;
       Filter  : Gsk.Enums.Gsk_Scaling_Filter) return Gsk_Texture_Scale_Node
   is
      Self : constant Gsk_Texture_Scale_Node := new Gsk_Texture_Scale_Node_Record;
   begin
      Gsk.Texture_Scale_Node.Initialize (Self, Texture, Bounds, Filter);
      return Self;
   end Gsk_Texture_Scale_Node_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self    : out Gsk_Texture_Scale_Node;
       Texture : not null access Gdk.Texture.Gdk_Texture_Record'Class;
       Bounds  : not null access Graphene.Rect.Graphene_Rect_T;
       Filter  : Gsk.Enums.Gsk_Scaling_Filter)
   is
   begin
      Self := new Gsk_Texture_Scale_Node_Record;
      Gsk.Texture_Scale_Node.Initialize (Self, Texture, Bounds, Filter);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self    : not null access Gsk_Texture_Scale_Node_Record'Class;
       Texture : not null access Gdk.Texture.Gdk_Texture_Record'Class;
       Bounds  : not null access Graphene.Rect.Graphene_Rect_T;
       Filter  : Gsk.Enums.Gsk_Scaling_Filter)
   is
      function Internal
         (Texture : System.Address;
          Bounds  : access Graphene.Rect.Graphene_Rect_T;
          Filter  : Gsk.Enums.Gsk_Scaling_Filter) return System.Address;
      pragma Import (C, Internal, "gsk_texture_scale_node_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Texture), Bounds, Filter));
      end if;
   end Initialize;

   ----------------
   -- Get_Filter --
   ----------------

   function Get_Filter
      (Self : Gsk_Texture_Scale_Node) return Gsk.Enums.Gsk_Scaling_Filter
   is
      function Internal
         (Self : System.Address) return Gsk.Enums.Gsk_Scaling_Filter;
      pragma Import (C, Internal, "gsk_texture_scale_node_get_filter");
   begin
      return Internal (Get_Object (Self));
   end Get_Filter;

   -----------------
   -- Get_Texture --
   -----------------

   function Get_Texture
      (Self : Gsk_Texture_Scale_Node) return Gdk.Texture.Gdk_Texture
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_texture_scale_node_get_texture");
      Stub_Gdk_Texture : Gdk.Texture.Gdk_Texture_Record;
   begin
      return Gdk.Texture.Gdk_Texture (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Texture));
   end Get_Texture;

   ------------
   -- Create --
   ------------

   overriding function Create
      (Object : not null access System.Address)
       return Gsk_Texture_Scale_Node_Record
   is
      pragma Unreferenced (Object);
      Result : Gsk_Texture_Scale_Node_Record;
   begin
      return Result;
   end Create;

end Gsk.Texture_Scale_Node;
