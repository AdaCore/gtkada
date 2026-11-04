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
with Glib.Object;           use Glib.Object;
with Interfaces.C.Pointers;

package body Gsk.Text_Node is

   -----------------------
   -- Gsk_Text_Node_New --
   -----------------------

   function Gsk_Text_Node_New
      (Font   : not null access Pango.Font.Pango_Font_Record'Class;
       Glyphs : not null access Pango.Glyph_String.Pango_Glyph_String;
       Color  : not null access Gdk.RGBA.Gdk_RGBA;
       Offset : not null access Graphene.Point.Graphene_Point_T)
       return Gsk_Text_Node
   is
      Self : constant Gsk_Text_Node := new Gsk_Text_Node_Record;
   begin
      Gsk.Text_Node.Initialize (Self, Font, Glyphs, Color, Offset);
      return Self;
   end Gsk_Text_Node_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self   : out Gsk_Text_Node;
       Font   : not null access Pango.Font.Pango_Font_Record'Class;
       Glyphs : not null access Pango.Glyph_String.Pango_Glyph_String;
       Color  : not null access Gdk.RGBA.Gdk_RGBA;
       Offset : not null access Graphene.Point.Graphene_Point_T)
   is
   begin
      Self := new Gsk_Text_Node_Record;
      Gsk.Text_Node.Initialize (Self, Font, Glyphs, Color, Offset);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self   : not null access Gsk_Text_Node_Record'Class;
       Font   : not null access Pango.Font.Pango_Font_Record'Class;
       Glyphs : not null access Pango.Glyph_String.Pango_Glyph_String;
       Color  : not null access Gdk.RGBA.Gdk_RGBA;
       Offset : not null access Graphene.Point.Graphene_Point_T)
   is
      function Internal
         (Font   : System.Address;
          Glyphs : access Pango.Glyph_String.Pango_Glyph_String;
          Color  : access Gdk.RGBA.Gdk_RGBA;
          Offset : access Graphene.Point.Graphene_Point_T)
          return System.Address;
      pragma Import (C, Internal, "gsk_text_node_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Font), Glyphs, Color, Offset));
      end if;
   end Initialize;

   ---------------
   -- Get_Color --
   ---------------

   function Get_Color
      (Self : Gsk_Text_Node) return access constant Gdk.RGBA.Gdk_RGBA
   is
      function Internal
         (Self : System.Address) return access constant Gdk.RGBA.Gdk_RGBA;
      pragma Import (C, Internal, "gsk_text_node_get_color");
   begin
      return Internal (Get_Object (Self));
   end Get_Color;

   --------------
   -- Get_Font --
   --------------

   function Get_Font (Self : Gsk_Text_Node) return Pango.Font.Pango_Font is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_text_node_get_font");
      Stub_Pango_Font : Pango.Font.Pango_Font_Record;
   begin
      return Pango.Font.Pango_Font (Get_User_Data (Internal (Get_Object (Self)), Stub_Pango_Font));
   end Get_Font;

   ----------------
   -- Get_Glyphs --
   ----------------

   function Get_Glyphs
      (Self : Gsk_Text_Node)
       return Pango.Glyph_String.Pango_Glyph_Info_Array
   is
      use Interfaces.C;
      type Aliased_Array is array
        (Positive range <>) of aliased Pango.Glyph_String.Pango_Glyph_Info
        with Convention => C;
      package P is new Interfaces.C.Pointers
        (Positive, Pango.Glyph_String.Pango_Glyph_Info,
         Aliased_Array, (0, (0,0,0), (Is_Cluster_Start => 0)));

      function Internal
         (Self     : System.Address;
          N_Glyphs : access Guint)
          return P.Pointer;
      pragma Import (C, Internal, "gsk_text_node_get_glyphs");
      Acc_N_Glyphs : aliased Guint;
      Tmp_Return   : P.Pointer;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_N_Glyphs'Access);
      declare
         Result : constant Pango.Glyph_String.Pango_Glyph_Info_Array :=
           Pango.Glyph_String.Pango_Glyph_Info_Array
             (P.Value (Tmp_Return, ptrdiff_t (Acc_N_Glyphs)));
      begin
         return Result;
      end;
   end Get_Glyphs;

   --------------------
   -- Get_Num_Glyphs --
   --------------------

   function Get_Num_Glyphs (Self : Gsk_Text_Node) return Guint is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gsk_text_node_get_num_glyphs");
   begin
      return Internal (Get_Object (Self));
   end Get_Num_Glyphs;

   ----------------
   -- Get_Offset --
   ----------------

   function Get_Offset
      (Self : Gsk_Text_Node)
       return access constant Graphene.Point.Graphene_Point_T
   is
      function Internal
         (Self : System.Address)
          return access constant Graphene.Point.Graphene_Point_T;
      pragma Import (C, Internal, "gsk_text_node_get_offset");
   begin
      return Internal (Get_Object (Self));
   end Get_Offset;

   ----------------------
   -- Has_Color_Glyphs --
   ----------------------

   function Has_Color_Glyphs (Self : Gsk_Text_Node) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_text_node_has_color_glyphs");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Has_Color_Glyphs;

   ------------
   -- Create --
   ------------

   overriding function Create
      (Object : not null access System.Address) return Gsk_Text_Node_Record
   is
      pragma Unreferenced (Object);
      Result : Gsk_Text_Node_Record;
   begin
      return Result;
   end Create;

end Gsk.Text_Node;
