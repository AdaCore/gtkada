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
with Gtkada.Bindings; use Gtkada.Bindings;
with Gtkada.Types;    use Gtkada.Types;

package body Gtk.Gradient is

   function From_Object_Free
     (B : access Gtk_Gradient'Class) return Gtk_Gradient
   is
      Result : constant Gtk_Gradient := Gtk_Gradient (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gtk_Gradient is
      S : Gtk_Gradient;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   -----------------------------
   -- Gtk_Gradient_New_Linear --
   -----------------------------

   function Gtk_Gradient_New_Linear
      (X0 : Gdouble;
       Y0 : Gdouble;
       X1 : Gdouble;
       Y1 : Gdouble) return Gtk_Gradient
   is
      function Internal
         (X0 : Gdouble;
          Y0 : Gdouble;
          X1 : Gdouble;
          Y1 : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_gradient_new_linear");
      Self : Gtk_Gradient;
   begin
      Self.Set_Object (Internal (X0, Y0, X1, Y1));
      return Self;
   end Gtk_Gradient_New_Linear;

   -----------------------------
   -- Gtk_Gradient_New_Radial --
   -----------------------------

   function Gtk_Gradient_New_Radial
      (X0      : Gdouble;
       Y0      : Gdouble;
       Radius0 : Gdouble;
       X1      : Gdouble;
       Y1      : Gdouble;
       Radius1 : Gdouble) return Gtk_Gradient
   is
      function Internal
         (X0      : Gdouble;
          Y0      : Gdouble;
          Radius0 : Gdouble;
          X1      : Gdouble;
          Y1      : Gdouble;
          Radius1 : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_gradient_new_radial");
      Self : Gtk_Gradient;
   begin
      Self.Set_Object (Internal (X0, Y0, Radius0, X1, Y1, Radius1));
      return Self;
   end Gtk_Gradient_New_Radial;

   --------------------
   -- Gtk_New_Linear --
   --------------------

   procedure Gtk_New_Linear
      (Self : out Gtk_Gradient;
       X0   : Gdouble;
       Y0   : Gdouble;
       X1   : Gdouble;
       Y1   : Gdouble)
   is
      function Internal
         (X0 : Gdouble;
          Y0 : Gdouble;
          X1 : Gdouble;
          Y1 : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_gradient_new_linear");
   begin
      Self.Set_Object (Internal (X0, Y0, X1, Y1));
   end Gtk_New_Linear;

   --------------------
   -- Gtk_New_Radial --
   --------------------

   procedure Gtk_New_Radial
      (Self    : out Gtk_Gradient;
       X0      : Gdouble;
       Y0      : Gdouble;
       Radius0 : Gdouble;
       X1      : Gdouble;
       Y1      : Gdouble;
       Radius1 : Gdouble)
   is
      function Internal
         (X0      : Gdouble;
          Y0      : Gdouble;
          Radius0 : Gdouble;
          X1      : Gdouble;
          Y1      : Gdouble;
          Radius1 : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_gradient_new_radial");
   begin
      Self.Set_Object (Internal (X0, Y0, Radius0, X1, Y1, Radius1));
   end Gtk_New_Radial;

   --------------------
   -- Add_Color_Stop --
   --------------------

   procedure Add_Color_Stop
      (Self   : Gtk_Gradient;
       Offset : Gdouble;
       Color  : Gtk.Symbolic_Color.Gtk_Symbolic_Color)
   is
      procedure Internal
         (Self   : System.Address;
          Offset : Gdouble;
          Color  : System.Address);
      pragma Import (C, Internal, "gtk_gradient_add_color_stop");
   begin
      Internal (Get_Object (Self), Offset, Get_Object (Color));
   end Add_Color_Stop;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gtk_Gradient) return Gtk_Gradient is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_gradient_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   -------------
   -- Resolve --
   -------------

   function Resolve
      (Self              : Gtk_Gradient;
       Props             : not null access Gtk.Style_Properties.Gtk_Style_Properties_Record'Class;
       Resolved_Gradient : access Cairo.Cairo_Pattern) return Boolean
   is
      function Internal
         (Self                  : System.Address;
          Props                 : System.Address;
          Acc_Resolved_Gradient : access Cairo.Cairo_Pattern)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_gradient_resolve");
      Acc_Resolved_Gradient : aliased Cairo.Cairo_Pattern;
      Tmp_Return            : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Get_Object (Props), Acc_Resolved_Gradient'Access);
      Resolved_Gradient.all := Acc_Resolved_Gradient;
      return Tmp_Return /= 0;
   end Resolve;

   -------------------------
   -- Resolve_For_Context --
   -------------------------

   function Resolve_For_Context
      (Self    : Gtk_Gradient;
       Context : not null access Gtk.Style_Context.Gtk_Style_Context_Record'Class)
       return Cairo.Cairo_Pattern
   is
      function Internal
         (Self    : System.Address;
          Context : System.Address) return Cairo.Cairo_Pattern;
      pragma Import (C, Internal, "gtk_gradient_resolve_for_context");
   begin
      return Internal (Get_Object (Self), Get_Object (Context));
   end Resolve_For_Context;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : Gtk_Gradient) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_gradient_to_string");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end To_String;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gtk_Gradient) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_gradient_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

end Gtk.Gradient;
