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
pragma Warnings(Off);  --  might be unused
with Gtkada.Types; use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Icon_Set is

   function From_Object_Free
     (B : access Gtk_Icon_Set'Class) return Gtk_Icon_Set
   is
      Result : constant Gtk_Icon_Set := Gtk_Icon_Set (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gtk_Icon_Set is
      S : Gtk_Icon_Set;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   function Lookup_Icon_Set
     (Style    : access Gtk.Style_Context.Gtk_Style_Context_Record'Class;
      Stock_Id : String)
   return Gtk_Icon_Set
   is
      function Internal
        (Style    : System.Address;
         Stock_Id : String)
      return System.Address;
      pragma Import (C, Internal, "gtk_style_context_lookup_icon_set");
   begin
      return From_Object (Internal (Get_Object (Style), Stock_Id & ASCII.NUL));
   end Lookup_Icon_Set;

   ----------------------
   -- Gtk_Icon_Set_New --
   ----------------------

   function Gtk_Icon_Set_New return Gtk_Icon_Set is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_icon_set_new");
      Self : Gtk_Icon_Set;
   begin
      Self.Set_Object (Internal);
      return Self;
   end Gtk_Icon_Set_New;

   ----------------------------------
   -- Gtk_Icon_Set_New_From_Pixbuf --
   ----------------------------------

   function Gtk_Icon_Set_New_From_Pixbuf
      (Pixbuf : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
       return Gtk_Icon_Set
   is
      function Internal (Pixbuf : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_icon_set_new_from_pixbuf");
      Self : Gtk_Icon_Set;
   begin
      Self.Set_Object (Internal (Get_Object (Pixbuf)));
      return Self;
   end Gtk_Icon_Set_New_From_Pixbuf;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Icon_Set) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_icon_set_new");
   begin
      Self.Set_Object (Internal);
   end Gtk_New;

   -------------------------
   -- Gtk_New_From_Pixbuf --
   -------------------------

   procedure Gtk_New_From_Pixbuf
      (Self   : out Gtk_Icon_Set;
       Pixbuf : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      function Internal (Pixbuf : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_icon_set_new_from_pixbuf");
   begin
      Self.Set_Object (Internal (Get_Object (Pixbuf)));
   end Gtk_New_From_Pixbuf;

   ----------------
   -- Add_Source --
   ----------------

   procedure Add_Source
      (Self   : Gtk_Icon_Set;
       Source : Gtk.Icon_Source.Gtk_Icon_Source)
   is
      procedure Internal (Self : System.Address; Source : System.Address);
      pragma Import (C, Internal, "gtk_icon_set_add_source");
   begin
      Internal (Get_Object (Self), Get_Object (Source));
   end Add_Source;

   ----------
   -- Copy --
   ----------

   function Copy (Self : Gtk_Icon_Set) return Gtk_Icon_Set is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_icon_set_copy");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Copy;

   ---------------
   -- Get_Sizes --
   ---------------

   function Get_Sizes (Self : Gtk_Icon_Set) return Gint_Array is
      type gint_array_bounded is array (Natural) of Gint;

      procedure Internal
         (Self        : System.Address;
          Sizes       : System.Address;
          Acc_N_Sizes : access Gint);
      pragma Import (C, Internal, "gtk_icon_set_get_sizes");

      procedure g_free (s : access gint_array_bounded);
      pragma Import (C, g_free, "g_free");

      Sizes : aliased access gint_array_bounded;
      Size : aliased Gint;
   begin
      Internal (Self.Get_Object, Sizes'Address, Size'Access);

      declare
         Result : Gint_Array (1 .. Integer (Size));
      begin
         for R in 0 .. Integer (Size) - 1 loop
             Result (R + 1) := Sizes (R);
         end loop;
         g_free (Sizes);

         return Result;
      end;
   end Get_Sizes;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gtk_Icon_Set) return Gtk_Icon_Set is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_icon_set_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   -----------------
   -- Render_Icon --
   -----------------

   function Render_Icon
      (Self      : Gtk_Icon_Set;
       Style     : access Gtk.Style.Gtk_Style_Record'Class;
       Direction : Gtk.Enums.Gtk_Text_Direction;
       State     : Gtk.Enums.Gtk_State_Type;
       Size      : Gtk.Enums.Gtk_Icon_Size;
       Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
       Detail    : UTF8_String := "") return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
         (Self      : System.Address;
          Style     : System.Address;
          Direction : Gtk.Enums.Gtk_Text_Direction;
          State     : Gtk.Enums.Gtk_State_Type;
          Size      : Gtk.Enums.Gtk_Icon_Size;
          Widget    : System.Address;
          Detail    : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_icon_set_render_icon");
      Tmp_Detail      : Gtkada.Types.Chars_Ptr;
      Stub_Gdk_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf_Record;
      Tmp_Return      : System.Address;
   begin
      if Detail = "" then
         Tmp_Detail := Gtkada.Types.Null_Ptr;
      else
         Tmp_Detail := New_String (Detail);
      end if;
      Tmp_Return := Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Style)), Direction, State, Size, Get_Object_Or_Null (GObject (Widget)), Tmp_Detail);
      Free (Tmp_Detail);
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Tmp_Return, Stub_Gdk_Pixbuf));
   end Render_Icon;

   ------------------------
   -- Render_Icon_Pixbuf --
   ------------------------

   function Render_Icon_Pixbuf
      (Self    : Gtk_Icon_Set;
       Context : not null access Gtk.Style_Context.Gtk_Style_Context_Record'Class;
       Size    : Gtk.Enums.Gtk_Icon_Size) return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
         (Self    : System.Address;
          Context : System.Address;
          Size    : Gtk.Enums.Gtk_Icon_Size) return System.Address;
      pragma Import (C, Internal, "gtk_icon_set_render_icon_pixbuf");
      Stub_Gdk_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Self), Get_Object (Context), Size), Stub_Gdk_Pixbuf));
   end Render_Icon_Pixbuf;

   -------------------------
   -- Render_Icon_Surface --
   -------------------------

   function Render_Icon_Surface
      (Self       : Gtk_Icon_Set;
       Context    : not null access Gtk.Style_Context.Gtk_Style_Context_Record'Class;
       Size       : Gtk.Enums.Gtk_Icon_Size;
       Scale      : Glib.Gint;
       For_Window : Gdk.Gdk_Window) return Cairo.Cairo_Surface
   is
      function Internal
         (Self       : System.Address;
          Context    : System.Address;
          Size       : Gtk.Enums.Gtk_Icon_Size;
          Scale      : Glib.Gint;
          For_Window : Gdk.Gdk_Window) return Cairo.Cairo_Surface;
      pragma Import (C, Internal, "gtk_icon_set_render_icon_surface");
   begin
      return Internal (Get_Object (Self), Get_Object (Context), Size, Scale, For_Window);
   end Render_Icon_Surface;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gtk_Icon_Set) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_icon_set_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

end Gtk.Icon_Set;
