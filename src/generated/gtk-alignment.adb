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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Alignment is

   package Type_Conversion_Gtk_Alignment is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Alignment_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Alignment);

   -----------------------
   -- Gtk_Alignment_New --
   -----------------------

   function Gtk_Alignment_New
      (Xalign : Gfloat;
       Yalign : Gfloat;
       Xscale : Gfloat;
       Yscale : Gfloat) return Gtk_Alignment
   is
      Alignment : constant Gtk_Alignment := new Gtk_Alignment_Record;
   begin
      Gtk.Alignment.Initialize (Alignment, Xalign, Yalign, Xscale, Yscale);
      return Alignment;
   end Gtk_Alignment_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Alignment : out Gtk_Alignment;
       Xalign    : Gfloat;
       Yalign    : Gfloat;
       Xscale    : Gfloat;
       Yscale    : Gfloat)
   is
   begin
      Alignment := new Gtk_Alignment_Record;
      Gtk.Alignment.Initialize (Alignment, Xalign, Yalign, Xscale, Yscale);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Alignment : not null access Gtk_Alignment_Record'Class;
       Xalign    : Gfloat;
       Yalign    : Gfloat;
       Xscale    : Gfloat;
       Yscale    : Gfloat)
   is
      function Internal
         (Xalign : Gfloat;
          Yalign : Gfloat;
          Xscale : Gfloat;
          Yscale : Gfloat) return System.Address;
      pragma Import (C, Internal, "gtk_alignment_new");
   begin
      if not Alignment.Is_Created then
         Set_Object (Alignment, Internal (Xalign, Yalign, Xscale, Yscale));
      end if;
   end Initialize;

   -----------------
   -- Get_Padding --
   -----------------

   procedure Get_Padding
      (Alignment      : not null access Gtk_Alignment_Record;
       Padding_Top    : out Guint;
       Padding_Bottom : out Guint;
       Padding_Left   : out Guint;
       Padding_Right  : out Guint)
   is
      procedure Internal
         (Alignment      : System.Address;
          Padding_Top    : out Guint;
          Padding_Bottom : out Guint;
          Padding_Left   : out Guint;
          Padding_Right  : out Guint);
      pragma Import (C, Internal, "gtk_alignment_get_padding");
   begin
      Internal (Get_Object (Alignment), Padding_Top, Padding_Bottom, Padding_Left, Padding_Right);
   end Get_Padding;

   ---------
   -- Set --
   ---------

   procedure Set
      (Alignment : not null access Gtk_Alignment_Record;
       Xalign    : Gfloat;
       Yalign    : Gfloat;
       Xscale    : Gfloat;
       Yscale    : Gfloat)
   is
      procedure Internal
         (Alignment : System.Address;
          Xalign    : Gfloat;
          Yalign    : Gfloat;
          Xscale    : Gfloat;
          Yscale    : Gfloat);
      pragma Import (C, Internal, "gtk_alignment_set");
   begin
      Internal (Get_Object (Alignment), Xalign, Yalign, Xscale, Yscale);
   end Set;

   -----------------
   -- Set_Padding --
   -----------------

   procedure Set_Padding
      (Alignment      : not null access Gtk_Alignment_Record;
       Padding_Top    : Guint;
       Padding_Bottom : Guint;
       Padding_Left   : Guint;
       Padding_Right  : Guint)
   is
      procedure Internal
         (Alignment      : System.Address;
          Padding_Top    : Guint;
          Padding_Bottom : Guint;
          Padding_Left   : Guint;
          Padding_Right  : Guint);
      pragma Import (C, Internal, "gtk_alignment_set_padding");
   begin
      Internal (Get_Object (Alignment), Padding_Top, Padding_Bottom, Padding_Left, Padding_Right);
   end Set_Padding;

end Gtk.Alignment;
