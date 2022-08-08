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
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Aspect_Frame is

   package Type_Conversion_Gtk_Aspect_Frame is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Aspect_Frame_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Aspect_Frame);

   --------------------------
   -- Gtk_Aspect_Frame_New --
   --------------------------

   function Gtk_Aspect_Frame_New
      (Label      : UTF8_String := "";
       Xalign     : Gfloat;
       Yalign     : Gfloat;
       Ratio      : Gfloat;
       Obey_Child : Boolean) return Gtk_Aspect_Frame
   is
      Aspect_Frame : constant Gtk_Aspect_Frame := new Gtk_Aspect_Frame_Record;
   begin
      Gtk.Aspect_Frame.Initialize (Aspect_Frame, Label, Xalign, Yalign, Ratio, Obey_Child);
      return Aspect_Frame;
   end Gtk_Aspect_Frame_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Aspect_Frame : out Gtk_Aspect_Frame;
       Label        : UTF8_String := "";
       Xalign       : Gfloat;
       Yalign       : Gfloat;
       Ratio        : Gfloat;
       Obey_Child   : Boolean)
   is
   begin
      Aspect_Frame := new Gtk_Aspect_Frame_Record;
      Gtk.Aspect_Frame.Initialize (Aspect_Frame, Label, Xalign, Yalign, Ratio, Obey_Child);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Aspect_Frame : not null access Gtk_Aspect_Frame_Record'Class;
       Label        : UTF8_String := "";
       Xalign       : Gfloat;
       Yalign       : Gfloat;
       Ratio        : Gfloat;
       Obey_Child   : Boolean)
   is
      function Internal
         (Label      : Gtkada.Types.Chars_Ptr;
          Xalign     : Gfloat;
          Yalign     : Gfloat;
          Ratio      : Gfloat;
          Obey_Child : Glib.Gboolean) return System.Address;
      pragma Import (C, Internal, "gtk_aspect_frame_new");
      Tmp_Label  : Gtkada.Types.Chars_Ptr;
      Tmp_Return : System.Address;
   begin
      if not Aspect_Frame.Is_Created then
         if Label = "" then
            Tmp_Label := Gtkada.Types.Null_Ptr;
         else
            Tmp_Label := New_String (Label);
         end if;
         Tmp_Return := Internal (Tmp_Label, Xalign, Yalign, Ratio, Boolean'Pos (Obey_Child));
         Free (Tmp_Label);
         Set_Object (Aspect_Frame, Tmp_Return);
      end if;
   end Initialize;

   ---------
   -- Set --
   ---------

   procedure Set
      (Aspect_Frame : not null access Gtk_Aspect_Frame_Record;
       Xalign       : Gfloat;
       Yalign       : Gfloat;
       Ratio        : Gfloat;
       Obey_Child   : Boolean)
   is
      procedure Internal
         (Aspect_Frame : System.Address;
          Xalign       : Gfloat;
          Yalign       : Gfloat;
          Ratio        : Gfloat;
          Obey_Child   : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_aspect_frame_set");
   begin
      Internal (Get_Object (Aspect_Frame), Xalign, Yalign, Ratio, Boolean'Pos (Obey_Child));
   end Set;

end Gtk.Aspect_Frame;
