-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with System;
with Gtk.Util; use Gtk.Util;

package body Gtk.Aspect_Frame is

   ---------------
   -- Get_Ratio --
   ---------------

   function Get_Ratio (Aspect_Frame : access Gtk_Aspect_Frame_Record)
     return Gfloat
   is
      function Internal (Widget : in System.Address) return Gfloat;
      pragma Import (C, Internal, "ada_aspect_frame_get_ratio");
   begin
      return Internal (Get_Object (Aspect_Frame));
   end Get_Ratio;

   ----------------
   -- Get_Xalign --
   ----------------

   function Get_Xalign (Aspect_Frame : access Gtk_Aspect_Frame_Record)
     return Gfloat
   is
      function Internal (Widget : in System.Address) return Gfloat;
      pragma Import (C, Internal, "ada_aspect_frame_get_xalign");
   begin
      return Internal (Get_Object (Aspect_Frame));
   end Get_Xalign;

   ----------------
   -- Get_Yalign --
   ----------------

   function Get_Yalign (Aspect_Frame : access Gtk_Aspect_Frame_Record)
     return Gfloat
   is
      function Internal (Widget : in System.Address) return Gfloat;
      pragma Import (C, Internal, "ada_aspect_frame_get_yalign");
   begin
      return Internal (Get_Object (Aspect_Frame));
   end Get_Yalign;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Aspect_Frame : out Gtk_Aspect_Frame;
      Label        : in String;
      Xalign       : in Gfloat;
      Yalign       : in Gfloat;
      Ratio        : in Gfloat;
      Obey_Child   : in Boolean) is
   begin
      Aspect_Frame := new Gtk_Aspect_Frame_Record;
      Initialize (Aspect_Frame, Label, Xalign, Yalign, Ratio, Obey_Child);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Aspect_Frame : access Gtk_Aspect_Frame_Record'Class;
      Label        : in String;
      Xalign       : in Gfloat;
      Yalign       : in Gfloat;
      Ratio        : in Gfloat;
      Obey_Child   : in Boolean)
   is
      function Internal
        (Label      : in System.Address;
         Xalign     : in Gfloat;
         Yalign     : in Gfloat;
         Ratio      : in Gfloat;
         Obey_Child : in Gint)
         return System.Address;
      pragma Import (C, Internal, "gtk_aspect_frame_new");
      S : aliased constant String := Label & ASCII.NUL;
      Sa : System.Address := System.Null_Address;
   begin
      if Label /= "" then
         Sa := S'Address;
      end if;
      Set_Object (Aspect_Frame, Internal (Sa, Xalign, Yalign, Ratio,
                                          Boolean'Pos (Obey_Child)));
      Initialize_User_Data (Aspect_Frame);
   end Initialize;

   ---------
   -- Set --
   ---------

   procedure Set
     (Aspect_Frame : access Gtk_Aspect_Frame_Record;
      Xalign       : in Gfloat;
      Yalign       : in Gfloat;
      Ratio        : in Gfloat;
      Obey_Child   : in Boolean)
   is
      procedure Internal
        (Aspect_Frame : in System.Address;
         Xalign       : in Gfloat;
         Yalign       : in Gfloat;
         Ratio        : in Gfloat;
         Obey_Child   : in Gint);
      pragma Import (C, Internal, "gtk_aspect_frame_set");

   begin
      Internal
        (Get_Object (Aspect_Frame), Xalign, Yalign, Ratio,
         Boolean'Pos (Obey_Child));
   end Set;

   --------------
   -- Generate --
   --------------

   procedure Generate (N    : in Node_Ptr;
                       File : in File_Type) is
      S : String_Ptr;
   begin
      S := Get_Field (N, "label");

      if S /= null then
         if Gettext_Support (N) then
            Gen_New (N, "Aspect_Frame", S.all,
              To_Float (Get_Field (N, "xalign").all),
              To_Float (Get_Field (N, "yalign").all),
              To_Float (Get_Field (N, "ratio").all),
              Get_Field (N, "obey_child").all,
              File, "-""", """");
         else
            Gen_New (N, "Aspect_Frame", S.all,
              To_Float (Get_Field (N, "xalign").all),
              To_Float (Get_Field (N, "yalign").all),
              To_Float (Get_Field (N, "ratio").all),
              Get_Field (N, "obey_child").all,
              File, """", """");
         end if;
      else
         Gen_New (N, "Aspect_Frame", "",
           To_Float (Get_Field (N, "xalign").all),
           To_Float (Get_Field (N, "yalign").all),
           To_Float (Get_Field (N, "ratio").all),
           Get_Field (N, "obey_child").all,
           File, """", """");
      end if;

      Frame.Generate (N, File);
   end Generate;

   procedure Generate (Aspect_Frame : in out Gtk.Object.Gtk_Object;
                       N            : in Node_Ptr) is
      S : String_Ptr;
   begin
      if not N.Specific_Data.Created then
         S := Get_Field (N, "label");

         if S /= null then
            Gtk_New (Gtk_Aspect_Frame (Aspect_Frame), S.all,
              Gfloat'Value (Get_Field (N, "xalign").all),
              Gfloat'Value (Get_Field (N, "yalign").all),
              Gfloat'Value (Get_Field (N, "ratio").all),
              Boolean'Value (Get_Field (N, "obey_child").all));

         else
            Gtk_New (Gtk_Aspect_Frame (Aspect_Frame), "",
              Gfloat'Value (Get_Field (N, "xalign").all),
              Gfloat'Value (Get_Field (N, "yalign").all),
              Gfloat'Value (Get_Field (N, "ratio").all),
              Boolean'Value (Get_Field (N, "obey_child").all));
         end if;

         Set_Object (Get_Field (N, "name"), Aspect_Frame);
         N.Specific_Data.Created := True;
      end if;

      Frame.Generate (Aspect_Frame, N);
   end Generate;

end Gtk.Aspect_Frame;
