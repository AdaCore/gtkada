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

with Gdk; use Gdk;
with Gdk.Bitmap;
with Gdk.Color;
with Gdk.Pixmap;
with Gtk.Widget;
with Gtk.Util; use Gtk.Util;
with System;

package body Gtk.Pixmap is

   ---------
   -- Get --
   ---------

   procedure Get
      (Pixmap : access Gtk_Pixmap_Record;
       Val    : in Gdk.Pixmap.Gdk_Pixmap'Class;
       Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class)
   is
      procedure Internal
         (Pixmap : in System.Address;
          Val    : in System.Address;
          Mask   : in System.Address);
      pragma Import (C, Internal, "gtk_pixmap_get");
   begin
      Internal (Get_Object (Pixmap),
                Get_Object (Val),
                Get_Object (Mask));
   end Get;

   --------------
   -- Get_Mask --
   --------------

   function Get_Mask (Widget : access Gtk_Pixmap_Record)
                      return      Gdk.Bitmap.Gdk_Bitmap'Class
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_pixmap_get_mask");
      Tmp : Gdk.Bitmap.Gdk_Bitmap;
   begin
      Set_Object (Tmp, Internal (Get_Object (Widget)));
      return Tmp;
   end Get_Mask;

   ----------------
   -- Get_Pixmap --
   ----------------

   function Get_Pixmap (Widget : access Gtk_Pixmap_Record)
                        return      Gdk.Pixmap.Gdk_Pixmap'Class
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_pixmap_get_pixmap");
      Tmp : Gdk.Pixmap.Gdk_Pixmap;
   begin
      Set_Object (Tmp, Internal (Get_Object (Widget)));
      return Tmp;
   end Get_Pixmap;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Widget : out Gtk_Pixmap;
       Pixmap : in Gdk.Pixmap.Gdk_Pixmap'Class;
       Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class) is
   begin
      Widget := new Gtk_Pixmap_Record;
      Initialize (Widget, Pixmap, Mask);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Widget : access Gtk_Pixmap_Record'Class;
       Pixmap : in Gdk.Pixmap.Gdk_Pixmap'Class;
       Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class)
   is
      function Internal
         (Pixmap : in System.Address;
          Mask   : in System.Address)
          return      System.Address;
      pragma Import (C, Internal, "gtk_pixmap_new");
   begin
      Set_Object (Widget, Internal (Get_Object (Pixmap),
                                    Get_Object (Mask)));
      Initialize_User_Data (Widget);
   end Initialize;

   ---------
   -- Set --
   ---------

   procedure Set
      (Pixmap : access Gtk_Pixmap_Record;
       Val    : in Gdk.Pixmap.Gdk_Pixmap'Class;
       Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class)
   is
      procedure Internal
         (Pixmap : in System.Address;
          Val    : in System.Address;
          Mask   : in System.Address);
      pragma Import (C, Internal, "gtk_pixmap_set");
   begin
      Internal (Get_Object (Pixmap),
                Get_Object (Val),
                Get_Object (Mask));
   end Set;

   -------------------
   -- Create_Pixmap --
   -------------------

   function Create_Pixmap
     (Filename : in String;
      Window   : access Gtk.Window.Gtk_Window_Record'Class)
      return Gtk_Pixmap
   is
      Gdkpixmap : Gdk.Pixmap.Gdk_Pixmap;
      Mask      : Gdk.Bitmap.Gdk_Bitmap;
      Pixmap    : Gtk_Pixmap;

      use Gtk.Widget;
      use Gtk.Window;

   begin
      if not Realized_Is_Set (Window) then
         Gtk.Window.Realize (Window);
      end if;

      Gdk.Pixmap.Create_From_Xpm
        (Gdkpixmap, Get_Window (Window), Mask, Gdk.Color.Null_Color, Filename);
      Gtk_New (Pixmap, Gdkpixmap, Mask);
      Gdk.Pixmap.Unref (Gdkpixmap);
      Gdk.Bitmap.Unref (Mask);
      return Pixmap;
   end Create_Pixmap;

   --------------
   -- Generate --
   --------------

   procedure Generate (N : in Node_Ptr; File : in File_Type) is
      Top : constant String_Ptr := Get_Field (Find_Top_Widget (N), "name");
      Cur : constant String_Ptr := Get_Field (N, "name");
      S   : String_Ptr;

   begin
      if not N.Specific_Data.Created then
         Add_Package ("Pixmap");

         S := Get_Field (N, "filename");

         if S /= null then
            Put_Line (File, "   " & To_Ada (Top.all) & "." & To_Ada (Cur.all) &
              " := Create_Pixmap (""" & S.all & """, " & To_Ada (Top.all) &
              ");");
            N.Specific_Data.Created := True;
         end if;
      end if;

      Misc.Generate (N, File);
   end Generate;

   procedure Generate (Pixmap : in out Object.Gtk_Object; N : in Node_Ptr) is
      Top : Node_Ptr;
      S   : String_Ptr;

   begin
      if not N.Specific_Data.Created then
         S := Get_Field (N, "filename");

         if S /= null then
            Top := Find_Top_Widget (N);
            Pixmap := Object.Gtk_Object (Create_Pixmap (S.all,
              Gtk.Window.Gtk_Window (Get_Object (Get_Field (Top, "name")))));
            N.Specific_Data.Created := True;
         end if;

         Set_Object (Get_Field (N, "name"), Pixmap);
         N.Specific_Data.Created := True;
      end if;

      Misc.Generate (Pixmap, N);
   end Generate;

end Gtk.Pixmap;
