-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
--         General Public License for more details.                  --
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

package body Gdk.Cursor is

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Cursor : in out Gdk_Cursor'Class)
   is
      procedure Internal (Cursor : in System.Address);
      pragma Import (C, Internal, "gdk_cursor_destroy");
   begin
      Internal (Get_Object (Cursor));
      Set_Object (Cursor, System.Null_Address);
   end Destroy;


   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New (Widget      : out Gdk_Cursor;
                      Cursor_Type : in Gdk.Types.Gdk_Cursor_Type)
   is
      function Internal (Cursor_Type : in Gdk.Types.Gdk_Cursor_Type)
                         return System.Address;
      pragma Import (C, Internal, "ada_gdk_cursor_new");
   begin
      Set_Object (Widget,
                  Internal (Cursor_Type));
   end Gdk_New;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New (Widget      : out Gdk_Cursor;
                      Cursor_Type : in Guint)
   is
      function Internal (Cursor_Type : in Guint)
                         return System.Address;
      pragma Import (C, Internal, "ada_gdk_cursor_new");
   begin
      Set_Object (Widget, Internal (Cursor_Type));
   end Gdk_New;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New
      (Widget : out Gdk_Cursor;
       Source : in Gdk.Pixmap.Gdk_Pixmap'Class;
       Mask   : in Gdk.Pixmap.Gdk_Pixmap'Class;
       Fg     : in Gdk.Color.Gdk_Color'Class;
       Bg     : in Gdk.Color.Gdk_Color'Class;
       X      : in Gint;
       Y      : in Gint)
   is
      function Internal
         (Source : in System.Address;
          Mask   : in System.Address;
          Fg     : in System.Address;
          Bg     : in System.Address;
          X      : in Gint;
          Y      : in Gint)
          return      System.Address;
      pragma Import (C, Internal, "gdk_cursor_new_from_pixmap");
   begin
      Set_Object (Widget, Internal (Get_Object (Source),
                                    Get_Object (Mask),
                                    Get_Object (Fg),
                                    Get_Object (Bg),
                                    X,
                                    Y));
   end Gdk_New;

   ----------------
   -- Set_Cursor --
   ----------------

   procedure Set_Cursor (Window : in Gdk.Window.Gdk_Window'Class;
                         Cursor : in Gdk_Cursor'Class)
   is
      procedure Internal (Window : System.Address; Cursor : System.Address);
      pragma Import (C, Internal, "gdk_window_set_cursor");
   begin
      Internal (Get_Object (Window), Get_Object (Cursor));
   end Set_Cursor;


end Gdk.Cursor;
