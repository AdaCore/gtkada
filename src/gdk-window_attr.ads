-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
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

with Gdk.Color;
with Gdk.Cursor;
with Gdk.Types;
with Gdk.Visual;
with Glib;

package Gdk.Window_Attr is

   type Gdk_Window_Attr is new Root_Type with private;

   procedure Gdk_New
     (Window_Attr       :    out Gdk_Window_Attr;
      Title             : in     String := "";
      Event_Mask        : in     Gdk.Types.Gdk_Event_Mask
        := Gdk.Types.Null_Event_Mask;
      X, Y              : in     Glib.Gint16 := 0;
      Width             : in     Glib.Gint16 := 0;
      Height            : in     Glib.Gint16 := 0;
      Wclass            : in     Gdk.Types.Gdk_Window_Class
        := Gdk.Types.Input_Output;
      Visual            : in     Gdk.Visual.Gdk_Visual'Class
        := Gdk.Visual.Null_Visual;
      Colormap          : in     Gdk.Color.Gdk_Colormap'Class
        := Gdk.Color.Null_Colormap;
      Window_Type       : in     Gdk.Types.Gdk_Window_Type
        := Gdk.Types.Root;
      Cursor            : in     Gdk.Cursor.Gdk_Cursor'Class
        := Gdk.Cursor.Null_Cursor;
      Wmclass_Name      : in     String := "";
      Wmclass_Class     : in     String := "";
      Override_Redirect : in    Boolean := True);

   procedure Destroy (Window_Attr : in out Gdk_Window_Attr);

   procedure Set_Title (Window_Attr : in Gdk_Window_Attr;
                        Title       : in String);

   function Get_Title (Window_Attr : in Gdk_Window_Attr) return String;


   procedure Set_Event_Mask (Window_Attr : in Gdk_Window_Attr;
                             Event_Mask  : in Gdk.Types.Gdk_Event_Mask);

   function Get_Event_Mask (Window_Attr : in Gdk_Window_Attr)
                            return Gdk.Types.Gdk_Event_Mask;


   procedure Set_X (Window_Attr : in Gdk_Window_Attr;
                    X           : in Glib.Gint16);

   function Get_X (Window_Attr : in Gdk_Window_Attr) return Glib.Gint16;


   procedure Set_Y (Window_Attr : in Gdk_Window_Attr;
                    Y           : in Glib.Gint16);

   function Get_y (Window_Attr : in Gdk_Window_Attr) return Glib.Gint16;


   procedure Set_Width (Window_Attr : in Gdk_Window_Attr;
                        Width       : in Glib.Gint16);

   function Get_Width (Window_Attr : in Gdk_Window_Attr) return Glib.Gint16;


   procedure Set_Height (Window_Attr : in Gdk_Window_Attr;
                         Height      : in Glib.Gint16);

   function Get_Height (Window_Attr : in Gdk_Window_Attr) return Glib.Gint16;


   procedure Set_Window_Class
     (Window_Attr : in Gdk_Window_Attr;
      Wclass      : in Gdk.Types.Gdk_Window_Class);

   function Get_Window_Class (Window_Attr : in Gdk_Window_Attr)
                              return Gdk.Types.Gdk_Window_Class;


   procedure Set_Visual (Window_Attr : in Gdk_Window_Attr;
                         Visual      : in Gdk.Visual.Gdk_Visual'class);

   function Get_Visual (Window_Attr : in Gdk_Window_Attr)
                        return Gdk.Visual.Gdk_Visual;


   procedure Set_Colormap (Window_Attr : in Gdk_Window_Attr;
                           Colormap    : in Gdk.Color.Gdk_Colormap'Class);

   function Get_Colormap (Window_Attr : in Gdk_Window_Attr)
                          return Gdk.Color.Gdk_Colormap;


   procedure Set_Window_Type (Window_Attr : in Gdk_Window_Attr;
                              Window_Type : in Gdk.Types.Gdk_Window_Type);

   function Get_Window_Type (Window_Attr : in Gdk_Window_Attr)
                             return Gdk.Types.Gdk_Window_Type;


   procedure Set_Cursor (Window_Attr : in Gdk_Window_Attr;
                         Cursor      : in Gdk.Cursor.Gdk_Cursor'Class);

   function Get_Cursor (Window_Attr : in Gdk_Window_Attr)
                        return Gdk.Cursor.Gdk_Cursor;


   procedure Set_Wmclass_Name (Window_Attr  : in Gdk_Window_Attr;
                               Wmclass_Name : in String);

   function Get_Wmclass_Name (Window_Attr : in Gdk_Window_Attr) return String;


   procedure Set_Wmclass_Class (Window_Attr   : in Gdk_Window_Attr;
                                Wmclass_Class : in String);

   function Get_Wmclass_Class (Window_Attr : in Gdk_Window_Attr) return String;


   procedure Set_Override_Redirect (Window_Attr       : in Gdk_Window_Attr;
                                    Override_Redirect : in Boolean);

   function Get_Override_Redirect (Window_Attr : in Gdk_Window_Attr)
                                   return Boolean;

private

   type Gdk_Window_Attr is new Root_Type with null record;

end Gdk.Window_Attr;
