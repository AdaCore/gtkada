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

with Interfaces.C.Strings;
with System;

package body Gdk.Window_Attr is

   ---------------
   --  Destroy  --
   ---------------

   procedure Destroy (Window_Attr : in out Gdk_Window_Attr) is
      procedure Internal (Window_Attr : in System.Address);
      pragma Import (C, Internal, "ada_gdk_window_attr_destroy");
   begin
      Internal (Get_Object (Window_Attr));
      Set_Object (Window_Attr, System.Null_Address);
   end Destroy;


   ---------------
   --  Gdk_New  --
   ---------------

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
      Override_Redirect : in    Boolean := True) is
      function Internal return System.Address;
      pragma Import (C, Internal, "ada_gdk_window_attr_new");
      Result : Gdk_Window_Attr;
   begin
      Set_Object (Result, Internal);

      Set_Title (Window_Attr, Title);
      Set_Event_Mask (Window_Attr, Event_Mask);
      Set_X (Window_Attr, X);
      Set_Y (Window_Attr, Y);
      Set_Width (Window_Attr, Width);
      Set_Height (Window_Attr, Height);
      Set_Window_Class (Window_Attr, Wclass);
      Set_Visual (Window_Attr, Visual);
      Set_Colormap (Window_Attr, Colormap);
      Set_Window_Type (Window_Attr, Window_Type);
      Set_Cursor (Window_Attr, Cursor);
      Set_Wmclass_Name (Window_Attr, Wmclass_Name);
      Set_Wmclass_Class (Window_Attr, Wmclass_Class);
      Set_Override_Redirect (Window_Attr, Override_Redirect);
   end Gdk_New;


   --------------------
   --  Get_Colormap  --
   --------------------

   function Get_Colormap (Window_Attr : in Gdk_Window_Attr)
                          return Gdk.Color.Gdk_Colormap is
      function Internal (Window_Attr : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "ada_gdk_window_attr_get_colormap");
      Result : Gdk.Color.Gdk_Colormap;
   begin
      Set_Object (Result, Internal (Get_Object (Window_Attr)));
      return Result;
   end Get_Colormap;


   ------------------
   --  Get_Cursor  --
   ------------------

   function Get_Cursor (Window_Attr : in Gdk_Window_Attr)
                        return Gdk.Cursor.Gdk_Cursor is
      function Internal (Window_Attr : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "ada_gdk_window_attr_get_cursor");
      Result : Gdk.Cursor.Gdk_Cursor;
   begin
      Set_Object (Result, Get_Object (Window_Attr));
      return Result;
   end Get_Cursor;


   ----------------------
   --  Get_Event_Mask  --
   ----------------------

   function Get_Event_Mask (Window_Attr : in Gdk_Window_Attr)
                            return Gdk.Types.Gdk_Event_Mask is
      function Internal (Window_Attr : in System.Address)
                         return Gdk.Types.Gdk_Event_Mask;
      pragma Import (C, Internal, "ada_gdk_window_attr_get_event_mask");
   begin
      return Internal (Get_Object (Window_Attr));
   end Get_Event_Mask;


   ------------------
   --  Get_Height  --
   ------------------

   function Get_Height (Window_Attr : in Gdk_Window_Attr) return Glib.Gint16 is
      function Internal (Window_Attr : in System.Address) return Glib.Gint16;
      pragma Import (C, Internal, "ada_gdk_window_attr_get_height");
   begin
      return Internal (Get_Object (Window_Attr));
   end Get_Height;


   -----------------------------
   --  Get_Override_Redirect  --
   -----------------------------

   function Get_Override_Redirect (Window_Attr : in Gdk_Window_Attr)
                                   return Boolean is
      function Internal (Window_Attr : in System.Address)
                   return Glib.Gboolean;
      pragma Import (C, Internal, "ada_gdk_window_attr_get_override_redirect");
   begin
      return Glib.To_Boolean (Internal (Get_Object (Window_Attr)));
   end Get_Override_Redirect;


   -----------------
   --  Get_Title  --
   -----------------

   function Get_Title (Window_Attr : in Gdk_Window_Attr) return String is
      function Internal (Window_Attr : in System.Address) return
        Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "ada_gdk_window_attr_get_title");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Window_Attr)));
   end Get_Title;


   ------------------
   --  Get_Visual  --
   ------------------

   function Get_Visual (Window_Attr : in Gdk_Window_Attr)
                        return Gdk.Visual.Gdk_Visual is
      function Internal (Window_Attr : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "ada_gdk_window_attr_get_visual");
      Result : Gdk.Visual.Gdk_Visual;
   begin
      Set_Object (Result, Internal (Get_Object (Window_Attr)));
      return Result;
   end Get_Visual;


   -----------------
   --  Get_Width  --
   -----------------

   function Get_Width (Window_Attr : in Gdk_Window_Attr) return Glib.Gint16 is
      function Internal (Window_Attr : in System.Address) return Glib.Gint16;
      pragma Import (C, Internal, "ada_gdk_window_attr_get_width");
   begin
      return Internal (Get_Object (Window_Attr));
   end Get_Width;


   ------------------------
   --  Get_Window_Class  --
   ------------------------

   function Get_Window_Class (Window_Attr : in Gdk_Window_Attr)
                              return Gdk.Types.Gdk_Window_Class is
      function Internal (Window_Attr : in System.Address)
                         return Gdk.Types.Gdk_Window_Class;
      pragma Import (C, Internal, "ada_gdk_window_attr_get_wclass");
   begin
      return Internal (Get_Object (Window_Attr));
   end Get_Window_Class;


   -----------------------
   --  Get_Window_Type  --
   -----------------------

   function Get_Window_Type (Window_Attr : in Gdk_Window_Attr)
                             return Gdk.Types.Gdk_Window_Type is
      function Internal (Window_Attr : in System.Address)
        return Gdk.Types.Gdk_Window_Type;
      pragma Import (C, Internal, "ada_gdk_window_attr_get_window_type");
   begin
      return Internal (Get_Object (Window_Attr));
   end Get_Window_Type;


   -------------------------
   --  Get_Wmclass_Class  --
   -------------------------

   function Get_Wmclass_Class (Window_Attr : in Gdk_Window_Attr)
                              return String is
      function Internal (Window_Attr : in System.Address)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "ada_gdk_window_attr_get_wmclass_class");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Window_Attr)));
   end Get_Wmclass_Class;


   ------------------------
   --  Get_Wmclass_Name  --
   ------------------------

   function Get_Wmclass_Name (Window_Attr : in Gdk_Window_Attr)
                              return String is
      function Internal (Window_Attr : in System.Address)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "ada_gdk_window_attr_get_wmclass_name");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Window_Attr)));
   end Get_Wmclass_Name;


   -------------
   --  Get_X  --
   -------------

   function Get_X (Window_Attr : in Gdk_Window_Attr) return Glib.Gint16 is
      function Internal (Window_Attr : in System.Address) return Glib.Gint16;
      pragma Import (C, Internal, "ada_gdk_window_attr_get_x");
   begin
      return Internal (Get_Object (Window_Attr));
   end Get_X;


   -------------
   --  Get_Y  --
   -------------

   function Get_Y (Window_Attr : in Gdk_Window_Attr) return Glib.Gint16 is
      function Internal (Window_Attr : in System.Address) return Glib.Gint16;
      pragma Import (C, Internal, "ada_gdk_window_attr_get_y");
   begin
      return Internal (Get_Object (Window_Attr));
   end Get_Y;


   --------------------
   --  Set_Colormap  --
   --------------------

   procedure Set_Colormap
     (Window_Attr : in out Gdk_Window_Attr;
      Colormap    : in     Gdk.Color.Gdk_Colormap'Class) is
      procedure Internal (Window_Attr, Colormap : in System.Address);
      pragma Import (C, Internal, "ada_gdk_window_attr_set_colormap");
   begin
      Internal (Get_Object (Window_Attr), Get_Object (Colormap));
   end Set_Colormap;


   ------------------
   --  Set_Cursor  --
   ------------------

   procedure Set_Cursor (Window_Attr : in out Gdk_Window_Attr;
                         Cursor      : in     Gdk.Cursor.Gdk_Cursor'Class) is
      procedure Internal (Window_Attr, Cursor : in System.Address);
      pragma Import (C, Internal, "ada_gdk_window_attr_set_cursor");
   begin
      Internal (Get_Object (Window_Attr), Get_Object (Cursor));
   end Set_Cursor;


   ----------------------
   --  Set_Event_Mask  --
   ----------------------

   procedure Set_Event_Mask (Window_Attr : in out Gdk_Window_Attr;
                             Event_Mask  : in     Gdk.Types.Gdk_Event_Mask) is
      procedure Internal (Window_Attr : in System.Address;
                          Event_Mask : in Gdk.Types.Gdk_Event_Mask);
      pragma Import (C, Internal, "ada_gdk_window_attr_set_event_mask");
   begin
      Internal (Get_Object (Window_Attr), Event_Mask);
   end Set_Event_Mask;


   ------------------
   --  Set_Height  --
   ------------------

   procedure Set_Height (Window_Attr : in out Gdk_Window_Attr;
                        height       : in     Glib.Gint16) is
      procedure Internal (Window_Attr : in System.Address;
                          height       : in Glib.Gint16);
      pragma Import (C, Internal, "ada_gdk_window_attr_set_height");
   begin
      Internal (Get_Object (Window_Attr), Height);
   end Set_Height;


   -----------------------------
   --  Set_Override_Redirect  --
   -----------------------------

   procedure Set_Override_Redirect (Window_Attr       : in out Gdk_Window_Attr;
                                    Override_Redirect : in     Boolean) is
      procedure Internal (Window_Attr : in System.Address;
                          Override_Redirect : in Glib.Gboolean);
      pragma Import (C, Internal, "ada_gdk_window_attr_set_override_redirect");
   begin
      Internal (Get_Object (Window_Attr),
                Glib.To_Gboolean (Override_Redirect));
   end Set_Override_Redirect;


   -----------------
   --  Set_Title  --
   -----------------

   procedure Set_Title (Window_Attr : in out Gdk_Window_Attr;
                        Title       : in     String) is
      procedure Internal (Window_Attr : in System.Address;
                          Title : in String);
      pragma Import (C, Internal, "ada_gdk_window_attr_set_title");
   begin
      Internal (Get_Object (Window_Attr), Title & Ascii.NUL);
   end Set_Title;


   ------------------
   --  Set_Visual  --
   ------------------

   procedure Set_Visual (Window_Attr : in out Gdk_Window_Attr;
                         Visual      : in     Gdk.Visual.Gdk_Visual'class) is
      procedure Internal (Window_Attr, Visual : in System.Address);
      pragma Import (C, Internal, "ada_gdk_window_attr_set_visual");
   begin
      Internal (Get_Object (Window_Attr), Get_Object (Visual));
   end Set_Visual;


   -----------------
   --  Set_Width  --
   -----------------

   procedure Set_Width (Window_Attr : in out Gdk_Window_Attr;
                        Width       : in     Glib.Gint16) is
      procedure Internal (Window_Attr : in System.Address;
                          Width       : in Glib.Gint16);
      pragma Import (C, Internal, "ada_gdk_window_attr_set_width");
   begin
      Internal (Get_Object (Window_Attr), Width);
   end Set_Width;


   ------------------------
   --  Set_Window_Class  --
   ------------------------

   procedure Set_Window_Class
     (Window_Attr : in out Gdk_Window_Attr;
      Wclass      : in     Gdk.Types.Gdk_Window_Class) is
      procedure Internal (Window_Attr : in System.Address;
                          Wclass : in Gdk.Types.Gdk_Window_Class);
      pragma Import (C, Internal, "ada_gdk_window_attr_set_wclass");
   begin
      Internal (Get_Object (Window_Attr), Wclass);
   end Set_Window_Class;


   -----------------------
   --  Set_Window_Type  --
   -----------------------

   procedure Set_Window_Type
     (Window_Attr : in out Gdk_Window_Attr;
      Window_Type : in     Gdk.Types.Gdk_Window_Type) is
      procedure Internal (Window_Attr : in System.Address;
                          Window_Type : in Gdk.Types.Gdk_Window_Type);
      pragma Import (C, Internal, "ada_gdk_window_attr_set_window_type");
   begin
      Internal (Get_Object (Window_Attr), Window_Type);
   end Set_Window_Type;


   -------------------------
   --  Set_Wmclass_Class  --
   -------------------------

   procedure Set_Wmclass_Class (Window_Attr  : in out Gdk_Window_Attr;
                                Wmclass_Class : in     String) is
      procedure Internal (Window_Attr  : in System.Address;
                          Wmclass_Class : in String);
      pragma Import (C, Internal, "ada_gdk_window_attr_set_wmclass_class");
   begin
      Internal (Get_Object (Window_Attr), Wmclass_Class & Ascii.NUL);
   end Set_Wmclass_Class;


   ------------------------
   --  Set_Wmclass_Name  --
   ------------------------

   procedure Set_Wmclass_Name (Window_Attr  : in out Gdk_Window_Attr;
                               Wmclass_Name : in     String) is
      procedure Internal (Window_Attr  : in System.Address;
                          Wmclass_Name : in String);
      pragma Import (C, Internal, "ada_gdk_window_attr_set_wmclass_name");
   begin
      Internal (Get_Object (Window_Attr), Wmclass_Name & Ascii.NUL);
   end Set_Wmclass_Name;


   -------------
   --  Set_X  --
   -------------

   procedure Set_X (Window_Attr : in out Gdk_Window_Attr;
                    X           : in     Glib.Gint16) is
      procedure Internal (Window_Attr : in System.Address;
                          X           : in Glib.Gint16);
      pragma Import (C, Internal, "ada_gdk_window_attr_set_x");
   begin
      Internal (Get_Object (Window_Attr), X);
   end Set_X;


   -------------
   --  Set_Y  --
   -------------

   procedure Set_Y (Window_Attr : in out Gdk_Window_Attr;
                    Y           : in     Glib.Gint16) is
      procedure Internal (Window_Attr : in System.Address;
                          Y           : in Glib.Gint16);
      pragma Import (C, Internal, "ada_gdk_window_attr_set_y");
   begin
      Internal (Get_Object (Window_Attr), Y);
   end Set_Y;

end Gdk.Window_Attr;


