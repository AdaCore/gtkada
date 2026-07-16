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
with Gdk.Display;
with Glib.Object;                use Glib.Object;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with System;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gdk.App_Launch_Context is

   package Type_Conversion_Gdk_App_Launch_Context is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_App_Launch_Context_Record);
   pragma Unreferenced (Type_Conversion_Gdk_App_Launch_Context);

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Self : not null access Gdk_App_Launch_Context_Record)
       return Gdk.Gdk_Display
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_app_launch_context_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Gdk_Display (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Display));
   end Get_Display;

   -----------------
   -- Set_Desktop --
   -----------------

   procedure Set_Desktop
      (Self    : not null access Gdk_App_Launch_Context_Record;
       Desktop : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Desktop : Glib.Gint);
      pragma Import (C, Internal, "gdk_app_launch_context_set_desktop");
   begin
      Internal (Get_Object (Self), Desktop);
   end Set_Desktop;

   --------------
   -- Set_Icon --
   --------------

   procedure Set_Icon
      (Self : not null access Gdk_App_Launch_Context_Record;
       Icon : Glib.G_Icon.G_Icon)
   is
      procedure Internal (Self : System.Address; Icon : Glib.G_Icon.G_Icon);
      pragma Import (C, Internal, "gdk_app_launch_context_set_icon");
   begin
      Internal (Get_Object (Self), Icon);
   end Set_Icon;

   -------------------
   -- Set_Icon_Name --
   -------------------

   procedure Set_Icon_Name
      (Self      : not null access Gdk_App_Launch_Context_Record;
       Icon_Name : UTF8_String := "")
   is
      procedure Internal
         (Self      : System.Address;
          Icon_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gdk_app_launch_context_set_icon_name");
      Tmp_Icon_Name : Gtkada.Types.Chars_Ptr;
   begin
      if Icon_Name = "" then
         Tmp_Icon_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Icon_Name := New_String (Icon_Name);
      end if;
      Internal (Get_Object (Self), Tmp_Icon_Name);
      Free (Tmp_Icon_Name);
   end Set_Icon_Name;

   -------------------
   -- Set_Timestamp --
   -------------------

   procedure Set_Timestamp
      (Self      : not null access Gdk_App_Launch_Context_Record;
       Timestamp : Guint32)
   is
      procedure Internal (Self : System.Address; Timestamp : Guint32);
      pragma Import (C, Internal, "gdk_app_launch_context_set_timestamp");
   begin
      Internal (Get_Object (Self), Timestamp);
   end Set_Timestamp;

end Gdk.App_Launch_Context;
