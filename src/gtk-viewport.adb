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

with System;
with Gdk; use Gdk;
with Gtk.Util; use Gtk.Util;
with Gtk.Container; use Gtk.Container;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;

package body Gtk.Viewport is

   ---------------------
   -- Get_Hadjustment --
   ---------------------

   function Get_Hadjustment (Viewport : access Gtk_Viewport_Record)
                             return        Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Viewport : in System.Address)
                         return        System.Address;
      pragma Import (C, Internal, "gtk_viewport_get_hadjustment");
      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Viewport)), Stub));
   end Get_Hadjustment;

   ---------------------
   -- Get_Vadjustment --
   ---------------------

   function Get_Vadjustment (Viewport : access Gtk_Viewport_Record)
                             return        Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Viewport : in System.Address)
                         return        System.Address;
      pragma Import (C, Internal, "gtk_viewport_get_vadjustment");
      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Viewport)), Stub));
   end Get_Vadjustment;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Widget      : out Gtk_Viewport;
       Hadjustment : in Gtk.Adjustment.Gtk_Adjustment :=
         Adjustment.Null_Adjustment;
       Vadjustment : in Gtk.Adjustment.Gtk_Adjustment :=
         Adjustment.Null_Adjustment) is
   begin
      Widget := new Gtk_Viewport_Record;
      Initialize (Widget, Hadjustment, Vadjustment);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Widget      : access Gtk_Viewport_Record;
       Hadjustment : in Gtk.Adjustment.Gtk_Adjustment;
       Vadjustment : in Gtk.Adjustment.Gtk_Adjustment)
   is
      function Internal
         (Hadjustment : in System.Address;
          Vadjustment : in System.Address)
          return           System.Address;
      pragma Import (C, Internal, "gtk_viewport_new");
   begin
      Set_Object (Widget, Internal (Get_Object (Hadjustment),
                                    Get_Object (Vadjustment)));
      Initialize_User_Data (Widget);
   end Initialize;

   ---------------------
   -- Set_Hadjustment --
   ---------------------

   procedure Set_Hadjustment
      (Viewport   : access Gtk_Viewport_Record;
       Adjustment : in Gtk.Adjustment.Gtk_Adjustment)
   is
      procedure Internal
         (Viewport   : in System.Address;
          Adjustment : in System.Address);
      pragma Import (C, Internal, "gtk_viewport_set_hadjustment");
   begin
      Internal (Get_Object (Viewport),
                Get_Object (Adjustment));
   end Set_Hadjustment;

   ---------------------
   -- Set_Shadow_Type --
   ---------------------

   procedure Set_Shadow_Type
      (Viewport : access Gtk_Viewport_Record;
       The_Type : in Gtk_Shadow_Type)
   is
      procedure Internal
         (Viewport : in System.Address;
          The_Type : in Gint);
      pragma Import (C, Internal, "gtk_viewport_set_shadow_type");
   begin
      Internal (Get_Object (Viewport),
                Gtk_Shadow_Type'Pos (The_Type));
   end Set_Shadow_Type;

   ---------------------
   -- Set_Vadjustment --
   ---------------------

   procedure Set_Vadjustment
      (Viewport   : access Gtk_Viewport_Record;
       Adjustment : in Gtk.Adjustment.Gtk_Adjustment)
   is
      procedure Internal
         (Viewport   : in System.Address;
          Adjustment : in System.Address);
      pragma Import (C, Internal, "gtk_viewport_set_vadjustment");
   begin
      Internal (Get_Object (Viewport),
                Get_Object (Adjustment));
   end Set_Vadjustment;

   --------------
   -- Generate --
   --------------

   procedure Generate (N    : in Node_Ptr;
                       File : in File_Type) is
   begin
      Gen_New (N, "Viewport", File => File);
      Gen_Set (N, "Viewport", "shadow_type", File => File);

      if not N.Specific_Data.Has_Container then
         if Get_Field (N.Parent, "class").all = "GtkScrolledWindow" then
            Gen_Call_Child
              (N, null, "Scrolled_window",
               "Add_with_Viewport", File => File);

         else
            Gen_Call_Child (N, null, "Container", "Add", File => File);
         end if;

         N.Specific_Data.Has_Container := True;
      end if;
   end Generate;

   procedure Generate (Viewport : in out Gtk_Object;
                       N        : in Node_Ptr) is
      S : String_Ptr;
   begin
      if not N.Specific_Data.Created then
         Gtk_New (Gtk_Viewport (Viewport));
         Set_Object (Get_Field (N, "name"), Viewport);

         S := Get_Field (N, "shadow_type");

         if S /= null then
            Set_Shadow_Type
              (Gtk_Viewport (Viewport),
               Gtk_Shadow_Type'Value (S (S'First + 4 .. S'Last)));
         end if;

         N.Specific_Data.Created := True;
      end if;

      if not N.Specific_Data.Has_Container then
         if Get_Field (N.Parent, "class").all = "GtkScrolledWindow" then
            Scrolled_Window.Add_With_Viewport
              (Gtk_Scrolled_Window
                (Get_Object (Get_Field (N.Parent, "name"))),
               Gtk_Viewport (Viewport));

         else
            Container.Add
              (Gtk_Container (Get_Object (Get_Field (N.Parent, "name"))),
               Gtk_Viewport (Viewport));
         end if;

         N.Specific_Data.Has_Container := True;
      end if;
   end Generate;

end Gtk.Viewport;
