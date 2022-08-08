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

package body Gtk.Viewport is

   package Type_Conversion_Gtk_Viewport is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Viewport_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Viewport);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Viewport    : out Gtk_Viewport;
       Hadjustment : Gtk.Adjustment.Gtk_Adjustment := null;
       Vadjustment : Gtk.Adjustment.Gtk_Adjustment := null)
   is
   begin
      Viewport := new Gtk_Viewport_Record;
      Gtk.Viewport.Initialize (Viewport, Hadjustment, Vadjustment);
   end Gtk_New;

   ----------------------
   -- Gtk_Viewport_New --
   ----------------------

   function Gtk_Viewport_New
      (Hadjustment : Gtk.Adjustment.Gtk_Adjustment := null;
       Vadjustment : Gtk.Adjustment.Gtk_Adjustment := null)
       return Gtk_Viewport
   is
      Viewport : constant Gtk_Viewport := new Gtk_Viewport_Record;
   begin
      Gtk.Viewport.Initialize (Viewport, Hadjustment, Vadjustment);
      return Viewport;
   end Gtk_Viewport_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Viewport    : not null access Gtk_Viewport_Record'Class;
       Hadjustment : Gtk.Adjustment.Gtk_Adjustment := null;
       Vadjustment : Gtk.Adjustment.Gtk_Adjustment := null)
   is
      function Internal
         (Hadjustment : System.Address;
          Vadjustment : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_viewport_new");
   begin
      if not Viewport.Is_Created then
         Set_Object (Viewport, Internal (Get_Object_Or_Null (GObject (Hadjustment)), Get_Object_Or_Null (GObject (Vadjustment))));
      end if;
   end Initialize;

   --------------------
   -- Get_Bin_Window --
   --------------------

   function Get_Bin_Window
      (Viewport : not null access Gtk_Viewport_Record) return Gdk.Gdk_Window
   is
      function Internal (Viewport : System.Address) return Gdk.Gdk_Window;
      pragma Import (C, Internal, "gtk_viewport_get_bin_window");
   begin
      return Internal (Get_Object (Viewport));
   end Get_Bin_Window;

   ---------------------
   -- Get_Shadow_Type --
   ---------------------

   function Get_Shadow_Type
      (Viewport : not null access Gtk_Viewport_Record)
       return Gtk.Enums.Gtk_Shadow_Type
   is
      function Internal
         (Viewport : System.Address) return Gtk.Enums.Gtk_Shadow_Type;
      pragma Import (C, Internal, "gtk_viewport_get_shadow_type");
   begin
      return Internal (Get_Object (Viewport));
   end Get_Shadow_Type;

   ---------------------
   -- Get_View_Window --
   ---------------------

   function Get_View_Window
      (Viewport : not null access Gtk_Viewport_Record) return Gdk.Gdk_Window
   is
      function Internal (Viewport : System.Address) return Gdk.Gdk_Window;
      pragma Import (C, Internal, "gtk_viewport_get_view_window");
   begin
      return Internal (Get_Object (Viewport));
   end Get_View_Window;

   ---------------------
   -- Set_Shadow_Type --
   ---------------------

   procedure Set_Shadow_Type
      (Viewport : not null access Gtk_Viewport_Record;
       The_Type : Gtk.Enums.Gtk_Shadow_Type)
   is
      procedure Internal
         (Viewport : System.Address;
          The_Type : Gtk.Enums.Gtk_Shadow_Type);
      pragma Import (C, Internal, "gtk_viewport_set_shadow_type");
   begin
      Internal (Get_Object (Viewport), The_Type);
   end Set_Shadow_Type;

   ----------------
   -- Get_Border --
   ----------------

   function Get_Border
      (Self   : not null access Gtk_Viewport_Record;
       Border : access Gtk.Style.Gtk_Border) return Boolean
   is
      function Internal
         (Self       : System.Address;
          Acc_Border : access Gtk.Style.Gtk_Border) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_scrollable_get_border");
      Acc_Border     : aliased Gtk.Style.Gtk_Border;
      Tmp_Acc_Border : aliased Gtk.Style.Gtk_Border;
      Tmp_Return     : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Acc_Border'Access);
      Acc_Border := Tmp_Acc_Border;
      Border.all := Acc_Border;
      return Tmp_Return /= 0;
   end Get_Border;

   ---------------------
   -- Get_Hadjustment --
   ---------------------

   function Get_Hadjustment
      (Self : not null access Gtk_Viewport_Record)
       return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scrollable_get_hadjustment");
      Stub_Gtk_Adjustment : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Adjustment));
   end Get_Hadjustment;

   ------------------------
   -- Get_Hscroll_Policy --
   ------------------------

   function Get_Hscroll_Policy
      (Self : not null access Gtk_Viewport_Record)
       return Gtk.Enums.Gtk_Scrollable_Policy
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Scrollable_Policy;
      pragma Import (C, Internal, "gtk_scrollable_get_hscroll_policy");
   begin
      return Internal (Get_Object (Self));
   end Get_Hscroll_Policy;

   ---------------------
   -- Get_Vadjustment --
   ---------------------

   function Get_Vadjustment
      (Self : not null access Gtk_Viewport_Record)
       return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scrollable_get_vadjustment");
      Stub_Gtk_Adjustment : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Adjustment));
   end Get_Vadjustment;

   ------------------------
   -- Get_Vscroll_Policy --
   ------------------------

   function Get_Vscroll_Policy
      (Self : not null access Gtk_Viewport_Record)
       return Gtk.Enums.Gtk_Scrollable_Policy
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Scrollable_Policy;
      pragma Import (C, Internal, "gtk_scrollable_get_vscroll_policy");
   begin
      return Internal (Get_Object (Self));
   end Get_Vscroll_Policy;

   ---------------------
   -- Set_Hadjustment --
   ---------------------

   procedure Set_Hadjustment
      (Self        : not null access Gtk_Viewport_Record;
       Hadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (Self        : System.Address;
          Hadjustment : System.Address);
      pragma Import (C, Internal, "gtk_scrollable_set_hadjustment");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Hadjustment)));
   end Set_Hadjustment;

   ------------------------
   -- Set_Hscroll_Policy --
   ------------------------

   procedure Set_Hscroll_Policy
      (Self   : not null access Gtk_Viewport_Record;
       Policy : Gtk.Enums.Gtk_Scrollable_Policy)
   is
      procedure Internal
         (Self   : System.Address;
          Policy : Gtk.Enums.Gtk_Scrollable_Policy);
      pragma Import (C, Internal, "gtk_scrollable_set_hscroll_policy");
   begin
      Internal (Get_Object (Self), Policy);
   end Set_Hscroll_Policy;

   ---------------------
   -- Set_Vadjustment --
   ---------------------

   procedure Set_Vadjustment
      (Self        : not null access Gtk_Viewport_Record;
       Vadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (Self        : System.Address;
          Vadjustment : System.Address);
      pragma Import (C, Internal, "gtk_scrollable_set_vadjustment");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Vadjustment)));
   end Set_Vadjustment;

   ------------------------
   -- Set_Vscroll_Policy --
   ------------------------

   procedure Set_Vscroll_Policy
      (Self   : not null access Gtk_Viewport_Record;
       Policy : Gtk.Enums.Gtk_Scrollable_Policy)
   is
      procedure Internal
         (Self   : System.Address;
          Policy : Gtk.Enums.Gtk_Scrollable_Policy);
      pragma Import (C, Internal, "gtk_scrollable_set_vscroll_policy");
   begin
      Internal (Get_Object (Self), Policy);
   end Set_Vscroll_Policy;

end Gtk.Viewport;
