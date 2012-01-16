------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

package body Gtk.Layout is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Layout_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Layout      : out Gtk_Layout;
       Hadjustment : Gtk.Adjustment.Gtk_Adjustment := null;
       Vadjustment : Gtk.Adjustment.Gtk_Adjustment := null)
   is
   begin
      Layout := new Gtk_Layout_Record;
      Gtk.Layout.Initialize (Layout, Hadjustment, Vadjustment);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Layout      : access Gtk_Layout_Record'Class;
       Hadjustment : Gtk.Adjustment.Gtk_Adjustment := null;
       Vadjustment : Gtk.Adjustment.Gtk_Adjustment := null)
   is
      function Internal
         (Hadjustment : System.Address;
          Vadjustment : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_layout_new");
   begin
      Set_Object (Layout, Internal (Get_Object_Or_Null (GObject (Hadjustment)), Get_Object_Or_Null (GObject (Vadjustment))));
   end Initialize;

   --------------------
   -- Get_Bin_Window --
   --------------------

   function Get_Bin_Window
      (Layout : access Gtk_Layout_Record) return Gdk.Window.Gdk_Window
   is
      function Internal
         (Layout : System.Address) return Gdk.Window.Gdk_Window;
      pragma Import (C, Internal, "gtk_layout_get_bin_window");
   begin
      return Internal (Get_Object (Layout));
   end Get_Bin_Window;

   ---------------------
   -- Get_Hadjustment --
   ---------------------

   function Get_Hadjustment
      (Layout : access Gtk_Layout_Record)
       return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Layout : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_layout_get_hadjustment");
      Stub_Gtk_Adjustment : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment (Get_User_Data (Internal (Get_Object (Layout)), Stub_Gtk_Adjustment));
   end Get_Hadjustment;

   --------------
   -- Get_Size --
   --------------

   procedure Get_Size
      (Layout : access Gtk_Layout_Record;
       Width  : out Guint;
       Height : out Guint)
   is
      procedure Internal
         (Layout : System.Address;
          Width  : out Guint;
          Height : out Guint);
      pragma Import (C, Internal, "gtk_layout_get_size");
   begin
      Internal (Get_Object (Layout), Width, Height);
   end Get_Size;

   ---------------------
   -- Get_Vadjustment --
   ---------------------

   function Get_Vadjustment
      (Layout : access Gtk_Layout_Record)
       return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Layout : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_layout_get_vadjustment");
      Stub_Gtk_Adjustment : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment (Get_User_Data (Internal (Get_Object (Layout)), Stub_Gtk_Adjustment));
   end Get_Vadjustment;

   ----------
   -- Move --
   ----------

   procedure Move
      (Layout       : access Gtk_Layout_Record;
       Child_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
       X            : Gint;
       Y            : Gint)
   is
      procedure Internal
         (Layout       : System.Address;
          Child_Widget : System.Address;
          X            : Gint;
          Y            : Gint);
      pragma Import (C, Internal, "gtk_layout_move");
   begin
      Internal (Get_Object (Layout), Get_Object (Child_Widget), X, Y);
   end Move;

   ---------
   -- Put --
   ---------

   procedure Put
      (Layout       : access Gtk_Layout_Record;
       Child_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
       X            : Gint;
       Y            : Gint)
   is
      procedure Internal
         (Layout       : System.Address;
          Child_Widget : System.Address;
          X            : Gint;
          Y            : Gint);
      pragma Import (C, Internal, "gtk_layout_put");
   begin
      Internal (Get_Object (Layout), Get_Object (Child_Widget), X, Y);
   end Put;

   ---------------------
   -- Set_Hadjustment --
   ---------------------

   procedure Set_Hadjustment
      (Layout     : access Gtk_Layout_Record;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (Layout     : System.Address;
          Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_layout_set_hadjustment");
   begin
      Internal (Get_Object (Layout), Get_Object (Adjustment));
   end Set_Hadjustment;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size
      (Layout : access Gtk_Layout_Record;
       Width  : Guint;
       Height : Guint)
   is
      procedure Internal
         (Layout : System.Address;
          Width  : Guint;
          Height : Guint);
      pragma Import (C, Internal, "gtk_layout_set_size");
   begin
      Internal (Get_Object (Layout), Width, Height);
   end Set_Size;

   ---------------------
   -- Set_Vadjustment --
   ---------------------

   procedure Set_Vadjustment
      (Layout     : access Gtk_Layout_Record;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (Layout     : System.Address;
          Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_layout_set_vadjustment");
   begin
      Internal (Get_Object (Layout), Get_Object (Adjustment));
   end Set_Vadjustment;

   ---------------------
   -- Get_Hadjustment --
   ---------------------

   function Get_Hadjustment
      (Self : access Gtk_Layout_Record) return Gtk.Adjustment.Gtk_Adjustment
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
      (Self : access Gtk_Layout_Record)
       return Gtk.Enums.Gtk_Scrollable_Policy
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_scrollable_get_hscroll_policy");
   begin
      return Gtk.Enums.Gtk_Scrollable_Policy'Val (Internal (Get_Object (Self)));
   end Get_Hscroll_Policy;

   ---------------------
   -- Get_Vadjustment --
   ---------------------

   function Get_Vadjustment
      (Self : access Gtk_Layout_Record) return Gtk.Adjustment.Gtk_Adjustment
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
      (Self : access Gtk_Layout_Record)
       return Gtk.Enums.Gtk_Scrollable_Policy
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_scrollable_get_vscroll_policy");
   begin
      return Gtk.Enums.Gtk_Scrollable_Policy'Val (Internal (Get_Object (Self)));
   end Get_Vscroll_Policy;

   ---------------------
   -- Set_Hadjustment --
   ---------------------

   procedure Set_Hadjustment
      (Self        : access Gtk_Layout_Record;
       Hadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (Self        : System.Address;
          Hadjustment : System.Address);
      pragma Import (C, Internal, "gtk_scrollable_set_hadjustment");
   begin
      Internal (Get_Object (Self), Get_Object (Hadjustment));
   end Set_Hadjustment;

   ------------------------
   -- Set_Hscroll_Policy --
   ------------------------

   procedure Set_Hscroll_Policy
      (Self   : access Gtk_Layout_Record;
       Policy : Gtk.Enums.Gtk_Scrollable_Policy)
   is
      procedure Internal (Self : System.Address; Policy : Integer);
      pragma Import (C, Internal, "gtk_scrollable_set_hscroll_policy");
   begin
      Internal (Get_Object (Self), Gtk.Enums.Gtk_Scrollable_Policy'Pos (Policy));
   end Set_Hscroll_Policy;

   ---------------------
   -- Set_Vadjustment --
   ---------------------

   procedure Set_Vadjustment
      (Self        : access Gtk_Layout_Record;
       Vadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (Self        : System.Address;
          Vadjustment : System.Address);
      pragma Import (C, Internal, "gtk_scrollable_set_vadjustment");
   begin
      Internal (Get_Object (Self), Get_Object (Vadjustment));
   end Set_Vadjustment;

   ------------------------
   -- Set_Vscroll_Policy --
   ------------------------

   procedure Set_Vscroll_Policy
      (Self   : access Gtk_Layout_Record;
       Policy : Gtk.Enums.Gtk_Scrollable_Policy)
   is
      procedure Internal (Self : System.Address; Policy : Integer);
      pragma Import (C, Internal, "gtk_scrollable_set_vscroll_policy");
   begin
      Internal (Get_Object (Self), Gtk.Enums.Gtk_Scrollable_Policy'Pos (Policy));
   end Set_Vscroll_Policy;

end Gtk.Layout;
