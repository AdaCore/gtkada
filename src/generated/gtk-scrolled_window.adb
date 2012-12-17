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

package body Gtk.Scrolled_Window is

   package Type_Conversion_Gtk_Scrolled_Window is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Scrolled_Window_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Scrolled_Window);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Scrolled_Window : out Gtk_Scrolled_Window;
       Hadjustment     : Gtk.Adjustment.Gtk_Adjustment := null;
       Vadjustment     : Gtk.Adjustment.Gtk_Adjustment := null)
   is
   begin
      Scrolled_Window := new Gtk_Scrolled_Window_Record;
      Gtk.Scrolled_Window.Initialize (Scrolled_Window, Hadjustment, Vadjustment);
   end Gtk_New;

   -----------------------------
   -- Gtk_Scrolled_Window_New --
   -----------------------------

   function Gtk_Scrolled_Window_New
      (Hadjustment : Gtk.Adjustment.Gtk_Adjustment := null;
       Vadjustment : Gtk.Adjustment.Gtk_Adjustment := null)
       return Gtk_Scrolled_Window
   is
      Scrolled_Window : constant Gtk_Scrolled_Window := new Gtk_Scrolled_Window_Record;
   begin
      Gtk.Scrolled_Window.Initialize (Scrolled_Window, Hadjustment, Vadjustment);
      return Scrolled_Window;
   end Gtk_Scrolled_Window_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record'Class;
       Hadjustment     : Gtk.Adjustment.Gtk_Adjustment := null;
       Vadjustment     : Gtk.Adjustment.Gtk_Adjustment := null)
   is
      function Internal
         (Hadjustment : System.Address;
          Vadjustment : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_new");
   begin
      Set_Object (Scrolled_Window, Internal (Get_Object_Or_Null (GObject (Hadjustment)), Get_Object_Or_Null (GObject (Vadjustment))));
   end Initialize;

   -----------------------
   -- Add_With_Viewport --
   -----------------------

   procedure Add_With_Viewport
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Child           : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Scrolled_Window : System.Address;
          Child           : System.Address);
      pragma Import (C, Internal, "gtk_scrolled_window_add_with_viewport");
   begin
      Internal (Get_Object (Scrolled_Window), Get_Object (Child));
   end Add_With_Viewport;

   ---------------------
   -- Get_Hadjustment --
   ---------------------

   function Get_Hadjustment
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal
         (Scrolled_Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_get_hadjustment");
      Stub_Gtk_Adjustment : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment (Get_User_Data (Internal (Get_Object (Scrolled_Window)), Stub_Gtk_Adjustment));
   end Get_Hadjustment;

   --------------------
   -- Get_Hscrollbar --
   --------------------

   function Get_Hscrollbar
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Scrollbar.Gtk_Scrollbar
   is
      function Internal
         (Scrolled_Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_get_hscrollbar");
      Stub_Gtk_Scrollbar : Gtk.Scrollbar.Gtk_Scrollbar_Record;
   begin
      return Gtk.Scrollbar.Gtk_Scrollbar (Get_User_Data (Internal (Get_Object (Scrolled_Window)), Stub_Gtk_Scrollbar));
   end Get_Hscrollbar;

   ----------------------------
   -- Get_Min_Content_Height --
   ----------------------------

   function Get_Min_Content_Height
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gint
   is
      function Internal (Scrolled_Window : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_scrolled_window_get_min_content_height");
   begin
      return Internal (Get_Object (Scrolled_Window));
   end Get_Min_Content_Height;

   ---------------------------
   -- Get_Min_Content_Width --
   ---------------------------

   function Get_Min_Content_Width
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gint
   is
      function Internal (Scrolled_Window : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_scrolled_window_get_min_content_width");
   begin
      return Internal (Get_Object (Scrolled_Window));
   end Get_Min_Content_Width;

   -------------------
   -- Get_Placement --
   -------------------

   function Get_Placement
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Enums.Gtk_Corner_Type
   is
      function Internal
         (Scrolled_Window : System.Address) return Gtk.Enums.Gtk_Corner_Type;
      pragma Import (C, Internal, "gtk_scrolled_window_get_placement");
   begin
      return Internal (Get_Object (Scrolled_Window));
   end Get_Placement;

   ----------------
   -- Get_Policy --
   ----------------

   procedure Get_Policy
      (Scrolled_Window   : not null access Gtk_Scrolled_Window_Record;
       Hscrollbar_Policy : out Gtk.Enums.Gtk_Policy_Type;
       Vscrollbar_Policy : out Gtk.Enums.Gtk_Policy_Type)
   is
      procedure Internal
         (Scrolled_Window   : System.Address;
          Hscrollbar_Policy : out Gtk.Enums.Gtk_Policy_Type;
          Vscrollbar_Policy : out Gtk.Enums.Gtk_Policy_Type);
      pragma Import (C, Internal, "gtk_scrolled_window_get_policy");
   begin
      Internal (Get_Object (Scrolled_Window), Hscrollbar_Policy, Vscrollbar_Policy);
   end Get_Policy;

   ---------------------
   -- Get_Shadow_Type --
   ---------------------

   function Get_Shadow_Type
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Enums.Gtk_Shadow_Type
   is
      function Internal
         (Scrolled_Window : System.Address) return Gtk.Enums.Gtk_Shadow_Type;
      pragma Import (C, Internal, "gtk_scrolled_window_get_shadow_type");
   begin
      return Internal (Get_Object (Scrolled_Window));
   end Get_Shadow_Type;

   ---------------------
   -- Get_Vadjustment --
   ---------------------

   function Get_Vadjustment
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal
         (Scrolled_Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_get_vadjustment");
      Stub_Gtk_Adjustment : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment (Get_User_Data (Internal (Get_Object (Scrolled_Window)), Stub_Gtk_Adjustment));
   end Get_Vadjustment;

   --------------------
   -- Get_Vscrollbar --
   --------------------

   function Get_Vscrollbar
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Scrollbar.Gtk_Scrollbar
   is
      function Internal
         (Scrolled_Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_get_vscrollbar");
      Stub_Gtk_Scrollbar : Gtk.Scrollbar.Gtk_Scrollbar_Record;
   begin
      return Gtk.Scrollbar.Gtk_Scrollbar (Get_User_Data (Internal (Get_Object (Scrolled_Window)), Stub_Gtk_Scrollbar));
   end Get_Vscrollbar;

   ---------------------
   -- Set_Hadjustment --
   ---------------------

   procedure Set_Hadjustment
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Hadjustment     : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
      
   is
      procedure Internal
         (Scrolled_Window : System.Address;
          Hadjustment     : System.Address);
      pragma Import (C, Internal, "gtk_scrolled_window_set_hadjustment");
   begin
      Internal (Get_Object (Scrolled_Window), Get_Object (Hadjustment));
   end Set_Hadjustment;

   ----------------------------
   -- Set_Min_Content_Height --
   ----------------------------

   procedure Set_Min_Content_Height
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Height          : Gint)
   is
      procedure Internal (Scrolled_Window : System.Address; Height : Gint);
      pragma Import (C, Internal, "gtk_scrolled_window_set_min_content_height");
   begin
      Internal (Get_Object (Scrolled_Window), Height);
   end Set_Min_Content_Height;

   ---------------------------
   -- Set_Min_Content_Width --
   ---------------------------

   procedure Set_Min_Content_Width
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Width           : Gint)
   is
      procedure Internal (Scrolled_Window : System.Address; Width : Gint);
      pragma Import (C, Internal, "gtk_scrolled_window_set_min_content_width");
   begin
      Internal (Get_Object (Scrolled_Window), Width);
   end Set_Min_Content_Width;

   -------------------
   -- Set_Placement --
   -------------------

   procedure Set_Placement
      (Scrolled_Window  : not null access Gtk_Scrolled_Window_Record;
       Window_Placement : Gtk.Enums.Gtk_Corner_Type)
   is
      procedure Internal
         (Scrolled_Window  : System.Address;
          Window_Placement : Gtk.Enums.Gtk_Corner_Type);
      pragma Import (C, Internal, "gtk_scrolled_window_set_placement");
   begin
      Internal (Get_Object (Scrolled_Window), Window_Placement);
   end Set_Placement;

   ----------------
   -- Set_Policy --
   ----------------

   procedure Set_Policy
      (Scrolled_Window   : not null access Gtk_Scrolled_Window_Record;
       Hscrollbar_Policy : Gtk.Enums.Gtk_Policy_Type;
       Vscrollbar_Policy : Gtk.Enums.Gtk_Policy_Type)
   is
      procedure Internal
         (Scrolled_Window   : System.Address;
          Hscrollbar_Policy : Gtk.Enums.Gtk_Policy_Type;
          Vscrollbar_Policy : Gtk.Enums.Gtk_Policy_Type);
      pragma Import (C, Internal, "gtk_scrolled_window_set_policy");
   begin
      Internal (Get_Object (Scrolled_Window), Hscrollbar_Policy, Vscrollbar_Policy);
   end Set_Policy;

   ---------------------
   -- Set_Shadow_Type --
   ---------------------

   procedure Set_Shadow_Type
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       The_Type        : Gtk.Enums.Gtk_Shadow_Type)
   is
      procedure Internal
         (Scrolled_Window : System.Address;
          The_Type        : Gtk.Enums.Gtk_Shadow_Type);
      pragma Import (C, Internal, "gtk_scrolled_window_set_shadow_type");
   begin
      Internal (Get_Object (Scrolled_Window), The_Type);
   end Set_Shadow_Type;

   ---------------------
   -- Set_Vadjustment --
   ---------------------

   procedure Set_Vadjustment
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Vadjustment     : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
      
   is
      procedure Internal
         (Scrolled_Window : System.Address;
          Vadjustment     : System.Address);
      pragma Import (C, Internal, "gtk_scrolled_window_set_vadjustment");
   begin
      Internal (Get_Object (Scrolled_Window), Get_Object (Vadjustment));
   end Set_Vadjustment;

   ---------------------
   -- Unset_Placement --
   ---------------------

   procedure Unset_Placement
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
   is
      procedure Internal (Scrolled_Window : System.Address);
      pragma Import (C, Internal, "gtk_scrolled_window_unset_placement");
   begin
      Internal (Get_Object (Scrolled_Window));
   end Unset_Placement;

end Gtk.Scrolled_Window;
