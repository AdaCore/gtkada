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

package body Gdk.Window is


   -------------
   --  Clear  --
   -------------

   procedure Clear (Window : in out Gdk_Window) is
      procedure Internal (Window : in System.Address);
      pragma Import (C, Internal, "gdk_window_clear");
   begin
      Internal (Get_Object (Window));
   end Clear;


   ----------------
   -- Clear_Area --
   ----------------

   procedure Clear_Area (Window : in Gdk_Window;
                         X      : in Gint;
                         Y      : in Gint;
                         Width  : in Gint;
                         Height : in Gint) is
      procedure Internal (Window : in System.Address;
                          X, Y, Width, Height : in Gint);
      pragma Import (C, Internal, "gdk_window_clear_area");
   begin
      Internal (Get_Object (Window), X, Y, Width, Height);
   end Clear_Area;


   --------------------
   --  Clear_Area_E  --
   --------------------

   procedure Clear_Area_E (Window : in Gdk_Window;
                           X      : in Gint;
                           Y      : in Gint;
                           Width  : in Gint;
                           Height : in Gint) is
      procedure Internal (Window : in System.Address;
                          X, Y, Width, Height : in Gint);
      pragma Import (C, Internal, "gdk_window_clear_area_e");
   begin
      Internal (Get_Object (Window), X, Y, Width, Height);
   end Clear_Area_E;


   ---------------
   --  Convert  --
   ---------------

   function Convert (S : in System.Address) return Gdk_Window'Class is
      Result : Gdk_Window;
   begin
      Set_Object (Result, S);
      return Result;
   end Convert;


   ---------------
   --  Convert  --
   ---------------

   function Convert (P : in Gdk_Window'Class) return System.Address is
   begin
      return Get_Object (P);
   end Convert;


   ---------------
   --  Destroy  --
   ---------------

   procedure Destroy (Window : in out Gdk_Window) is
      procedure Internal (Window : in System.Address);
      pragma Import (C, Internal, "gdk_window_destroy");
   begin
      Internal (Get_Object (Window));
      Set_Object (Window, System.Null_Address);
   end Destroy;


   -------------------
   --  Foreign_New  --
   -------------------

   procedure Foreign_New (Window :    out Gdk_Window;
                          An_Id  : in     Guint32) is
      function Internal (An_Id : in Guint32) return System.Address;
      pragma Import (C, Internal, "gdk_window_foreign_new");
   begin
      Set_Object (Window, Internal (An_Id));
   end Foreign_New;


   ---------------
   --  Gdk_New  --
   ---------------

   procedure Gdk_New
     (Window          :    out Gdk_Window;
      Parent          : in     Gdk_Window;
      Attributes      : in     Gdk.Window_Attr.Gdk_Window_Attr;
      Attributes_Mask : in     Gdk.Types.Gdk_Window_Attributes_Type) is
      function Internal
        (Parent : in System.Address;
         Attributes : in System.Address;
         Attributes_Mask : in Gdk.Types.Gdk_Window_Attributes_Type)
         return System.Address;
      pragma Import (C, Internal, "gdk_window_new");
   begin
      Set_Object (Window,
                  Internal (Parent => Get_Object (Parent),
                            Attributes => Get_Object (Attributes),
                            Attributes_Mask => Attributes_Mask));
   end Gdk_New;


   ------------------
   -- Get_Children --
   ------------------

   function Get_Children (Window : in Gdk_Window'Class)
                          return Gdk_Window_List.Glist is
      function Internal (Window : in System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_window_get_children");
      Result : Gdk_Window_List.Glist;
   begin
      Gdk_Window_List.Set_Object (Result, Internal (Get_Object (Window)));
      return Result;
   end Get_Children;


   ------------------
   -- Get_Colormap --
   ------------------

   function Get_Colormap (Window : in Gdk_Window)
                          return Gdk.Color.Gdk_Colormap
   is
      function Internal (Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_window_get_colormap");
      Cmap : Gdk.Color.Gdk_Colormap;
   begin
      Set_Object (Cmap, Internal (Get_Object (Window)));
      return Cmap;
   end Get_Colormap;


   --------------------------------
   --  Get_Desk_Relative_Origin  --
   --------------------------------

   procedure Get_Desk_Relative_Origin (Window  : in     Gdk_Window;
                                       X       :    out Gint;
                                       Y       :    out Gint;
                                       Success :    out Boolean) is
      function Internal (Window : in     System.Address;
                         X, Y   : in     System.Address)
        return Gboolean;
      pragma Import (C, Internal, "gdk_window_get_deskrelative_origin");
      Result : Gboolean;
   begin
      Result := Internal (Get_Object (Window), X'Address, Y'Address);
      Success := To_Boolean (Result);
   end Get_Desk_Relative_Origin;


   ------------------
   --  Get_Events  --
   ------------------

   function Get_Events (Window : in Gdk_Window)
                        return Gdk.Types.Gdk_Event_Mask is
      function Internal (Window : in System.Address)
                         return Gdk.Types.Gdk_Event_Mask;
      pragma Import (C, Internal, "gdk_window_get_events");
   begin
      return Internal (Get_Object (Window));
   end Get_Events;


   --------------------
   --  Get_Geometry  --
   --------------------

   procedure Get_Geometry (Window : in     Gdk_Window;
                           X      :    out Gint;
                           Y      :    out Gint;
                           Width  :    out Gint;
                           Height :    out Gint;
                           Depth  :    out Gint) is
      procedure Internal (Window : in     System.Address;
                          X, Y   :    out Gint;
                          Width  :    out Gint;
                          Height :    out Gint;
                          Depth  :    out Gint);
      pragma Import (C, Internal, "gdk_window_get_geometry");
   begin
      Internal (Get_Object (Window), X, Y, Width, Height, Depth);
   end Get_Geometry;


   ------------------
   --  Get_Origin  --
   ------------------

   procedure Get_Origin (Window  : in     Gdk_Window;
                         X       :    out Gint;
                         Y       :    out Gint;
                         Success :    out Boolean) is
      function Internal (Window : in System.Address;
                         X, Y   : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_window_get_origin");
   begin
      Success := To_Boolean (Internal (Get_Object (Window),
                                       X'Address, Y'Address));
   end Get_Origin;


   -----------------
   --  Get_Parent --
   -----------------

   procedure Get_Parent (Window : in     Gdk_Window;
                         Parent :    out Gdk_Window) is
      function Internal (Window : in System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_window_get_parent");
   begin
      Set_Object (Parent, Internal (Get_Object (Window)));
   end Get_Parent;


   -------------------
   --  Get_Pointer  --
   -------------------

   procedure Get_Pointer (Window : in out Gdk_Window;
                          X      :    out Gint;
                          Y      :    out Gint;
                          Mask   :    out Gdk.Types.Gdk_Modifier_Type;
                          Result :    out Gdk_Window) is
      function Internal (Window : in System.Address;
                         X      : in System.Address;
                         Y      : in System.Address;
                         Mask   : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "gdk_window_get_pointer");
   begin
      Set_Object (Result, Internal (Get_Object (Window),
                                    X'Address,
                                    Y'Address,
                                    Mask'Address));
   end Get_Pointer;


   --------------------
   --  Get_Position  --
   --------------------

   procedure Get_Position (Window : in     Gdk_Window;
                           X      :    out Gint;
                           Y      :    out Gint) is
      procedure Internal (Window : in     System.Address;
                          X, Y   :    out Gint);
      pragma Import (C, Internal, "gdk_window_get_position");
   begin
      Internal (Get_Object (Window), X, Y);
   end Get_Position;


   -----------------------
   --  Get_Root_Origin  --
   -----------------------

   procedure Get_Root_Origin (Window : in     Gdk_Window;
                              X      :    out Gint;
                              Y      :    out Gint) is
      procedure Internal (Window : in     System.Address;
                          X, Y   :    out Gint);
      pragma Import (C, Internal, "gdk_window_get_root_origin");
   begin
      Internal (Get_Object (Window), X, Y);
   end Get_Root_Origin;


   ----------------
   --  Get_Size  --
   ----------------

   procedure Get_Size (Window : in     Gdk_Window;
                       Width  :    out Gint;
                       Height :    out Gint) is
      procedure Internal (Window : in     System.Address;
                          Width  :    out Gint;
                          Height :    out Gint);
      pragma Import (C, Internal, "gdk_window_get_size");
   begin
      Internal (Get_Object (Window), Width, Height);
   end Get_Size;


   --------------------
   --  Get_Toplevel  --
   --------------------

   procedure Get_Toplevel (Window   : in     Gdk_Window;
                           Toplevel :    out Gdk_Window) is
      function Internal (Window : in System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_window_get_toplevel");
   begin
      Set_Object (Toplevel, Internal (Get_Object (Window)));
   end Get_Toplevel;


   ---------------------
   --  Get_Toplevels  --
   ---------------------

   function Get_Toplevels return Gdk_Window_List.Glist is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_window_get_toplevels");
      Result : Gdk_Window_List.Glist;
   begin
      Gdk_Window_List.Set_Object (Result, Internal);
      return Result;
   end Get_Toplevels;


   ----------------
   --  Get_Type  --
   ----------------

   function Get_Type (Window : in Gdk_Window)
                      return Gdk.Types.Gdk_Window_Type is
      function Internal (Window : in System.Address)
                         return Gdk.Types.Gdk_Window_Type;
      pragma Import (C, Internal, "gdk_window_get_type");
   begin
      return Internal (Get_Object (Window));
   end Get_Type;


   ------------------
   --  Get_Visual  --
   ------------------

   function Get_Visual (Window : in Gdk_Window) return Gdk.Visual.Gdk_Visual is
      function Internal (Window : in System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_window_get_visual");
      Result : Gdk.Visual.Gdk_Visual;
   begin
      Set_Object (Result, Internal (Get_Object (Window)));
      return Result;
   end Get_Visual;


   -------------------
   --  Is_Viewable  --
   -------------------

   function Is_Viewable (Window : in Gdk_Window) return Boolean is
      function Internal (Window : in System.Address) return Gboolean;
      pragma Import (C, Internal, "gdk_window_is_viewable");
   begin
      return To_Boolean (Internal (Get_Object (Window)));
   end Is_Viewable;


   ------------------
   --  Is_Visible  --
   ------------------

   function Is_Visible (Window : in Gdk_Window) return Boolean is
      function Internal (Window : in System.Address) return Gboolean;
      pragma Import (C, Internal, "gdk_window_is_visible");
   begin
      return To_Boolean (Internal (Get_Object (Window)));
   end Is_Visible;


   ------------
   --  Hide  --
   ------------

   procedure Hide (Window : in Gdk_Window) is
      procedure Internal (Window : in System.Address);
      pragma Import (C, Internal, "gdk_window_hide");
   begin
      Internal (Get_Object (Window));
   end Hide;


   -------------
   --  Lower  --
   -------------

   procedure Lower (Window : in Gdk_Window) is
      procedure Internal (Window : in System.Address);
      pragma Import (C, Internal, "gdk_window_lower");
   begin
      Internal (Get_Object (Window));
   end Lower;


   --------------------------
   --  Merge_Child_Shapes  --
   --------------------------

   procedure Merge_Child_Shapes (Window : in out Gdk_Window) is
      procedure Internal (Window : in System.Address);
      pragma Import (C, Internal, "gdk_window_merge_child_shapes");
   begin
      Internal (Get_Object (Window));
   end Merge_Child_Shapes;


   ------------
   --  Move  --
   ------------

   procedure Move (Window : in out Gdk_Window;
                   X      : in     Gint;
                   Y      : in     Gint) is
      procedure Internal (Window : in System.Address;
                          X, Y   : in Gint);
      pragma Import (C, Internal, "gdk_window_move");
   begin
      Internal (Get_Object (Window), X, Y);
   end Move;


   -------------------
   --  Move_Resize  --
   -------------------

   procedure Move_Resize (Window : in out Gdk_Window;
                          X      : in     Gint;
                          Y      : in     Gint;
                          Width  : in     Gint;
                          Height : in     Gint) is
      procedure Internal (Window : in System.Address;
                          X, Y   : in Gint;
                          Width  : in Gint;
                          Height : in Gint);
      pragma Import (C, Internal, "gdk_window_move_resize");
   begin
      Internal (Get_Object (Window), X, Y, Width, Height);
   end Move_Resize;


   -------------
   --  Raise  --
   -------------

   procedure Gdk_Raise (Window : in Gdk_Window) is
      procedure Internal (Window : in System.Address);
      pragma Import (C, Internal, "gdk_window_raise");
   begin
      Internal (Get_Object (Window));
   end Gdk_Raise;


   -----------
   --  Ref  --
   -----------

   procedure Ref (Window : in out Gdk_Window) is
      procedure Internal (Window : in System.Address);
      pragma Import (C, Internal, "gdk_window_ref");
      --
      --  gdk_window_ref is actually a function returning
      --  a shallow copy of the given window. As we do not
      --  use the returned value, we declare it as a procedure
      --  here. According to the AARM, this is licit.
   begin
      Internal (Get_Object (Window));
   end Ref;


   ----------------
   --  Reparent  --
   ----------------

   procedure Reparent (Window     : in out Gdk_Window;
                       New_Parent : in     Gdk_Window;
                       X          : in     Gint;
                       Y          : in     Gint) is
      procedure Internal (Window    : in System.Address;
                          New_Paent : in System.Address;
                          X, Y      : in Gint);
      pragma Import (C, Internal, "gdk_window_reparent");
   begin
      Internal (Get_Object (Window), Get_Object (New_Parent), X, Y);
   end Reparent;


   --------------
   --  Resize  --
   --------------

   procedure Resize (Window : in out Gdk_Window;
                     Width  : in     Gint;
                     Height : in     Gint) is
      procedure Internal (Window : in System.Address;
                          Width  : in Gint;
                          Height : in Gint);
      pragma Import (C, Internal, "gdk_window_resize");
   begin
      Internal (Get_Object (Window), Width, Height);
   end Resize;


   ----------------------
   --  Set_Background  --
   ----------------------

   procedure Set_Background (Window : in out Gdk_Window;
                             Color  : in     Gdk.Color.Gdk_Color) is
      procedure Internal (Window : in System.Address;
                          Color  : in System.Address);
      pragma Import (C, Internal, "gdk_window_set_background");
   begin
      Internal (Get_Object (Window), Color'Address);
      --
      --  FIXME: This "'Address" stuff needs to be tested!
      --  FIXME: Another way to proceed is to pass declare the Color
      --  FIXME: parameter to be "in out" in the Internal procedure
      --  FIXME: instead of "in". But semantically, I prefer this
      --  FIXME: solution.
   end Set_Background;


   ------------------------
   --  Set_Child_Shapes  --
   ------------------------

   procedure Set_Child_Shapes (Window : in out Gdk_Window) is
      procedure Internal (Window : in System.Address);
      pragma Import (C, Internal, "gdk_window_set_child_shapes");
   begin
      Internal (Get_Object (Window));
   end Set_Child_Shapes;


   --------------------
   --  Set_Colormap  --
   --------------------

   procedure Set_Colormap (Window   : in out Gdk_Window;
                           Colormap : in     Gdk.Color.Gdk_Colormap) is
      procedure Internal (Window   : in System.Address;
                          Colormap : in System.Address);
      pragma Import (C, Internal, "gdk_window_set_colormap");
   begin
      Internal (Get_Object (Window), Get_Object (Colormap));
   end Set_Colormap;


   ----------------
   -- Set_Cursor --
   ----------------

   procedure Set_Cursor (Window : in out Gdk_Window;
                         Cursor : in     Gdk.Cursor.Gdk_Cursor)
   is
      procedure Internal (Window : in System.Address;
                          Cursor : in System.Address);
      pragma Import (C, Internal, "gdk_window_set_cursor");
   begin
      Internal (Get_Object (Window), Get_Object (Cursor));
   end Set_Cursor;


   ----------------------
   --  Set_Decoration  --
   ----------------------

   procedure Set_Decorations
     (Window      : in out Gdk_Window;
      Decorations : in     Gdk.Types.Gdk_Wm_Decoration) is
      procedure Internal (Window      : in System.Address;
                          Decorations : in Gdk.Types.Gdk_Wm_Decoration);
      pragma Import (C, Internal, "gdk_window_set_decorations");
   begin
      Internal (Get_Object (Window), Decorations);
   end Set_Decorations;


   ------------------
   --  Set_Events  --
   ------------------

   procedure Set_Events (Window     : in out Gdk_Window;
                         Event_Mask : in     Gdk.Types.Gdk_Event_Mask) is
      procedure Internal (Window     : in System.Address;
                          Event_Mask : in Gdk.Types.Gdk_Event_Mask);
      pragma Import (C, Internal, "gdk_window_set_events");
   begin
      Internal (Get_Object (Window), Event_Mask);
   end Set_Events;


   ---------------------
   --  Set_Functions  --
   ---------------------

   procedure Set_Functions (Window    : in out Gdk_Window;
                            Functions : in     Gdk.Types.Gdk_Wm_Function) is
      procedure Internal (Window    : in System.Address;
                          Functions : in Gdk.Types.Gdk_Wm_Function);
      pragma Import (C, Internal, "gdk_window_set_functions");
   begin
      Internal (Get_Object (Window), Functions);
   end Set_Functions;


   --------------------------
   --  Set_Geometry_Hints  --
   --------------------------

   procedure Set_Geometry_Hints
     (Window   : in out Gdk_Window;
      Geometry : in out Gdk.Types.Gdk_Geometry;
      Flags    : in     Gdk.Types.Gdk_Window_Hints) is
      procedure Internal (Window   : in System.Address;
                          Geometry : in out Gdk.Types.Gdk_Geometry;
                          Flags    : in     Gdk.Types.Gdk_Window_Hints);
      pragma Import (C, Internal, "gdk_window_set_geometry_hints");
   begin
      Internal (Get_Object (Window), Geometry, Flags);
   end Set_Geometry_Hints;


   -----------------
   --  Set_Group  --
   -----------------

   procedure Set_Group (Window : in out Gdk_Window;
                        Leader : in     Gdk_Window) is
      procedure Internal (Window, Leader : in System.Address);
      pragma Import (C, Internal, "gdk_window_set_group");
   begin
      Internal (Get_Object (Window), Get_Object (Leader));
   end Set_Group;


   -----------------
   --  Set_Hints  --
   -----------------

   procedure Set_Hints (Window     : in out Gdk_Window;
                        X          : in     Gint;
                        Y          : in     Gint;
                        Min_Width  : in     Gint;
                        Min_Height : in     Gint;
                        Max_Width  : in     Gint;
                        Max_Height : in     Gint;
                        Flags      : in     Gdk.Types.Gdk_Window_Hints) is
      procedure Internal
        (Window                : in System.Address;
         X, Y                  : in Gint;
         Min_Width, Min_Height : in Gint;
         Max_Width, Max_Height : in Gint;
         Flags                 : in Gdk.Types.Gdk_Window_Hints);
      pragma Import (C, Internal, "gdk_window_set_hints");
   begin
      Internal (Get_Object (Window), X, Y, Min_Width, Min_Height,
                Max_Width, Max_Height, Flags);
   end Set_Hints;


   -----------------------------
   --  Set_Override_Redirect  --
   -----------------------------

   procedure Set_Override_Redirect
     (Window            : in out Gdk_Window;
      Override_Redirect : in     Boolean := True) is
      procedure Internal (Window            : in System.Address;
                          Override_Redirect : in Gboolean);
      pragma Import (C, Internal, "gdk_window_set_override_redirect");
   begin
      Internal (Get_Object (Window), To_Gboolean (Override_Redirect));
   end Set_Override_Redirect;


   ----------------
   --  Set_Role  --
   ----------------

   procedure Set_Role (Window : in out Gdk_Window;
                       Role   : in     String) is
      procedure Internal (Window : in System.Address;
                          Role   : in String);
      pragma Import (C, Internal, "gdk_window_set_role");
   begin
      Internal (Get_Object (Window), Role & Ascii.NUL);
   end Set_Role;


   -----------------
   --  Set_Title  --
   -----------------

   procedure Set_Title (Window : in out Gdk_Window;
                        Title  : in     String) is
      procedure Internal (Window : in System.Address;
                          Title  : in String);
      pragma Import (C, Internal, "gdk_window_set_title");
   begin
      Internal (Get_Object (Window), Title & Ascii.NUL);
   end Set_Title;


   -------------------------
   --  Set_Transient_For  --
   -------------------------

   procedure Set_Transient_For (Window : in out Gdk_Window;
                                Leader : in     Gdk_Window) is
      procedure Internal (Window : in System.Address;
                          Leader : in System.Address);
      pragma Import (C, Internal, "gdk_window_set_transient_for");
   begin
      Internal (Get_Object (Window), Get_Object (Leader));
   end Set_Transient_For;


   ------------
   --  Show  --
   ------------

   procedure Show (Window : in Gdk_Window) is
      procedure Internal (Window : in System.Address);
      pragma Import (C, Internal, "gdk_window_show");
   begin
      Internal (Get_Object (Window));
   end Show;


   -------------
   --  Unref  --
   -------------

   procedure Unref (Window : in out Gdk_Window) is
      procedure Internal (Window : in System.Address);
      pragma Import (C, Internal, "gdk_window_unref");
   begin
      Internal (Get_Object (Window));
      Set_Object (Window, System.Null_Address);
   end Unref;


   -----------------------
   -- Window_At_Pointer --
   -----------------------

   procedure Window_At_Pointer (Win_X  : out Gint;
                                Win_Y  : out Gint;
                                Window : out Gdk_Window) is
      function Internal (Win_X  : in System.Address;
                         Win_Y  : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gdk_window_at_pointer");
   begin
      Set_Object (Window, Internal (Win_X'Address,
                                    Win_Y'Address));
   end Window_At_Pointer;


   ----------------
   --  Withdraw  --
   ----------------

   procedure Withdraw (Window : in out Gdk_Window) is
      procedure Internal (Window : in System.Address);
      pragma Import (C, Internal, "gdk_window_withdraw");
   begin
      Internal (Get_Object (Window));
   end Withdraw;

end Gdk.Window;
