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
   -- Destroy --
   -------------

   procedure Destroy (Window : in out Gdk_Window) is
      procedure Internal (Window : in Gdk_Window);
      pragma Import (C, Internal, "gdk_window_destroy");
   begin
      Internal (Window);
      Window := Null_Window;
   end Destroy;

   -----------------
   -- Foreign_New --
   -----------------

   procedure Foreign_New
     (Window :    out Gdk_Window;
      An_Id  : in     Guint32)
   is
      function Internal (An_Id : in Guint32) return Gdk_Window;
      pragma Import (C, Internal, "gdk_window_foreign_new");
   begin
      Window := Internal (An_Id);
   end Foreign_New;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New
     (Window          :    out Gdk_Window;
      Parent          : in     Gdk_Window;
      Attributes      : in     Gdk.Window_Attr.Gdk_Window_Attr;
      Attributes_Mask : in     Gdk.Types.Gdk_Window_Attributes_Type)
   is
      function Internal
        (Parent          : in Gdk_Window;
         Attributes      : in Gdk.Window_Attr.Gdk_Window_Attr;
         Attributes_Mask : in Gdk.Types.Gdk_Window_Attributes_Type)
         return Gdk_Window;
      pragma Import (C, Internal, "gdk_window_new");

   begin
      Window := Internal (Parent => Parent,
                          Attributes => Attributes,
                          Attributes_Mask => Attributes_Mask);
   end Gdk_New;

   ------------------
   -- Get_Children --
   ------------------

   function Get_Children
     (Window : in Gdk_Window) return Gdk_Window_List.Glist
   is
      function Internal (Window : in Gdk_Window) return System.Address;
      pragma Import (C, Internal, "gdk_window_get_children");
      Result : Gdk_Window_List.Glist;

   begin
      Gdk_Window_List.Set_Object (Result, Internal (Window));
      return Result;
   end Get_Children;

   ------------------------------
   -- Get_Desk_Relative_Origin --
   ------------------------------

   procedure Get_Desk_Relative_Origin
     (Window  : in     Gdk_Window;
      X       :    out Gint;
      Y       :    out Gint;
      Success :    out Boolean)
   is
      function Internal
        (Window : in Gdk_Window; X, Y : in System.Address) return Gboolean;
      pragma Import (C, Internal, "gdk_window_get_deskrelative_origin");

      Result : Gboolean;
      X_Out, Y_Out : aliased Gint;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

   begin
      Result := Internal (Window, X_Out'Address, Y_Out'Address);
      X := X_Out;
      Y := Y_Out;
      Success := To_Boolean (Result);
   end Get_Desk_Relative_Origin;

   ----------------
   -- Get_Origin --
   ----------------

   procedure Get_Origin
     (Window  : in     Gdk_Window;
      X       :    out Gint;
      Y       :    out Gint;
      Success :    out Boolean)
   is
      function Internal
        (Window : in Gdk_Window;
         X, Y   : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_window_get_origin");

      X_Out, Y_Out : aliased Gint;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

   begin
      Success :=
        To_Boolean (Internal (Window, X_Out'Address, Y_Out'Address));
      X := X_Out;
      Y := Y_Out;
   end Get_Origin;

   -----------------
   -- Get_Pointer --
   -----------------

   procedure Get_Pointer
     (Window : in     Gdk_Window;
      X      :    out Gint;
      Y      :    out Gint;
      Mask   :    out Gdk.Types.Gdk_Modifier_Type;
      Result :    out Gdk_Window)
   is
      function Internal
        (Window : in Gdk_Window;
         X      : in System.Address;
         Y      : in System.Address;
         Mask   : in System.Address) return Gdk_Window;
      pragma Import (C, Internal, "gdk_window_get_pointer");

      X_Out, Y_Out : aliased Gint;
      Mask_Out : aliased Gdk.Types.Gdk_Modifier_Type;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

   begin
      Result :=
        Internal (Window, X_Out'Address, Y_Out'Address, Mask_Out'Address);
      X := X_Out;
      Y := Y_Out;
      Mask := Mask_Out;
   end Get_Pointer;

   -------------------
   -- Get_Toplevels --
   -------------------

   function Get_Toplevels return Gdk_Window_List.Glist is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_window_get_toplevels");
      Result : Gdk_Window_List.Glist;

   begin
      Gdk_Window_List.Set_Object (Result, Internal);
      return Result;
   end Get_Toplevels;

   ----------------
   -- Is_Viewable --
   -----------------

   function Is_Viewable (Window : in Gdk_Window) return Boolean is
      function Internal (Window : in Gdk_Window) return Gboolean;
      pragma Import (C, Internal, "gdk_window_is_viewable");
   begin
      return Boolean'Val (Internal (Window));
   end Is_Viewable;

   ----------------
   -- Is_Visible --
   ----------------

   function Is_Visible (Window : in Gdk_Window) return Boolean is
      function Internal (Window : in Gdk_Window) return Gboolean;
      pragma Import (C, Internal, "gdk_window_is_visible");
   begin
      return Boolean'Val (Internal (Window));
   end Is_Visible;

   ---------------------
   -- Set_Back_Pixmap --
   ---------------------

   procedure Set_Back_Pixmap
     (Window          : Gdk_Window;
      Pixmap          : Gdk.Gdk_Pixmap;
      Parent_Relative : Gint)
   is
      procedure Internal
        (Window : Gdk_Window; Pixmap : Gdk.Gdk_Pixmap; Relative : Gint);
      pragma Import (C, Internal, "gdk_window_set_back_pixmap");

   begin
      Internal (Window, Pixmap, Parent_Relative);
   end Set_Back_Pixmap;

   procedure Set_Back_Pixmap
     (Window          : Gdk_Window;
      Pixmap          : Gdk.Gdk_Pixmap;
      Parent_Relative : Boolean) is
   begin
      Set_Back_Pixmap (Window, Pixmap, Boolean'Pos (Parent_Relative));
   end Set_Back_Pixmap;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
     (Window : in Gdk_Window;
      Color  : in Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Window : in Gdk_Window; Color  : in System.Address);
      pragma Import (C, Internal, "gdk_window_set_background");
      use type Gdk.Color.Gdk_Color;

      Col : aliased Gdk.Color.Gdk_Color := Color;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
      Color_A : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Window, Color_A);
   end Set_Background;

   -------------------
   -- Set_Icon_Name --
   -------------------

   procedure Set_Icon_Name (Window : in Gdk_Window; Name : in String) is
      procedure Internal (Window : in Gdk_Window; Name : in String);
      pragma Import (C, Internal, "gdk_window_set_icon_name");

   begin
      Internal (Window, Name & ASCII.NUL);
   end Set_Icon_Name;

   ---------------------------
   -- Set_Override_Redirect --
   ---------------------------

   procedure Set_Override_Redirect
     (Window            : in Gdk_Window;
      Override_Redirect : in Boolean := True)
   is
      procedure Internal
        (Window : in Gdk_Window; Override_Redirect : in Gboolean);
      pragma Import (C, Internal, "gdk_window_set_override_redirect");

   begin
      Internal (Window, Boolean'Pos (Override_Redirect));
   end Set_Override_Redirect;

   --------------
   -- Set_Role --
   --------------

   procedure Set_Role (Window : in Gdk_Window; Role : in String) is
      procedure Internal (Window : in Gdk_Window; Role : in String);
      pragma Import (C, Internal, "gdk_window_set_role");

   begin
      Internal (Window, Role & ASCII.NUL);
   end Set_Role;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title (Window : in Gdk_Window; Title  : in String) is
      procedure Internal (Window : in Gdk_Window; Title  : in String);
      pragma Import (C, Internal, "gdk_window_set_title");
   begin
      Internal (Window, Title & ASCII.NUL);
   end Set_Title;

   -----------------------
   -- Window_At_Pointer --
   -----------------------

   procedure Window_At_Pointer
     (Win_X  : out Gint;
      Win_Y  : out Gint;
      Window : out Gdk_Window)
   is
      function Internal
        (Win_X  : in System.Address;
         Win_Y  : in System.Address) return Gdk_Window;
      pragma Import (C, Internal, "gdk_window_at_pointer");

      X_Out, Y_Out : aliased Gint;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

   begin
      Window := Internal (X_Out'Address, Y_Out'Address);
      Win_X := X_Out;
      Win_Y := Y_Out;
   end Window_At_Pointer;

end Gdk.Window;
