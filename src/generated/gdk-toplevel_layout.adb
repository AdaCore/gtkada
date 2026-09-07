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
with Glib.Object; use Glib.Object;

package body Gdk.Toplevel_Layout is

   function From_Object_Free
     (B : access Gdk_Toplevel_Layout'Class) return Gdk_Toplevel_Layout
   is
      Result : constant Gdk_Toplevel_Layout := Gdk_Toplevel_Layout (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gdk_Toplevel_Layout is
      S : Gdk_Toplevel_Layout;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New (Self : out Gdk_Toplevel_Layout) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_toplevel_layout_new");
   begin
      Self.Set_Object (Internal);
   end Gdk_New;

   -----------------------------
   -- Gdk_Toplevel_Layout_New --
   -----------------------------

   function Gdk_Toplevel_Layout_New return Gdk_Toplevel_Layout is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_toplevel_layout_new");
      Self : Gdk_Toplevel_Layout;
   begin
      Self.Set_Object (Internal);
      return Self;
   end Gdk_Toplevel_Layout_New;

   ----------
   -- Copy --
   ----------

   function Copy (Self : Gdk_Toplevel_Layout) return Gdk_Toplevel_Layout is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_toplevel_layout_copy");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Copy;

   -----------
   -- Equal --
   -----------

   function Equal
      (Self  : Gdk_Toplevel_Layout;
       Other : Gdk_Toplevel_Layout) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Other : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_toplevel_layout_equal");
   begin
      return Internal (Get_Object (Self), Get_Object (Other)) /= 0;
   end Equal;

   --------------------
   -- Get_Fullscreen --
   --------------------

   function Get_Fullscreen
      (Self       : Gdk_Toplevel_Layout;
       Fullscreen : out Boolean) return Boolean
   is
      function Internal
         (Self           : System.Address;
          Acc_Fullscreen : access Glib.Gboolean) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_toplevel_layout_get_fullscreen");
      Acc_Fullscreen     : aliased Boolean;
      Tmp_Acc_Fullscreen : aliased Glib.Gboolean;
      Tmp_Return         : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Acc_Fullscreen'Access);
      Acc_Fullscreen := Tmp_Acc_Fullscreen /= 0;
      Fullscreen := Acc_Fullscreen;
      return Tmp_Return /= 0;
   end Get_Fullscreen;

   ----------------------------
   -- Get_Fullscreen_Monitor --
   ----------------------------

   function Get_Fullscreen_Monitor
      (Self : Gdk_Toplevel_Layout) return Gdk.Monitor.Gdk_Monitor
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_toplevel_layout_get_fullscreen_monitor");
      Stub_Gdk_Monitor : Gdk.Monitor.Gdk_Monitor_Record;
   begin
      return Gdk.Monitor.Gdk_Monitor (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Monitor));
   end Get_Fullscreen_Monitor;

   -------------------
   -- Get_Maximized --
   -------------------

   function Get_Maximized
      (Self      : Gdk_Toplevel_Layout;
       Maximized : out Boolean) return Boolean
   is
      function Internal
         (Self          : System.Address;
          Acc_Maximized : access Glib.Gboolean) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_toplevel_layout_get_maximized");
      Acc_Maximized     : aliased Boolean;
      Tmp_Acc_Maximized : aliased Glib.Gboolean;
      Tmp_Return        : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Acc_Maximized'Access);
      Acc_Maximized := Tmp_Acc_Maximized /= 0;
      Maximized := Acc_Maximized;
      return Tmp_Return /= 0;
   end Get_Maximized;

   -------------------
   -- Get_Resizable --
   -------------------

   function Get_Resizable (Self : Gdk_Toplevel_Layout) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_toplevel_layout_get_resizable");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Resizable;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gdk_Toplevel_Layout) return Gdk_Toplevel_Layout is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_toplevel_layout_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   --------------------
   -- Set_Fullscreen --
   --------------------

   procedure Set_Fullscreen
      (Self       : Gdk_Toplevel_Layout;
       Fullscreen : Boolean;
       Monitor    : access Gdk.Monitor.Gdk_Monitor_Record'Class)
   is
      procedure Internal
         (Self       : System.Address;
          Fullscreen : Glib.Gboolean;
          Monitor    : System.Address);
      pragma Import (C, Internal, "gdk_toplevel_layout_set_fullscreen");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Fullscreen), Get_Object_Or_Null (GObject (Monitor)));
   end Set_Fullscreen;

   -------------------
   -- Set_Maximized --
   -------------------

   procedure Set_Maximized (Self : Gdk_Toplevel_Layout; Maximized : Boolean) is
      procedure Internal (Self : System.Address; Maximized : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_toplevel_layout_set_maximized");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Maximized));
   end Set_Maximized;

   -------------------
   -- Set_Resizable --
   -------------------

   procedure Set_Resizable (Self : Gdk_Toplevel_Layout; Resizable : Boolean) is
      procedure Internal (Self : System.Address; Resizable : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_toplevel_layout_set_resizable");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Resizable));
   end Set_Resizable;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gdk_Toplevel_Layout) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_toplevel_layout_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

end Gdk.Toplevel_Layout;
