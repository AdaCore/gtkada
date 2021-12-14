------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2021, AdaCore                     --
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

with System;

with Glib.Object; use Glib.Object;

with Gtk;    use Gtk;

package body Gdk.Dnd is

   -----------------
   -- Drag_Status --
   -----------------

   procedure Drag_Status
     (Context : Drag_Context;
      Action  : Drag_Action;
      Time    : Guint32)
   is
      procedure Internal
        (Context : System.Address;
         Action  : Gint;
         Time    : Guint32);
      pragma Import (C, Internal, "gdk_drag_status");
   begin
      Internal (Get_Object (Context),
                Drag_Action'Pos (Action),
                Time);
   end Drag_Status;

   ----------------
   -- Drop_Reply --
   ----------------

   procedure Drop_Reply
     (Context : Drag_Context;
      Ok      : Boolean;
      Time    : Guint32)
   is
      procedure Internal
        (Context : System.Address;
         Ok      : Gint;
         Time    : Guint32);
      pragma Import (C, Internal, "gdk_drop_reply");
   begin
      Internal (Get_Object (Context),
                Boolean'Pos (Ok),
                Time);
   end Drop_Reply;

   -----------------
   -- Drop_Finish --
   -----------------

   procedure Drop_Finish
     (Context : Drag_Context;
      Success : Boolean;
      Time    : Guint32)
   is
      procedure Internal
        (Context : System.Address;
         Success : Gint;
         Time    : Guint32);
      pragma Import (C, Internal, "gdk_drop_finish");
   begin
      Internal (Get_Object (Context),
                Boolean'Pos (Success),
                Time);
   end Drop_Finish;

   ------------------------
   -- Drag_Get_Selection --
   ------------------------

   function Drag_Get_Selection (Context : Drag_Context)
                                return Gdk_Atom
   is
      function Internal (Context : System.Address)
                         return Gdk_Atom;
      pragma Import (C, Internal, "gdk_drag_get_selection");
   begin
      return Internal (Get_Object (Context));
   end Drag_Get_Selection;

   ----------------
   -- Drag_Begin --
   ----------------

   function Drag_Begin
     (Window  : Gdk.Gdk_Window;
      Targets : Gtk.Target_List.Gtk_Target_List)
      return Drag_Context
   is
      function Internal
        (Window  : Gdk_Window;
         Targets : System.Address)
         return System.Address;
      pragma Import (C, Internal, "gdk_drag_begin");
      Stub : Gdk.Drag_Contexts.Drag_Context_Record;
   begin
      return Drag_Context
        (Get_User_Data (Internal (Window, Targets.Get_Object), Stub));
   end Drag_Begin;

   -----------------------
   -- Drag_Get_Protocol --
   -----------------------

   function Drag_Context_Get_Protocol
     (Context  : Drag_Context;
      Protocol : Drag_Protocol)
      return Drag_Protocol
   is
      function Internal
        (Context  : System.Address;
         Protocol : Drag_Protocol)
         return Drag_Protocol;
      pragma Import (C, Internal, "gdk_drag_context_get_protocol");
   begin
      return Internal (Get_Object (Context), Protocol);
   end Drag_Context_Get_Protocol;

   ----------------------
   -- Drag_Find_Window --
   ----------------------

   procedure Drag_Find_Window_For_Screen
     (Context     : Drag_Context;
      Drag_Window : Gdk.Gdk_Window;
      Screen      : Gdk.Screen.Gdk_Screen;
      X_Root      : Gint;
      Y_Root      : Gint;
      Dest_Window : Gdk.Gdk_Window;
      Protocol    : Drag_Protocol)
   is
      procedure Internal
        (Context     : System.Address;
         Drag_Window : Gdk_Window;
         Screen      : System.Address;
         X_Root      : Gint;
         Y_Root      : Gint;
         Dest_Window : Gdk_Window;
         Protocol    : Drag_Protocol);
      pragma Import (C, Internal, "gdk_drag_find_window_for_screen");
   begin
      Internal (Get_Object (Context),
                Drag_Window,
                Get_Object (Screen),
                X_Root,
                Y_Root,
                Dest_Window,
                Protocol);
   end Drag_Find_Window_For_Screen;

   -----------------
   -- Drag_Motion --
   -----------------

   function Drag_Motion
     (Context          : Drag_Context;
      Dest_Window      : Gdk.Gdk_Window;
      Protocol         : Drag_Protocol;
      X_Root           : Gint;
      Y_Root           : Gint;
      Suggested_Action : Drag_Action;
      Possible_Actions : Drag_Action;
      Time             : Guint32)
      return Boolean
   is
      function Internal
        (Context          : System.Address;
         Dest_Window      : Gdk_Window;
         Protocol         : Gint;
         X_Root           : Gint;
         Y_Root           : Gint;
         Suggested_Action : Gint;
         Possible_Actions : Gint;
         Time             : Guint32)
         return Gint;
      pragma Import (C, Internal, "gdk_drag_motion");
   begin
      return Boolean'Val (Internal (Get_Object (Context),
                                    Dest_Window,
                                    Drag_Protocol'Pos (Protocol),
                                    X_Root,
                                    Y_Root,
                                    Drag_Action'Pos (Suggested_Action),
                                    Drag_Action'Pos (Possible_Actions),
                                    Time));
   end Drag_Motion;

   ---------------
   -- Drag_Drop --
   ---------------

   procedure Drag_Drop
     (Context : Drag_Context;
      Time    : Guint32)
   is
      procedure Internal
        (Context : System.Address;
         Time    : Guint32);
      pragma Import (C, Internal, "gdk_drag_drop");
   begin
      Internal (Get_Object (Context), Time);
   end Drag_Drop;

   ----------------
   -- Drag_Abort --
   ----------------

   procedure Drag_Abort
     (Context : Drag_Context;
      Time    : Guint32)
   is
      procedure Internal
        (Context : System.Address;
         Time    : Guint32);
      pragma Import (C, Internal, "gdk_drag_abort");
   begin
      Internal (Get_Object (Context), Time);
   end Drag_Abort;

   -----------------
   -- Get_Targets --
   -----------------

   function Get_Targets (Context : Drag_Context) return Gdk_Atom_Array is
      function Targets_Count (Context : System.Address) return Guint;
      pragma Import (C, Targets_Count, "ada_gtk_dnd_context_targets_count");

      procedure Internal (Context : System.Address; Result : Gdk_Atom_Array);
      pragma Import (C, Internal, "ada_gtk_dnd_context_get_targets");

      Length : constant Natural := Natural
        (Targets_Count (Get_Object (Context)));
      Result : Gdk_Atom_Array (0 .. Length - 1);
   begin
      Internal (Get_Object (Context), Result);
      return Result;
   end Get_Targets;

end Gdk.Dnd;
