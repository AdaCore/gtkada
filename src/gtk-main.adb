------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2012, AdaCore                     --
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

with Gdk.Types;               use Gdk.Types;
with Interfaces.C.Strings;    use Interfaces.C.Strings;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body Gtk.Main is

   --------------
   -- Do_Event --
   --------------

   procedure Do_Event (Event : Gdk.Event.Gdk_Event) is
      procedure Internal (Event : System.Address);
      pragma Import (C, Internal, "gtk_main_do_event");

   begin
      Internal (Gdk.Event.To_Address (Event));
   end Do_Event;

   --------------------
   -- Events_Pending --
   --------------------

   function Events_Pending return Boolean is
      function Internal return Gint;
      pragma Import (C, Internal, "gtk_events_pending");
   begin
      return Boolean'Val (Internal);
   end Events_Pending;

   ----------
   -- Init --
   ----------

   gnat_argc : Interfaces.C.int;
   pragma Import (C, gnat_argc);

   gnat_argv : System.Address;
   pragma Import (C, gnat_argv);

   procedure Init is
      procedure Internal (argc : System.Address; argv : System.Address);
      pragma Import (C, Internal, "gtk_init");

   begin
      Internal (gnat_argc'Address, gnat_argv'Address);
   end Init;

   ----------------
   -- Init_Check --
   ----------------

   function Init_Check return Boolean is
      function Internal
        (argc : System.Address; argv : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_init_check");

   begin
      return Boolean'Val (Internal (gnat_argc'Address, gnat_argv'Address));
   end Init_Check;

   ----------------------
   -- Get_Event_Widget --
   ----------------------

   function Get_Event_Widget
     (Event : Gdk.Event.Gdk_Event) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Event : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_get_event_widget");

   begin
      return Gtk.Widget.Convert (Internal (Gdk.Event.To_Address (Event)));
   end Get_Event_Widget;

   --------------
   -- Grab_Add --
   --------------

   procedure Grab_Add (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_grab_add");

   begin
      Internal (Get_Object (Widget));
   end Grab_Add;

   -----------------
   -- Grab_Remove --
   -----------------

   procedure Grab_Remove
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_grab_remove");

   begin
      Internal (Get_Object (Widget));
   end Grab_Remove;

   ----------------------
   -- Grab_Get_Current --
   ----------------------

   function Grab_Get_Current return Gtk.Widget.Gtk_Widget is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_grab_get_current");

   begin
      return Gtk.Widget.Convert (Internal);
   end Grab_Get_Current;

   --------------------
   -- Main_Iteration --
   --------------------

   function Main_Iteration (Blocking : Boolean := True) return Boolean is
      function Internal (Blocking : Gboolean) return Gint;
      pragma Import (C, Internal, "gtk_main_iteration_do");

   begin
      return Boolean'Val (Internal (Boolean'Pos (Blocking)));
   end Main_Iteration;

   ----------
   -- Idle --
   ----------

   package body Idle is

      type Data_Type_Access is access Data_Type;
      type Cb_Record is record
         Func : Callback;
         User_Destroy : Destroy_Callback;
         Data : Data_Type_Access;
      end record;
      type Cb_Record_Access is access Cb_Record;

      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Cb_Record_Access);

      ----------
      -- Free --
      ----------

      procedure Free_Data (D : System.Address) is
         procedure Internal is new Ada.Unchecked_Deallocation
           (Cb_Record, Cb_Record_Access);
         procedure Internal2 is new Ada.Unchecked_Deallocation
           (Data_Type, Data_Type_Access);
         Data : Cb_Record_Access := Convert (D);

      begin
         if Data.User_Destroy /= null then
            Data.User_Destroy (Data.Data.all);
         end if;
         Internal2 (Data.Data);
         Internal (Data);
      end Free_Data;

      ----------------
      -- General_Cb --
      ----------------

      function General_Cb (D : System.Address) return Gint is
         Data : constant Cb_Record_Access := Convert (D);
      begin
         return Boolean'Pos (Data.Func (Data.Data.all));
      end General_Cb;

      ---------
      -- Add --
      ---------

      function Add
        (Cb       : Callback;
         D        : Data_Type;
         Priority : Idle_Priority := Priority_Default_Idle;
         Destroy  : Destroy_Callback := null)
         return Idle_Handler_Id
      is
         function Internal
           (Priority : Idle_Priority;
            Func     : System.Address;
            Marshal  : System.Address;
            Data     : System.Address;
            Destroy  : System.Address) return Idle_Handler_Id;
         pragma Import (C, Internal, "gtk_idle_add_full");

         function Convert is new Ada.Unchecked_Conversion
           (Cb_Record_Access, System.Address);

         Data : constant Cb_Record_Access := new Cb_Record'
           (Func => Cb,
            User_Destroy => Destroy,
            Data => new Data_Type'(D));
      begin
         return Internal (Priority, General_Cb'Address, System.Null_Address,
                          Convert (Data), Free_Data'Address);
      end Add;

   end Idle;

   -------------------
   -- Check_Version --
   -------------------

   function Check_Version
     (Required_Major : Guint := Gtk.Major_Version;
      Required_Minor : Guint := Gtk.Minor_Version;
      Required_Micro : Guint := Gtk.Micro_Version)
      return String
   is
      function Internal
        (Required_Major : Guint;
         Required_Minor : Guint;
         Required_Micro : Guint)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_check_version");
   begin
      return Value (Internal (Required_Major, Required_Minor, Required_Micro));
   end Check_Version;

   -----------------------------
   -- Get_Current_Event_State --
   -----------------------------

   procedure Get_Current_Event_State
     (State             : out Gdk_Modifier_Type;
      Had_Current_Event : out Boolean)
   is
      function Internal (State : access Gdk_Modifier_Type) return Gboolean;
      pragma Import (C, Internal, "gtk_get_current_event_state");
      St : aliased Gdk_Modifier_Type;
   begin
      Had_Current_Event := Boolean'Val (Internal (St'Unchecked_Access));
      State := St;
   end Get_Current_Event_State;

   ---------------------
   -- Propagate_Event --
   ---------------------

   procedure Propagate_Event
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event)
   is
      procedure Internal
        (Widget : System.Address; Event : Gdk.Event.Gdk_Event);
      pragma Import (C, Internal, "gtk_propagate_event");
   begin
      Internal (Get_Object (Widget), Event);
   end Propagate_Event;

end Gtk.Main;
