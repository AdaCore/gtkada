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

--  Monitors a file or directory for changes.
--
--  To obtain a Glib.File_Monitors.Gfile_Monitor for a file or directory, use
--  Glib.GFile.Monitor, Glib.GFile.Monitor_File, or
--  Glib.GFile.Monitor_Directory.
--
--  To get informed about changes to the file or directory you are monitoring,
--  connect to the Glib.File_Monitors.Gfile_Monitor::changed signal. The signal
--  will be emitted in the [thread-default main
--  context][g-main-context-push-thread-default] of the thread that the monitor
--  was created in (though if the global default main context is blocked, this
--  may cause notifications to be blocked even if the thread-default context is
--  still running).

pragma Warnings (Off, "*is already use-visible*");
with Glib.GEnums;     use Glib.GEnums;
with Glib.GFile;      use Glib.GFile;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;

package Glib.File_Monitors is

   type Gfile_Monitor_Record is new GObject_Record with null record;
   type Gfile_Monitor is access all Gfile_Monitor_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_file_monitor_get_type");

   -------------
   -- Methods --
   -------------

   function Cancel
      (Self : not null access Gfile_Monitor_Record) return Boolean;
   --  Cancels a file monitor.
   --  @return always True

   procedure Emit_Event
      (Self       : not null access Gfile_Monitor_Record;
       Child      : Glib.GFile.Gfile;
       Other_File : Glib.GFile.Gfile;
       Event_Type : Glib.GEnums.GFile_Monitor_Event);
   --  Emits the Glib.File_Monitors.Gfile_Monitor::changed signal if a change
   --  has taken place. Should be called from file monitor implementations
   --  only.
   --  Implementations are responsible to call this method from the
   --  [thread-default main context][g-main-context-push-thread-default] of the
   --  thread that the monitor was created in.
   --  @param Child a Glib.GFile.Gfile.
   --  @param Other_File a Glib.GFile.Gfile.
   --  @param Event_Type a set of Glib.GEnums.GFile_Monitor_Event flags.

   function Is_Cancelled
      (Self : not null access Gfile_Monitor_Record) return Boolean;
   --  Returns whether the monitor is canceled.
   --  @return True if monitor is canceled. False otherwise.

   procedure Set_Rate_Limit
      (Self        : not null access Gfile_Monitor_Record;
       Limit_Msecs : Glib.Gint);
   --  Sets the rate limit to which the Monitor will report consecutive change
   --  events to the same file.
   --  @param Limit_Msecs a non-negative integer with the limit in
   --  milliseconds to poll for changes

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Cancelled_Property : constant Glib.Properties.Property_Boolean;

   Rate_Limit_Property : constant Glib.Properties.Property_Int;

   -------------
   -- Signals --
   -------------

   type Cb_Gfile_Monitor_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void is not null access procedure
     (Self       : access Gfile_Monitor_Record'Class;
      File       : Glib.GFile.Gfile;
      Other_File : Glib.GFile.Gfile;
      Event_Type : Glib.GEnums.GFile_Monitor_Event);

   type Cb_GObject_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void is not null access procedure
     (Self       : access Glib.Object.GObject_Record'Class;
      File       : Glib.GFile.Gfile;
      Other_File : Glib.GFile.Gfile;
      Event_Type : Glib.GEnums.GFile_Monitor_Event);

   Signal_Changed : constant Glib.Signal_Name := "changed";
   procedure On_Changed
      (Self  : not null access Gfile_Monitor_Record;
       Call  : Cb_Gfile_Monitor_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void;
       After : Boolean := False);
   procedure On_Changed
      (Self  : not null access Gfile_Monitor_Record;
       Call  : Cb_GObject_Gfile_Gfile_Or_Null_GFile_Monitor_Event_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when File has been changed.
   --
   --  If using Glib.GEnums.G_File_Monitor_Watch_Moves on a directory monitor,
   --  and the information is available (and if supported by the backend),
   --  Event_Type may be Glib.GEnums.Renamed, Glib.GEnums.Moved_In or
   --  Glib.GEnums.Moved_Out.
   --
   --  In all cases File will be a child of the monitored directory. For
   --  renames, File will be the old name and Other_File is the new name. For
   --  "moved in" events, File is the name of the file that appeared and
   --  Other_File is the old name that it was moved from (in another
   --  directory). For "moved out" events, File is the name of the file that
   --  used to be in this directory and Other_File is the name of the file at
   --  its new location.
   --
   --  It makes sense to treat Glib.GEnums.Moved_In as equivalent to
   --  Glib.GEnums.Created and Glib.GEnums.Moved_Out as equivalent to
   --  Glib.GEnums.Deleted, with extra information. Glib.GEnums.Renamed is
   --  equivalent to a delete/create pair. This is exactly how the events will
   --  be reported in the case that the Glib.GEnums.G_File_Monitor_Watch_Moves
   --  flag is not in use.
   --
   --  If using the deprecated flag Glib.GEnums.G_File_Monitor_Send_Moved flag
   --  and Event_Type is Glib.GEnums.Moved, File will be set to a
   --  Glib.GFile.Gfile containing the old path, and Other_File will be set to
   --  a Glib.GFile.Gfile containing the new path.
   --
   --  In all the other cases, Other_File will be set to NULL.
   -- 
   --  Callback parameters:
   --    --  @param File a Glib.GFile.Gfile.
   --    --  @param Other_File a Glib.GFile.Gfile or NULL.
   --    --  @param Event_Type a Glib.GEnums.GFile_Monitor_Event.

private
   Rate_Limit_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("rate-limit");
   Cancelled_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("cancelled");
end Glib.File_Monitors;
