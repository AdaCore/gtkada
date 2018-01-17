------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                       Copyright (C) 2013-2018, AdaCore                   --
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

--  <description>
--  This object adds support for opening files from OS commands, using the
--  standard Glib Application mechanism for opening those files.
--
--  This requires some support that is system-specific:
--
--  On windows, this registers a DDE server using the application's Id as name.
--  So for example com.adacore.TestGtk will register a DDE server whose name
--  is TestGtk.
--
--  On OSX, this requires the application to be part of a bundle. This bundle
--  needs to declare the file patterns that are supported by the application.
--  </description>
--  <group>Layout containers</group>

with System;

with Glib;             use Glib;
with Glib.Application;

with Gtk.Application;  use Gtk.Application;

package Gtkada.Application is

   type Gtkada_Application_Record is new Gtk_Application_Record
     with private;
   type Gtkada_Application is access all Gtkada_Application_Record'Class;

   type Gtkada_Application_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gtkada_Application_Flags);
   --  Flags used to define the behaviour of a Glib.Application.Gapplication.

   Gtkada_Application_Flags_None     : constant Gtkada_Application_Flags := 0;
   Gtkada_Application_Handles_Open   : constant Gtkada_Application_Flags := 1;
   Gtkada_Application_OSX_FullScreen : constant Gtkada_Application_Flags := 2;

   procedure Gtk_New
      (Self           : out Gtkada_Application;
       Application_Id : UTF8_String := "";
       Flags          : Glib.Application.GApplication_Flags;
       Gtkada_Flags   : Gtkada_Application_Flags);
   procedure Initialize
      (Self           : not null access Gtkada_Application_Record'Class;
       Application_Id : UTF8_String := "";
       Flags          : Glib.Application.GApplication_Flags;
       Gtkada_Flags   : Gtkada_Application_Flags);
   function Gtk_Application_New
      (Application_Id : UTF8_String := "";
       Flags          : Glib.Application.GApplication_Flags;
       Gtkada_Flags   : Gtkada_Application_Flags)
       return Gtkada_Application;

   type GFile is private;
   type GFile_Array is array (Positive range <>) of GFile;

   function Get_Path (File : GFile) return UTF8_String;

   type Cb_Gtkada_Application_Files is access procedure
     (Application : Gtkada_Application;
      Files       : GFile_Array);

   procedure On_Open
     (Self      : not null access Gtkada_Application_Record;
      Call      : Cb_Gtkada_Application_Files);
private

   type Gtkada_Application_Record is new Gtk_Application_Record
     with null record;

   type GFile is new System.Address;

   pragma Convention (C, GFile_Array);

end Gtkada.Application;
