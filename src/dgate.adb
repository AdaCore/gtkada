-----------------------------------------------------------------------
--                   DGATE - GtkAda Components                       --
--                                                                   --
--                      Copyright (C) 1999                           --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- Dynagate is free software;  you can redistribute it and/or modify --
-- it under the terms of the GNU General Public License as published --
-- by the Free Software Foundation; either version 2 of the License, --
-- or (at your option) any later version.                            --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------
 
--  DGATE stands for Dynamic GATE.
--  Parse a Glade's XML project file, declare the required callbacks and
--  create the widgets associated with the project file.
--  Dyngate can very easily be used in conjunction with GLADE to test during
--  the development.
 
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Glib.Glade;
with Gtk; use Gtk;
with Gtk.Glade; use Gtk.Glade;
with Gtk.Main;
with Gtk.Util; use Gtk.Util;
with System;
with DGate_Callbacks;
with Unchecked_Conversion;
 
procedure DGate is

   use Glib;
   use Glib.Glade;
   use Glib.Glade.Glib_XML;

   N : Node_Ptr;
 
   type String_Access is access all String;
   for String_Access'Size use Standard'Address_Size;

   function To_Address is new Unchecked_Conversion
     (String_Access, System.Address);
 
   procedure Register_Signals (N : Node_Ptr);
   --  Call Set_Signal for each signal declared in the N tree.
 
   procedure Usage;

   procedure Register_Signals (N : Node_Ptr) is
      P       : Node_Ptr;
      Name    : String_Ptr;
      Handler : String_Ptr;
      S       : String_Access;
 
   begin
      if N.Tag.all = "signal" then
         Name := Get_Field (N, "name");
         Handler := Get_Field (N, "handler");

         if Name /= null then
            S := new String '(Name.all & ':' & Handler.all);
            Gtk.Util.Set_Signal
              (Handler.all, DGate_Callbacks.Generic_Callback'Access,
               To_Address (S));
            return;
         end if;
      end if;
 
      if N.Child /= null then
         Register_Signals (N.Child);
         P := N.Child.Next;
 
         while P /= null loop
            Register_Signals (P);
            P := P.Next;
         end loop;
      end if;
   end Register_Signals;

   procedure Usage is
   begin
      Ada.Text_IO.Put_Line ("Usage: dgate project-file");
   end Usage;

begin
   if Argument_Count = 0 then
      Usage;
   else
      Gtk.Main.Set_Locale;
      Gtk.Main.Init;
      N := Parse (Argument (1));
      Register_Signals (N);
      Instanciate (N);
      Gtk.Main.Main;
   end if;

exception
   when others =>
      Put_Line
        ("DGATE: Internal error. Please send a bug report with the XML");
      Put_Line ("file " & Argument (1) & " and the GtkAda version to " &
        "gtkada@ada.eu.org");
end DGate;
