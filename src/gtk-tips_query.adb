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

with System;

package body Gtk.Tips_Query is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Tips_Query) is
   begin
      Widget := new Gtk_Tips_Query_Record;
      Gtk.Tips_Query.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Tips_Query_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_tips_query_new");

   begin
      Set_Object (Widget, Internal);
   end Initialize;

   ----------------
   -- Set_Caller --
   ----------------

   procedure Set_Caller
      (Tips_Query : access Gtk_Tips_Query_Record;
       Caller     : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Tips_Query : System.Address;
         Caller     : System.Address);
      pragma Import (C, Internal, "gtk_tips_query_set_caller");

   begin
      Internal (Get_Object (Tips_Query), Get_Object (Caller));
   end Set_Caller;

   ----------------
   -- Set_Labels --
   ----------------

   procedure Set_Labels
     (Tips_Query     : access Gtk_Tips_Query_Record;
      Label_Inactive : UTF8_String;
      Label_No_Tip   : UTF8_String)
   is
      procedure Internal
        (Tips_Query     : System.Address;
         Label_Inactive : UTF8_String;
         Label_No_Tip   : UTF8_String);
      pragma Import (C, Internal, "gtk_tips_query_set_labels");

   begin
      Internal (Get_Object (Tips_Query),
                Label_Inactive & ASCII.NUL,
                Label_No_Tip & ASCII.NUL);
   end Set_Labels;

   -----------------
   -- Start_Query --
   -----------------

   procedure Start_Query (Tips_Query : access Gtk_Tips_Query_Record) is
      procedure Internal (Tips_Query : System.Address);
      pragma Import (C, Internal, "gtk_tips_query_start_query");

   begin
      Internal (Get_Object (Tips_Query));
   end Start_Query;

   ----------------
   -- Stop_Query --
   ----------------

   procedure Stop_Query (Tips_Query : access Gtk_Tips_Query_Record) is
      procedure Internal (Tips_Query : System.Address);
      pragma Import (C, Internal, "gtk_tips_query_stop_query");

   begin
      Internal (Get_Object (Tips_Query));
   end Stop_Query;

end Gtk.Tips_Query;
