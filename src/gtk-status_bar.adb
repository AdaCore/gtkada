-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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

with System;
with Gtk.Util; use Gtk.Util;
with Unchecked_Conversion;

package body Gtk.Status_Bar is

   -------------
   -- Convert --
   -------------

   function Convert (Msg : Status_Bar_Msg) return System.Address is
   begin
      return Msg'Address;
      --  This function is anyway not supposed to be used
   end Convert;

   function Convert (Msg : System.Address) return Status_Bar_Msg is
      type Status_Bar_Msg_Access is access all Status_Bar_Msg;
      function Internal is new Unchecked_Conversion (System.Address,
                                                     Status_Bar_Msg_Access);
   begin
      return Internal (Msg).all;
   end Convert;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Statusbar : out Gtk_Status_Bar) is
   begin
      Statusbar := new Gtk_Status_Bar_Record;
      Initialize (Statusbar);
   end Gtk_New;

   --------------------
   -- Get_Context_Id --
   --------------------

   function Get_Context_Id (Statusbar : access Gtk_Status_Bar_Record;
                            Context_Description : in String)
                            return Context_Id
   is
      function Internal (Statusbar : in System.Address;
                         Context_Description : in String)
                         return Context_Id;
      pragma Import (C, Internal, "gtk_statusbar_get_context_id");
   begin
      return Internal (Get_Object (Statusbar),
                       Context_Description & ASCII.Nul);
   end Get_Context_Id;

   ------------------
   -- Get_Messages --
   ------------------

   function Get_Messages (Statusbar : access Gtk_Status_Bar_Record)
                          return Messages_List.GSlist
   is
      function Internal (Statusbar : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "ada_status_get_messages");
      List : Messages_List.GSlist;
   begin
      Messages_List.Set_Object (List, Internal (Get_Object (Statusbar)));
      return List;
   end Get_Messages;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Statusbar : access Gtk_Status_Bar_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_statusbar_new");
   begin
      Set_Object (Statusbar, Internal);
      Initialize_User_Data (Statusbar);
   end Initialize;

   ----------
   -- Push --
   ----------

   function Push
     (Statusbar : access Gtk_Status_Bar_Record;
      Context   : in Context_Id;
      Text      : in String)
      return Message_Id
   is
      function Internal
        (Statusbar : in System.Address;
         Context   : in Context_Id;
         Text      : in String)
         return Message_Id;
      pragma Import (C, Internal, "gtk_statusbar_push");
   begin
      return Internal (Get_Object (Statusbar), Context, Text & ASCII.Nul);
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (Statusbar : access Gtk_Status_Bar_Record;
                  Context   : in Context_Id)
   is
      procedure Internal (Statusbar : in System.Address;
                          Context   : in Context_Id);
      pragma Import (C, Internal, "gtk_statusbar_pop");
   begin
      Internal (Get_Object (Statusbar), Context);
   end Pop;

   ------------
   -- Remove --
   ------------

   procedure Remove (Statusbar : access Gtk_Status_Bar_Record;
                     Context   : in Context_Id;
                     Message   : in Message_Id)
   is
      procedure Internal (Statusbar : in System.Address;
                          Context   : in Context_Id;
                          Message   : in Message_Id);
      pragma Import (C, Internal, "gtk_statusbar_remove");
   begin
      Internal (Get_Object (Statusbar), Context, Message);
   end Remove;

   --------------
   -- Generate --
   --------------

   procedure Generate (N         : in Node_Ptr;
                       File      : in File_Type) is
   begin
      Gen_New (N, "Status_Bar", File => File);
      Box.Generate (N, File);
   end Generate;

   procedure Generate (Statusbar : in out Gtk_Object;
                       N         : in Node_Ptr) is
   begin
      if not N.Specific_Data.Created then
         Gtk_New (Gtk_Status_Bar (Statusbar));
         Set_Object (Get_Field (N, "name"), Statusbar);
         N.Specific_Data.Created := True;
      end if;

      Box.Generate (Statusbar, N);
   end Generate;

end Gtk.Status_Bar;
