-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
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
with Gdk; use Gdk;
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

   procedure Gtk_New (Widget : out Gtk_Status_Bar) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_statusbar_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   --------------------
   -- Get_Context_Id --
   --------------------

   function Get_Context_Id (Statusbar : in Gtk_Status_Bar'Class;
                            Context_Description : in String)
                            return Context_Id
   is
      function Internal (Statusbar : in System.Address;
                         Context_Description : in String)
                         return Context_Id;
      pragma Import (C, Internal, "gtk_statusbar_get_context_id");
   begin
      return Internal (Get_Object (Statusbar),
                       Context_Description & Ascii.NUL);
   end Get_Context_Id;

   ------------------
   -- Get_Messages --
   ------------------

   function Get_Messages (Statusbar : in Gtk_Status_Bar'Class)
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

   ----------
   -- Push --
   ----------

   function Push
     (Statusbar : in Gtk_Status_Bar'Class;
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
      return Internal (Get_Object (Statusbar), Context, Text & Ascii.NUL);
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (Statusbar : in Gtk_Status_Bar'Class;
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

   procedure Remove (Statusbar : in Gtk_Status_Bar'Class;
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


end Gtk.Status_Bar;
