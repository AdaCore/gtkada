-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
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

with Glib.Object; use Glib.Object;
with Glib.Values; use Glib.Values;

with Gtk.Arguments; use Gtk.Arguments;

with Gtk.Handlers;

package body Gtkada.Printing is

   package Object_Callback is new Gtk.Handlers.Callback
     (Gtkada_Print_Operation_Record);

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Draw_Page_Handler_Wrapper
     (Op   : access Gtkada_Print_Operation_Record'Class;
      Args : Glib.Values.GValues);
   --  Wrapper around callback for "draw-page".

   --  ??? Other wrappers go here

   -------------------------------
   -- Draw_Page_Handler_Wrapper --
   -------------------------------

   procedure Draw_Page_Handler_Wrapper
     (Op   : access Gtkada_Print_Operation_Record'Class;
      Args : Glib.Values.GValues)
   is
      Context_Addr : constant System.Address := To_Address (Args, 1);
      Context_Stub : Gtk_Print_Context_Record;
      Context  : constant Gtk_Print_Context :=
        Gtk_Print_Context (Get_User_Data (Context_Addr, Context_Stub));
      Page_Num : constant Gint := To_Gint (Args, 2);

   begin
      if Op.Draw_Page /= null then
         Op.Draw_Page (Gtkada_Print_Operation (Op), Context, Page_Num);
      end if;
   end Draw_Page_Handler_Wrapper;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Op : out Gtkada_Print_Operation) is
   begin
      Op := new Gtkada_Print_Operation_Record;
      Gtkada.Printing.Initialize (Op);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Gtkada_Print_Operation_Record'Class) is
   begin
      Gtk.Print_Operation.Initialize (Widget);
   end Initialize;

   -------------------------------
   -- Install_Draw_Page_Handler --
   -------------------------------

   procedure Install_Draw_Page_Handler
     (Op      : access Gtkada_Print_Operation_Record'Class;
      Handler : Draw_Page_Handler)
   is
   begin
      Op.Draw_Page := Handler;
   end Install_Draw_Page_Handler;

   ---------
   -- Run --
   ---------

   function Connect_And_Run
     (Op        : access Gtkada_Print_Operation_Record'Class;
      Action    : Gtk_Print_Operation_Action;
      Parent    : access Gtk.Window.Gtk_Window_Record'Class;
      Error     : Glib.Error.GError := null)
      return Gtk_Print_Operation_Result
   is
   begin
      --  Connect all handlers

      if Op.Draw_Page /= null then
         Object_Callback.Connect
           (Op, "draw-page", Draw_Page_Handler_Wrapper'Access);
      end if;

      --  ??? Add other connections here

      return Gtk.Print_Operation.Run
        (Gtk_Print_Operation (Op), Action, Parent, Error);
   end Connect_And_Run;

end Gtkada.Printing;
