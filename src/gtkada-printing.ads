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

--  <description>
--
--  This package provides a ready to use high level printing object.
--
--  To use this package,
--
--      ??? to be completed
--
--  </description>
--  <group>Miscellaneous</group>

with System;

with Glib; use Glib;
with Glib.Error;

with Gtk.Page_Setup;      use Gtk.Page_Setup;
with Gtk.Print_Context;   use Gtk.Print_Context;
with Gtk.Print_Operation; use Gtk.Print_Operation;
with Gtk.Window;

package Gtkada.Printing is

   type Gtkada_Print_Operation_Record is new
     Gtk_Print_Operation_Record with private;
   type Gtkada_Print_Operation is access all Gtkada_Print_Operation_Record;

   procedure Gtk_New (Op : out Gtkada_Print_Operation);
   procedure Initialize (Widget : access Gtkada_Print_Operation_Record'Class);
   --  Initialize the print operation

   function Connect_And_Run
     (Op        : access Gtkada_Print_Operation_Record'Class;
      Action    : Gtk_Print_Operation_Action;
      Parent    : access Gtk.Window.Gtk_Window_Record'Class;
      Error     : Glib.Error.GError := null)
      return Gtk_Print_Operation_Result;
   --  Runs the print operation, using the handlers installed in Op.
   --  See Gtk.Print_Operations.Run.

   -----------------
   -- begin-print --
   -----------------

   type Begin_Print_Handler is access procedure
     (Op      : Gtkada_Print_Operation;
      Context : Gtk_Print_Context);
   --  Called after the user has finished changing print settings in the
   --  dialog, before the actual rendering starts.
   --
   --  A typical use is to use the parameters from the
   --  Gtk_Print_Context and paginate the document accordingly, and then
   --  set the number of pages with Gtk.Print_Operation.Set_N_Pages.

   procedure Install_Begin_Print_Handler
     (Op      : access Gtkada_Print_Operation_Record'Class;
      Handler : Begin_Print_Handler);
   --  Install a Begin_Print_Handler.

   ----------
   -- done --
   ----------

   type Done_Handler is access procedure
     (Op     : Gtkada_Print_Operation;
      Result : Gtk_Print_Operation_Result);
   --  Called when the print operation run has finished doing everything
   --  required for printing.
   --
   --  Result gives you information about what happened during the run.
   --  If Result is Result_Error then you can call Get_Error for more
   --  information.
   --
   --  If you enabled print status tracking then
   --  Gtk.Print_Operation.Is_Finished may still return False after
   --  done was emitted.

   procedure Install_Done_Handler
     (Op      : access Gtkada_Print_Operation_Record'Class;
      Handler : Done_Handler);
   --  Install a Done_Handler.

   ---------------
   -- draw-page --
   ---------------

   type Draw_Page_Handler is access procedure
     (Op          : Gtkada_Print_Operation;
      Context     : Gtk_Print_Context;
      Page_Number : Gint);
   --  Called for every page that is printed. This handler must render the
   --  page Page_Number onto the cairo context obtained from Context using
   --  Gtk.Print_Context.Get_Cairo_Context.
   --
   --  Use Gtk.Print_Operation.Set_Use_Full_Page and
   --  Gtk.Print_Operation.Set_Unit before starting the print operation to set
   --  up the transformation of the cairo context according to your needs.

   procedure Install_Draw_Page_Handler
     (Op      : access Gtkada_Print_Operation_Record'Class;
      Handler : Draw_Page_Handler);
   --  Install a Draw_Page_Handler.

   ---------------
   -- end-print --
   ---------------

   type End_Print_Handler is access procedure
     (Op      : Gtkada_Print_Operation;
      Context : Gtk_Print_Context);
   --  Called after all pages have been rendered.
   --
   --  This handler can clean up any resources that have been allocated
   --  in the ::begin-print handler.

   procedure Install_End_Print_Handler
     (Op      : access Gtkada_Print_Operation_Record'Class;
      Handler : End_Print_Handler);
   --  Install an End_Print_Handler.

   --------------
   -- paginate --
   --------------

   type Paginate_Handler is access function
     (Op      : Gtkada_Print_Operation;
      Context : Gtk_Print_Context)
     return Boolean;
   --  Called after the ::begin-print signal, but before the actual rendering
   --  starts. It keeps getting emitted until a connected signal handler
   --  returns True.
   --
   --  The ::paginate signal is intended to be used for paginating a document
   --  in small chunks, to avoid blocking the user interface for a long
   --  time. The signal handler should update the number of pages using
   --  Gtk.Print_Operation.Set_N_Pages, and return True if the document
   --  has been completely paginated.
   --
   --  If you don't need to do pagination in chunks, you can simply do
   --  it all in the ::begin-print handler, and set the number of pages
   --  from there.

   procedure Install_Paginate_Handler
     (Op      : access Gtkada_Print_Operation_Record'Class;
      Handler : Paginate_Handler);
   --  Install a Paginate_Handler.

   ------------------------
   -- request-page-setup --
   ------------------------

   type Request_Page_Setup_Handler is access procedure
     (Op          : Gtkada_Print_Operation;
      Context     : Gtk_Print_Context;
      Page_Number : Gint;
      Setup       : Gtk_Page_Setup;
      User_Data   : System.Address);
   --  Called once for every page that is printed, to give the application
   --  a chance to modify the page setup. Any changes done to setup will be
   --  in force only for printing this page.

   procedure Install_Request_Page_Setup_Handler
     (Op      : access Gtkada_Print_Operation_Record'Class;
      Handler : Request_Page_Setup_Handler);
   --  Install a Request_Page_Setup_Handler.

   --------------------
   -- status-changed --
   --------------------

   type Status_Changed_Handler is access procedure
     (Op : Gtkada_Print_Operation);
   --  Called between the various phases of the print operation.
   --  See Gtk_Print_Status for the phases that are being discriminated.
   --  Use Gtk.Print_Operation.Get_Status to find out the current
   --  status.

   procedure Install_Status_Changed_Handler
     (Op      : access Gtkada_Print_Operation_Record'Class;
      Handler : Status_Changed_Handler);
   --  Install a Status_Changed_Handler.

private

   type Gtkada_Print_Operation_Record is
     new Gtk_Print_Operation_Record
   with record
      Begin_Print        : Begin_Print_Handler        := null;
      Done               : Done_Handler               := null;
      Draw_Page          : Draw_Page_Handler          := null;
      End_Print          : End_Print_Handler          := null;
      Paginate           : Paginate_Handler           := null;
      Request_Page_Setup : Request_Page_Setup_Handler := null;
      Status_Changed     : Status_Changed_Handler     := null;
   end record;

end Gtkada.Printing;
